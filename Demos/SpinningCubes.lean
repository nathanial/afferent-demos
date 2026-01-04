/-
  Spinning Cubes Demo
  Demonstrates 3D rendering with perspective projection, depth testing, and lighting.
  Shows a 5x5 grid of colored cubes rotating at different phases.
-/
import Afferent

open Afferent Afferent.FFI Afferent.Render CanvasM
open Linalg

namespace Demos

/-- Render spinning cubes with a given view matrix.
    Internal helper used by both static and FPS camera versions. -/
private def renderCubesWithView (renderer : Renderer) (t : Float)
    (proj view : Mat4) : IO Unit := do
  -- Light direction (normalized, pointing from upper-right-front)
  let lightDir := #[0.5, 0.7, 0.5]

  -- Draw 5x5 grid of cubes
  for row in [:5] do
    for col in [:5] do
      -- Position in grid
      let x := (col.toFloat - 2.0) * 2.0
      let y := (row.toFloat - 2.0) * 2.0

      -- Phase offset for staggered rotation
      let phase := (row * 5 + col).toFloat * 0.25

      -- Build model matrix: translate then rotate
      let translateMat := Mat4.translation x y 0
      let rotateYMat := Mat4.rotationY (t + phase)
      let rotateXMat := Mat4.rotationX (t * 0.7 + phase)

      -- Combine: model = translate * rotateY * rotateX
      let model := translateMat * rotateYMat * rotateXMat

      -- MVP = proj * view * model
      let viewModel := view * model
      let mvp := proj * viewModel

      -- Draw the cube
      Renderer.drawMesh3D renderer
        Mesh.cubeVertices
        Mesh.cubeIndices
        mvp.toArray
        model.toArray
        lightDir
        0.5  -- ambient light factor

/-- Render a 5x5 grid of spinning cubes with static camera.
    t: elapsed time in seconds
    renderer: the FFI renderer
    screenWidth/screenHeight: for aspect ratio calculation -/
def renderSpinningCubes (renderer : Renderer) (t : Float) (screenWidth screenHeight : Float) : IO Unit := do
  let aspect := screenWidth / screenHeight
  let fovY := Float.pi / 4.0  -- 45 degrees
  let proj := Mat4.perspective fovY aspect 0.1 100.0
  let view := Mat4.lookAt ⟨0, 0, 12⟩ ⟨0, 0, 0⟩ Vec3.unitY
  renderCubesWithView renderer t proj view

/-- Render a 5x5 grid of spinning cubes with FPS camera.
    t: elapsed time in seconds
    renderer: the FFI renderer
    screenWidth/screenHeight: for aspect ratio calculation
    camera: FPS camera state -/
def renderSpinningCubesWithCamera (renderer : Renderer) (t : Float)
    (screenWidth screenHeight : Float) (camera : FPSCamera) : IO Unit := do
  let aspect := screenWidth / screenHeight
  let fovY := Float.pi / 4.0  -- 45 degrees
  let proj := Mat4.perspective fovY aspect 0.1 100.0
  let view := camera.viewMatrix
  renderCubesWithView renderer t proj view

def stepSpinningCubesFrame (c : Canvas) (t dt : Float) (keyCode : UInt16) (screenScale : Float)
    (fontMedium fontSmall : Afferent.Font) (camera : FPSCamera) : IO (Canvas × FPSCamera) := do
  let mut fpsCamera := camera
  let mut locked ← FFI.Window.getPointerLock c.ctx.window
  if keyCode == FFI.Key.escape then
    FFI.Window.setPointerLock c.ctx.window (!locked)
    locked := !locked
    c.clearKey
  else if !locked then
    let click ← FFI.Window.getClick c.ctx.window
    match click with
    | some ce =>
      FFI.Window.clearClick c.ctx.window
      if ce.button == 0 then
        FFI.Window.setPointerLock c.ctx.window true
        locked := true
    | none => pure ()

  let wDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.w
  let aDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.a
  let sDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.s
  let dDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.d
  let qDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.q
  let eDown ← FFI.Window.isKeyDown c.ctx.window FFI.Key.e

  let (dx, dy) ←
    if locked then
      FFI.Window.getMouseDelta c.ctx.window
    else
      pure (0.0, 0.0)

  fpsCamera := fpsCamera.update dt wDown sDown aDown dDown eDown qDown dx dy

  let c ← run' c do
    let (currentW, currentH) ← getCurrentSize
    let renderer ← getRenderer
    renderSpinningCubesWithCamera renderer t currentW currentH fpsCamera
    resetTransform
    setFillColor Color.white
    if locked then
      fillTextXY
        "3D Spinning Cubes - WASD+Q/E to move, mouse to look, Escape to release (Space to advance)"
        (20 * screenScale) (30 * screenScale) fontMedium
    else
      fillTextXY
        "3D Spinning Cubes - WASD+Q/E to move, click or Escape to capture mouse (Space to advance)"
        (20 * screenScale) (30 * screenScale) fontMedium

    fillTextXY
      (s!"lock={locked} dt={dt} w={wDown} a={aDown} s={sDown} d={dDown} q={qDown} e={eDown} dx={dx} dy={dy} pos=({fpsCamera.x},{fpsCamera.y},{fpsCamera.z}) yaw={fpsCamera.yaw} pitch={fpsCamera.pitch}")
      (20 * screenScale) (55 * screenScale) fontSmall
  pure (c, fpsCamera)

end Demos
