/-
  Spinning Cubes Demo
  Demonstrates 3D rendering with perspective projection, depth testing, and lighting.
  Shows a 5x5 grid of colored cubes rotating at different phases.
-/
import Afferent
import Afferent.Arbor
import Demos.Demo
import Trellis

open Afferent Afferent.FFI Afferent.Render CanvasM
open Linalg

namespace Demos

/-- State for the spinning cubes demo. -/
structure SpinningCubesState where
  camera : Render.FPSCamera
  locked : Bool := false
  lastDx : Float := 0.0
  lastDy : Float := 0.0
  wDown : Bool := false
  aDown : Bool := false
  sDown : Bool := false
  dDown : Bool := false
  qDown : Bool := false
  eDown : Bool := false

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

private def applyViewport (proj : Mat4) (offsetX offsetY contentW contentH fullW fullH : Float) : Mat4 := Id.run do
  let sx := if fullW <= 0.0 then 1.0 else contentW / fullW
  let sy := if fullH <= 0.0 then 1.0 else contentH / fullH
  let tx := (2.0 * offsetX / fullW) + sx - 1.0
  let ty := 1.0 - (2.0 * offsetY / fullH) - sy
  let mut ndc := Mat4.identity
  ndc := ndc.set 0 0 sx
  ndc := ndc.set 1 1 sy
  ndc := ndc.set 0 3 tx
  ndc := ndc.set 1 3 ty
  ndc * proj

def renderSpinningCubesWithCameraViewport (renderer : Renderer) (t : Float)
    (contentW contentH offsetX offsetY fullW fullH : Float) (camera : FPSCamera) : IO Unit := do
  let aspect := contentW / contentH
  let fovY := Float.pi / 4.0
  let proj := Mat4.perspective fovY aspect 0.1 100.0
  let proj := applyViewport proj offsetX offsetY contentW contentH fullW fullH
  let view := camera.viewMatrix
  renderCubesWithView renderer t proj view

/-- Render spinning cubes with static camera into a viewport sub-region.
    Used for thumbnail/overview display where no FPS camera controls are needed. -/
def renderSpinningCubesViewport (renderer : Renderer) (t : Float)
    (contentW contentH offsetX offsetY fullW fullH : Float) : IO Unit := do
  let aspect := contentW / contentH
  let fovY := Float.pi / 4.0
  let proj := Mat4.perspective fovY aspect 0.1 100.0
  let proj := applyViewport proj offsetX offsetY contentW contentH fullW fullH
  let view := Mat4.lookAt ⟨0, 0, 12⟩ ⟨0, 0, 0⟩ Vec3.unitY
  renderCubesWithView renderer t proj view

def updateSpinningCubesState (env : DemoEnv) (state : SpinningCubesState) : IO SpinningCubesState := do
  let mut camera := state.camera
  let mut locked ← FFI.Window.getPointerLock env.window
  if env.keyCode == FFI.Key.escape then
    FFI.Window.setPointerLock env.window (!locked)
    locked := !locked
    env.clearKey
  let wDown ← FFI.Window.isKeyDown env.window FFI.Key.w
  let aDown ← FFI.Window.isKeyDown env.window FFI.Key.a
  let sDown ← FFI.Window.isKeyDown env.window FFI.Key.s
  let dDown ← FFI.Window.isKeyDown env.window FFI.Key.d
  let qDown ← FFI.Window.isKeyDown env.window FFI.Key.q
  let eDown ← FFI.Window.isKeyDown env.window FFI.Key.e
  let (dx, dy) ←
    if locked then
      FFI.Window.getMouseDelta env.window
    else
      pure (0.0, 0.0)
  camera := camera.update env.dt wDown sDown aDown dDown eDown qDown dx dy
  pure { state with
    camera := camera
    locked := locked
    lastDx := dx
    lastDy := dy
    wDown := wDown
    aDown := aDown
    sDown := sDown
    dDown := dDown
    qDown := qDown
    eDown := eDown
  }

def spinningCubesWidget (t : Float) (screenScale : Float) (windowW windowH : Float)
    (fontMedium fontSmall : Afferent.Font) (state : SpinningCubesState) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        let rect := layout.contentRect
        let renderer ← getRenderer
        renderSpinningCubesWithCameraViewport renderer t w h rect.x rect.y windowW windowH state.camera
        resetTransform
        setFillColor Color.white
        if state.locked then
          fillTextXY
            "3D Spinning Cubes - WASD+Q/E to move, mouse to look, Escape to release (Space to advance)"
            (20 * screenScale) (30 * screenScale) fontMedium
        else
          fillTextXY
            "3D Spinning Cubes - WASD+Q/E to move, click or Escape to capture mouse (Space to advance)"
            (20 * screenScale) (30 * screenScale) fontMedium
        fillTextXY
          (s!"lock={state.locked} w={state.wDown} a={state.aDown} s={state.sDown} d={state.dDown} q={state.qDown} e={state.eDown} dx={state.lastDx} dy={state.lastDy} pos=({state.camera.x},{state.camera.y},{state.camera.z}) yaw={state.camera.yaw} pitch={state.camera.pitch}")
          (20 * screenScale) (55 * screenScale) fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

def stepSpinningCubesFrame (c : Canvas) (t dt : Float) (keyCode : UInt16) (screenScale : Float)
    (screenWidth screenHeight : Float)
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
    let renderer ← getRenderer
    renderSpinningCubesWithCamera renderer t screenWidth screenHeight fpsCamera
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
