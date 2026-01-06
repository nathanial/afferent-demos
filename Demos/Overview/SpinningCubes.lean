/-
  SpinningCubes Widget for Overview Grid
  Shows a 5x5 grid of spinning cubes with an FPS camera.
-/
import Afferent
import Afferent.Arbor
import Demos.Demo
import Trellis

open Afferent Afferent.Arbor Afferent.FFI Afferent.Render CanvasM
open Linalg

namespace Demos

def spinningCubesWidgetName : String := "overview-spinning-cubes"

/-- State for the spinning cubes overview cell. -/
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

def spinningCubesInitialState : SpinningCubesState :=
  { camera := FPSCamera.default }

/-- Render spinning cubes with a given view matrix. -/
private def renderCubesWithView (renderer : Renderer) (t : Float)
    (proj view : Mat4) : IO Unit := do
  let lightDir := #[0.5, 0.7, 0.5]
  for row in [:5] do
    for col in [:5] do
      let x := (col.toFloat - 2.0) * 2.0
      let y := (row.toFloat - 2.0) * 2.0
      let phase := (row * 5 + col).toFloat * 0.25
      let translateMat := Mat4.translation x y 0
      let rotateYMat := Mat4.rotationY (t + phase)
      let rotateXMat := Mat4.rotationX (t * 0.7 + phase)
      let model := translateMat * rotateYMat * rotateXMat
      let viewModel := view * model
      let mvp := proj * viewModel
      Renderer.drawMesh3D renderer
        Mesh.cubeVertices
        Mesh.cubeIndices
        mvp.toArray
        model.toArray
        lightDir
        0.5

private def applyViewport (proj : Mat4) (offsetX offsetY contentW contentH fullW fullH : Float) : Mat4 :=
  Id.run do
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

/-- SpinningCubes widget for Overview grid with FPS camera support. -/
def spinningCubesOverviewWidget (t : Float) (windowW windowH : Float) (camera : FPSCamera) : WidgetBuilder := do
  namedCustom spinningCubesWidgetName (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        let renderer ← getRenderer
        let rect := layout.contentRect
        renderSpinningCubesWithCameraViewport renderer t w h rect.x rect.y windowW windowH camera
    )
  }) (style := {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  })

end Demos
