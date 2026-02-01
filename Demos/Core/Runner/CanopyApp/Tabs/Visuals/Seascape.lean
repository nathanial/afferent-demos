/-
  Demo Runner - Canopy app visuals Seascape tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Visuals.Seascape
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Afferent.Render
open Trellis

namespace Demos

structure SeascapeInputState where
  w : Bool := false
  a : Bool := false
  s : Bool := false
  d : Bool := false
  q : Bool := false
  e : Bool := false
  deriving Inhabited

structure SeascapeFullState where
  camera : FPSCamera := seascapeCamera
  locked : Bool := false
  keys : SeascapeInputState := {}
  delta : MouseDeltaData := { dx := 0.0, dy := 0.0 }
  lastTime : Float := 0.0

def seascapeTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let seascapeName ← registerComponentW "seascape"
  let clickEvents ← useClick seascapeName
  let keyEvents ← useKeyboardAll
  let mouseDeltas ← useMouseDelta

  -- Map click events to state update functions (lock pointer)
  let clickUpdates ← Event.mapM (fun _ =>
    fun (s : SeascapeFullState) =>
      if !s.locked then
        -- Note: We cannot call FFI.Window.setPointerLock in pure FRP
        -- The lock state change is tracked, actual FFI call happens in render
        { s with locked := true }
      else
        s
    ) clickEvents

  -- Map key events to state update functions
  let keyUpdates ← Event.mapM (fun data =>
    let key := data.event.key
    let isPress := data.event.isPress
    fun (s : SeascapeFullState) =>
      let newLocked := if key == .escape && isPress then !s.locked else s.locked
      let newKeys := match key with
        | .char 'w' => { s.keys with w := isPress }
        | .char 'a' => { s.keys with a := isPress }
        | .char 's' => { s.keys with s := isPress }
        | .char 'd' => { s.keys with d := isPress }
        | .char 'q' => { s.keys with q := isPress }
        | .char 'e' => { s.keys with e := isPress }
        | _ => s.keys
      { s with locked := newLocked, keys := newKeys }
    ) keyEvents

  -- Map mouse delta events to state update functions
  let deltaUpdates ← Event.mapM (fun delta =>
    fun (s : SeascapeFullState) => { s with delta := delta }
    ) mouseDeltas

  -- Map elapsed time to camera updates
  let timeUpdates ← Event.mapM (fun t =>
    fun (s : SeascapeFullState) =>
      let dt := if s.lastTime == 0.0 then 0.0 else max 0.0 (t - s.lastTime)
      let dx := if s.locked then s.delta.dx else 0.0
      let dy := if s.locked then s.delta.dy else 0.0
      let camera := s.camera.update dt s.keys.w s.keys.s s.keys.a s.keys.d s.keys.e s.keys.q dx dy
      { s with camera := camera, lastTime := t }
    ) elapsedTime.updated

  -- Merge all updates and fold into state
  let allUpdates ← Event.mergeAllListM [clickUpdates, keyUpdates, deltaUpdates, timeUpdates]
  let state ← foldDyn (fun f s => f s) ({} : SeascapeFullState) allUpdates

  let _ ← dynWidget state fun s => do
    -- Handle pointer lock FFI call when lock state changes
    SpiderM.liftIO do
      FFI.Window.setPointerLock env.window s.locked
    let (windowW, windowH) ← SpiderM.liftIO do
      let (w, h) ← FFI.Window.getSize env.window
      pure (w.toFloat, h.toFloat)
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    let seascapeState : SeascapeState := { camera := s.camera, locked := s.locked }
    emit (pure (namedColumn seascapeName 0 containerStyle #[
      seascapeWidget s.lastTime env.screenScale windowW windowH env.fontMedium env.fontSmall seascapeState
    ]))
  pure ()

end Demos
