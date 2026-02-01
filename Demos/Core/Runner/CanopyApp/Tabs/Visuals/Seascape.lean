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

def seascapeTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let stateRef ← SpiderM.liftIO (IO.mkRef { camera := seascapeCamera })
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let keysRef ← SpiderM.liftIO (IO.mkRef ({} : SeascapeInputState))
  let lockRef ← SpiderM.liftIO (IO.mkRef false)
  let deltaRef ← SpiderM.liftIO (IO.mkRef { dx := 0.0, dy := 0.0 })
  let seascapeName ← registerComponentW "seascape"
  let clickEvents ← useClick seascapeName
  let clickAction ← Event.mapM (fun _ => do
    let locked ← lockRef.get
    if !locked then
      FFI.Window.setPointerLock env.window true
      lockRef.set true
    ) clickEvents
  performEvent_ clickAction

  let keyEvents ← useKeyboardAll
  let keyAction ← Event.mapM (fun data => do
    let key := data.event.key
    let isPress := data.event.isPress
    if key == .escape && isPress then
      let locked ← lockRef.get
      FFI.Window.setPointerLock env.window (!locked)
      lockRef.set (!locked)
    match key with
    | .char 'w' => keysRef.modify fun s => { s with w := isPress }
    | .char 'a' => keysRef.modify fun s => { s with a := isPress }
    | .char 's' => keysRef.modify fun s => { s with s := isPress }
    | .char 'd' => keysRef.modify fun s => { s with d := isPress }
    | .char 'q' => keysRef.modify fun s => { s with q := isPress }
    | .char 'e' => keysRef.modify fun s => { s with e := isPress }
    | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let mouseDeltas ← useMouseDelta
  let deltaAction ← Event.mapM (fun delta => deltaRef.set delta) mouseDeltas
  performEvent_ deltaAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← stateRef.get
      let locked ← lockRef.get
      let keys ← keysRef.get
      let delta ← deltaRef.get
      let dx := if locked then delta.dx else 0.0
      let dy := if locked then delta.dy else 0.0
      let camera := current.camera.update dt keys.w keys.s keys.a keys.d keys.e keys.q dx dy
      let next := { current with camera := camera, locked := locked }
      stateRef.set next
      lastTimeRef.set t
      pure next
    let (windowW, windowH) ← SpiderM.liftIO do
      let (w, h) ← FFI.Window.getSize env.window
      pure (w.toFloat, h.toFloat)
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn seascapeName 0 containerStyle #[
      seascapeWidget t env.screenScale windowW windowH env.fontMedium env.fontSmall state
    ]))
  pure ()

end Demos
