/-
  Demo Runner - Canopy app shell for demo tabs.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Core.DemoRegistry
import Demos.Overview.Card
import Demos.Overview.DemoGrid
import Demos.Overview.SpinningCubes
import Demos.Layout.Flexbox
import Demos.Layout.CssGrid
import Demos.Reactive.Showcase.App
import Demos.Perf.Circles
import Demos.Perf.Sprites
import Demos.Perf.Widget.App
import Demos.Visuals.Seascape
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

structure CanopyAppState where
  render : ComponentRender

private structure SeascapeInputState where
  w : Bool := false
  a : Bool := false
  s : Bool := false
  d : Bool := false
  q : Bool := false
  e : Bool := false
  deriving Inhabited

private def roundTo (v : Float) (places : Nat) : Float :=
  let factor := (10 : Float) ^ places.toFloat
  (v * factor).round / factor

private def formatFloat (v : Float) (places : Nat := 2) : String :=
  let s := toString (roundTo v places)
  if s.any (· == '.') then
    let s := s.dropRightWhile (· == '0')
    if s.endsWith "." then s.dropRight 1 else s
  else
    s

private def formatStatsLines (stats : RunnerStats) : Array String :=
  #[
    s!"layout {formatFloat stats.layoutMs}ms • collect {formatFloat stats.collectMs}ms • exec {formatFloat stats.executeMs}ms",
    s!"cmds {stats.commandCount} • widgets {stats.widgetCount} • layouts {stats.layoutCount}",
    s!"draws {stats.drawCalls} • batched {stats.batchedCalls} • single {stats.individualCalls}",
    s!"cache hits {stats.cacheHits} • misses {stats.cacheMisses}",
    s!"frame {formatFloat stats.frameMs}ms • {formatFloat stats.fps 1} fps"
  ]

private def demoFontsFromEnv (env : DemoEnv) : DemoFonts := {
  label := env.fontSmallId
  small := env.fontSmallId
  medium := env.fontMediumId
  large := env.fontLargeId
  huge := env.fontHugeId
}

private def overviewTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let demoFonts := demoFontsFromEnv env
  let cubes := spinningCubesInitialState
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (demoGridWidget env.screenScale t demoFonts cubes env.windowWidthF env.windowHeightF))
  pure ()

private def circlesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (particlesRef : IO.Ref Render.Dynamic.ParticleState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    let particles ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← particlesRef.get
      let next := current.updateBouncing dt env.circleRadius
      particlesRef.set next
      lastTimeRef.set t
      pure next
    emit (pure (circlesPerfWidget t env.fontMedium particles env.circleRadius))
  pure ()

private def spritesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (particlesRef : IO.Ref Render.Dynamic.ParticleState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    let particles ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← particlesRef.get
      let next := current.updateBouncing dt env.spriteHalfSize
      particlesRef.set next
      lastTimeRef.set t
      pure next
    emit (pure (spritesPerfWidget env.screenScale env.fontMedium env.spriteTexture particles env.spriteHalfSize))
  pure ()

private def layoutTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (layoutWidgetFlex env.fontMediumId env.fontSmallId env.screenScale))
  pure ()

private def cssGridTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (cssGridWidget env.fontMediumId env.fontSmallId env.screenScale))
  pure ()

private def reactiveShowcaseTabContent (appState : ReactiveShowcase.AppState) : WidgetM Unit := do
  emit appState.render

private def widgetPerfTabContent (appState : WidgetPerf.AppState) : WidgetM Unit := do
  emit appState.render

private def seascapeTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref SeascapeState) (lastTimeRef : IO.Ref Float)
    (keysRef : IO.Ref SeascapeInputState) (lockRef : IO.Ref Bool)
    (deltaRef : IO.Ref Afferent.Canopy.Reactive.MouseDeltaData) : WidgetM Unit := do
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

private def statsFooter (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let footerHeight := 110.0 * env.screenScale
  let footerStyle : BoxStyle := {
    backgroundColor := some (Color.gray 0.08)
    padding := EdgeInsets.symmetric (6.0 * env.screenScale) (4.0 * env.screenScale)
    width := .percent 1.0
    height := .length footerHeight
    flexItem := some (FlexItem.fixed footerHeight)
  }
  column' (gap := 2.0 * env.screenScale) (style := footerStyle) do
    let _ ← dynWidget elapsedTime fun _ => do
      let stats ← SpiderM.liftIO env.statsRef.get
      let lines := formatStatsLines stats
      for line in lines do
        caption' line
      pure ()
    pure ()

private def demoStubContent (id : DemoId) : WidgetM Unit := do
  let inst := demoInstance id
  filledPanel' 24 do
    heading2' inst.name
    caption' s!"{inst.shortName} • stub"
    spacer' 0 8
    bodyText' "This demo has not been migrated to Canopy yet."
    bodyText' "Replace this panel with the real demo widget."

/-- Create the demo shell as a single Canopy widget tree. -/
def createCanopyApp (env : DemoEnv) : ReactiveM CanopyAppState := do
  let elapsedTime ← useElapsedTime
  let reactiveShowcaseApp ← ReactiveShowcase.createApp env
  let widgetPerfApp ← WidgetPerf.createApp env
  let circlesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 42
    IO.mkRef particles
  let circlesTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let spritesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 123
    IO.mkRef particles
  let spritesTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let seascapeRef ← SpiderM.liftIO (IO.mkRef { camera := seascapeCamera })
  let seascapeTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let seascapeKeysRef ← SpiderM.liftIO (IO.mkRef ({} : SeascapeInputState))
  let seascapeLockRef ← SpiderM.liftIO (IO.mkRef false)
  let seascapeDeltaRef ← SpiderM.liftIO (IO.mkRef { dx := 0.0, dy := 0.0 })
  let tabs : Array TabDef := demoIds.map fun id => {
    label := (demoInstance id).shortName
    content := match id with
      | .demoGrid => overviewTabContent env elapsedTime
      | .circlesPerf => circlesTabContent env elapsedTime circlesRef circlesTimeRef
      | .spritesPerf => spritesTabContent env elapsedTime spritesRef spritesTimeRef
      | .layout => layoutTabContent env elapsedTime
      | .cssGrid => cssGridTabContent env elapsedTime
      | .reactiveShowcase => reactiveShowcaseTabContent reactiveShowcaseApp
      | .widgetPerf => widgetPerfTabContent widgetPerfApp
      | .seascape => seascapeTabContent env elapsedTime seascapeRef seascapeTimeRef
          seascapeKeysRef seascapeLockRef seascapeDeltaRef
      | _ => demoStubContent id
  }

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.08)
      padding := EdgeInsets.uniform 16
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 16) (style := rootStyle) do
      heading1' "Afferent Demos"

      let contentStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }

      column' (gap := 0) (style := contentStyle) do
        let _ ← tabView tabs 0
        pure ()
      statsFooter env elapsedTime

  pure { render := render }

end Demos
