/-
  WidgetPerf Bench Tests
  Benchmark full event pipeline for 1000 switch widgets (no GPU draw).
-/
import Crucible
import Reactive
import Afferent
import Afferent.Arbor
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.WidgetPerf.App
import Trellis
import Linalg.Geometry.AABB2D
import Linalg.Vec2

namespace AfferentDemosTests.WidgetPerfBench

open Crucible
open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Demos.WidgetPerf
open Trellis

private def nanosToMs (n : Nat) : Float :=
  n.toFloat / 1000000.0

private def deltaMs (start stop : Nat) : Float :=
  nanosToMs (stop - start)

private def fmtMs (v : Float) : String :=
  let scaled := (v * 10.0).toUInt32.toFloat / 10.0
  s!"{scaled}"

private def fmtNanosMs (nanos : Nat) : String :=
  fmtMs (nanos.toFloat / 1000000.0)

private def fmtAvgNanosMs (nanos : Nat) (count : Nat) : String :=
  if count == 0 then "0.0" else fmtMs (nanos.toFloat / count.toFloat / 1000000.0)

private structure BenchConfig where
  warmupFrames : Nat := 5
  sampleFrames : Nat := 20
  screenW : Float := 2000.0
  screenH : Float := 1200.0
  dt : Float := 1.0 / 60.0
  withHover : Bool := true

deriving Inhabited

private structure BenchAccum where
  frames : Nat := 0
  updateMs : Float := 0
  buildMs : Float := 0
  measureMs : Float := 0
  layoutMs : Float := 0
  hitIndexMs : Float := 0
  collectMs : Float := 0
  hitTestMs : Float := 0
  hoverMs : Float := 0

deriving Inhabited

private def BenchAccum.add (acc : BenchAccum)
    (update build measure layout hitIndex collect hitTest hover : Float) : BenchAccum :=
  { acc with
    frames := acc.frames + 1
    updateMs := acc.updateMs + update
    buildMs := acc.buildMs + build
    measureMs := acc.measureMs + measure
    layoutMs := acc.layoutMs + layout
    hitIndexMs := acc.hitIndexMs + hitIndex
    collectMs := acc.collectMs + collect
    hitTestMs := acc.hitTestMs + hitTest
    hoverMs := acc.hoverMs + hover
  }

private def avg (sum : Float) (frames : Nat) : Float :=
  if frames == 0 then 0 else sum / frames.toFloat

private structure BenchResult where
  frames : Nat
  targetCount : Nat
  widgetCount : Nat
  layoutNodeCount : Nat
  updateMs : Float
  buildMs : Float
  measureMs : Float
  layoutMs : Float
  hitIndexMs : Float
  collectMs : Float
  hitTestMs : Float
  hoverMs : Float

deriving Inhabited

private def BenchResult.totalMs (r : BenchResult) : Float :=
  r.updateMs + r.buildMs + r.measureMs + r.layoutMs +
    r.hitIndexMs + r.collectMs + r.hitTestMs + r.hoverMs

private def BenchResult.format (label : String) (r : BenchResult) : String :=
  s!"{label}: frames={r.frames}, targets={r.targetCount}, widgets={r.widgetCount}, nodes={r.layoutNodeCount}, " ++
  s!"update={fmtMs r.updateMs}ms, build={fmtMs r.buildMs}ms, measure={fmtMs r.measureMs}ms, " ++
  s!"layout={fmtMs r.layoutMs}ms, " ++
  s!"hitIndex={fmtMs r.hitIndexMs}ms, collect={fmtMs r.collectMs}ms, " ++
  s!"hitTest={fmtMs r.hitTestMs}ms, hover={fmtMs r.hoverMs}ms, total={fmtMs r.totalMs}ms"

private def BenchResult.diff (base next : BenchResult) : BenchResult :=
  { frames := next.frames
    targetCount := next.targetCount
    widgetCount := next.widgetCount
    layoutNodeCount := next.layoutNodeCount
    updateMs := next.updateMs - base.updateMs
    buildMs := next.buildMs - base.buildMs
    measureMs := next.measureMs - base.measureMs
    layoutMs := next.layoutMs - base.layoutMs
    hitIndexMs := next.hitIndexMs - base.hitIndexMs
    collectMs := next.collectMs - base.collectMs
    hitTestMs := next.hitTestMs - base.hitTestMs
    hoverMs := next.hoverMs - base.hoverMs }

private structure BenchAssets where
  registry : FontRegistry
  fontCanopy : Font
  fontCanopySmall : Font
  theme : Theme

private def loadBenchAssets : IO BenchAssets := do
  let fontCanopy ← Font.load "/System/Library/Fonts/Monaco.ttf" 14
  let fontCanopySmall ← Font.load "/System/Library/Fonts/Monaco.ttf" 10
  let (reg1, canopyId) := FontRegistry.empty.register fontCanopy "canopy"
  let (reg2, canopySmallId) := reg1.register fontCanopySmall "canopySmall"
  let registry := reg2.setDefault fontCanopy
  let theme : Theme := { Theme.dark with font := canopyId, smallFont := canopySmallId }
  pure { registry, fontCanopy, fontCanopySmall, theme }

private def destroyBenchAssets (assets : BenchAssets) : IO Unit := do
  Font.destroy assets.fontCanopy
  Font.destroy assets.fontCanopySmall

private def widgetPerfRender (theme : Theme) (selected : WidgetType) : ReactiveM ComponentRender := do
  let (selectionEvent, fireSelection) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)
  let selectedIndex := (allWidgetTypes.findIdx? (· == selected)).getD 0
  let selectedType ← Reactive.holdDyn selectedIndex selectionEvent

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.1)
      padding := EdgeInsets.uniform 16
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 16) (style := rootStyle) do
      heading1' "Widget Performance Test" theme
      caption' "Select a widget type to render 1000 instances" theme

      let contentRowStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }
      flexRow' { FlexContainer.row 16 with alignItems := .stretch }
          (style := contentRowStyle) do
        let leftPanelStyle : BoxStyle := {
          minWidth := some 180
          height := .percent 1.0
        }
        column' (gap := 8) (style := leftPanelStyle) do
          caption' "Widget type:" theme

          let result ← listBox widgetTypeNames theme { fillHeight := true }

          let selAction ← Event.mapM (fun idx => fireSelection idx) result.onSelect
          performEvent_ selAction

          let _ ← dynWidget selectedType (fun sel =>
            caption' s!"Selected: {widgetTypeNames.getD sel "none"}" theme)
          pure ()

        let rightPanelStyle : BoxStyle := {
          flexItem := some (FlexItem.growing 1)
          width := .percent 1.0
          height := .percent 1.0
        }
        column' (gap := 0) (style := rightPanelStyle) do
          let _ ← dynWidget selectedType (fun selIdx => do
            let wtype := allWidgetTypes.getD selIdx .label
            renderWidgetGrid wtype theme)
          pure ()

  pure render

private structure BenchApp where
  render : ComponentRender
  inputs : ReactiveInputs
  spiderEnv : Reactive.Host.SpiderEnv

private def initBenchApp (theme : Theme) (selected : WidgetType) : IO BenchApp := do
  let spiderEnv ← Reactive.Host.SpiderEnv.new Reactive.Host.defaultErrorHandler
  let (render, inputs) ← (do
    let (events, inputs) ← Afferent.Canopy.Reactive.createInputs
    let render ← ReactiveM.run events (widgetPerfRender theme selected)
    pure (render, inputs)
  ).run spiderEnv
  spiderEnv.postBuildTrigger ()
  pure { render, inputs, spiderEnv }

private structure BenchFrameCache where
  widget : Widget
  layouts : Trellis.LayoutResult
  hitIndex : HitTestIndex

private def collectCentersByPrefix (index : HitTestIndex) (namePrefix : String) : Array (Float × Float) :=
  index.items.foldl (init := #[]) fun acc item =>
    match item.widget.name? with
    | some name =>
        if name.startsWith namePrefix then
          let center := Linalg.AABB2D.center item.screenBounds
          acc.push (center.x, center.y)
        else
          acc
    | none => acc

private def runBench (render : ComponentRender) (inputs : ReactiveInputs)
    (registry : FontRegistry) (config : BenchConfig) (targetPrefix : String) : IO BenchResult := do
  let renderCache ← IO.mkRef RenderCache.empty
  let totalFrames := config.warmupFrames + config.sampleFrames
  let mut cache : Option BenchFrameCache := none
  let mut hoverPoints : Array (Float × Float) := #[]
  let mut targetCount : Nat := 0
  let mut widgetCount : Nat := 0
  let mut layoutNodeCount : Nat := 0
  let mut accum : BenchAccum := {}

  for frameIdx in [0:totalFrames] do
    let mut hitTestMs := 0.0
    let mut hoverMs := 0.0
    if config.withHover then
      match cache with
      | some cached =>
          let fallback := (config.screenW / 2.0, config.screenH / 2.0)
          let point :=
            if hoverPoints.isEmpty then fallback
            else hoverPoints.getD (frameIdx % hoverPoints.size) fallback
          let (x, y) := point
          let tHit0 ← IO.monoNanosNow
          let hitPath := Afferent.Arbor.hitTestPathIndexed cached.hitIndex x y
          let tHit1 ← IO.monoNanosNow
          let hoverData : HoverData := {
            x := x
            y := y
            hitPath := hitPath
            widget := cached.widget
            layouts := cached.layouts
            nameMap := cached.hitIndex.nameMap
          }
          let tHover0 ← IO.monoNanosNow
          inputs.fireHover hoverData
          let tHover1 ← IO.monoNanosNow
          hitTestMs := deltaMs tHit0 tHit1
          hoverMs := deltaMs tHover0 tHover1
      | none => pure ()

    let tUpdate0 ← IO.monoNanosNow
    inputs.fireAnimationFrame config.dt
    let builder ← render
    let tUpdate1 ← IO.monoNanosNow

    let tBuild0 ← IO.monoNanosNow
    let widget := Afferent.Arbor.buildFrom 0 builder
    let tBuild1 ← IO.monoNanosNow

    let tMeasure0 ← IO.monoNanosNow
    let measureResult ← Afferent.runWithFonts registry
      (Afferent.Arbor.measureWidget widget config.screenW config.screenH)
    let tMeasure1 ← IO.monoNanosNow

    let tLayout0 ← IO.monoNanosNow
    let layouts := Trellis.layout measureResult.node config.screenW config.screenH
    let layoutCount := layouts.layouts.size
    let _ := layouts.layoutMap.size
    let tLayout1 ← IO.monoNanosNow

    let tHitIndex0 ← IO.monoNanosNow
    let hitIndex := Afferent.Arbor.buildHitTestIndex measureResult.widget layouts
    let tHitIndex1 ← IO.monoNanosNow

    let tCollect0 ← IO.monoNanosNow
    let _ ← Afferent.Arbor.collectCommandsCachedWithStats renderCache
      measureResult.widget layouts
    let tCollect1 ← IO.monoNanosNow

    cache := some { widget := measureResult.widget, layouts := layouts, hitIndex := hitIndex }

    if hoverPoints.isEmpty then
      hoverPoints := collectCentersByPrefix hitIndex targetPrefix
      targetCount := hoverPoints.size
    if widgetCount == 0 then
      widgetCount := measureResult.widget.widgetCount
    if layoutNodeCount == 0 then
      layoutNodeCount := layoutCount

    if frameIdx >= config.warmupFrames then
      accum := accum.add
        (deltaMs tUpdate0 tUpdate1)
        (deltaMs tBuild0 tBuild1)
        (deltaMs tMeasure0 tMeasure1)
        (deltaMs tLayout0 tLayout1)
        (deltaMs tHitIndex0 tHitIndex1)
        (deltaMs tCollect0 tCollect1)
        hitTestMs
        hoverMs

  let frames := accum.frames
  pure {
    frames := frames
    targetCount := targetCount
    widgetCount := widgetCount
    layoutNodeCount := layoutNodeCount
    updateMs := avg accum.updateMs frames
    buildMs := avg accum.buildMs frames
    measureMs := avg accum.measureMs frames
    layoutMs := avg accum.layoutMs frames
    hitIndexMs := avg accum.hitIndexMs frames
    collectMs := avg accum.collectMs frames
    hitTestMs := avg accum.hitTestMs frames
    hoverMs := avg accum.hoverMs frames
  }

open Crucible

testSuite "WidgetPerf Bench"

test "switch pipeline baseline vs hover" := do
  let hoverMetrics ← Afferent.Canopy.Reactive.enableHoverMetrics
  let dynMetrics ← Afferent.Canopy.Reactive.enableDynWidgetMetrics
  let assets ← loadBenchAssets
  let appBaseline ← initBenchApp assets.theme .switch
  let appHover ← initBenchApp assets.theme .switch

  let baseConfig : BenchConfig := { withHover := false }
  let hoverConfig : BenchConfig := { withHover := true }

  HoverMetrics.reset hoverMetrics
  DynWidgetMetrics.reset dynMetrics
  let baseline ← runBench appBaseline.render appBaseline.inputs assets.registry baseConfig "switch-"

  HoverMetrics.reset hoverMetrics
  DynWidgetMetrics.reset dynMetrics
  let hover ← runBench appHover.render appHover.inputs assets.registry hoverConfig "switch-"
  let delta := BenchResult.diff baseline hover
  let hoverSnap ← HoverMetrics.snapshot hoverMetrics
  let dynSnap ← DynWidgetMetrics.snapshot dynMetrics

  IO.println (BenchResult.format "baseline" baseline)
  IO.println (BenchResult.format "hover" hover)
  IO.println (BenchResult.format "delta(hover-baseline)" delta)
  IO.println s!"hover map: total={fmtNanosMs hoverSnap.mapNanos}ms, avg={fmtAvgNanosMs hoverSnap.mapNanos hoverSnap.mapCount}ms, count={hoverSnap.mapCount}"
  IO.println s!"hover map (switch): total={fmtNanosMs hoverSnap.mapSwitchNanos}ms, avg={fmtAvgNanosMs hoverSnap.mapSwitchNanos hoverSnap.mapSwitchCount}ms, count={hoverSnap.mapSwitchCount}"
  IO.println s!"hover update: total={fmtNanosMs hoverSnap.holdNanos}ms, avg={fmtAvgNanosMs hoverSnap.holdNanos hoverSnap.holdCount}ms, count={hoverSnap.holdCount}"
  IO.println s!"hover update (switch): total={fmtNanosMs hoverSnap.holdSwitchNanos}ms, avg={fmtAvgNanosMs hoverSnap.holdSwitchNanos hoverSnap.holdSwitchCount}ms, count={hoverSnap.holdSwitchCount}"
  IO.println s!"dynWidget rebuild: total={fmtNanosMs dynSnap.rebuildNanos}ms, avg={fmtAvgNanosMs dynSnap.rebuildNanos dynSnap.rebuildCount}ms, count={dynSnap.rebuildCount}"

  ensure (hover.targetCount == 1000)
    s!"Expected 1000 switch widgets, got {hover.targetCount}"

  Afferent.Canopy.Reactive.disableHoverMetrics
  Afferent.Canopy.Reactive.disableDynWidgetMetrics
  destroyBenchAssets assets

test "dropdown pipeline baseline vs hover" := do
  let assets ← loadBenchAssets
  let appBaseline ← initBenchApp assets.theme .dropdown
  let appHover ← initBenchApp assets.theme .dropdown

  let baseConfig : BenchConfig := { withHover := false }
  let hoverConfig : BenchConfig := { withHover := true }

  let baseline ← runBench appBaseline.render appBaseline.inputs assets.registry baseConfig "dropdown-trigger-"
  let hover ← runBench appHover.render appHover.inputs assets.registry hoverConfig "dropdown-trigger-"
  let delta := BenchResult.diff baseline hover

  IO.println (BenchResult.format "baseline (dropdown)" baseline)
  IO.println (BenchResult.format "hover (dropdown)" hover)
  IO.println (BenchResult.format "delta(hover-baseline) (dropdown)" delta)

  ensure (hover.targetCount == 1000)
    s!"Expected 1000 dropdown triggers, got {hover.targetCount}"

  destroyBenchAssets assets

test "stepper pipeline baseline vs hover" := do
  let assets ← loadBenchAssets
  let appBaseline ← initBenchApp assets.theme .stepper
  let appHover ← initBenchApp assets.theme .stepper

  let baseConfig : BenchConfig := { withHover := false }
  let hoverConfig : BenchConfig := { withHover := true }

  let baseline ← runBench appBaseline.render appBaseline.inputs assets.registry baseConfig "stepper-dec"
  let hover ← runBench appHover.render appHover.inputs assets.registry hoverConfig "stepper-dec"
  let delta := BenchResult.diff baseline hover

  IO.println (BenchResult.format "baseline (stepper)" baseline)
  IO.println (BenchResult.format "hover (stepper)" hover)
  IO.println (BenchResult.format "delta(hover-baseline) (stepper)" delta)

  ensure (hover.targetCount == 1000)
    s!"Expected 1000 stepper buttons, got {hover.targetCount}"

  destroyBenchAssets assets

#generate_tests

end AfferentDemosTests.WidgetPerfBench
