/-
  Demo Registry - Demo ids, states, and instances.
-/
import Demos.Core.Demo
import Demos.Overview.DemoGrid
import Demos.Overview.Card
import Demos.Layout.Flexbox
import Demos.Perf.Grid
import Demos.Perf.Triangles
import Demos.Perf.Circles
import Demos.Perf.Sprites
import Demos.Layout.CssGrid
import Demos.Overview.SpinningCubes
import Demos.Visuals.Seascape
import Demos.Visuals.ShapeGallery
import Demos.Visuals.LineCaps
import Demos.Visuals.DashedLines
import Demos.Perf.Lines
import Demos.Visuals.TextureMatrix
import Demos.Visuals.Orbital
import Demos.Visuals.Worldmap
import Demos.Overview.Fonts
import Demos.Reactive.Showcase.App
import Demos.Perf.Widget.App
import Demos.Chat.App
import Demos.Linalg.Shared
import Demos.Linalg.VectorInterpolation
import Demos.Linalg.VectorArithmetic
import Demos.Linalg.VectorProjection
import Demos.Linalg.VectorField
import Demos.Linalg.CrossProduct3D
import Demos.Linalg.Matrix2DTransform
import Demos.Linalg.Matrix3DTransform
import Demos.Linalg.ProjectionExplorer
import Demos.Linalg.MatrixDecomposition
import Demos.Linalg.QuaternionVisualizer
import Demos.Linalg.SlerpInterpolation
import Demos.Linalg.EulerGimbalLock
import Demos.Linalg.DualQuaternionBlending
import Demos.Linalg.RayCastingPlayground
import Demos.Linalg.PrimitiveOverlapTester
import Demos.Linalg.BarycentricCoordinates
import Demos.Linalg.FrustumCullingDemo
import Demos.Linalg.BezierCurveEditor
import Demos.Linalg.CatmullRomSplineEditor
import Demos.Linalg.BSplineCurveDemo
import Demos.Linalg.ArcLengthParameterization
import Demos.Linalg.BezierPatchSurface
import Demos.Linalg.EasingFunctionGallery
import Demos.Linalg.SmoothDampFollower
import Demos.Linalg.SpringAnimationPlayground
import Afferent.Canopy.Reactive
import Reactive.Host.Spider
import Worldmap
import Tileset
import Std.Data.HashMap

open Afferent

namespace Demos

initialize frameNameMapRef : IO.Ref (Std.HashMap String Afferent.Arbor.WidgetId) ← IO.mkRef {}

def setFrameNameMap (m : Std.HashMap String Afferent.Arbor.WidgetId) : IO Unit :=
  frameNameMapRef.set m

def getFrameNameMap : IO (Std.HashMap String Afferent.Arbor.WidgetId) :=
  frameNameMapRef.get

inductive DemoId where
  | demoGrid
  | gridPerf
  | trianglesPerf
  | circlesPerf
  | spritesPerf
  | layout
  | cssGrid
  | reactiveShowcase
  | widgetPerf
  | seascape
  | shapeGallery
  | worldmap
  | lineCaps
  | dashedLines
  | linesPerf
  | textureMatrix
  | orbitalInstanced
  | fontShowcase
  | chatDemo
  | vectorInterpolation
  | vectorArithmetic
  | vectorProjection
  | vectorField
  | crossProduct3D
  | matrix2DTransform
  | matrix3DTransform
  | projectionExplorer
  | matrixDecomposition
  | quaternionVisualizer
  | slerpInterpolation
  | eulerGimbalLock
  | dualQuaternionBlending
  | rayCastingPlayground
  | primitiveOverlapTester
  | barycentricCoordinates
  | frustumCullingDemo
  | bezierCurveEditor
  | catmullRomSplineEditor
  | bSplineCurveDemo
  | arcLengthParameterization
  | bezierPatchSurface
  | easingFunctionGallery
  | smoothDampFollower
  | springAnimationPlayground
  deriving Repr, BEq, Inhabited

structure CirclesState where
  particles : Render.Dynamic.ParticleState

structure SpritesState where
  particles : Render.Dynamic.ParticleState

structure DemoGridState where
  spinningCubes : SpinningCubesState

structure ShapeGalleryState where
  index : Nat

structure WorldmapState where
  mapState : Worldmap.MapState
  tileManager : Tileset.TileManager
  spiderEnv : Reactive.Host.SpiderEnv

/-- Diagnostic stats for Canopy demos. -/
structure CanopyDemoStats where
  /-- Number of subscriptions in the root scope. -/
  scopeSubscriptionCount : Nat := 0

/-- State for the reactive showcase demo, keeping the SpiderEnv alive. -/
structure ReactiveShowcaseDemoState where
  /-- The app state with render function. -/
  appState : ReactiveShowcase.AppState
  /-- The reactive events (for resetting registry each frame). -/
  events : Afferent.Canopy.Reactive.ReactiveEvents
  /-- The reactive inputs for firing events. -/
  inputs : Afferent.Canopy.Reactive.ReactiveInputs
  /-- The Spider environment (keeps subscriptions alive). -/
  spiderEnv : Reactive.Host.SpiderEnv
  /-- Cached widget from last render (updated each frame). -/
  cachedWidget : Afferent.Arbor.WidgetBuilder
  /-- Diagnostic stats (updated each frame). -/
  stats : CanopyDemoStats := {}

/-- State for the Widget perf diagnostic test. -/
structure WidgetPerfDemoState where
  appState : WidgetPerf.AppState
  /-- The reactive events (for resetting registry each frame). -/
  events : Afferent.Canopy.Reactive.ReactiveEvents
  inputs : Afferent.Canopy.Reactive.ReactiveInputs
  spiderEnv : Reactive.Host.SpiderEnv
  cachedWidget : Afferent.Arbor.WidgetBuilder
  /-- Diagnostic stats (updated each frame). -/
  stats : CanopyDemoStats := {}

/-- State for the Chat demo. -/
structure ChatDemoState where
  appState : ChatDemo.AppState
  /-- The reactive events (for resetting registry each frame). -/
  events : Afferent.Canopy.Reactive.ReactiveEvents
  inputs : Afferent.Canopy.Reactive.ReactiveInputs
  spiderEnv : Reactive.Host.SpiderEnv
  cachedWidget : Afferent.Arbor.WidgetBuilder
  /-- Diagnostic stats (updated each frame). -/
  stats : CanopyDemoStats := {}

/-- Demo state mapping by id. -/
def DemoState : DemoId → Type
  | .demoGrid => DemoGridState
  | .gridPerf => Unit
  | .trianglesPerf => Unit
  | .circlesPerf => CirclesState
  | .spritesPerf => SpritesState
  | .layout => Unit
  | .cssGrid => Unit
  | .reactiveShowcase => ReactiveShowcaseDemoState
  | .widgetPerf => WidgetPerfDemoState
  | .seascape => SeascapeState
  | .shapeGallery => ShapeGalleryState
  | .worldmap => WorldmapState
  | .lineCaps => Unit
  | .dashedLines => Unit
  | .linesPerf => Unit
  | .textureMatrix => Unit
  | .orbitalInstanced => Unit
  | .fontShowcase => Unit
  | .chatDemo => ChatDemoState
  | .vectorInterpolation => Linalg.VectorInterpolationState
  | .vectorArithmetic => Linalg.VectorArithmeticState
  | .vectorProjection => Linalg.VectorProjectionState
  | .vectorField => Linalg.VectorFieldState
  | .crossProduct3D => Linalg.CrossProduct3DState
  | .matrix2DTransform => Linalg.Matrix2DTransformState
  | .matrix3DTransform => Linalg.Matrix3DTransformState
  | .projectionExplorer => Linalg.ProjectionExplorerState
  | .matrixDecomposition => Linalg.MatrixDecompositionState
  | .quaternionVisualizer => Linalg.QuaternionVisualizerState
  | .slerpInterpolation => Linalg.SlerpInterpolationState
  | .eulerGimbalLock => Linalg.EulerGimbalLockState
  | .dualQuaternionBlending => Linalg.DualQuaternionBlendingState
  | .rayCastingPlayground => Linalg.RayCastingPlaygroundState
  | .primitiveOverlapTester => Linalg.PrimitiveOverlapTesterState
  | .barycentricCoordinates => Linalg.BarycentricCoordinatesState
  | .frustumCullingDemo => Linalg.FrustumCullingDemoState
  | .bezierCurveEditor => Linalg.BezierCurveEditorState
  | .catmullRomSplineEditor => Linalg.CatmullRomSplineEditorState
  | .bSplineCurveDemo => Linalg.BSplineCurveDemoState
  | .arcLengthParameterization => Linalg.ArcLengthParameterizationState
  | .bezierPatchSurface => Linalg.BezierPatchSurfaceState
  | .easingFunctionGallery => Linalg.EasingFunctionGalleryState
  | .smoothDampFollower => Linalg.SmoothDampFollowerState
  | .springAnimationPlayground => Linalg.SpringAnimationPlaygroundState

class Demo (id : DemoId) where
  name : String
  shortName : String  -- Short name for tab display
  msaaEnabled : Bool := true
  init : DemoEnv → IO (DemoState id)
  update : DemoEnv → DemoState id → IO (DemoState id) := fun _ s => pure s
  view : DemoEnv → DemoState id → Option Afferent.Arbor.WidgetBuilder := fun _ _ => none
  handleClick : DemoEnv → DemoState id → Afferent.Arbor.WidgetId → Array Afferent.Arbor.WidgetId →
      Afferent.FFI.ClickEvent → IO (DemoState id) := fun _ s _ _ _ => pure s
  /-- Handle click with access to computed layouts (for position-based interactions). -/
  handleClickWithLayouts : DemoEnv → DemoState id → Afferent.Arbor.WidgetId → Array Afferent.Arbor.WidgetId →
      Afferent.FFI.ClickEvent → Trellis.LayoutResult → Afferent.Arbor.Widget → IO (DemoState id) :=
      fun env s contentId hitPath click _layouts _widget => handleClick env s contentId hitPath click
  /-- Handle mouse hover (called when hovered widget changes). -/
  handleHover : DemoEnv → DemoState id → Afferent.Arbor.WidgetId → Array Afferent.Arbor.WidgetId →
      Float → Float → IO (DemoState id) := fun _ s _ _ _ _ => pure s
  /-- Handle hover with access to computed layouts (for drag interactions). -/
  handleHoverWithLayouts : DemoEnv → DemoState id → Afferent.Arbor.WidgetId → Array Afferent.Arbor.WidgetId →
      Float → Float → Trellis.LayoutResult → Afferent.Arbor.Widget → IO (DemoState id) :=
      fun env s contentId hitPath mouseX mouseY _layouts _widget => handleHover env s contentId hitPath mouseX mouseY
  /-- Handle keyboard input (called when a key is pressed). -/
  handleKey : DemoEnv → DemoState id → Afferent.Arbor.KeyEvent → IO (DemoState id) := fun _ s _ => pure s
  /-- Handle scroll wheel input (called when scroll occurs over demo content). -/
  handleScrollWithLayouts : DemoEnv → DemoState id → Array Afferent.Arbor.WidgetId →
      Afferent.Arbor.ScrollEvent → Trellis.LayoutResult → Afferent.Arbor.Widget →
      IO (DemoState id) := fun _ s _ _ _ _ => pure s
  /-- Handle mouse button release (for ending drag interactions). -/
  handleMouseUpWithLayouts : DemoEnv → DemoState id → Float → Float →
      Array Afferent.Arbor.WidgetId → Trellis.LayoutResult → Afferent.Arbor.Widget →
      IO (DemoState id) := fun _ s _ _ _ _ _ => pure s
  step : Canvas → DemoEnv → DemoState id → IO (Canvas × DemoState id)
  onExit : Canvas → DemoEnv → DemoState id → IO (DemoState id) := fun _ _ s => pure s

structure AnyDemo where
  id : DemoId
  state : DemoState id

instance : Inhabited AnyDemo :=
  ⟨{ id := .demoGrid, state := {
    spinningCubes := spinningCubesInitialState
  } }⟩

private def demoFontsFromEnv (env : DemoEnv) : DemoFonts := {
  label := env.fontSmallId,
  small := env.fontSmallId,
  medium := env.fontMediumId,
  large := env.fontLargeId,
  huge := env.fontHugeId
}

private def findWidgetIdByName (widget : Afferent.Arbor.Widget)
    (target : String) (maxDepth : Nat := 100) : Option Afferent.Arbor.WidgetId :=
  go widget target maxDepth
where
  go (widget : Afferent.Arbor.Widget) (target : String) (fuel : Nat)
      : Option Afferent.Arbor.WidgetId :=
    match fuel with
    | 0 => none  -- Depth limit reached
    | fuel' + 1 =>
      let widgetName := Afferent.Arbor.Widget.name? widget
      match widgetName with
      | some name =>
          if name == target then
            some (Afferent.Arbor.Widget.id widget)
          else
            findInChildren widget target fuel'
      | none =>
          findInChildren widget target fuel'
  findInChildren (widget : Afferent.Arbor.Widget) (target : String) (fuel : Nat)
      : Option Afferent.Arbor.WidgetId :=
    let children := Afferent.Arbor.Widget.children widget
    let rec loop (idx : Nat) : Option Afferent.Arbor.WidgetId :=
      if idx >= children.size then
        none
      else
        match children[idx]? with
        | some child =>
            match go child target fuel with
            | some result => some result
            | none => loop (idx + 1)
        | none => loop (idx + 1)
    loop 0

private def hitPathHasNamedWidget (widget : Afferent.Arbor.Widget)
    (hitPath : Array Afferent.Arbor.WidgetId) (name : String) : Bool :=
  match findWidgetIdByName widget name with
  | some wid => hitPath.any (· == wid)
  | none => false

/-- Calculate slider value from click position given the slider's layout. -/
private def calculateSliderValue (clickX : Float) (layouts : Trellis.LayoutResult)
    (widget : Afferent.Arbor.Widget) (sliderName : String) : Option Float :=
  match findWidgetIdByName widget sliderName with
  | some wid =>
      match layouts.get wid with
      | some layout =>
          let rect := layout.contentRect
          let dims := Afferent.Canopy.Slider.defaultDimensions
          -- Calculate value from click x relative to track position
          let relativeX := clickX - rect.x
          let trackWidth := dims.trackWidth
          let value := relativeX / trackWidth
          -- Clamp to 0.0-1.0
          let clampedValue := if value < 0.0 then 0.0 else if value > 1.0 then 1.0 else value
          some clampedValue
      | none => none
  | none => none

instance : Demo .demoGrid where
  name := "DEMO mode"
  shortName := "Overview"
  init := fun _ => pure {
    spinningCubes := spinningCubesInitialState
  }
  update := fun env state => do
    let nextCubes ← updateSpinningCubesState env state.spinningCubes
    pure { state with spinningCubes := nextCubes }
  view := fun env state =>
    let demoFonts := demoFontsFromEnv env
    some (demoGridWidget env.screenScale env.t demoFonts state.spinningCubes
      env.windowWidthF env.windowHeightF)
  handleClick := fun env state contentId hitPath click => do
    if click.button != 0 then
      pure state
    else
      let demoFonts := demoFontsFromEnv env
      let gridWidget :=
        Afferent.Arbor.buildFrom contentId
          (demoGridWidget env.screenScale env.t demoFonts state.spinningCubes
            env.windowWidthF env.windowHeightF)
      let clickedSpinningCubes := hitPathHasNamedWidget gridWidget hitPath spinningCubesWidgetName
      if clickedSpinningCubes then
        FFI.Window.setPointerLock env.window true
      pure state
  step := fun c _ s => pure (c, s)

instance : Demo .gridPerf where
  name := "GRID (squares) performance test"
  shortName := "Grid"
  init := fun _ => pure ()
  view := fun env _ =>
    some (gridPerfWidget env.t env.fontMedium env.gridParticles env.halfSize)
  step := fun c _ s => pure (c, s)

instance : Demo .trianglesPerf where
  name := "TRIANGLES performance test"
  shortName := "Triangles"
  init := fun _ => pure ()
  view := fun env _ =>
    some (trianglesPerfWidget env.t env.fontMedium env.gridParticles env.halfSize)
  step := fun c _ s => pure (c, s)

instance : Demo .circlesPerf where
  name := "CIRCLES (bouncing) performance test"
  shortName := "Circles"
  init := fun env => do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 42
    IO.println s!"Created {particles.count} bouncing circles"
    pure { particles := particles }
  update := fun env s => do
    let nextParticles := s.particles.updateBouncing env.dt env.circleRadius
    pure { s with particles := nextParticles }
  view := fun env s =>
    some (circlesPerfWidget env.t env.fontMedium s.particles env.circleRadius)
  step := fun c _ s => pure (c, s)

instance : Demo .spritesPerf where
  name := "SPRITES (Bunnymark) performance test"
  shortName := "Sprites"
  msaaEnabled := false
  init := fun env => do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 123
    IO.println s!"Created {particles.count} bouncing sprites"
    pure { particles := particles }
  update := fun env s => do
    let nextParticles := s.particles.updateBouncing env.dt env.spriteHalfSize
    pure { s with particles := nextParticles }
  view := fun env s =>
    some (spritesPerfWidget env.screenScale env.fontMedium env.spriteTexture s.particles env.spriteHalfSize)
  step := fun c _ s => pure (c, s)

instance : Demo .layout where
  name := "LAYOUT demo (Flexbox cards)"
  shortName := "Layout"
  init := fun _ => pure ()
  view := fun env _ =>
    some (layoutWidgetFlex env.fontMediumId env.fontSmallId env.screenScale)
  step := fun c _ s => pure (c, s)

instance : Demo .cssGrid where
  name := "CSS GRID demo (full-size)"
  shortName := "CSS Grid"
  init := fun _ => pure ()
  view := fun env _ =>
    some (cssGridWidget env.fontMediumId env.fontSmallId env.screenScale)
  step := fun c _ s => pure (c, s)

instance : Demo .reactiveShowcase where
  name := "REACTIVE widget showcase (FRP)"
  shortName := "Reactive"
  init := fun env => do
    -- Create SpiderEnv manually (don't use runFresh which disposes scope)
    let spiderEnv ← Reactive.Host.SpiderEnv.new Reactive.Host.defaultErrorHandler

    -- Create theme with demo fonts
    let theme : Afferent.Canopy.Theme := {
      Afferent.Canopy.Theme.dark with
      font := env.fontCanopyId
      smallFont := env.fontCanopySmallId
    }

    -- Run the app setup within the env
    let (appState, events, inputs) ← (do
      let (events, inputs) ← Afferent.Canopy.Reactive.createInputs env.fontRegistry theme (some env.fontCanopy)
      let appState ← Afferent.Canopy.Reactive.ReactiveM.run events (ReactiveShowcase.createApp env)
      pure (appState, events, inputs)
    ).run spiderEnv

    -- Fire post-build event (but don't dispose!)
    spiderEnv.postBuildTrigger ()

    -- Initial render
    let initialWidget ← appState.render

    pure { appState, events, inputs, spiderEnv, cachedWidget := initialWidget }

  update := fun env state => do
    -- Fire animation frame event with delta time
    state.inputs.fireAnimationFrame env.dt
    -- Re-render (samples all dynamics)
    let widget ← state.appState.render
    -- Collect diagnostic stats
    let scopeCount ← state.spiderEnv.currentScope.subscriptionCount
    let stats : CanopyDemoStats := {
      scopeSubscriptionCount := scopeCount
    }
    pure { state with cachedWidget := widget, stats := stats }

  view := fun _env state => some state.cachedWidget

  handleClickWithLayouts := fun _env state _contentId hitPath click layouts widget => do
    -- Fire click event into reactive network
    let nameMap ← getFrameNameMap
    let clickData : Afferent.Canopy.Reactive.ClickData := { click, hitPath, widget, layouts, nameMap }
    state.inputs.fireClick clickData
    pure state

  handleHoverWithLayouts := fun _env state _contentId hitPath mouseX mouseY layouts widget => do
    let nameMap ← getFrameNameMap
    let hoverData : Afferent.Canopy.Reactive.HoverData := { x := mouseX, y := mouseY, hitPath, widget, layouts, nameMap }
    state.inputs.fireHover hoverData
    pure state

  handleKey := fun _env state keyEvent => do
    let keyData : Afferent.Canopy.Reactive.KeyData := { event := keyEvent, focusedWidget := none }
    state.inputs.fireKey keyData
    pure state

  handleScrollWithLayouts := fun _env state hitPath scrollEvt layouts widget => do
    dbg_trace s!"[ReactiveShowcase] scroll event: deltaY={scrollEvt.deltaY} hitPath.size={hitPath.size}"
    let nameMap ← getFrameNameMap
    let scrollData : Afferent.Canopy.Reactive.ScrollData := { scroll := scrollEvt, hitPath, widget, layouts, nameMap }
    state.inputs.fireScroll scrollData
    pure state

  handleMouseUpWithLayouts := fun _env state mouseX mouseY hitPath layouts widget => do
    let nameMap ← getFrameNameMap
    let mouseUpData : Afferent.Canopy.Reactive.MouseButtonData := {
      x := mouseX
      y := mouseY
      button := 0  -- left button
      hitPath
      widget
      layouts
      nameMap
    }
    state.inputs.fireMouseUp mouseUpData
    pure state

  step := fun c _ s => pure (c, s)

  -- Note: We do NOT dispose the SpiderEnv on exit because the demo state
  -- is preserved and reused when the user switches back to this tab.
  -- Disposing would break all FRP subscriptions. The SpiderEnv will be
  -- cleaned up naturally when the app closes and the state is GC'd.

instance : Demo .widgetPerf where
  name := "WIDGET PERF diagnostic test"
  shortName := "Widget Perf"
  init := fun env => do
    let spiderEnv ← Reactive.Host.SpiderEnv.new Reactive.Host.defaultErrorHandler
    let theme : Afferent.Canopy.Theme := {
      Afferent.Canopy.Theme.dark with
      font := env.fontCanopyId
      smallFont := env.fontCanopySmallId
    }
    let (appState, events, inputs) ← (do
      let (events, inputs) ← Afferent.Canopy.Reactive.createInputs env.fontRegistry theme (some env.fontCanopy)
      let appState ← Afferent.Canopy.Reactive.ReactiveM.run events (WidgetPerf.createApp env)
      pure (appState, events, inputs)
    ).run spiderEnv
    spiderEnv.postBuildTrigger ()
    let initialWidget ← appState.render
    pure { appState, events, inputs, spiderEnv, cachedWidget := initialWidget }

  update := fun env state => do
    -- Reset registry to prevent memory leak from unbounded widget name growth
    state.inputs.fireAnimationFrame env.dt
    let widget ← state.appState.render
    -- Collect diagnostic stats
    let scopeCount ← state.spiderEnv.currentScope.subscriptionCount
    let stats : CanopyDemoStats := {
      scopeSubscriptionCount := scopeCount
    }
    pure { state with cachedWidget := widget, stats := stats }

  view := fun _env state => some state.cachedWidget

  handleClickWithLayouts := fun _env state _contentId hitPath click layouts widget => do
    let nameMap ← getFrameNameMap
    let clickData : Afferent.Canopy.Reactive.ClickData := { click, hitPath, widget, layouts, nameMap }
    state.inputs.fireClick clickData
    pure state

  handleHoverWithLayouts := fun _env state _contentId hitPath mouseX mouseY layouts widget => do
    let nameMap ← getFrameNameMap
    let hoverData : Afferent.Canopy.Reactive.HoverData := { x := mouseX, y := mouseY, hitPath, widget, layouts, nameMap }
    state.inputs.fireHover hoverData
    pure state

  handleScrollWithLayouts := fun _env state hitPath scrollEvt layouts widget => do
    let nameMap ← getFrameNameMap
    let scrollData : Afferent.Canopy.Reactive.ScrollData := { scroll := scrollEvt, hitPath, widget, layouts, nameMap }
    state.inputs.fireScroll scrollData
    pure state

  step := fun c _ s => pure (c, s)

/-- Get Canopy demo stats from an AnyDemo if it's a Canopy demo. -/
def getCanopyStats (d : AnyDemo) : Option CanopyDemoStats :=
  match d with
  | ⟨.reactiveShowcase, state⟩ => some state.stats
  | ⟨.widgetPerf, state⟩ => some state.stats
  | ⟨.chatDemo, state⟩ => some state.stats
  | _ => none

instance : Demo .seascape where
  name := "SEASCAPE demo (Gerstner waves)"
  shortName := "Seascape"
  msaaEnabled := false
  init := fun _ => pure { camera := Demos.seascapeCamera }
  update := fun env s => updateSeascapeState env s
  view := fun env s =>
    some (seascapeWidget env.t env.screenScale env.windowWidthF env.windowHeightF env.fontMedium env.fontSmall s)
  handleClick := fun env s _ _ click => do
    if click.button == 0 && !s.locked then
      FFI.Window.setPointerLock env.window true
      pure { s with locked := true }
    else
      pure s
  step := fun c _ s => pure (c, s)
  onExit := fun c _ s => do
    FFI.Window.setPointerLock c.ctx.window false
    pure s

instance : Demo .shapeGallery where
  name := "SHAPE GALLERY (arrow keys to navigate)"
  shortName := "Shapes"
  init := fun _ => pure { index := 0 }
  update := fun env s => do
    let mut idx := s.index
    if env.keyCode == FFI.Key.right then
      idx := (idx + 1) % Demos.shapeGalleryCount
      env.clearKey
    else if env.keyCode == FFI.Key.left then
      idx := if idx == 0 then Demos.shapeGalleryCount - 1 else idx - 1
      env.clearKey
    pure { s with index := idx }
  view := fun env s =>
    some (shapeGalleryWidget s.index env.screenScale env.fontLarge env.fontSmall env.fontMedium)
  step := fun c _ s => pure (c, s)

instance : Demo .worldmap where
  name := "WORLDMAP demo (drag to pan, scroll to zoom)"
  shortName := "Map"
  init := fun env => do
    -- Create SpiderEnv to keep reactive subscriptions alive
    let spiderEnv ← Reactive.Host.SpiderEnv.new Reactive.Host.defaultErrorHandler

    -- Create MapState using new config structure
    let config : Worldmap.MapStateConfig := {
      lat := 37.7749
      lon := -122.4194
      zoom := 12
      width := env.physWidth.toNat
      height := env.physHeight.toNat
      provider := Tileset.TileProvider.cartoDarkRetina
      fallbackParentDepth := 3
      fallbackChildDepth := 2
      fadeFrames := 12
      persistentFallbackZoom := some 1
    }
    let mapState ← Worldmap.MapState.init config

    -- Create TileManager in SpiderM context
    let tileConfig : Tileset.TileManagerConfig := {
      provider := Tileset.TileProvider.cartoDarkRetina
      diskCacheDir := "./tile_cache"
      diskCacheMaxSize := 500 * 1024 * 1024  -- 500 MB
    }
    let tileManager ← (Tileset.TileManager.new tileConfig).run spiderEnv

    pure { mapState, tileManager, spiderEnv }

  update := fun env s => do
    -- Request tiles in SpiderM context using persistent env
    let mapState ← (requestWorldmapTiles s.mapState s.tileManager).run s.spiderEnv
    let nextState ← updateWorldmapDemo env mapState s.tileManager
    pure { s with mapState := nextState }

  view := fun env s =>
    some (worldmapWidget env.screenScale env.fontMedium env.fontSmall env.windowWidthF env.windowHeightF s.mapState)
  step := fun c _ s => pure (c, s)
  onExit := fun _ _ s => do
    Tileset.TileManager.shutdown s.tileManager
    s.spiderEnv.currentScope.dispose
    pure s

instance : Demo .lineCaps where
  name := "LINE CAPS & JOINS demo"
  shortName := "Line Caps"
  init := fun _ => pure ()
  view := fun env _ =>
    some (lineCapsWidget env.screenScale env.fontSmall env.fontMedium)
  step := fun c _ s => pure (c, s)

instance : Demo .dashedLines where
  name := "DASHED LINES demo"
  shortName := "Dashed"
  init := fun _ => pure ()
  view := fun env _ =>
    some (dashedLinesWidget env.screenScale env.fontSmall env.fontMedium)
  step := fun c _ s => pure (c, s)

instance : Demo .linesPerf where
  name := "100k LINES performance test"
  shortName := "Lines"
  msaaEnabled := false
  init := fun _ => pure ()
  view := fun env _ =>
    some (linesPerfWidget env.t env.lineBuffer env.lineCount env.lineWidth
      env.fontMedium env.windowWidthF env.windowHeightF)
  step := fun c _ s => pure (c, s)

instance : Demo .textureMatrix where
  name := "TEXTURE MATRIX demo (u_matrix scaling)"
  shortName := "Textures"
  init := fun _ => pure ()
  view := fun env _ =>
    some (textureMatrixWidget env.t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.fontSmall env.spriteTexture)
  step := fun c _ s => pure (c, s)

instance : Demo .orbitalInstanced where
  name := "ORBITAL instanced demo"
  shortName := "Orbital"
  msaaEnabled := false
  init := fun _ => pure ()
  view := fun env _ =>
    some (orbitalInstancedWidget env.t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.orbitalCount env.orbitalParams env.orbitalBuffer)
  step := fun c _ s => pure (c, s)

instance : Demo .fontShowcase where
  name := "FONT SHOWCASE demo"
  shortName := "Fonts"
  init := fun _ => pure ()
  view := fun env _ =>
    some (fontShowcaseWidget env.showcaseFonts env.fontMediumId env.screenScale)
  step := fun c _ s => pure (c, s)

instance : Demo .chatDemo where
  name := "CHAT WIDGET demo"
  shortName := "Chat"
  init := fun env => do
    let spiderEnv ← Reactive.Host.SpiderEnv.new Reactive.Host.defaultErrorHandler
    let theme : Afferent.Canopy.Theme := {
      Afferent.Canopy.Theme.dark with
      font := env.fontCanopyId
      smallFont := env.fontCanopySmallId
    }
    let (appState, events, inputs) ← (do
      let (events, inputs) ← Afferent.Canopy.Reactive.createInputs env.fontRegistry theme (some env.fontCanopy)
      let appState ← Afferent.Canopy.Reactive.ReactiveM.run events (ChatDemo.createApp env)
      pure (appState, events, inputs)
    ).run spiderEnv
    spiderEnv.postBuildTrigger ()
    let initialWidget ← appState.render
    pure { appState, events, inputs, spiderEnv, cachedWidget := initialWidget }

  update := fun env state => do
    state.inputs.fireAnimationFrame env.dt
    let widget ← state.appState.render
    let scopeCount ← state.spiderEnv.currentScope.subscriptionCount
    let stats : CanopyDemoStats := { scopeSubscriptionCount := scopeCount }
    pure { state with cachedWidget := widget, stats := stats }

  view := fun _env state => some state.cachedWidget

  handleClickWithLayouts := fun _env state _contentId hitPath click layouts widget => do
    let nameMap ← getFrameNameMap
    let clickData : Afferent.Canopy.Reactive.ClickData := { click, hitPath, widget, layouts, nameMap }
    state.inputs.fireClick clickData
    pure state

  handleHoverWithLayouts := fun _env state _contentId hitPath mouseX mouseY layouts widget => do
    let nameMap ← getFrameNameMap
    let hoverData : Afferent.Canopy.Reactive.HoverData := { x := mouseX, y := mouseY, hitPath, widget, layouts, nameMap }
    state.inputs.fireHover hoverData
    pure state

  handleKey := fun _env state keyEvent => do
    let keyData : Afferent.Canopy.Reactive.KeyData := { event := keyEvent, focusedWidget := none }
    state.inputs.fireKey keyData
    pure state

  handleScrollWithLayouts := fun _env state hitPath scrollEvt layouts widget => do
    let nameMap ← getFrameNameMap
    let scrollData : Afferent.Canopy.Reactive.ScrollData := { scroll := scrollEvt, hitPath, widget, layouts, nameMap }
    state.inputs.fireScroll scrollData
    pure state

  handleMouseUpWithLayouts := fun _env state mouseX mouseY hitPath layouts widget => do
    let nameMap ← getFrameNameMap
    let mouseUpData : Afferent.Canopy.Reactive.MouseButtonData := {
      x := mouseX
      y := mouseY
      button := 0
      hitPath
      widget
      layouts
      nameMap
    }
    state.inputs.fireMouseUp mouseUpData
    pure state

  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Vector Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .vectorInterpolation where
  name := "VECTOR INTERPOLATION (space to toggle animation)"
  shortName := "Lerp"
  init := fun _ => pure Linalg.vectorInterpolationInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: space toggles animation
    if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    -- Update animation
    if state.animating then
      let newT := (state.t + env.dt * 0.5)
      state := { state with t := if newT >= 1.0 then newT - 1.0 else newT }
    pure state
  view := fun env s => some (Linalg.vectorInterpolationWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    -- Calculate world position from click using content area dimensions
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 50.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    -- Check if near any draggable point
    if Linalg.nearPoint worldPos state.vectorA 0.5 then
      pure { state with dragging := some .vectorA }
    else if Linalg.nearPoint worldPos state.vectorB 0.5 then
      pure { state with dragging := some .vectorB }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | some target =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 50.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        match target with
        | .vectorA => pure { state with vectorA := worldPos }
        | .vectorB => pure { state with vectorB := worldPos }
    | none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := none }
  step := fun c _ s => pure (c, s)

instance : Demo .vectorArithmetic where
  name := "VECTOR ARITHMETIC (1/2/3 to switch ops)"
  shortName := "Arithmetic"
  init := fun _ => pure Linalg.vectorArithmeticInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: 1=add, 2=sub, 3=scale
    if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with operation := .add }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with operation := .sub }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with operation := .scale }
    -- +/- to adjust scale factor
    else if env.keyCode == 24 then  -- = (plus without shift)
      env.clearKey
      state := { state with scaleFactor := state.scaleFactor + 0.1 }
    else if env.keyCode == 27 then  -- - (minus)
      env.clearKey
      let newScale := if state.scaleFactor > 0.2 then state.scaleFactor - 0.1 else 0.1
      state := { state with scaleFactor := newScale }
    pure state
  view := fun env s => some (Linalg.vectorArithmeticWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 50.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    if Linalg.nearPoint worldPos state.vectorA 0.5 then
      pure { state with dragging := some .vectorA }
    else if Linalg.nearPoint worldPos state.vectorB 0.5 then
      pure { state with dragging := some .vectorB }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | some target =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 50.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        match target with
        | .vectorA => pure { state with vectorA := worldPos }
        | .vectorB => pure { state with vectorB := worldPos }
    | none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := none }
  step := fun c _ s => pure (c, s)

instance : Demo .vectorProjection where
  name := "VECTOR PROJECTION (P/R/B to switch modes)"
  shortName := "Projection"
  init := fun _ => pure Linalg.vectorProjectionInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: P=projection, R=reflection, B=both
    if env.keyCode == FFI.Key.p then
      env.clearKey
      state := { state with showMode := .projection }
    else if env.keyCode == FFI.Key.r then
      env.clearKey
      state := { state with showMode := .reflection }
    else if env.keyCode == FFI.Key.b then
      env.clearKey
      state := { state with showMode := .both }
    pure state
  view := fun env s => some (Linalg.vectorProjectionWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 50.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    if Linalg.nearPoint worldPos state.vectorV 0.5 then
      pure { state with dragging := some .vectorV }
    else if Linalg.nearPoint worldPos state.vectorU 0.5 then
      pure { state with dragging := some .vectorU }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | some target =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 50.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        match target with
        | .vectorV => pure { state with vectorV := worldPos }
        | .vectorU => pure { state with vectorU := worldPos }
    | none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := none }
  step := fun c _ s => pure (c, s)

instance : Demo .vectorField where
  name := "VECTOR FIELD (1-4 to switch fields)"
  shortName := "Field"
  init := fun _ => pure Linalg.vectorFieldInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: 1-4 for field types
    if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with fieldType := .radial }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with fieldType := .rotational }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with fieldType := .gradient }
    else if env.keyCode == FFI.Key.num4 then
      env.clearKey
      state := { state with fieldType := .saddle }
    -- +/- to adjust grid resolution
    else if env.keyCode == 24 then  -- = (plus)
      env.clearKey
      state := { state with gridResolution := Nat.min 24 (state.gridResolution + 2) }
    else if env.keyCode == 27 then  -- - (minus)
      env.clearKey
      state := { state with gridResolution := Nat.max 4 (state.gridResolution - 2) }
    pure state
  view := fun env s => some (Linalg.vectorFieldWidget env s)
  step := fun c _ s => pure (c, s)

instance : Demo .crossProduct3D where
  name := "3D CROSS PRODUCT (drag to rotate)"
  shortName := "Cross 3D"
  init := fun _ => pure Linalg.crossProduct3DInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: P=toggle parallelogram, R=reset camera
    if env.keyCode == FFI.Key.p then
      env.clearKey
      state := { state with showParallelogram := !state.showParallelogram }
    else if env.keyCode == FFI.Key.r then
      env.clearKey
      state := { state with cameraYaw := 0.6, cameraPitch := 0.4 }
    pure state
  view := fun env s => some (Linalg.crossProduct3DWidget env s)
  handleClickWithLayouts := fun _env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.01
        let rawPitch := state.cameraPitch + dy * 0.01
        let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
        pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | _ => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Matrix Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .matrix2DTransform where
  name := "2D MATRIX TRANSFORM (1-9 for presets)"
  shortName := "Mat2D"
  init := fun _ => pure Linalg.matrix2DTransformInitialState
  update := fun env s => do
    let mut state := s
    -- Handle keyboard: 1-9 for presets
    if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with preset := .identity, matrix := Linalg.presetToMatrix .identity }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with preset := .rotation45, matrix := Linalg.presetToMatrix .rotation45 }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with preset := .rotation90, matrix := Linalg.presetToMatrix .rotation90 }
    else if env.keyCode == FFI.Key.num4 then
      env.clearKey
      state := { state with preset := .scale2x, matrix := Linalg.presetToMatrix .scale2x }
    else if env.keyCode == FFI.Key.num5 then
      env.clearKey
      state := { state with preset := .scaleNonUniform, matrix := Linalg.presetToMatrix .scaleNonUniform }
    else if env.keyCode == FFI.Key.num6 then
      env.clearKey
      state := { state with preset := .shearX, matrix := Linalg.presetToMatrix .shearX }
    else if env.keyCode == FFI.Key.num7 then
      env.clearKey
      state := { state with preset := .shearY, matrix := Linalg.presetToMatrix .shearY }
    else if env.keyCode == FFI.Key.num8 then
      env.clearKey
      state := { state with preset := .reflectX, matrix := Linalg.presetToMatrix .reflectX }
    else if env.keyCode == FFI.Key.num9 then
      env.clearKey
      state := { state with preset := .reflectY, matrix := Linalg.presetToMatrix .reflectY }
    -- Tab to cycle editable matrix cell
    else if env.keyCode == FFI.Key.tab then
      env.clearKey
      state := { state with editingCell := Linalg.nextMatrixCell state.editingCell }
    -- Arrow keys to move selected cell
    else if env.keyCode == FFI.Key.left then
      env.clearKey
      state := { state with editingCell := Linalg.moveMatrixCell state.editingCell 0 (-1) }
    else if env.keyCode == FFI.Key.right then
      env.clearKey
      state := { state with editingCell := Linalg.moveMatrixCell state.editingCell 0 1 }
    else if env.keyCode == FFI.Key.up then
      env.clearKey
      state := { state with editingCell := Linalg.moveMatrixCell state.editingCell (-1) 0 }
    else if env.keyCode == FFI.Key.down then
      env.clearKey
      state := { state with editingCell := Linalg.moveMatrixCell state.editingCell 1 0 }
    -- +/- to edit selected cell
    else if env.keyCode == 24 then  -- = (plus)
      env.clearKey
      if state.editingCell != .none then
        let updated := Linalg.modifyMatrixCell state.matrix state.editingCell 0.1
        state := { state with matrix := updated, preset := .custom }
    else if env.keyCode == 27 then  -- - (minus)
      env.clearKey
      if state.editingCell != .none then
        let updated := Linalg.modifyMatrixCell state.matrix state.editingCell (-0.1)
        state := { state with matrix := updated, preset := .custom }
    -- IJKL to translate
    else if env.keyCode == FFI.Key.i then
      env.clearKey
      let t := state.translation
      state := { state with translation := Linalg.Vec2.mk t.x (t.y + 0.1), preset := .custom }
    else if env.keyCode == FFI.Key.k then
      env.clearKey
      let t := state.translation
      state := { state with translation := Linalg.Vec2.mk t.x (t.y - 0.1), preset := .custom }
    else if env.keyCode == FFI.Key.j then
      env.clearKey
      let t := state.translation
      state := { state with translation := Linalg.Vec2.mk (t.x - 0.1) t.y, preset := .custom }
    else if env.keyCode == FFI.Key.l then
      env.clearKey
      let t := state.translation
      state := { state with translation := Linalg.Vec2.mk (t.x + 0.1) t.y, preset := .custom }
    -- G to toggle grid
    else if env.keyCode == FFI.Key.g then
      env.clearKey
      state := { state with showGrid := !state.showGrid }
    -- V to toggle basis vectors
    else if env.keyCode == FFI.Key.v then
      env.clearKey
      state := { state with showBasisVectors := !state.showBasisVectors }
    -- S to cycle shapes
    else if env.keyCode == FFI.Key.s then
      env.clearKey
      let newShape := match state.shape with
        | .square => .triangle
        | .triangle => .arrow
        | .arrow => .square
      state := { state with shape := newShape }
    -- Space to toggle animation
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    -- Update animation
    if state.animating then
      let newT := state.animT + env.dt * 0.5
      state := { state with animT := if newT >= 1.0 then 0.0 else newT }
    pure state
  view := fun env s => some (Linalg.matrix2DTransformWidget env s)
  step := fun c _ s => pure (c, s)

instance : Demo .matrix3DTransform where
  name := "3D TRANSFORM CHAIN (reorder transforms)"
  shortName := "Mat3D"
  init := fun _ => pure Linalg.matrix3DTransformInitialState
  update := fun env s => do
    let mut state := s
    -- 1-8 to select transform
    if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with selectedIndex := some 0 }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with selectedIndex := some 1 }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with selectedIndex := some 2 }
    -- Up arrow to move selected transform up
    else if env.keyCode == FFI.Key.up then
      env.clearKey
      if let some idx := state.selectedIndex then
        if idx > 0 then
          let mut arr := state.transforms
          let temp := arr.getD idx (.rotateX 0)
          arr := arr.set! idx (arr.getD (idx - 1) (.rotateX 0))
          arr := arr.set! (idx - 1) temp
          state := { state with transforms := arr, selectedIndex := some (idx - 1) }
    -- Down arrow to move selected transform down
    else if env.keyCode == FFI.Key.down then
      env.clearKey
      if let some idx := state.selectedIndex then
        if idx + 1 < state.transforms.size then
          let mut arr := state.transforms
          let temp := arr.getD idx (.rotateX 0)
          arr := arr.set! idx (arr.getD (idx + 1) (.rotateX 0))
          arr := arr.set! (idx + 1) temp
          state := { state with transforms := arr, selectedIndex := some (idx + 1) }
    -- A to toggle axes
    else if env.keyCode == FFI.Key.a then
      env.clearKey
      state := { state with showAxes := !state.showAxes }
    -- I to toggle intermediate steps
    else if env.keyCode == FFI.Key.i then
      env.clearKey
      state := { state with showIntermediateSteps := !state.showIntermediateSteps }
    -- R to reset camera
    else if env.keyCode == FFI.Key.r then
      env.clearKey
      state := { state with cameraYaw := 0.5, cameraPitch := 0.3 }
    pure state
  view := fun env s => some (Linalg.matrix3DTransformWidget env s)
  handleClickWithLayouts := fun _env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    if state.dragging then
      let dx := mouseX - state.lastMouseX
      let dy := mouseY - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.01
      let rawPitch := state.cameraPitch + dy * 0.01
      let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
      pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    else
      pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .projectionExplorer where
  name := "PROJECTION MATRIX EXPLORER"
  shortName := "Proj"
  init := fun _ => pure Linalg.projectionExplorerInitialState
  update := fun env s => do
    let mut state := s
    -- Tab to switch projection type
    if env.keyCode == FFI.Key.tab then
      env.clearKey
      state := { state with projType := match state.projType with
        | .perspective => .orthographic
        | .orthographic => .perspective
      }
    -- F to adjust far plane
    else if env.keyCode == FFI.Key.f then
      env.clearKey
      let newFar := state.far + 0.5
      state := { state with far := if newFar > 10.0 then 10.0 else newFar }
    -- N to adjust near plane
    else if env.keyCode == FFI.Key.n then
      env.clearKey
      let newNear := state.near + 0.1
      let maxNear := state.far - 0.5
      let clampedNear := if newNear > maxNear then maxNear else newNear
      state := { state with near := if clampedNear < 0.1 then 0.1 else clampedNear }
    -- + to increase FOV/size
    else if env.keyCode == 24 then  -- = (plus)
      env.clearKey
      match state.projType with
      | .perspective =>
          let newFov := state.fov + 0.1
          let maxFov := 2.513  -- ~80% of pi
          state := { state with fov := if newFov > maxFov then maxFov else newFov }
      | .orthographic =>
          let newSize := state.orthoSize + 0.2
          state := { state with orthoSize := if newSize > 5.0 then 5.0 else newSize }
    -- - to decrease FOV/size
    else if env.keyCode == 27 then  -- -
      env.clearKey
      match state.projType with
      | .perspective =>
          let newFov := state.fov - 0.1
          state := { state with fov := if newFov < 0.3 then 0.3 else newFov }
      | .orthographic =>
          let newSize := state.orthoSize - 0.2
          state := { state with orthoSize := if newSize < 0.5 then 0.5 else newSize }
    -- C to toggle clip space view
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with showClipSpace := !state.showClipSpace }
    -- O to toggle test objects
    else if env.keyCode == FFI.Key.o then
      env.clearKey
      state := { state with showTestObjects := !state.showTestObjects }
    pure state
  view := fun env s => some (Linalg.projectionExplorerWidget env s)
  handleClickWithLayouts := fun _env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    if state.dragging then
      let dx := mouseX - state.lastMouseX
      let dy := mouseY - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.01
      let rawPitch := state.cameraPitch + dy * 0.01
      let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
      pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    else
      pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ => do
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .matrixDecomposition where
  name := "MATRIX DECOMPOSITION"
  shortName := "Decomp"
  init := fun _ => pure Linalg.matrixDecompositionInitialState
  update := fun env s => do
    let mut state := s
    -- 1-6 for presets
    let presetKey := if env.keyCode == FFI.Key.num1 then some 0
                     else if env.keyCode == FFI.Key.num2 then some 1
                     else if env.keyCode == FFI.Key.num3 then some 2
                     else if env.keyCode == FFI.Key.num4 then some 3
                     else if env.keyCode == FFI.Key.num5 then some 4
                     else if env.keyCode == FFI.Key.num6 then some 5
                     else none
    if let some i := presetKey then
      if i < Linalg.decompositionPresets.size then
        env.clearKey
        let (_, m) := Linalg.decompositionPresets.getD i ("", Linalg.Mat2.identity)
        state := { state with
          matrix := m
          decomp := Linalg.decomposeMatrix2D m
          presetIndex := i
        }
    -- Tab to cycle decomposition steps
    if env.keyCode == FFI.Key.tab then
      env.clearKey
      state := { state with currentStep := match state.currentStep with
        | .original => .afterRotation1
        | .afterRotation1 => .afterScale
        | .afterScale => .afterRotation2
        | .afterRotation2 => .original
      }
    -- C to toggle components
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with showComponents := !state.showComponents }
    pure state
  view := fun env s => some (Linalg.matrixDecompositionWidget env s)
  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Rotation System Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .quaternionVisualizer where
  name := "QUATERNION VISUALIZER (arcball + sliders)"
  shortName := "Quat"
  init := fun _ => pure Linalg.quaternionVisualizerInitialState
  update := fun env s => do
    let mut state := s
    -- Reset
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.quaternionVisualizerInitialState
    -- Apply Euler angles to quaternion
    else if env.keyCode == FFI.Key.e then
      env.clearKey
      let e := state.eulerAngles
      let q := Linalg.Quat.fromEuler e.x e.y e.z
      state := { state with quat := q.normalize }
    -- Select Euler component
    else if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with selectedEuler := 0 }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with selectedEuler := 1 }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with selectedEuler := 2 }
    -- Adjust Euler angles
    else if env.keyCode == FFI.Key.left then
      env.clearKey
      let delta := -5.0 * Linalg.Float.pi / 180.0
      let e := state.eulerAngles
      let e' := match state.selectedEuler with
        | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
        | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
        | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
      state := { state with eulerAngles := e' }
    else if env.keyCode == FFI.Key.right then
      env.clearKey
      let delta := 5.0 * Linalg.Float.pi / 180.0
      let e := state.eulerAngles
      let e' := match state.selectedEuler with
        | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
        | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
        | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
      state := { state with eulerAngles := e' }
    pure state
  view := fun env s => some (Linalg.quaternionVisualizerWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 && click.button != 1 then
      return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let labels : Array (Linalg.QuatComponent × Nat) := #[(.x, 0), (.y, 1), (.z, 2), (.w, 3)]
    let mut hitSlider : Option Linalg.QuatComponent := none
    for (comp, idx) in labels do
      let layout := Linalg.sliderLayoutFor env.physWidthF env.physHeightF env.screenScale idx
      let hit := localX >= layout.x && localX <= layout.x + layout.width
        && localY >= layout.y - 8.0 && localY <= layout.y + layout.height + 8.0
      if hit then
        hitSlider := some comp
    match hitSlider with
    | some comp =>
        let layout := Linalg.sliderLayoutFor env.physWidthF env.physHeightF env.screenScale
          (match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3)
        let t := (localX - layout.x) / layout.width
        let value := Linalg.clampUnit (t * 2.0 - 1.0)
        let q := (Linalg.setQuatComponent state.quat comp value).normalize
        pure { state with quat := q, dragging := .slider comp, lastMouseX := click.x, lastMouseY := click.y }
    | none =>
        if click.button == 1 then
          pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
        else
          pure { state with dragging := .arcball, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let yaw := state.cameraYaw + dx * 0.005
        let pitch := state.cameraPitch + dy * 0.005
        pure { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | .arcball =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let rotY := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitY (dx * 0.008)
        let rotX := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitX (dy * 0.008)
        let q := Linalg.Quat.multiply rotY (Linalg.Quat.multiply rotX state.quat) |>.normalize
        pure { state with quat := q, lastMouseX := mouseX, lastMouseY := mouseY }
    | .slider comp =>
        let localX := mouseX - env.contentOffsetX
        let layout := Linalg.sliderLayoutFor env.physWidthF env.physHeightF env.screenScale
          (match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3)
        let t := (localX - layout.x) / layout.width
        let value := Linalg.clampUnit (t * 2.0 - 1.0)
        let q := (Linalg.setQuatComponent state.quat comp value).normalize
        pure { state with quat := q, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .slerpInterpolation where
  name := "SLERP vs LERP (quaternion)"
  shortName := "Slerp"
  init := fun _ => pure Linalg.slerpInterpolationInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let newT := state.t + env.dt * 0.35
      state := { state with t := if newT > 1.0 then newT - 1.0 else newT }
    pure state
  view := fun env s => some (Linalg.slerpInterpolationWidget env s)
  handleClickWithLayouts := fun _env state _ _ click _ _ => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _ _ mouseX mouseY _ _ => do
    if !state.dragging then return state
    let dx := mouseX - state.lastMouseX
    let dy := mouseY - state.lastMouseY
    let newYaw := state.cameraYaw + dx * 0.005
    let newPitch := state.cameraPitch + dy * 0.005
    pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .eulerGimbalLock where
  name := "EULER GIMBAL LOCK"
  shortName := "Gimbal"
  init := fun _ => pure Linalg.eulerGimbalLockInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.eulerGimbalLockInitialState
    else if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with selectedAxis := 0 }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with selectedAxis := 1 }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with selectedAxis := 2 }
    else if env.keyCode == FFI.Key.o then
      env.clearKey
      let nextOrder := match state.euler.order with
        | .XYZ => .XZY
        | .XZY => .YXZ
        | .YXZ => .YZX
        | .YZX => .ZXY
        | .ZXY => .ZYX
        | .ZYX => .XYZ
      state := { state with euler := { state.euler with order := nextOrder } }
    else if env.keyCode == FFI.Key.left || env.keyCode == FFI.Key.right then
      let delta := if env.keyCode == FFI.Key.left then -5.0 else 5.0
      env.clearKey
      let e := state.euler
      let radDelta := delta * Linalg.Float.pi / 180.0
      let e' := match state.selectedAxis with
        | 0 => { e with a1 := e.a1 + radDelta }
        | 1 => { e with a2 := e.a2 + radDelta }
        | _ => { e with a3 := e.a3 + radDelta }
      state := { state with euler := e' }
    pure state
  view := fun env s => some (Linalg.eulerGimbalLockWidget env s)
  handleClickWithLayouts := fun _env state _ _ click _ _ => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _ _ mouseX mouseY _ _ => do
    if !state.dragging then return state
    let dx := mouseX - state.lastMouseX
    let dy := mouseY - state.lastMouseY
    let newYaw := state.cameraYaw + dx * 0.005
    let newPitch := state.cameraPitch + dy * 0.005
    pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .dualQuaternionBlending where
  name := "DUAL QUATERNION BLENDING (LBS vs DLB)"
  shortName := "DualQuat"
  init := fun _ => pure Linalg.dualQuaternionBlendingInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.dualQuaternionBlendingInitialState
    else if env.keyCode == FFI.Key.t then
      env.clearKey
      state := { state with twist := state.twist + 0.1 }
    else if env.keyCode == FFI.Key.g then
      env.clearKey
      state := { state with twist := state.twist - 0.1 }
    else if env.keyCode == FFI.Key.b then
      env.clearKey
      state := { state with bend := state.bend + 0.1 }
    else if env.keyCode == FFI.Key.v then
      env.clearKey
      state := { state with bend := state.bend - 0.1 }
    pure state
  view := fun env s => some (Linalg.dualQuaternionBlendingWidget env s)
  handleClickWithLayouts := fun _env state _ _ click _ _ => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _ _ mouseX mouseY _ _ => do
    if !state.dragging then return state
    let dx := mouseX - state.lastMouseX
    let dy := mouseY - state.lastMouseY
    let newYaw := state.cameraYaw + dx * 0.005
    let newPitch := state.cameraPitch + dy * 0.005
    pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)


-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Geometry Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .rayCastingPlayground where
  name := "RAY CASTING PLAYGROUND"
  shortName := "Ray"
  init := fun _ => pure Linalg.rayCastingPlaygroundInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.rayCastingPlaygroundInitialState
    pure state
  view := fun env s => some (Linalg.rayCastingPlaygroundWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 && click.button != 1 then
      return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    let origin2 := Linalg.Vec2.mk state.rayOrigin.x state.rayOrigin.z
    let target2 := Linalg.Vec2.mk state.rayTarget.x state.rayTarget.z
    if click.button == 1 then
      pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
    else if Linalg.nearPoint worldPos origin2 0.5 then
      pure { state with dragging := .origin, lastMouseX := click.x, lastMouseY := click.y }
    else if Linalg.nearPoint worldPos target2 0.5 then
      pure { state with dragging := .direction, lastMouseX := click.x, lastMouseY := click.y }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.005
        let newPitch := state.cameraPitch + dy * 0.005
        pure { state with cameraYaw := newYaw, cameraPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | .origin =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        let newOrigin := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
        pure { state with rayOrigin := newOrigin, lastMouseX := mouseX, lastMouseY := mouseY }
    | .direction =>
        let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
        let newTarget := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
        pure { state with rayTarget := newTarget, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .primitiveOverlapTester where
  name := "PRIMITIVE OVERLAP TESTER"
  shortName := "Overlap"
  init := fun _ => pure Linalg.primitiveOverlapTesterInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.primitiveOverlapTesterInitialState
    else if env.keyCode == FFI.Key.num1 then
      env.clearKey
      state := { state with mode := .sphereSphere }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      state := { state with mode := .aabbAabb }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      state := { state with mode := .sphereAabb }
    pure state
  view := fun env s => some (Linalg.primitiveOverlapTesterWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    if Linalg.nearPoint worldPos state.centerA 0.6 then
      pure { state with dragging := .shapeA }
    else if Linalg.nearPoint worldPos state.centerB 0.6 then
      pure { state with dragging := .shapeB }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
    match state.dragging with
    | .shapeA => pure { state with centerA := worldPos }
    | .shapeB => pure { state with centerB := worldPos }
    | .none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .barycentricCoordinates where
  name := "BARYCENTRIC COORDINATES"
  shortName := "Bary"
  init := fun _ => pure Linalg.barycentricCoordinatesInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.barycentricCoordinatesInitialState
    pure state
  view := fun env s => some (Linalg.barycentricCoordinatesWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (click.x, click.y) origin scale
    if Linalg.nearPoint worldPos state.point 0.4 then
      pure { state with dragging := true }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    if !state.dragging then return state
    let origin := (env.contentOffsetX + env.physWidthF / 2, env.contentOffsetY + env.physHeightF / 2)
    let scale := 70.0 * env.screenScale
    let worldPos := Linalg.screenToWorld (mouseX, mouseY) origin scale
    pure { state with point := worldPos }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

instance : Demo .frustumCullingDemo where
  name := "FRUSTUM CULLING DEMO"
  shortName := "Frustum"
  init := fun _ => pure Linalg.frustumCullingDemoInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.frustumCullingDemoInitialState
    else if env.keyCode == FFI.Key.j then
      env.clearKey
      state := { state with camYaw := state.camYaw - 0.08 }
    else if env.keyCode == FFI.Key.l then
      env.clearKey
      state := { state with camYaw := state.camYaw + 0.08 }
    else if env.keyCode == FFI.Key.i then
      env.clearKey
      let newPitch := state.camPitch + 0.08
      state := { state with camPitch := if newPitch > 1.2 then 1.2 else newPitch }
    else if env.keyCode == FFI.Key.k then
      env.clearKey
      let newPitch := state.camPitch - 0.08
      state := { state with camPitch := if newPitch < -1.2 then -1.2 else newPitch }
    else if env.keyCode == 24 then
      env.clearKey
      let newDist := state.camDist - 0.3
      state := { state with camDist := if newDist < 2.0 then 2.0 else newDist }
    else if env.keyCode == 27 then
      env.clearKey
      state := { state with camDist := state.camDist + 0.3 }
    pure state
  view := fun env s => some (Linalg.frustumCullingDemoWidget env s)
  handleClickWithLayouts := fun _env state _ _ click _ _ => do
    if click.button != 0 then return state
    pure { state with dragging := true, lastMouseX := click.x, lastMouseY := click.y }
  handleHoverWithLayouts := fun _env state _ _ mouseX mouseY _ _ => do
    if !state.dragging then return state
    let dx := mouseX - state.lastMouseX
    let dy := mouseY - state.lastMouseY
    let newYaw := state.viewYaw + dx * 0.005
    let newPitch := state.viewPitch + dy * 0.005
    pure { state with viewYaw := newYaw, viewPitch := newPitch, lastMouseX := mouseX, lastMouseY := mouseY }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := false }
  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Curve and Spline Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .bezierCurveEditor where
  name := "BEZIER CURVE EDITOR"
  shortName := "Bezier"
  init := fun _ => pure Linalg.bezierCurveEditorInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.bezierCurveEditorInitialState
    else if env.keyCode == FFI.Key.q then
      env.clearKey
      state := { state with mode := .quadratic }
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with mode := .cubic }
    pure state
  view := fun env s => some (Linalg.bezierCurveEditorWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let sliderX := env.physWidthF - 250.0 * env.screenScale
    let sliderY := 95.0 * env.screenScale
    let sliderW := 180.0 * env.screenScale
    let sliderH := 8.0 * env.screenScale
    let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
      && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
    if hitSlider then
      let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
      pure { state with t := t, dragging := .slider }
    else
      let origin := (env.physWidthF / 2, env.physHeightF / 2)
      let scale := 70.0 * env.screenScale
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      let points := match state.mode with
        | .quadratic => state.quadPoints
        | .cubic => state.cubicPoints
      let mut hit : Option Nat := none
      for i in [:points.size] do
        let p := points.getD i Linalg.Vec2.zero
        if Linalg.nearPoint worldPos p 0.45 then
          hit := some i
      match hit with
      | some idx => pure { state with dragging := .control idx }
      | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .slider =>
        let localX := mouseX - env.contentOffsetX
        let sliderX := env.physWidthF - 250.0 * env.screenScale
        let sliderW := 180.0 * env.screenScale
        let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
        pure { state with t := t }
    | .control idx =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        let state' := match state.mode with
          | .quadratic =>
              if idx < state.quadPoints.size then
                { state with quadPoints := state.quadPoints.set! idx worldPos }
              else state
          | .cubic =>
              if idx < state.cubicPoints.size then
                { state with cubicPoints := state.cubicPoints.set! idx worldPos }
              else state
        pure state'
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .catmullRomSplineEditor where
  name := "CATMULL-ROM SPLINE EDITOR"
  shortName := "Catmull"
  init := fun _ => pure Linalg.catmullRomSplineEditorInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.catmullRomSplineEditorInitialState
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with closed := !state.closed }
    else if env.keyCode == FFI.Key.delete then
      env.clearKey
      if state.points.size > 0 then
        state := { state with points := state.points.pop }
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let newT := state.t + env.dt * 0.2
      state := { state with t := if newT > 1.0 then newT - 1.0 else newT }
    pure state
  view := fun env s => some (Linalg.catmullRomSplineEditorWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let sliderX := env.physWidthF - 260.0 * env.screenScale
    let sliderY := 95.0 * env.screenScale
    let sliderW := 190.0 * env.screenScale
    let sliderH := 8.0 * env.screenScale
    let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
      && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
    if hitSlider then
      let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
      pure { state with alpha := alpha, dragging := .slider }
    else
      let origin := (env.physWidthF / 2, env.physHeightF / 2)
      let scale := 70.0 * env.screenScale
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      let mut hit : Option Nat := none
      for i in [:state.points.size] do
        let p := state.points.getD i Linalg.Vec2.zero
        if Linalg.nearPoint worldPos p 0.45 then
          hit := some i
      match hit with
      | some idx => pure { state with dragging := .point idx }
      | none =>
          let newPoints := state.points.push worldPos
          pure { state with points := newPoints, dragging := .point (newPoints.size - 1) }
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .slider =>
        let localX := mouseX - env.contentOffsetX
        let sliderX := env.physWidthF - 260.0 * env.screenScale
        let sliderW := 190.0 * env.screenScale
        let alpha := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
        pure { state with alpha := alpha }
    | .point idx =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        if idx < state.points.size then
          pure { state with points := state.points.set! idx worldPos }
        else
          pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .bSplineCurveDemo where
  name := "B-SPLINE CURVE DEMO"
  shortName := "B-Spline"
  init := fun _ => pure Linalg.bSplineCurveDemoInitialState
  update := fun env s => do
    let mut state := s
    let maxDegree := Nat.min 5 (state.controlPoints.size - 1)
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.bSplineCurveDemoInitialState
    else if env.keyCode == FFI.Key.u then
      env.clearKey
      state := { state with knots := Linalg.BSpline.uniform state.controlPoints state.degree |>.knots }
    else if env.keyCode == FFI.Key.num1 then
      env.clearKey
      let d := Nat.min 1 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    else if env.keyCode == FFI.Key.num2 then
      env.clearKey
      let d := Nat.min 2 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    else if env.keyCode == FFI.Key.num3 then
      env.clearKey
      let d := Nat.min 3 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    else if env.keyCode == FFI.Key.num4 then
      env.clearKey
      let d := Nat.min 4 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    else if env.keyCode == FFI.Key.num5 then
      env.clearKey
      let d := Nat.min 5 maxDegree
      state := { state with degree := d, knots := Linalg.BSpline.uniform state.controlPoints d |>.knots }
    pure state
  view := fun env s => some (Linalg.bSplineCurveDemoWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let rectX := 40.0 * env.screenScale
    let rectY := env.physHeightF - 190.0 * env.screenScale
    let rectW := env.physWidthF - 80.0 * env.screenScale
    let rectH := 120.0 * env.screenScale
    let knotY := rectY + rectH + 12.0
    let spline := { controlPoints := state.controlPoints, knots := state.knots, degree := state.degree : Linalg.BSpline Linalg.Vec2 }
    let mut hitKnot : Option Nat := none
    for i in [:spline.knots.size] do
      let editable := i > spline.degree && i < spline.knots.size - spline.degree - 1
      if editable then
        let knot := spline.knots.getD i 0.0
        let x := rectX + knot * rectW
        let dx := localX - x
        let dy := localY - knotY
        if dx * dx + dy * dy <= 70.0 then
          hitKnot := some i
    match hitKnot with
    | some idx =>
        let t := Linalg.Float.clamp ((localX - rectX) / rectW) 0.0 1.0
        let prev := spline.knots.getD (idx - 1) 0.0
        let next := spline.knots.getD (idx + 1) 1.0
        let v := Linalg.Float.clamp t prev next
        let knots := spline.knots.set! idx v
        pure { state with knots := knots, dragging := .knot idx }
    | none =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2 - 40 * env.screenScale)
        let scale := 60.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (localX, localY) origin scale
        let mut hitPt : Option Nat := none
        for i in [:state.controlPoints.size] do
          let p := state.controlPoints.getD i Linalg.Vec2.zero
          if Linalg.nearPoint worldPos p 0.45 then
            hitPt := some i
        match hitPt with
        | some idx => pure { state with dragging := .point idx }
        | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .point idx =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2 - 40 * env.screenScale)
        let scale := 60.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        if idx < state.controlPoints.size then
          pure { state with controlPoints := state.controlPoints.set! idx worldPos }
        else
          pure state
    | .knot idx =>
        let rectX := 40.0 * env.screenScale
        let rectW := env.physWidthF - 80.0 * env.screenScale
        let t := Linalg.Float.clamp ((mouseX - env.contentOffsetX - rectX) / rectW) 0.0 1.0
        let prev := state.knots.getD (idx - 1) 0.0
        let next := state.knots.getD (idx + 1) 1.0
        let v := Linalg.Float.clamp t prev next
        pure { state with knots := state.knots.set! idx v }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .arcLengthParameterization where
  name := "ARC-LENGTH PARAMETERIZATION"
  shortName := "ArcLen"
  init := fun _ => pure Linalg.arcLengthParameterizationInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.arcLengthParameterizationInitialState
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let p0 := state.controlPoints.getD 0 Linalg.Vec2.zero
      let p1 := state.controlPoints.getD 1 Linalg.Vec2.zero
      let p2 := state.controlPoints.getD 2 Linalg.Vec2.zero
      let p3 := state.controlPoints.getD 3 Linalg.Vec2.zero
      let curve := Linalg.Bezier3.mk p0 p1 p2 p3
      let evalFn := fun t => Linalg.Bezier3.evalVec2 curve t
      let table := Linalg.ArcLengthTable.build evalFn 120
      let newT := state.t + env.dt * 0.2
      let newS := state.s + state.speed * env.dt
      let wrappedS := if table.totalLength > Linalg.Float.epsilon then
        if newS > table.totalLength then newS - table.totalLength else newS
      else 0.0
      state := { state with t := (if newT > 1.0 then newT - 1.0 else newT), s := wrappedS }
    pure state
  view := fun env s => some (Linalg.arcLengthParameterizationWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let sliderX := env.physWidthF - 260.0 * env.screenScale
    let sliderY := 95.0 * env.screenScale
    let sliderW := 190.0 * env.screenScale
    let sliderH := 8.0 * env.screenScale
    let hitSlider := localX >= sliderX && localX <= sliderX + sliderW
      && localY >= sliderY - 8.0 && localY <= sliderY + sliderH + 8.0
    if hitSlider then
      let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
      let speed := 0.2 + t * 3.8
      pure { state with speed := speed, dragging := .slider }
    else
      let origin := (env.physWidthF / 2, env.physHeightF / 2)
      let scale := 70.0 * env.screenScale
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      let mut hit : Option Nat := none
      for i in [:state.controlPoints.size] do
        let p := state.controlPoints.getD i Linalg.Vec2.zero
        if Linalg.nearPoint worldPos p 0.45 then
          hit := some i
      match hit with
      | some idx => pure { state with dragging := .point idx }
      | none => pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .slider =>
        let localX := mouseX - env.contentOffsetX
        let sliderX := env.physWidthF - 260.0 * env.screenScale
        let sliderW := 190.0 * env.screenScale
        let t := Linalg.Float.clamp ((localX - sliderX) / sliderW) 0.0 1.0
        let speed := 0.2 + t * 3.8
        pure { state with speed := speed }
    | .point idx =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        if idx < state.controlPoints.size then
          pure { state with controlPoints := state.controlPoints.set! idx worldPos }
        else
          pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .bezierPatchSurface where
  name := "BEZIER PATCH SURFACE"
  shortName := "Patch"
  init := fun _ => pure Linalg.bezierPatchSurfaceInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.bezierPatchSurfaceInitialState
    else if env.keyCode == FFI.Key.n then
      env.clearKey
      state := { state with showNormals := !state.showNormals }
    else if env.keyCode == FFI.Key.left then
      env.clearKey
      let newTess := if state.tessellation > 2 then state.tessellation - 1 else 2
      state := { state with tessellation := newTess }
    else if env.keyCode == FFI.Key.right then
      env.clearKey
      let newTess := if state.tessellation < 18 then state.tessellation + 1 else 18
      state := { state with tessellation := newTess }
    else if env.keyCode == FFI.Key.up || env.keyCode == FFI.Key.down then
      let delta := if env.keyCode == FFI.Key.up then 0.2 else -0.2
      env.clearKey
      match state.selected with
      | some idx =>
          let row := idx / 4
          let col := idx % 4
          let p := state.patch.getPoint row col
          let patch := state.patch.setPoint row col (Linalg.Vec3.mk p.x p.y (p.z + delta))
          state := { state with patch := patch }
      | none => state := state
    pure state
  view := fun env s => some (Linalg.bezierPatchSurfaceWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let rectX := env.physWidthF - 260.0 * env.screenScale
    let rectY := 110.0 * env.screenScale
    let rectW := 220.0 * env.screenScale
    let rectH := 220.0 * env.screenScale
    let withinMini := localX >= rectX && localX <= rectX + rectW
      && localY >= rectY && localY <= rectY + rectH
    if click.button == 1 then
      pure { state with dragging := .camera, lastMouseX := click.x, lastMouseY := click.y }
    else if click.button == 0 && withinMini then
      let origin := (rectX + rectW / 2, rectY + rectH / 2)
      let scale := rectW / 5.8
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      let mut hit : Option Nat := none
      for idx in [:16] do
        let row := idx / 4
        let col := idx % 4
        let p := state.patch.getPoint row col
        let p2 := Linalg.Vec2.mk p.x p.y
        if Linalg.nearPoint worldPos p2 0.35 then
          hit := some idx
      match hit with
      | some idx => pure { state with selected := some idx, dragging := .point idx }
      | none => pure state
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .camera =>
        let dx := mouseX - state.lastMouseX
        let dy := mouseY - state.lastMouseY
        let yaw := state.cameraYaw + dx * 0.005
        let pitch := state.cameraPitch + dy * 0.005
        pure { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := mouseX, lastMouseY := mouseY }
    | .point idx =>
        let rectX := env.physWidthF - 260.0 * env.screenScale
        let rectY := 110.0 * env.screenScale
        let rectW := 220.0 * env.screenScale
        let rectH := 220.0 * env.screenScale
        let origin := (rectX + rectW / 2, rectY + rectH / 2)
        let scale := rectW / 5.8
        let localX := mouseX - env.contentOffsetX
        let localY := mouseY - env.contentOffsetY
        let worldPos := Linalg.screenToWorld (localX, localY) origin scale
        let row := idx / 4
        let col := idx % 4
        let p := state.patch.getPoint row col
        let patch := state.patch.setPoint row col (Linalg.Vec3.mk worldPos.x worldPos.y p.z)
        pure { state with patch := patch }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

-- ═══════════════════════════════════════════════════════════════════════════
-- Linalg Easing & Interpolation Demos
-- ═══════════════════════════════════════════════════════════════════════════

instance : Demo .easingFunctionGallery where
  name := "EASING FUNCTION GALLERY"
  shortName := "Easing"
  init := fun _ => pure Linalg.easingFunctionGalleryInitialState
  update := fun env s => do
    let mut state := s
    let count := Linalg.easingEntryCount
    if count == 0 then
      return state
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.easingFunctionGalleryInitialState
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    else if env.keyCode == FFI.Key.c then
      env.clearKey
      state := { state with compareMode := !state.compareMode }
    else if env.keyCode == FFI.Key.tab then
      env.clearKey
      state := { state with selected := (state.selected + 1) % count }
    else if env.keyCode == FFI.Key.left then
      env.clearKey
      state := { state with selected := (state.selected + count - 1) % count }
    else if env.keyCode == FFI.Key.right then
      env.clearKey
      state := { state with selected := (state.selected + 1) % count }
    else if env.keyCode == FFI.Key.x then
      env.clearKey
      state := { state with compare := (state.compare + 1) % count }
    else if env.keyCode == FFI.Key.up then
      env.clearKey
      state := { state with speed := Linalg.Float.clamp (state.speed + 0.1) 0.1 3.0 }
    else if env.keyCode == FFI.Key.down then
      env.clearKey
      state := { state with speed := Linalg.Float.clamp (state.speed - 0.1) 0.1 3.0 }

    if state.compare == state.selected then
      state := { state with compare := (state.compare + 1) % count }

    if state.animating then
      let newT := state.t + env.dt * state.speed
      state := { state with t := if newT > 1.0 then newT - 1.0 else newT }
    pure state
  view := fun env s => some (Linalg.easingFunctionGalleryWidget env s)
  step := fun c _ s => pure (c, s)

instance : Demo .smoothDampFollower where
  name := "SMOOTH DAMP FOLLOWER"
  shortName := "SmoothD"
  init := fun _ => pure Linalg.smoothDampFollowerInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.smoothDampFollowerInitialState
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let (_newPos, newState) := Linalg.SmoothDampState2.step
        state.dampState state.target state.smoothTime env.dt state.maxSpeed
      let speed := newState.velocity.length
      let mut history := state.history.push speed
      if history.size > 120 then
        history := history.eraseIdxIfInBounds 0
      state := { state with dampState := newState, history := history }
    pure state
  view := fun env s => some (Linalg.smoothDampFollowerWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let layoutSmooth := Linalg.smoothDampSliderLayout env.physWidthF env.physHeightF env.screenScale 0
    let layoutMax := Linalg.smoothDampSliderLayout env.physWidthF env.physHeightF env.screenScale 1
    let hitSmooth := localX >= layoutSmooth.x && localX <= layoutSmooth.x + layoutSmooth.width
      && localY >= layoutSmooth.y - 8.0 && localY <= layoutSmooth.y + layoutSmooth.height + 8.0
    let hitMax := localX >= layoutMax.x && localX <= layoutMax.x + layoutMax.width
      && localY >= layoutMax.y - 8.0 && localY <= layoutMax.y + layoutMax.height + 8.0
    if hitSmooth then
      let t := Linalg.Float.clamp ((localX - layoutSmooth.x) / layoutSmooth.width) 0.0 1.0
      let value := Linalg.smoothDampSmoothTimeFrom t
      pure { state with smoothTime := value, dragging := .slider .smoothTime }
    else if hitMax then
      let t := Linalg.Float.clamp ((localX - layoutMax.x) / layoutMax.width) 0.0 1.0
      let value := Linalg.smoothDampMaxSpeedFrom t
      pure { state with maxSpeed := value, dragging := .slider .maxSpeed }
    else
      let origin := (env.physWidthF / 2, env.physHeightF / 2)
      let scale := 70.0 * env.screenScale
      let worldPos := Linalg.screenToWorld (localX, localY) origin scale
      if Linalg.nearPoint worldPos state.target 0.45 then
        pure { state with dragging := .target }
      else
        pure { state with target := worldPos, dragging := .target }
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX mouseY _layouts _widget => do
    match state.dragging with
    | .none => pure state
    | .target =>
        let origin := (env.physWidthF / 2, env.physHeightF / 2)
        let scale := 70.0 * env.screenScale
        let worldPos := Linalg.screenToWorld (mouseX - env.contentOffsetX, mouseY - env.contentOffsetY) origin scale
        pure { state with target := worldPos }
    | .slider which =>
        let localX := mouseX - env.contentOffsetX
        match which with
        | .smoothTime =>
            let layout := Linalg.smoothDampSliderLayout env.physWidthF env.physHeightF env.screenScale 0
            let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
            pure { state with smoothTime := Linalg.smoothDampSmoothTimeFrom t }
        | .maxSpeed =>
            let layout := Linalg.smoothDampSliderLayout env.physWidthF env.physHeightF env.screenScale 1
            let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
            pure { state with maxSpeed := Linalg.smoothDampMaxSpeedFrom t }
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

instance : Demo .springAnimationPlayground where
  name := "SPRING ANIMATION PLAYGROUND"
  shortName := "Spring"
  init := fun _ => pure Linalg.springAnimationPlaygroundInitialState
  update := fun env s => do
    let mut state := s
    if env.keyCode == FFI.Key.r then
      env.clearKey
      state := Linalg.springAnimationPlaygroundInitialState
    else if env.keyCode == FFI.Key.space then
      env.clearKey
      state := { state with animating := !state.animating }
    if state.animating then
      let newTime := state.time + env.dt
      let time := if newTime > 4.0 then newTime - 4.0 else newTime
      let ω := 2.0 * Linalg.Float.pi * state.frequency
      let x := Linalg.springResponse time state.dampingRatio ω
      let v := Linalg.springVelocity time state.dampingRatio ω
      let energy := 0.5 * (x * x + (v / ω) * (v / ω))
      let mut history := state.energyHistory.push energy
      if history.size > 140 then
        history := history.eraseIdxIfInBounds 0
      state := { state with time := time, energyHistory := history }
    pure state
  view := fun env s => some (Linalg.springAnimationPlaygroundWidget env s)
  handleClickWithLayouts := fun env state _contentId _hitPath click _layouts _widget => do
    if click.button != 0 then return state
    let localX := click.x - env.contentOffsetX
    let localY := click.y - env.contentOffsetY
    let layoutDamp := Linalg.springSliderLayout env.physWidthF env.physHeightF env.screenScale 0
    let layoutFreq := Linalg.springSliderLayout env.physWidthF env.physHeightF env.screenScale 1
    let hitDamp := localX >= layoutDamp.x && localX <= layoutDamp.x + layoutDamp.width
      && localY >= layoutDamp.y - 8.0 && localY <= layoutDamp.y + layoutDamp.height + 8.0
    let hitFreq := localX >= layoutFreq.x && localX <= layoutFreq.x + layoutFreq.width
      && localY >= layoutFreq.y - 8.0 && localY <= layoutFreq.y + layoutFreq.height + 8.0
    if hitDamp then
      let t := Linalg.Float.clamp ((localX - layoutDamp.x) / layoutDamp.width) 0.0 1.0
      pure { state with dampingRatio := Linalg.springDampingFrom t, dragging := .sliderDamping }
    else if hitFreq then
      let t := Linalg.Float.clamp ((localX - layoutFreq.x) / layoutFreq.width) 0.0 1.0
      pure { state with frequency := Linalg.springFrequencyFrom t, dragging := .sliderFrequency }
    else
      pure state
  handleHoverWithLayouts := fun env state _contentId _hitPath mouseX _mouseY _layouts _widget => do
    let localX := mouseX - env.contentOffsetX
    match state.dragging with
    | .sliderDamping =>
        let layout := Linalg.springSliderLayout env.physWidthF env.physHeightF env.screenScale 0
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        pure { state with dampingRatio := Linalg.springDampingFrom t }
    | .sliderFrequency =>
        let layout := Linalg.springSliderLayout env.physWidthF env.physHeightF env.screenScale 1
        let t := Linalg.Float.clamp ((localX - layout.x) / layout.width) 0.0 1.0
        pure { state with frequency := Linalg.springFrequencyFrom t }
    | .none => pure state
  handleMouseUpWithLayouts := fun _env state _ _ _ _ _ =>
    pure { state with dragging := .none }
  step := fun c _ s => pure (c, s)

def demoInstance (id : DemoId) : Demo id := by
  cases id <;> infer_instance

namespace AnyDemo

def name (d : AnyDemo) : String :=
  (demoInstance d.id).name

def shortName (d : AnyDemo) : String :=
  (demoInstance d.id).shortName

def msaaEnabled (d : AnyDemo) : Bool :=
  (demoInstance d.id).msaaEnabled

def update (d : AnyDemo) (env : DemoEnv) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.update env d.state
  pure { id := d.id, state := state' }

def view? (d : AnyDemo) (env : DemoEnv) : Option Afferent.Arbor.WidgetBuilder :=
  (demoInstance d.id).view env d.state

def handleClick (d : AnyDemo) (env : DemoEnv) (contentId : Afferent.Arbor.WidgetId)
    (hitPath : Array Afferent.Arbor.WidgetId) (click : Afferent.FFI.ClickEvent) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleClick env d.state contentId hitPath click
  pure { id := d.id, state := state' }

def handleClickWithLayouts (d : AnyDemo) (env : DemoEnv) (contentId : Afferent.Arbor.WidgetId)
    (hitPath : Array Afferent.Arbor.WidgetId) (click : Afferent.FFI.ClickEvent)
    (layouts : Trellis.LayoutResult) (widget : Afferent.Arbor.Widget) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleClickWithLayouts env d.state contentId hitPath click layouts widget
  pure { id := d.id, state := state' }

def handleHover (d : AnyDemo) (env : DemoEnv) (contentId : Afferent.Arbor.WidgetId)
    (hitPath : Array Afferent.Arbor.WidgetId) (mouseX mouseY : Float) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleHover env d.state contentId hitPath mouseX mouseY
  pure { id := d.id, state := state' }

def handleHoverWithLayouts (d : AnyDemo) (env : DemoEnv) (contentId : Afferent.Arbor.WidgetId)
    (hitPath : Array Afferent.Arbor.WidgetId) (mouseX mouseY : Float)
    (layouts : Trellis.LayoutResult) (widget : Afferent.Arbor.Widget) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleHoverWithLayouts env d.state contentId hitPath mouseX mouseY layouts widget
  pure { id := d.id, state := state' }

def handleKey (d : AnyDemo) (env : DemoEnv) (keyEvent : Afferent.Arbor.KeyEvent) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleKey env d.state keyEvent
  pure { id := d.id, state := state' }

def handleScrollWithLayouts (d : AnyDemo) (env : DemoEnv)
    (hitPath : Array Afferent.Arbor.WidgetId) (scrollEvt : Afferent.Arbor.ScrollEvent)
    (layouts : Trellis.LayoutResult) (widget : Afferent.Arbor.Widget) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleScrollWithLayouts env d.state hitPath scrollEvt layouts widget
  pure { id := d.id, state := state' }

def handleMouseUpWithLayouts (d : AnyDemo) (env : DemoEnv) (mouseX mouseY : Float)
    (hitPath : Array Afferent.Arbor.WidgetId)
    (layouts : Trellis.LayoutResult) (widget : Afferent.Arbor.Widget) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleMouseUpWithLayouts env d.state mouseX mouseY hitPath layouts widget
  pure { id := d.id, state := state' }

def step (d : AnyDemo) (c : Canvas) (env : DemoEnv) : IO (Canvas × AnyDemo) := do
  let inst := demoInstance d.id
  let (c', state') ← inst.step c env d.state
  pure (c', { id := d.id, state := state' })

def onExit (d : AnyDemo) (c : Canvas) (env : DemoEnv) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.onExit c env d.state
  pure { id := d.id, state := state' }

end AnyDemo

def mkAnyDemo (id : DemoId) (env : DemoEnv) : IO AnyDemo := do
  let inst := demoInstance id
  let state ← inst.init env
  pure { id := id, state := state }

/-- Build the demo list in display order. -/
def buildDemoList (env : DemoEnv) : IO (Array AnyDemo) := do
  let demoGrid ← mkAnyDemo .demoGrid env
  let gridPerf ← mkAnyDemo .gridPerf env
  let trianglesPerf ← mkAnyDemo .trianglesPerf env
  let circlesPerf ← mkAnyDemo .circlesPerf env
  let spritesPerf ← mkAnyDemo .spritesPerf env
  let layoutDemo ← mkAnyDemo .layout env
  let cssGridDemo ← mkAnyDemo .cssGrid env
  let reactiveShowcaseDemo ← mkAnyDemo .reactiveShowcase env
  let widgetPerfDemo ← mkAnyDemo .widgetPerf env
  let seascapeDemo ← mkAnyDemo .seascape env
  let shapeGalleryDemo ← mkAnyDemo .shapeGallery env
  let worldmapDemo ← mkAnyDemo .worldmap env
  let lineCapsDemo ← mkAnyDemo .lineCaps env
  let dashedLinesDemo ← mkAnyDemo .dashedLines env
  let linesPerfDemo ← mkAnyDemo .linesPerf env
  let textureMatrixDemo ← mkAnyDemo .textureMatrix env
  let orbitalInstancedDemo ← mkAnyDemo .orbitalInstanced env
  let fontShowcaseDemo ← mkAnyDemo .fontShowcase env
  let chatDemoDemo ← mkAnyDemo .chatDemo env
  -- Linalg vector demos
  let vectorInterpDemo ← mkAnyDemo .vectorInterpolation env
  let vectorArithDemo ← mkAnyDemo .vectorArithmetic env
  let vectorProjDemo ← mkAnyDemo .vectorProjection env
  let vectorFieldDemo ← mkAnyDemo .vectorField env
  let crossProduct3DDemo ← mkAnyDemo .crossProduct3D env
  -- Linalg matrix demos
  let matrix2DDemo ← mkAnyDemo .matrix2DTransform env
  let matrix3DDemo ← mkAnyDemo .matrix3DTransform env
  let projExplorerDemo ← mkAnyDemo .projectionExplorer env
  let matrixDecompDemo ← mkAnyDemo .matrixDecomposition env
  -- Linalg rotation demos
  let quatDemo ← mkAnyDemo .quaternionVisualizer env
  let slerpDemo ← mkAnyDemo .slerpInterpolation env
  let gimbalDemo ← mkAnyDemo .eulerGimbalLock env
  let dualQuatDemo ← mkAnyDemo .dualQuaternionBlending env
  -- Linalg geometry demos
  let rayPlayDemo ← mkAnyDemo .rayCastingPlayground env
  let overlapDemo ← mkAnyDemo .primitiveOverlapTester env
  let baryDemo ← mkAnyDemo .barycentricCoordinates env
  let frustumDemo ← mkAnyDemo .frustumCullingDemo env
  -- Linalg curve demos
  let bezierDemo ← mkAnyDemo .bezierCurveEditor env
  let catmullDemo ← mkAnyDemo .catmullRomSplineEditor env
  let bsplineDemo ← mkAnyDemo .bSplineCurveDemo env
  let arcLenDemo ← mkAnyDemo .arcLengthParameterization env
  let patchDemo ← mkAnyDemo .bezierPatchSurface env
  -- Linalg easing demos
  let easingDemo ← mkAnyDemo .easingFunctionGallery env
  let smoothDampDemo ← mkAnyDemo .smoothDampFollower env
  let springDemo ← mkAnyDemo .springAnimationPlayground env
  pure #[demoGrid, gridPerf, trianglesPerf, circlesPerf, spritesPerf, layoutDemo, cssGridDemo,
    reactiveShowcaseDemo, widgetPerfDemo, seascapeDemo, shapeGalleryDemo, worldmapDemo,
    lineCapsDemo, dashedLinesDemo, linesPerfDemo, textureMatrixDemo, orbitalInstancedDemo,
    fontShowcaseDemo, chatDemoDemo,
    -- Linalg vector demos
    vectorInterpDemo, vectorArithDemo, vectorProjDemo, vectorFieldDemo, crossProduct3DDemo,
    -- Linalg matrix demos
    matrix2DDemo, matrix3DDemo, projExplorerDemo, matrixDecompDemo,
    -- Linalg rotation demos
    quatDemo, slerpDemo, gimbalDemo, dualQuatDemo,
    -- Linalg geometry demos
    rayPlayDemo, overlapDemo, baryDemo, frustumDemo,
    -- Linalg curve demos
    bezierDemo, catmullDemo, bsplineDemo, arcLenDemo, patchDemo,
    -- Linalg easing demos
    easingDemo, smoothDampDemo, springDemo]

end Demos
