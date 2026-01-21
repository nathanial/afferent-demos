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
import Afferent.Canopy.Reactive
import Reactive.Host.Spider
import Worldmap
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

    -- Run the app setup within the env
    let (appState, events, inputs) ← (do
      let (events, inputs) ← Afferent.Canopy.Reactive.createInputs
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
    let (appState, events, inputs) ← (do
      let (events, inputs) ← Afferent.Canopy.Reactive.createInputs
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
    let diskConfig : Worldmap.TileDiskCacheConfig := {
      cacheDir := "./tile_cache"
      tilesetName := "carto-dark-2x"
      maxSizeBytes := Worldmap.defaultDiskCacheSizeBytes
    }
    let mapState ← Worldmap.MapState.init 37.7749 (-122.4194) 12
      (env.physWidth.toNat : Int) (env.physHeight.toNat : Int) diskConfig
    pure { mapState := mapState }
  update := fun env s => do
    let nextState ← updateWorldmapDemo env s.mapState
    pure { s with mapState := nextState }
  view := fun env s =>
    some (worldmapWidget env.screenScale env.fontMedium env.fontSmall env.windowWidthF env.windowHeightF s.mapState)
  step := fun c _ s => pure (c, s)

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
  pure #[demoGrid, gridPerf, trianglesPerf, circlesPerf, spritesPerf, layoutDemo, cssGridDemo,
    reactiveShowcaseDemo, widgetPerfDemo, seascapeDemo, shapeGalleryDemo, worldmapDemo,
    lineCapsDemo, dashedLinesDemo, linesPerfDemo, textureMatrixDemo, orbitalInstancedDemo,
    fontShowcaseDemo]

end Demos
