/-
  Demo Registry - Core demo instances.
-/
import Demos.Core.DemoRegistry.Types

open Afferent

namespace Demos

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


end Demos
