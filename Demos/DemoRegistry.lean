/-
  Demo Registry - Demo ids, states, and instances.
-/
import Demos.Demo
import Demos.Overview.DemoGrid
import Demos.Overview.Card
import Demos.Layout
import Demos.GridPerf
import Demos.TrianglesPerf
import Demos.CirclesPerf
import Demos.SpritesPerf
import Demos.Grid
import Demos.Widgets
import Demos.Overview.Counter
import Demos.Overview.SpinningCubes
import Demos.Seascape
import Demos.ShapeGallery
import Demos.LineCaps
import Demos.DashedLines
import Demos.LinesPerf
import Demos.TextureMatrix
import Demos.OrbitalInstanced
import Demos.WorldmapDemo
import Demos.CanopyShowcase
import Worldmap

open Afferent

namespace Demos

inductive DemoId where
  | demoGrid
  | gridPerf
  | trianglesPerf
  | circlesPerf
  | spritesPerf
  | layout
  | cssGrid
  | widgets
  | canopyWidgets
  | seascape
  | shapeGallery
  | worldmap
  | lineCaps
  | dashedLines
  | linesPerf
  | textureMatrix
  | orbitalInstanced
  deriving Repr, BEq, Inhabited

structure CirclesState where
  particles : Render.Dynamic.ParticleState

structure SpritesState where
  particles : Render.Dynamic.ParticleState

structure DemoGridState where
  counter : CounterState
  spinningCubes : SpinningCubesState

structure ShapeGalleryState where
  index : Nat

structure WorldmapState where
  mapState : Worldmap.MapState

/-- Demo state mapping by id. -/
def DemoState : DemoId → Type
  | .demoGrid => DemoGridState
  | .gridPerf => Unit
  | .trianglesPerf => Unit
  | .circlesPerf => CirclesState
  | .spritesPerf => SpritesState
  | .layout => Unit
  | .cssGrid => Unit
  | .widgets => Unit
  | .canopyWidgets => CanopyShowcaseState
  | .seascape => SeascapeState
  | .shapeGallery => ShapeGalleryState
  | .worldmap => WorldmapState
  | .lineCaps => Unit
  | .dashedLines => Unit
  | .linesPerf => Unit
  | .textureMatrix => Unit
  | .orbitalInstanced => Unit

class Demo (id : DemoId) where
  name : String
  shortName : String  -- Short name for tab display
  msaaEnabled : Bool := true
  init : DemoEnv → IO (DemoState id)
  update : DemoEnv → DemoState id → IO (DemoState id) := fun _ s => pure s
  view : DemoEnv → DemoState id → Option Afferent.Arbor.WidgetBuilder := fun _ _ => none
  handleClick : DemoEnv → DemoState id → Afferent.Arbor.WidgetId → Array Afferent.Arbor.WidgetId →
      Afferent.FFI.ClickEvent → IO (DemoState id) := fun _ s _ _ _ => pure s
  /-- Handle mouse hover (called when hovered widget changes). -/
  handleHover : DemoEnv → DemoState id → Afferent.Arbor.WidgetId → Array Afferent.Arbor.WidgetId →
      Float → Float → IO (DemoState id) := fun _ s _ _ _ _ => pure s
  /-- Handle keyboard input (called when a key is pressed). -/
  handleKey : DemoEnv → DemoState id → Afferent.Arbor.KeyEvent → IO (DemoState id) := fun _ s _ => pure s
  step : Canvas → DemoEnv → DemoState id → IO (Canvas × DemoState id)
  onExit : Canvas → DemoEnv → DemoState id → IO (DemoState id) := fun _ _ s => pure s

structure AnyDemo where
  id : DemoId
  state : DemoState id

instance : Inhabited AnyDemo :=
  ⟨{ id := .demoGrid, state := {
    counter := CounterState.initial
    spinningCubes := spinningCubesInitialState
  } }⟩

private def demoFontsFromEnv (env : DemoEnv) : DemoFonts := {
  label := env.fontSmallId,
  small := env.fontSmallId,
  medium := env.fontMediumId,
  large := env.fontLargeId,
  huge := env.fontHugeId
}

private partial def findWidgetIdByName (widget : Afferent.Arbor.Widget)
    (target : String) : Option Afferent.Arbor.WidgetId :=
  let widgetName := Afferent.Arbor.Widget.name? widget
  match widgetName with
  | some name =>
      if name == target then
        some (Afferent.Arbor.Widget.id widget)
      else
        findInChildren widget target
  | none =>
      findInChildren widget target
where
  findInChildren (widget : Afferent.Arbor.Widget) (target : String)
      : Option Afferent.Arbor.WidgetId :=
    let children := Afferent.Arbor.Widget.children widget
    let rec loop (idx : Nat) : Option Afferent.Arbor.WidgetId :=
      if idx >= children.size then
        none
      else
        match children[idx]? with
        | some child =>
            match findWidgetIdByName child target with
            | some result => some result
            | none => loop (idx + 1)
        | none => loop (idx + 1)
    loop 0

private def hitPathHasNamedWidget (widget : Afferent.Arbor.Widget)
    (hitPath : Array Afferent.Arbor.WidgetId) (name : String) : Bool :=
  match findWidgetIdByName widget name with
  | some wid => hitPath.any (· == wid)
  | none => false

instance : Demo .demoGrid where
  name := "DEMO mode"
  shortName := "Overview"
  init := fun _ => pure {
    counter := CounterState.initial
    spinningCubes := spinningCubesInitialState
  }
  update := fun env state => do
    let nextCubes ← updateSpinningCubesState env state.spinningCubes
    pure { state with spinningCubes := nextCubes }
  view := fun env state =>
    let demoFonts := demoFontsFromEnv env
    some (demoGridWidget env.screenScale env.t demoFonts state.counter.value state.spinningCubes
      env.windowWidthF env.windowHeightF)
  handleClick := fun env state contentId hitPath click => do
    if click.button != 0 then
      pure state
    else
      let demoFonts := demoFontsFromEnv env
      let gridWidget :=
        Afferent.Arbor.buildFrom contentId
          (demoGridWidget env.screenScale env.t demoFonts state.counter.value state.spinningCubes
            env.windowWidthF env.windowHeightF)
      let clickedIncrement := hitPathHasNamedWidget gridWidget hitPath counterIncrementName
      let clickedDecrement := hitPathHasNamedWidget gridWidget hitPath counterDecrementName
      let clickedReset := hitPathHasNamedWidget gridWidget hitPath counterResetName
      let clickedSpinningCubes := hitPathHasNamedWidget gridWidget hitPath spinningCubesWidgetName
      if clickedSpinningCubes then
        FFI.Window.setPointerLock env.window true
      let nextCounter :=
        if clickedIncrement then
          CounterState.increment state.counter
        else if clickedDecrement then
          CounterState.decrement state.counter
        else if clickedReset then
          CounterState.reset state.counter
        else
          state.counter
      pure { state with counter := nextCounter }
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

instance : Demo .widgets where
  name := "WIDGET demo (full-size)"
  shortName := "Widgets"
  init := fun _ => pure ()
  view := fun env _ =>
    some (widgetDemo env.fontMediumId env.fontSmallId env.screenScale)
  step := fun c _ s => pure (c, s)

instance : Demo .canopyWidgets where
  name := "CANOPY widget library showcase"
  shortName := "Canopy"
  init := fun env => do
    -- Compute initial cursor position for pre-filled text input
    let initialText := "Hello, World!"
    let (cursorX, _) ← env.fontMedium.measureText initialText
    let textInput2 : Afferent.Canopy.TextInputState := {
      value := initialText
      cursor := initialText.length
      cursorPixelX := cursorX
    }
    pure { CanopyShowcaseState.initial with textInput2State := textInput2 }
  update := fun env state => do
    -- Animate switches towards their target states
    let animSpeed := 8.0  -- Animation speed (higher = faster)
    let dt := env.dt
    let rawFactor := animSpeed * dt
    let lerpFactor := if rawFactor > 1.0 then 1.0 else rawFactor
    -- Animate switch1: target is 1.0 if on, 0.0 if off
    let target1 := if state.switch1 then 1.0 else 0.0
    let diff1 := target1 - state.switch1Anim
    let newAnim1 := if diff1.abs < 0.01 then target1
                    else state.switch1Anim + diff1 * lerpFactor
    -- Animate switch2: target is 1.0 if on, 0.0 if off
    let target2 := if state.switch2 then 1.0 else 0.0
    let diff2 := target2 - state.switch2Anim
    let newAnim2 := if diff2.abs < 0.01 then target2
                    else state.switch2Anim + diff2 * lerpFactor
    pure { state with switch1Anim := newAnim1, switch2Anim := newAnim2 }
  view := fun env state =>
    some (canopyShowcaseWidget env.fontMediumId env.fontSmallId env.screenScale state)
  handleClick := fun env state contentId hitPath click => do
    if click.button != 0 then
      pure state
    else
      let widget :=
        Afferent.Arbor.buildFrom contentId
          (canopyShowcaseWidget env.fontMediumId env.fontSmallId env.screenScale state)
      -- Check button clicks
      let clickedPrimary := hitPathHasNamedWidget widget hitPath btnPrimaryName
      let clickedSecondary := hitPathHasNamedWidget widget hitPath btnSecondaryName
      let clickedOutline := hitPathHasNamedWidget widget hitPath btnOutlineName
      let clickedGhost := hitPathHasNamedWidget widget hitPath btnGhostName
      -- Check checkbox clicks
      let clickedCb1 := hitPathHasNamedWidget widget hitPath checkbox1Name
      let clickedCb2 := hitPathHasNamedWidget widget hitPath checkbox2Name
      -- Check text input clicks
      let clickedInput1 := hitPathHasNamedWidget widget hitPath textInput1Name
      let clickedInput2 := hitPathHasNamedWidget widget hitPath textInput2Name
      -- Check radio button clicks
      let clickedRadio1 := hitPathHasNamedWidget widget hitPath radio1Name
      let clickedRadio2 := hitPathHasNamedWidget widget hitPath radio2Name
      let clickedRadio3 := hitPathHasNamedWidget widget hitPath radio3Name
      -- Check switch clicks
      let clickedSwitch1 := hitPathHasNamedWidget widget hitPath switch1Name
      let clickedSwitch2 := hitPathHasNamedWidget widget hitPath switch2Name
      -- Update button click count
      let nextClickCount :=
        if clickedPrimary || clickedSecondary || clickedOutline || clickedGhost then
          state.buttonClickCount + 1
        else
          state.buttonClickCount
      -- Update checkbox states
      let nextCb1 := if clickedCb1 then !state.checkbox1 else state.checkbox1
      let nextCb2 := if clickedCb2 then !state.checkbox2 else state.checkbox2
      -- Update radio selection
      let nextRadioSelection :=
        if clickedRadio1 then "option1"
        else if clickedRadio2 then "option2"
        else if clickedRadio3 then "option3"
        else state.radioSelection
      -- Update switch states
      let nextSwitch1 := if clickedSwitch1 then !state.switch1 else state.switch1
      let nextSwitch2 := if clickedSwitch2 then !state.switch2 else state.switch2
      -- Update focus
      let nextFocus :=
        if clickedInput1 then some textInput1Name
        else if clickedInput2 then some textInput2Name
        else if clickedPrimary || clickedSecondary || clickedOutline || clickedGhost ||
                clickedCb1 || clickedCb2 || clickedRadio1 || clickedRadio2 || clickedRadio3 ||
                clickedSwitch1 || clickedSwitch2 then
          none  -- Clicking elsewhere clears focus
        else
          state.focusedInput
      pure { state with
        buttonClickCount := nextClickCount
        checkbox1 := nextCb1
        checkbox2 := nextCb2
        radioSelection := nextRadioSelection
        switch1 := nextSwitch1
        switch2 := nextSwitch2
        focusedInput := nextFocus
      }
  handleHover := fun env state contentId hitPath _mouseX _mouseY => do
    let widget :=
      Afferent.Arbor.buildFrom contentId
        (canopyShowcaseWidget env.fontMediumId env.fontSmallId env.screenScale state)
    -- Check which widgets are hovered
    let hoveredPrimary := hitPathHasNamedWidget widget hitPath btnPrimaryName
    let hoveredSecondary := hitPathHasNamedWidget widget hitPath btnSecondaryName
    let hoveredOutline := hitPathHasNamedWidget widget hitPath btnOutlineName
    let hoveredGhost := hitPathHasNamedWidget widget hitPath btnGhostName
    let hoveredCb1 := hitPathHasNamedWidget widget hitPath checkbox1Name
    let hoveredCb2 := hitPathHasNamedWidget widget hitPath checkbox2Name
    let hoveredRadio1 := hitPathHasNamedWidget widget hitPath radio1Name
    let hoveredRadio2 := hitPathHasNamedWidget widget hitPath radio2Name
    let hoveredRadio3 := hitPathHasNamedWidget widget hitPath radio3Name
    let hoveredSwitch1 := hitPathHasNamedWidget widget hitPath switch1Name
    let hoveredSwitch2 := hitPathHasNamedWidget widget hitPath switch2Name
    -- Update widget states
    let mut ws := state.widgetStates
    ws := ws.setHovered btnPrimaryName hoveredPrimary
    ws := ws.setHovered btnSecondaryName hoveredSecondary
    ws := ws.setHovered btnOutlineName hoveredOutline
    ws := ws.setHovered btnGhostName hoveredGhost
    ws := ws.setHovered checkbox1Name hoveredCb1
    ws := ws.setHovered checkbox2Name hoveredCb2
    ws := ws.setHovered radio1Name hoveredRadio1
    ws := ws.setHovered radio2Name hoveredRadio2
    ws := ws.setHovered radio3Name hoveredRadio3
    ws := ws.setHovered switch1Name hoveredSwitch1
    ws := ws.setHovered switch2Name hoveredSwitch2
    pure { state with widgetStates := ws }
  handleKey := fun env state keyEvent => do
    -- Only process keyboard input if a text input is focused
    match state.focusedInput with
    | some inputName =>
        if inputName == textInput1Name then
          let newInputState := Afferent.Canopy.TextInput.handleKeyPress keyEvent state.textInput1State none
          -- Measure text before cursor for accurate cursor positioning
          let beforeCursor := newInputState.value.take newInputState.cursor
          let (cursorX, _) ← env.fontMedium.measureText beforeCursor
          let finalState := { newInputState with cursorPixelX := cursorX }
          pure { state with textInput1State := finalState }
        else if inputName == textInput2Name then
          let newInputState := Afferent.Canopy.TextInput.handleKeyPress keyEvent state.textInput2State none
          -- Measure text before cursor for accurate cursor positioning
          let beforeCursor := newInputState.value.take newInputState.cursor
          let (cursorX, _) ← env.fontMedium.measureText beforeCursor
          let finalState := { newInputState with cursorPixelX := cursorX }
          pure { state with textInput2State := finalState }
        else
          pure state
    | none => pure state
  step := fun c _ s => pure (c, s)

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

def handleHover (d : AnyDemo) (env : DemoEnv) (contentId : Afferent.Arbor.WidgetId)
    (hitPath : Array Afferent.Arbor.WidgetId) (mouseX mouseY : Float) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleHover env d.state contentId hitPath mouseX mouseY
  pure { id := d.id, state := state' }

def handleKey (d : AnyDemo) (env : DemoEnv) (keyEvent : Afferent.Arbor.KeyEvent) : IO AnyDemo := do
  let inst := demoInstance d.id
  let state' ← inst.handleKey env d.state keyEvent
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
  let widgetsDemo ← mkAnyDemo .widgets env
  let canopyWidgetsDemo ← mkAnyDemo .canopyWidgets env
  let seascapeDemo ← mkAnyDemo .seascape env
  let shapeGalleryDemo ← mkAnyDemo .shapeGallery env
  let worldmapDemo ← mkAnyDemo .worldmap env
  let lineCapsDemo ← mkAnyDemo .lineCaps env
  let dashedLinesDemo ← mkAnyDemo .dashedLines env
  let linesPerfDemo ← mkAnyDemo .linesPerf env
  let textureMatrixDemo ← mkAnyDemo .textureMatrix env
  let orbitalInstancedDemo ← mkAnyDemo .orbitalInstanced env
  pure #[demoGrid, gridPerf, trianglesPerf, circlesPerf, spritesPerf, layoutDemo, cssGridDemo,
    widgetsDemo, canopyWidgetsDemo, seascapeDemo, shapeGalleryDemo, worldmapDemo,
    lineCapsDemo, dashedLinesDemo, linesPerfDemo,
    textureMatrixDemo, orbitalInstancedDemo]

end Demos
