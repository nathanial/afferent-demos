/-
  Demo Registry - Demo ids, states, and instances.
-/
import Demos.Demo
import Demos.DemoGrid
import Demos.GridPerf
import Demos.TrianglesPerf
import Demos.CirclesPerf
import Demos.SpritesPerf
import Demos.Layout
import Demos.Grid
import Demos.Widgets
import Demos.Interactive
import Demos.SpinningCubes
import Demos.Seascape
import Demos.PathFeatures
import Demos.ShapeGallery
import Demos.LineCaps
import Demos.DashedLines
import Demos.LinesPerf
import Demos.TextureMatrix
import Demos.OrbitalInstanced
import Demos.WorldmapDemo
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
  | interactive
  | spinningCubes
  | seascape
  | pathFeatures
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

structure SpinningCubesState where
  camera : Render.FPSCamera

structure SeascapeState where
  camera : Render.FPSCamera

structure ShapeGalleryState where
  index : Nat

structure WorldmapState where
  mapState : Worldmap.MapState

structure InteractiveState where
  counter : CounterState

/-- Demo state mapping by id. -/
def DemoState : DemoId → Type
  | .demoGrid => Unit
  | .gridPerf => Unit
  | .trianglesPerf => Unit
  | .circlesPerf => CirclesState
  | .spritesPerf => SpritesState
  | .layout => Unit
  | .cssGrid => Unit
  | .widgets => Unit
  | .interactive => InteractiveState
  | .spinningCubes => SpinningCubesState
  | .seascape => SeascapeState
  | .pathFeatures => Unit
  | .shapeGallery => ShapeGalleryState
  | .worldmap => WorldmapState
  | .lineCaps => Unit
  | .dashedLines => Unit
  | .linesPerf => Unit
  | .textureMatrix => Unit
  | .orbitalInstanced => Unit

class Demo (id : DemoId) where
  name : String
  msaaEnabled : Bool := true
  init : DemoEnv → IO (DemoState id)
  step : Canvas → DemoEnv → DemoState id → IO (Canvas × DemoState id)
  onExit : Canvas → DemoEnv → DemoState id → IO (DemoState id) := fun _ _ s => pure s

structure AnyDemo where
  id : DemoId
  state : DemoState id

instance : Inhabited AnyDemo :=
  ⟨{ id := .demoGrid, state := () }⟩

instance : Demo .demoGrid where
  name := "DEMO mode"
  init := fun _ => pure ()
  step := fun c env s => do
    let (currentW, currentH) ← c.ctx.getCurrentSize
    let c ← renderDemoGridFrame c env.screenScale currentW currentH env.fontSmall env.fonts env.fontRegistry env.fontSmallId env.t
    pure (c, s)

instance : Demo .gridPerf where
  name := "GRID (squares) performance test"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderGridPerfFrame c env.t env.fontMedium env.gridParticles env.halfSize
    pure (c, s)

instance : Demo .trianglesPerf where
  name := "TRIANGLES performance test"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderTrianglesPerfFrame c env.t env.fontMedium env.gridParticles env.halfSize
    pure (c, s)

instance : Demo .circlesPerf where
  name := "CIRCLES (bouncing) performance test"
  init := fun env => do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 42
    IO.println s!"Created {particles.count} bouncing circles"
    pure { particles := particles }
  step := fun c env s => do
    let (c', nextParticles) ←
      stepCirclesPerfFrame c env.dt env.t env.fontMedium s.particles env.circleRadius env.screenScale
    pure (c', { s with particles := nextParticles })

instance : Demo .spritesPerf where
  name := "SPRITES (Bunnymark) performance test"
  msaaEnabled := false
  init := fun env => do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 123
    IO.println s!"Created {particles.count} bouncing sprites"
    pure { particles := particles }
  step := fun c env s => do
    let (c', nextParticles) ←
      stepSpritesPerfFrame c env.dt env.fontMedium s.particles env.spriteTexture env.spriteHalfSize env.screenScale
    pure (c', { s with particles := nextParticles })

instance : Demo .layout where
  name := "LAYOUT demo (full-size)"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderLayoutDemoFrame c env.layoutFont env.fontMedium env.layoutOffsetX env.layoutOffsetY env.layoutScale env.screenScale
    pure (c, s)

instance : Demo .cssGrid where
  name := "CSS GRID demo (full-size)"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderCssGridDemoFrame c env.layoutFont env.fontMedium env.layoutOffsetX env.layoutOffsetY env.layoutScale env.screenScale
    pure (c, s)

instance : Demo .widgets where
  name := "WIDGET demo (full-size)"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderWidgetDemoFrame c env.fontRegistry env.fontMediumId env.fontSmallId env.physWidthF env.physHeightF env.screenScale env.fontMedium
    pure (c, s)

instance : Demo .interactive where
  name := "INTERACTIVE demo (click the buttons!)"
  init := fun _ => pure { counter := CounterState.initial }
  step := fun c env s => do
    let (c', nextCounter) ←
      stepInteractiveDemoFrame c env.fontRegistry env.fontMediumId env.fontSmallId env.physWidthF env.physHeightF env.screenScale s.counter env.fontMedium
    pure (c', { s with counter := nextCounter })

instance : Demo .spinningCubes where
  name := "3D SPINNING CUBES demo"
  init := fun _ => pure { camera := default }
  step := fun c env s => do
    let (c', nextCamera) ←
      stepSpinningCubesFrame c env.t env.dt env.keyCode env.screenScale env.fontMedium env.fontSmall s.camera
    pure (c', { s with camera := nextCamera })
  onExit := fun c _ s => do
    FFI.Window.setPointerLock c.ctx.window false
    pure s

instance : Demo .seascape where
  name := "SEASCAPE demo (Gerstner waves)"
  msaaEnabled := false
  init := fun _ => pure { camera := Demos.seascapeCamera }
  step := fun c env s => do
    let (c', nextCamera) ←
      stepSeascapeDemoFrame c env.t env.dt env.keyCode env.screenScale env.fontMedium env.fontSmall s.camera
    pure (c', { s with camera := nextCamera })
  onExit := fun c _ s => do
    FFI.Window.setPointerLock c.ctx.window false
    pure s

instance : Demo .pathFeatures where
  name := "PATH FEATURES demo (non-convex, arcTo, transforms)"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderPathFeaturesDemoFrame c env.screenScale env.fontSmall env.fontMedium
    pure (c, s)

instance : Demo .shapeGallery where
  name := "SHAPE GALLERY (arrow keys to navigate)"
  init := fun _ => pure { index := 0 }
  step := fun c env s => do
    let mut idx := s.index
    if env.keyCode == FFI.Key.right then
      idx := (idx + 1) % Demos.shapeGalleryCount
      c.clearKey
    else if env.keyCode == FFI.Key.left then
      idx := if idx == 0 then Demos.shapeGalleryCount - 1 else idx - 1
      c.clearKey
    let c ← renderShapeGalleryDemoFrame c idx env.physWidthF env.physHeightF env.screenScale env.fontLarge env.fontSmall env.fontMedium
    pure (c, { s with index := idx })

instance : Demo .worldmap where
  name := "WORLDMAP demo (drag to pan, scroll to zoom)"
  init := fun env => do
    let diskConfig : Worldmap.TileDiskCacheConfig := {
      cacheDir := "./tile_cache"
      tilesetName := "carto-dark-2x"
      maxSizeBytes := Worldmap.defaultDiskCacheSizeBytes
    }
    let mapState ← Worldmap.MapState.init 37.7749 (-122.4194) 12
      (env.physWidth.toNat : Int) (env.physHeight.toNat : Int) diskConfig
    pure { mapState := mapState }
  step := fun c env s => do
    let (c', nextState) ← stepWorldmapDemoFrame c s.mapState env.screenScale env.fontMedium env.fontSmall
    pure (c', { s with mapState := nextState })

instance : Demo .lineCaps where
  name := "LINE CAPS & JOINS demo"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderLineCapsDemoFrame c env.screenScale env.fontSmall env.fontMedium
    pure (c, s)

instance : Demo .dashedLines where
  name := "DASHED LINES demo"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderDashedLinesDemoFrame c env.screenScale env.fontSmall env.fontMedium
    pure (c, s)

instance : Demo .linesPerf where
  name := "100k LINES performance test"
  msaaEnabled := false
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderLinesPerfFrame c env.t env.lineBuffer env.lineCount env.lineWidth env.fontMedium
    pure (c, s)

instance : Demo .textureMatrix where
  name := "TEXTURE MATRIX demo (u_matrix scaling)"
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderTextureMatrixDemoFrame c env.t env.screenScale env.fontMedium env.fontSmall env.spriteTexture
    pure (c, s)

instance : Demo .orbitalInstanced where
  name := "ORBITAL instanced demo"
  msaaEnabled := false
  init := fun _ => pure ()
  step := fun c env s => do
    let c ← renderOrbitalInstancedDemoFrame c env.t env.screenScale env.fontMedium env.orbitalCount env.orbitalParams env.orbitalBuffer
    pure (c, s)

def demoInstance (id : DemoId) : Demo id := by
  cases id <;> infer_instance

namespace AnyDemo

def name (d : AnyDemo) : String :=
  (demoInstance d.id).name

def msaaEnabled (d : AnyDemo) : Bool :=
  (demoInstance d.id).msaaEnabled

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
  let interactiveDemo ← mkAnyDemo .interactive env
  let spinningCubesDemo ← mkAnyDemo .spinningCubes env
  let seascapeDemo ← mkAnyDemo .seascape env
  let pathFeaturesDemo ← mkAnyDemo .pathFeatures env
  let shapeGalleryDemo ← mkAnyDemo .shapeGallery env
  let worldmapDemo ← mkAnyDemo .worldmap env
  let lineCapsDemo ← mkAnyDemo .lineCaps env
  let dashedLinesDemo ← mkAnyDemo .dashedLines env
  let linesPerfDemo ← mkAnyDemo .linesPerf env
  let textureMatrixDemo ← mkAnyDemo .textureMatrix env
  let orbitalInstancedDemo ← mkAnyDemo .orbitalInstanced env
  pure #[demoGrid, gridPerf, trianglesPerf, circlesPerf, spritesPerf, layoutDemo, cssGridDemo,
    widgetsDemo, interactiveDemo, spinningCubesDemo, seascapeDemo, pathFeaturesDemo,
    shapeGalleryDemo, worldmapDemo, lineCapsDemo, dashedLinesDemo, linesPerfDemo,
    textureMatrixDemo, orbitalInstancedDemo]

end Demos
