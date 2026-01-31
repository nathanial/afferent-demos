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
import Demos.Linalg.NoiseExplorer2D
import Demos.Linalg.FBMTerrainGenerator
import Demos.Linalg.DomainWarpingDemo
import Demos.Linalg.WorleyCellularNoise
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
  | noiseExplorer2D
  | fbmTerrainGenerator
  | domainWarpingDemo
  | worleyCellularNoise
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
  | .noiseExplorer2D => Linalg.NoiseExplorerState
  | .fbmTerrainGenerator => Linalg.FBMTerrainState
  | .domainWarpingDemo => Linalg.DomainWarpingState
  | .worleyCellularNoise => Linalg.WorleyCellularState

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


end Demos
