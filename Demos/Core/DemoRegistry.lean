/-
  Demo Registry - Demo ids, states, and instances.
-/
import Demos.Core.DemoRegistry.Types
import Demos.Core.DemoRegistry.CoreDemos
import Demos.Core.DemoRegistry.LinalgDemos

open Afferent

namespace Demos

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

/-- Demo ids in display order. -/
def demoIds : Array DemoId := #[
  .demoGrid, .gridPerf, .trianglesPerf, .circlesPerf, .spritesPerf, .layout, .cssGrid,
  .reactiveShowcase, .widgetPerf, .seascape, .shapeGallery, .worldmap, .lineCaps,
  .dashedLines, .linesPerf, .textureMatrix, .orbitalInstanced, .fontShowcase, .chatDemo,
  -- Linalg vector demos
  .vectorInterpolation, .vectorArithmetic, .vectorProjection, .vectorField, .crossProduct3D,
  -- Linalg matrix demos
  .matrix2DTransform, .matrix3DTransform, .projectionExplorer, .matrixDecomposition,
  -- Linalg rotation demos
  .quaternionVisualizer, .slerpInterpolation, .eulerGimbalLock, .dualQuaternionBlending,
  -- Linalg geometry demos
  .rayCastingPlayground, .primitiveOverlapTester, .barycentricCoordinates, .frustumCullingDemo,
  -- Linalg curve demos
  .bezierCurveEditor, .catmullRomSplineEditor, .bSplineCurveDemo, .arcLengthParameterization, .bezierPatchSurface,
  -- Linalg easing demos
  .easingFunctionGallery, .smoothDampFollower, .springAnimationPlayground,
  -- Linalg noise demos
  .noiseExplorer2D, .fbmTerrainGenerator, .domainWarpingDemo, .worleyCellularNoise
]

/-- Build the demo list in display order. -/
def buildDemoList (env : DemoEnv) : IO (Array AnyDemo) := do
  demoIds.mapM (fun id => mkAnyDemo id env)

end Demos
