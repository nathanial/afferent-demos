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
  -- Linalg noise demos
  let noiseExplorerDemo ← mkAnyDemo .noiseExplorer2D env
  let terrainDemo ← mkAnyDemo .fbmTerrainGenerator env
  let warpDemo ← mkAnyDemo .domainWarpingDemo env
  let worleyDemo ← mkAnyDemo .worleyCellularNoise env
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
    easingDemo, smoothDampDemo, springDemo,
    -- Linalg noise demos
    noiseExplorerDemo, terrainDemo, warpDemo, worleyDemo]

end Demos
