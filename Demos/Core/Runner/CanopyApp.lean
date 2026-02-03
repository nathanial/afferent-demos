/-
  Demo Runner - Canopy app shell for demo tabs.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Core.Runner.CanopyApp.Support
import Demos.Core.Runner.CanopyApp.Tabs.Core
import Demos.Core.Runner.CanopyApp.Tabs.Linalg
import Demos.Core.Runner.CanopyApp.Tabs.Visuals
import Tileset
import Worldmap
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
  /-- Cleanup resources allocated by the app (e.g., tile manager workers). -/
  shutdown : IO Unit

/-- Create the demo shell as a single Canopy widget tree. -/
def createCanopyApp (env : DemoEnv) : ReactiveM CanopyAppState := do
  let reactiveShowcaseApp ← ReactiveShowcase.createApp env
  let widgetPerfApp ← WidgetPerf.createApp env
  let chatDemoApp ← ChatDemo.createApp env
  let worldmapTileConfig : Tileset.TileManagerConfig := {
    provider := Tileset.TileProvider.cartoDarkRetina
    diskCacheDir := "./tile_cache"
    diskCacheMaxSize := 500 * 1024 * 1024
  }
  let worldmapManager ← Tileset.TileManager.new worldmapTileConfig
  let tabs : Array TabDef := #[
    { label := "Overview", content := overviewTabContent env },
    { label := "Circles", content := circlesTabContent env },
    { label := "Sprites", content := spritesTabContent env },
    { label := "Layout", content := layoutTabContent env },
    { label := "CSS Grid", content := cssGridTabContent env },
    { label := "Reactive", content := reactiveShowcaseTabContent reactiveShowcaseApp },
    { label := "Widget Perf", content := widgetPerfTabContent widgetPerfApp },
    { label := "Seascape", content := seascapeTabContent env },
    { label := "Shapes", content := shapeGalleryTabContent env },
    { label := "Map", content := worldmapTabContent env worldmapManager },
    { label := "Line Caps", content := lineCapsTabContent env },
    { label := "Dashed", content := dashedLinesTabContent env },
    { label := "Lines", content := linesPerfTabContent env },
    { label := "Textures", content := textureMatrixTabContent env },
    { label := "Orbital", content := orbitalInstancedTabContent env },
    { label := "Fonts", content := fontShowcaseTabContent env },
    { label := "Chat", content := chatDemoTabContent chatDemoApp },
    { label := "Lerp", content := vectorInterpolationTabContent env },
    { label := "Arithmetic", content := vectorArithmeticTabContent env },
    { label := "Projection", content := vectorProjectionTabContent env },
    { label := "Field", content := vectorFieldTabContent env },
    { label := "Field3D", content := vectorField3DTabContent env },
    { label := "Cross 3D", content := crossProduct3DTabContent env },
    { label := "Mat2D", content := matrix2DTransformTabContent env },
    { label := "Mat3D", content := matrix3DTransformTabContent env },
    { label := "Proj", content := projectionExplorerTabContent env },
    { label := "Decomp", content := matrixDecompositionTabContent env },
    { label := "Quat", content := quaternionVisualizerTabContent env },
    { label := "Slerp", content := slerpInterpolationTabContent env },
    { label := "Gimbal", content := eulerGimbalLockTabContent env },
    { label := "DualQuat", content := dualQuaternionBlendingTabContent env },
    { label := "Ray", content := rayCastingPlaygroundTabContent env },
    { label := "Overlap", content := primitiveOverlapTesterTabContent env },
    { label := "Voronoi", content := voronoiDelaunayDualTabContent env },
    { label := "Hull", content := convexHull2DTabContent env },
    { label := "ConvexD", content := convexDecompositionTabContent env },
    { label := "Hierarchy", content := transformHierarchyTabContent env },
    { label := "Bary", content := barycentricCoordinatesTabContent env },
    { label := "Frustum", content := frustumCullingDemoTabContent env },
    { label := "Quadtree", content := quadtreeVisualizerTabContent env },
    { label := "Octree", content := octreeViewer3DTabContent env },
    { label := "BVH", content := bvhRayTracerTabContent env },
    { label := "KDTree", content := kdTreeNearestNeighborTabContent env },
    { label := "Integrate", content := particleIntegrationComparisonTabContent env },
    { label := "Collision", content := collisionResponseDemoTabContent env },
    { label := "Rigid", content := rigidBodySimulatorTabContent env },
    { label := "Inertia", content := inertiaTensorVisualizerTabContent env },
    { label := "Swept", content := sweptCollisionDemoTabContent env },
    { label := "Constraint", content := constraintSolverTabContent env },
    { label := "Bezier", content := bezierCurveEditorTabContent env },
    { label := "Catmull", content := catmullRomSplineEditorTabContent env },
    { label := "B-Spline", content := bSplineCurveDemoTabContent env },
    { label := "ArcLen", content := arcLengthParameterizationTabContent env },
    { label := "Patch", content := bezierPatchSurfaceTabContent env },
    { label := "Easing", content := easingFunctionGalleryTabContent env },
    { label := "SmoothD", content := smoothDampFollowerTabContent env },
    { label := "Spring", content := springAnimationPlaygroundTabContent env },
    { label := "Noise2D", content := noiseExplorer2DTabContent env },
    { label := "Terrain", content := fbmTerrainGeneratorTabContent env },
    { label := "Warp", content := domainWarpingDemoTabContent env },
    { label := "Worley", content := worleyCellularNoiseTabContent env }
  ]

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
      let elapsedTime ← useElapsedTime
      statsFooter env elapsedTime

  pure { render := render, shutdown := Tileset.TileManager.shutdown worldmapManager }

end Demos
