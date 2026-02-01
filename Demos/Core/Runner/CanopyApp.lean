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
  let elapsedTime ← useElapsedTime
  let reactiveShowcaseApp ← ReactiveShowcase.createApp env
  let widgetPerfApp ← WidgetPerf.createApp env
  let chatDemoApp ← ChatDemo.createApp env
  let circlesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 42
    IO.mkRef particles
  let circlesTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let spritesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 123
    IO.mkRef particles
  let spritesTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let seascapeRef ← SpiderM.liftIO (IO.mkRef { camera := seascapeCamera })
  let seascapeTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let seascapeKeysRef ← SpiderM.liftIO (IO.mkRef ({} : SeascapeInputState))
  let seascapeLockRef ← SpiderM.liftIO (IO.mkRef false)
  let seascapeDeltaRef ← SpiderM.liftIO (IO.mkRef { dx := 0.0, dy := 0.0 })
  let shapeGalleryIndexRef ← SpiderM.liftIO (IO.mkRef 0)
  let vectorInterpolationRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorInterpolationInitialState)
  let vectorInterpolationTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let vectorArithmeticRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorArithmeticInitialState)
  let vectorProjectionRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorProjectionInitialState)
  let vectorFieldRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorFieldInitialState)
  let crossProduct3DRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.crossProduct3DInitialState)
  let matrix2DTransformRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.matrix2DTransformInitialState)
  let matrix2DTransformTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let matrix3DTransformRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.matrix3DTransformInitialState)
  let projectionExplorerRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.projectionExplorerInitialState)
  let matrixDecompositionRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.matrixDecompositionInitialState)
  let quaternionVisualizerRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.quaternionVisualizerInitialState)
  let slerpInterpolationRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.slerpInterpolationInitialState)
  let slerpInterpolationTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let eulerGimbalLockRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.eulerGimbalLockInitialState)
  let dualQuaternionBlendingRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.dualQuaternionBlendingInitialState)
  let rayCastingPlaygroundRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.rayCastingPlaygroundInitialState)
  let primitiveOverlapTesterRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.primitiveOverlapTesterInitialState)
  let barycentricCoordinatesRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.barycentricCoordinatesInitialState)
  let frustumCullingDemoRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.frustumCullingDemoInitialState)
  let bezierCurveEditorRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.bezierCurveEditorInitialState)
  let catmullRomSplineEditorRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.catmullRomSplineEditorInitialState)
  let catmullRomSplineTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let bSplineCurveDemoRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.bSplineCurveDemoInitialState)
  let arcLengthParameterizationRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.arcLengthParameterizationInitialState)
  let arcLengthTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let bezierPatchSurfaceRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.bezierPatchSurfaceInitialState)
  let easingFunctionGalleryRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.easingFunctionGalleryInitialState)
  let easingTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let smoothDampFollowerRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.smoothDampFollowerInitialState)
  let smoothDampTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let springAnimationPlaygroundRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.springAnimationPlaygroundInitialState)
  let springTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let noiseExplorer2DRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.noiseExplorer2DInitialState)
  let fbmTerrainGeneratorRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.fbmTerrainInitialState)
  let domainWarpingDemoRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.domainWarpingInitialState)
  let domainWarpingTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let worleyCellularNoiseRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.worleyCellularInitialState)
  let worldmapStateRef ← SpiderM.liftIO do
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
    let state ← Worldmap.MapState.init config
    IO.mkRef state
  let worldmapTileConfig : Tileset.TileManagerConfig := {
    provider := Tileset.TileProvider.cartoDarkRetina
    diskCacheDir := "./tile_cache"
    diskCacheMaxSize := 500 * 1024 * 1024
  }
  let worldmapManager ← Tileset.TileManager.new worldmapTileConfig
  let tabs : Array TabDef := #[
    { label := "Overview", content := overviewTabContent env elapsedTime },
    { label := "Circles", content := circlesTabContent env elapsedTime circlesRef circlesTimeRef },
    { label := "Sprites", content := spritesTabContent env elapsedTime spritesRef spritesTimeRef },
    { label := "Layout", content := layoutTabContent env elapsedTime },
    { label := "CSS Grid", content := cssGridTabContent env elapsedTime },
    { label := "Reactive", content := reactiveShowcaseTabContent reactiveShowcaseApp },
    { label := "Widget Perf", content := widgetPerfTabContent widgetPerfApp },
    { label := "Seascape", content :=
        seascapeTabContent env elapsedTime seascapeRef seascapeTimeRef
          seascapeKeysRef seascapeLockRef seascapeDeltaRef },
    { label := "Shapes", content := shapeGalleryTabContent env elapsedTime shapeGalleryIndexRef },
    { label := "Map", content := worldmapTabContent env elapsedTime worldmapStateRef worldmapManager },
    { label := "Line Caps", content := lineCapsTabContent env elapsedTime },
    { label := "Dashed", content := dashedLinesTabContent env elapsedTime },
    { label := "Lines", content := linesPerfTabContent env elapsedTime },
    { label := "Textures", content := textureMatrixTabContent env elapsedTime },
    { label := "Orbital", content := orbitalInstancedTabContent env elapsedTime },
    { label := "Fonts", content := fontShowcaseTabContent env elapsedTime },
    { label := "Chat", content := chatDemoTabContent chatDemoApp },
    { label := "Lerp", content :=
        vectorInterpolationTabContent env elapsedTime vectorInterpolationRef vectorInterpolationTimeRef },
    { label := "Arithmetic", content := vectorArithmeticTabContent env elapsedTime vectorArithmeticRef },
    { label := "Projection", content := vectorProjectionTabContent env elapsedTime vectorProjectionRef },
    { label := "Field", content := vectorFieldTabContent env elapsedTime vectorFieldRef },
    { label := "Cross 3D", content := crossProduct3DTabContent env elapsedTime crossProduct3DRef },
    { label := "Mat2D", content :=
        matrix2DTransformTabContent env elapsedTime matrix2DTransformRef matrix2DTransformTimeRef },
    { label := "Mat3D", content := matrix3DTransformTabContent env elapsedTime matrix3DTransformRef },
    { label := "Proj", content := projectionExplorerTabContent env elapsedTime projectionExplorerRef },
    { label := "Decomp", content := matrixDecompositionTabContent env elapsedTime matrixDecompositionRef },
    { label := "Quat", content := quaternionVisualizerTabContent env elapsedTime quaternionVisualizerRef },
    { label := "Slerp", content :=
        slerpInterpolationTabContent env elapsedTime slerpInterpolationRef slerpInterpolationTimeRef },
    { label := "Gimbal", content := eulerGimbalLockTabContent env elapsedTime eulerGimbalLockRef },
    { label := "DualQuat", content := dualQuaternionBlendingTabContent env elapsedTime dualQuaternionBlendingRef },
    { label := "Ray", content := rayCastingPlaygroundTabContent env elapsedTime rayCastingPlaygroundRef },
    { label := "Overlap", content := primitiveOverlapTesterTabContent env elapsedTime primitiveOverlapTesterRef },
    { label := "Bary", content := barycentricCoordinatesTabContent env elapsedTime barycentricCoordinatesRef },
    { label := "Frustum", content := frustumCullingDemoTabContent env elapsedTime frustumCullingDemoRef },
    { label := "Bezier", content := bezierCurveEditorTabContent env elapsedTime bezierCurveEditorRef },
    { label := "Catmull", content :=
        catmullRomSplineEditorTabContent env elapsedTime catmullRomSplineEditorRef catmullRomSplineTimeRef },
    { label := "B-Spline", content := bSplineCurveDemoTabContent env elapsedTime bSplineCurveDemoRef },
    { label := "ArcLen", content :=
        arcLengthParameterizationTabContent env elapsedTime arcLengthParameterizationRef arcLengthTimeRef },
    { label := "Patch", content := bezierPatchSurfaceTabContent env elapsedTime bezierPatchSurfaceRef },
    { label := "Easing", content := easingFunctionGalleryTabContent env elapsedTime easingFunctionGalleryRef easingTimeRef },
    { label := "SmoothD", content := smoothDampFollowerTabContent env elapsedTime smoothDampFollowerRef smoothDampTimeRef },
    { label := "Spring", content :=
        springAnimationPlaygroundTabContent env elapsedTime springAnimationPlaygroundRef springTimeRef },
    { label := "Noise2D", content := noiseExplorer2DTabContent env elapsedTime noiseExplorer2DRef },
    { label := "Terrain", content := fbmTerrainGeneratorTabContent env elapsedTime fbmTerrainGeneratorRef },
    { label := "Warp", content :=
        domainWarpingDemoTabContent env elapsedTime domainWarpingDemoRef domainWarpingTimeRef },
    { label := "Worley", content := worleyCellularNoiseTabContent env elapsedTime worleyCellularNoiseRef }
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
      statsFooter env elapsedTime

  pure { render := render, shutdown := Tileset.TileManager.shutdown worldmapManager }

end Demos
