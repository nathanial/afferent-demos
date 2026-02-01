/-
  Demo Runner - Canopy app shell for demo tabs.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Core.DemoRegistry
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
  let tabs : Array TabDef := demoIds.map fun id => {
    label := (demoInstance id).shortName
    content := match id with
      | .demoGrid => overviewTabContent env elapsedTime
      | .circlesPerf => circlesTabContent env elapsedTime circlesRef circlesTimeRef
      | .spritesPerf => spritesTabContent env elapsedTime spritesRef spritesTimeRef
      | .linesPerf => linesPerfTabContent env elapsedTime
      | .layout => layoutTabContent env elapsedTime
      | .cssGrid => cssGridTabContent env elapsedTime
      | .reactiveShowcase => reactiveShowcaseTabContent reactiveShowcaseApp
      | .widgetPerf => widgetPerfTabContent widgetPerfApp
      | .seascape => seascapeTabContent env elapsedTime seascapeRef seascapeTimeRef
          seascapeKeysRef seascapeLockRef seascapeDeltaRef
      | .shapeGallery => shapeGalleryTabContent env elapsedTime shapeGalleryIndexRef
      | .worldmap => worldmapTabContent env elapsedTime worldmapStateRef worldmapManager
      | .lineCaps => lineCapsTabContent env elapsedTime
      | .dashedLines => dashedLinesTabContent env elapsedTime
      | .textureMatrix => textureMatrixTabContent env elapsedTime
      | .orbitalInstanced => orbitalInstancedTabContent env elapsedTime
      | .fontShowcase => fontShowcaseTabContent env elapsedTime
      | .chatDemo => chatDemoTabContent chatDemoApp
      | .vectorInterpolation =>
          vectorInterpolationTabContent env elapsedTime vectorInterpolationRef vectorInterpolationTimeRef
      | .vectorArithmetic =>
          vectorArithmeticTabContent env elapsedTime vectorArithmeticRef
      | .vectorProjection =>
          vectorProjectionTabContent env elapsedTime vectorProjectionRef
      | .vectorField =>
          vectorFieldTabContent env elapsedTime vectorFieldRef
      | .crossProduct3D =>
          crossProduct3DTabContent env elapsedTime crossProduct3DRef
      | .matrix2DTransform =>
          matrix2DTransformTabContent env elapsedTime matrix2DTransformRef matrix2DTransformTimeRef
      | .matrix3DTransform =>
          matrix3DTransformTabContent env elapsedTime matrix3DTransformRef
      | .projectionExplorer =>
          projectionExplorerTabContent env elapsedTime projectionExplorerRef
      | .matrixDecomposition =>
          matrixDecompositionTabContent env elapsedTime matrixDecompositionRef
      | .quaternionVisualizer =>
          quaternionVisualizerTabContent env elapsedTime quaternionVisualizerRef
      | .slerpInterpolation =>
          slerpInterpolationTabContent env elapsedTime slerpInterpolationRef slerpInterpolationTimeRef
      | .eulerGimbalLock =>
          eulerGimbalLockTabContent env elapsedTime eulerGimbalLockRef
      | .dualQuaternionBlending =>
          dualQuaternionBlendingTabContent env elapsedTime dualQuaternionBlendingRef
      | .rayCastingPlayground =>
          rayCastingPlaygroundTabContent env elapsedTime rayCastingPlaygroundRef
      | .primitiveOverlapTester =>
          primitiveOverlapTesterTabContent env elapsedTime primitiveOverlapTesterRef
      | .barycentricCoordinates =>
          barycentricCoordinatesTabContent env elapsedTime barycentricCoordinatesRef
      | .frustumCullingDemo =>
          frustumCullingDemoTabContent env elapsedTime frustumCullingDemoRef
      | .bezierCurveEditor =>
          bezierCurveEditorTabContent env elapsedTime bezierCurveEditorRef
      | .catmullRomSplineEditor =>
          catmullRomSplineEditorTabContent env elapsedTime catmullRomSplineEditorRef catmullRomSplineTimeRef
      | _ => demoStubContent id
  }

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

  pure { render := render }

end Demos
