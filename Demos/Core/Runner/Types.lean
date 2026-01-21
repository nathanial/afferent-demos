/-
  Demo Runner - Shared types and constants.
-/
import Afferent
import Afferent.Arbor
import Demos.Core.DemoRegistry
import Demos.Core.TabBar
import Std.Data.HashMap
import Init.Data.FloatArray

set_option maxRecDepth 1024

open Afferent

namespace Demos

structure FontPack where
  registry : FontRegistry
  smallId : Afferent.Arbor.FontId
  mediumId : Afferent.Arbor.FontId
  largeId : Afferent.Arbor.FontId
  hugeId : Afferent.Arbor.FontId
  canopyId : Afferent.Arbor.FontId
  canopySmallId : Afferent.Arbor.FontId
  /-- Font showcase fonts keyed by "family-size" (e.g., "monaco-12", "helvetica-36") -/
  showcaseFonts : Std.HashMap String Afferent.Arbor.FontId

structure LoadingState where
  fontSmall : Option Font := none
  fontMedium : Option Font := none
  fontLarge : Option Font := none
  fontHuge : Option Font := none
  fontCanopy : Option Font := none
  fontCanopySmall : Option Font := none
  layoutFont : Option Font := none
  /-- Font showcase fonts keyed by "family-size" (e.g., "monaco-12") -/
  showcaseFonts : Std.HashMap String Font := {}
  /-- Track which showcase fonts have been loaded -/
  showcaseFontsLoaded : Nat := 0
  fontPack : Option FontPack := none
  spriteTexture : Option FFI.Texture := none
  lineSegments : Option (Array Float × Nat) := none
  lineBuffer : Option FFI.Buffer := none
  gridParticles : Option Render.Dynamic.ParticleState := none
  orbitalParams : Option FloatArray := none
  orbitalBuffer : Option FFI.FloatBuffer := none

structure LoadedAssets where
  screenScale : Float
  fontSmall : Font
  fontMedium : Font
  fontLarge : Font
  fontHuge : Font
  fontCanopy : Font
  fontCanopySmall : Font
  layoutFont : Font
  /-- Font showcase fonts keyed by "family-size" (e.g., "monaco-12") -/
  showcaseFonts : Std.HashMap String Font
  fontPack : FontPack
  spriteTexture : FFI.Texture
  halfSize : Float
  circleRadius : Float
  spriteHalfSize : Float
  gridParticles : Render.Dynamic.ParticleState
  lineBuffer : FFI.Buffer
  lineCount : Nat
  lineWidth : Float
  orbitalCount : Nat
  orbitalParams : FloatArray
  orbitalBuffer : FFI.FloatBuffer
  physWidthF : Float
  physHeightF : Float
  physWidth : UInt32
  physHeight : UInt32
  layoutOffsetX : Float
  layoutOffsetY : Float
  layoutScale : Float

/-- Fixed tabbar height in logical pixels. -/
def tabBarHeight : Float := ({} : TabBarStyle).height
def footerBarHeight : Float := 90

structure RootBuild where
  widget : Afferent.Arbor.Widget
  tabBar : TabBarResult
  contentId : Afferent.Arbor.WidgetId

structure FrameCache where
  rootBuild : RootBuild
  measuredWidget : Afferent.Arbor.Widget
  layouts : Trellis.LayoutResult
  hitIndex : Afferent.Arbor.HitTestIndex

structure RunningState where
  assets : LoadedAssets
  demos : Array AnyDemo
  demoRefs : Array (IO.Ref AnyDemo)
  displayMode : Nat
  frameCount : Nat
  fpsAccumulator : Float
  displayFps : Float
  renderCommandCount : Nat
  widgetCount : Nat
  cacheHits : Nat
  cacheMisses : Nat
  cacheSize : Nat
  cacheCapacity : Nat
  batchedCalls : Nat
  individualCalls : Nat
  rectsBatched : Nat
  circlesBatched : Nat
  strokeRectsBatched : Nat
  linesBatched : Nat
  textsBatched : Nat
  peakRssKb : UInt64
  minorFaults : UInt64
  majorFaults : UInt64
  framesLeft : Nat
  tabBar : TabBarResult
  frameCache : Option FrameCache := none
  lastHoverPath : Array Afferent.Arbor.WidgetId := #[]
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  prevLeftDown : Bool := false
  -- Timing stats (in milliseconds)
  timeUpdateMs : Float := 0.0
  timeBuildMs : Float := 0.0
  timeLayoutMs : Float := 0.0
  timeCollectMs : Float := 0.0
  timeGpuMs : Float := 0.0
  collectLookupMs : Float := 0.0
  collectTouchMs : Float := 0.0
  collectEmitAllMs : Float := 0.0
  collectSpecMs : Float := 0.0
  collectInsertMs : Float := 0.0
  collectLookupCount : Nat := 0
  collectTouchCount : Nat := 0
  collectEmitAllCount : Nat := 0
  collectSpecCount : Nat := 0
  collectInsertCount : Nat := 0
  -- GPU phase breakdown
  timeFlattenMs : Float := 0.0
  timeCoalesceMs : Float := 0.0
  timeBatchLoopMs : Float := 0.0
  timeDrawCallsMs : Float := 0.0
  -- Canopy demo stats (for debugging memory leaks)
  canopyStats : Option CanopyDemoStats := none

inductive AppState where
  | loading (state : LoadingState)
  | running (state : RunningState)

end Demos
