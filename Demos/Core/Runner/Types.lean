/-
  Demo Runner - Shared types and constants.
-/
import Afferent
import Afferent.Arbor
import Afferent.Canopy.Reactive
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
def tabBarHeight : Float := tabBarTotalHeight ({} : TabBarStyle)
def footerBarHeight : Float := 90

structure RootBuild where
  widget : Afferent.Arbor.Widget
  tabBar : TabBarResult
  contentId : Afferent.Arbor.WidgetId

structure FrameCache where
  measuredWidget : Afferent.Arbor.Widget
  layouts : Trellis.LayoutResult
  hitIndex : Afferent.Arbor.HitTestIndex

structure RunningState where
  assets : LoadedAssets
  render : Afferent.Canopy.Reactive.ComponentRender
  events : Afferent.Canopy.Reactive.ReactiveEvents
  inputs : Afferent.Canopy.Reactive.ReactiveInputs
  spiderEnv : Reactive.Host.SpiderEnv
  cachedWidget : Afferent.Arbor.WidgetBuilder
  frameCache : Option FrameCache := none
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  prevLeftDown : Bool := false

inductive AppState where
  | loading (state : LoadingState)
  | running (state : RunningState)

end Demos
