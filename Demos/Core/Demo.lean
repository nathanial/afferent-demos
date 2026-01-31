/-
  Demo Typeclass - Shared demo environment and polymorphic demo handling.
-/
import Afferent
import Afferent.Arbor
import Init.Data.FloatArray
import Trellis
import Std.Data.HashMap

open Afferent

namespace Demos

structure RunnerStats where
  frameMs : Float := 0.0
  fps : Float := 0.0
  layoutMs : Float := 0.0
  collectMs : Float := 0.0
  executeMs : Float := 0.0
  commandCount : Nat := 0
  cacheHits : Nat := 0
  cacheMisses : Nat := 0
  widgetCount : Nat := 0
  layoutCount : Nat := 0
  deriving Inhabited

structure DemoEnv where
  /-- Runner stats shared with the Canopy footer widget. -/
  statsRef : IO.Ref RunnerStats
  screenScale : Float
  t : Float
  dt : Float
  keyCode : UInt16
  clearKey : IO Unit
  window : Afferent.FFI.Window
  fontSmall : Afferent.Font
  fontMedium : Afferent.Font
  fontLarge : Afferent.Font
  fontHuge : Afferent.Font
  fontCanopy : Afferent.Font
  fontCanopySmall : Afferent.Font
  layoutFont : Afferent.Font
  fontRegistry : Afferent.FontRegistry
  fontMediumId : Afferent.Arbor.FontId
  fontSmallId : Afferent.Arbor.FontId
  fontLargeId : Afferent.Arbor.FontId
  fontHugeId : Afferent.Arbor.FontId
  fontCanopyId : Afferent.Arbor.FontId
  fontCanopySmallId : Afferent.Arbor.FontId
  /-- Font showcase fonts keyed by "family-size" (e.g., "monaco-12", "helvetica-36") -/
  showcaseFonts : Std.HashMap String Afferent.Arbor.FontId
  spriteTexture : Afferent.FFI.Texture
  circleRadius : Float
  spriteHalfSize : Float
  lineBuffer : Afferent.FFI.Buffer
  lineCount : Nat
  lineWidth : Float
  orbitalCount : Nat
  orbitalParams : FloatArray
  orbitalBuffer : Afferent.FFI.FloatBuffer
  windowWidthF : Float
  windowHeightF : Float
  physWidthF : Float
  physHeightF : Float
  physWidth : UInt32
  physHeight : UInt32
  contentOffsetX : Float
  contentOffsetY : Float
  layoutOffsetX : Float
  layoutOffsetY : Float
  layoutScale : Float

def withContentRect (layout : Trellis.ComputedLayout) (draw : Float → Float → Afferent.CanvasM Unit) : Afferent.CanvasM Unit := do
  let rect := layout.contentRect
  Afferent.CanvasM.save
  Afferent.CanvasM.setBaseTransform (Transform.translate rect.x rect.y)
  Afferent.CanvasM.resetTransform
  Afferent.CanvasM.clip (Afferent.Rect.mk' 0 0 rect.width rect.height)
  draw rect.width rect.height
  Afferent.CanvasM.restore

end Demos
