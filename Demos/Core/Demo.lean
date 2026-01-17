/-
  Demo Typeclass - Shared demo environment and polymorphic demo handling.
-/
import Afferent
import Afferent.Arbor
import Init.Data.FloatArray
import Trellis

open Afferent

namespace Demos

structure DemoEnv where
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
  spriteTexture : Afferent.FFI.Texture
  halfSize : Float
  circleRadius : Float
  spriteHalfSize : Float
  gridParticles : Afferent.Render.Dynamic.ParticleState
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
