/-
  Demo Typeclass - Shared demo environment and polymorphic demo handling.
-/
import Afferent
import Afferent.Arbor
import Demos.Text
import Init.Data.FloatArray

open Afferent

namespace Demos

structure DemoEnv where
  screenScale : Float
  t : Float
  dt : Float
  keyCode : UInt16
  fontSmall : Afferent.Font
  fontMedium : Afferent.Font
  fontLarge : Afferent.Font
  fontHuge : Afferent.Font
  layoutFont : Afferent.Font
  fonts : Fonts
  fontRegistry : Afferent.FontRegistry
  fontMediumId : Afferent.Arbor.FontId
  fontSmallId : Afferent.Arbor.FontId
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
  physWidthF : Float
  physHeightF : Float
  physWidth : UInt32
  physHeight : UInt32
  layoutOffsetX : Float
  layoutOffsetY : Float
  layoutScale : Float

end Demos
