/-
  Sprites Performance Test - Bunnymark-style textured sprites
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Render textured sprites using FloatBuffer (high-performance Bunnymark).
    Lean physics, FloatBuffer for zero-copy GPU rendering. -/
def renderSpriteTestFastM (font : Font) (particles : Render.Dynamic.ParticleState)
    (spriteBuffer : FFI.FloatBuffer) (texture : FFI.Texture) (halfSize : Float) : CanvasM Unit := do
  setFillColor Color.white
  fillTextXY s!"Sprites: {particles.count} textured sprites [FloatBuffer] (Space to advance)" 20 30 font
  -- Write particle positions to FloatBuffer (1 FFI call per sprite)
  Render.Dynamic.writeSpritesToBuffer particles spriteBuffer halfSize
  -- Render from FloatBuffer (zero-copy to GPU)
  let renderer ← getRenderer
  Render.Dynamic.drawSpritesFromBuffer renderer texture spriteBuffer particles.count.toUInt32 halfSize particles.screenWidth particles.screenHeight

end Demos
