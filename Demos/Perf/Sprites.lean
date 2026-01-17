/-
  Sprites Performance Test - Bunnymark-style textured sprites
-/
import Afferent
import Afferent.Arbor
import Demos.Core.Demo
import Trellis

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

def spritesPerfWidget (screenScale : Float) (font : Font) (texture : FFI.Texture)
    (particles : Render.Dynamic.ParticleState) (halfSize : Float) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun _ _ => do
        resetTransform
        setFillColor Color.white
        fillTextXY
          s!"Sprites: {particles.count} textured sprites (Space to advance)"
          (20 * screenScale) (30 * screenScale) font
        fillDynamicSprites texture particles halfSize
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

def stepSpritesPerfFrame (c : Canvas) (dt : Float) (font : Font)
    (particles : Render.Dynamic.ParticleState) (texture : FFI.Texture) (halfSize : Float)
    (screenScale : Float) : IO (Canvas × Render.Dynamic.ParticleState) := do
  let nextParticles := particles.updateBouncing dt halfSize
  let c ← run' c do
    resetTransform
    setFillColor Color.white
    fillTextXY
      s!"Sprites: {nextParticles.count} textured sprites (Space to advance)"
      (20 * screenScale) (30 * screenScale) font
    fillDynamicSprites texture nextParticles halfSize
  pure (c, nextParticles)

end Demos
