/-
  Grid Performance Test - Spinning squares in a grid
-/
import Afferent
import Afferent.Arbor
import Demos.Core.Demo
import Trellis

open Afferent CanvasM

namespace Demos

private def ensureFloatBufferCapacity (requiredFloats : Nat) (c : Canvas) : IO Canvas := do
  match c.floatBuffer with
  | some buf =>
      if c.floatBufferCapacity >= requiredFloats then
        pure c
      else
        FFI.FloatBuffer.destroy buf
        let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
        pure { c with floatBuffer := some newBuf, floatBufferCapacity := requiredFloats }
  | none =>
      let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
      pure { c with floatBuffer := some newBuf, floatBufferCapacity := requiredFloats }

private def gridFitTransform (rect : Trellis.LayoutRect) (particles : Render.Dynamic.ParticleState)
    : Float × Float × Float :=
  let baseW := particles.screenWidth
  let baseH := particles.screenHeight
  let safeW := max 1.0 rect.width
  let safeH := max 1.0 rect.height
  let scale := min (safeW / baseW) (safeH / baseH)
  let offsetX := rect.x + (safeW - baseW * scale) / 2.0
  let offsetY := rect.y + (safeH - baseH * scale) / 2.0
  (offsetX, offsetY, scale)

private def drawGridInstanced (offsetX offsetY scale : Float) (t : Float)
    (particles : Render.Dynamic.ParticleState) (halfSize : Float) : CanvasM Unit := do
  if scale <= 0.0 then
    pure ()
  else
    let baseW := particles.screenWidth
    let baseH := particles.screenHeight
    let halfSizeScaled := halfSize * scale
    let required := particles.count * 8
    liftCanvas (fun c => do
      let c ← ensureFloatBufferCapacity required c
      match c.floatBuffer with
      | some buf =>
          Render.Dynamic.writeInstancedAnimatedToBuffer particles buf halfSizeScaled t 3.0
          let a := (2.0 * scale) / baseW
          let d := (-2.0 * scale) / baseH
          let tx := -1.0 + (2.0 * offsetX / baseW)
          let ty := 1.0 - (2.0 * offsetY / baseH)
          let sizeModeScreen : UInt32 := 1
          let colorModeHSV : UInt32 := 1
          let hueSpeed : Float := 0.2
          FFI.Renderer.drawInstancedShapesBuffer
            c.ctx.renderer
            0
            buf
            particles.count.toUInt32
            a 0.0 0.0 d tx ty
            baseW baseH
            sizeModeScreen
            t
            hueSpeed
            colorModeHSV
          pure c
      | none => pure c
    )

def gridPerfWidget (t : Float) (font : Font)
    (particles : Render.Dynamic.ParticleState) (halfSize : Float) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      let rect := layout.contentRect
      let (offsetX, offsetY, scale) := gridFitTransform rect particles
      saved do
        resetTransform
        setFillColor Color.white
        fillTextXY s!"Grid: {particles.count} dynamic squares (Space to advance)"
          (offsetX + 20) (offsetY + 30) font
        drawGridInstanced offsetX offsetY scale t particles halfSize
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos
