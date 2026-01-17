/-
  Texture Scale Demo
  Demonstrates scaling a sprite on the CPU.
-/
import Afferent
import Afferent.Arbor
import Demos.Core.Demo
import Trellis

open Afferent CanvasM

namespace Demos

def textureMatrixWidget (t : Float) (screenScale : Float) (windowWidth windowHeight : Float)
    (fontMedium fontSmall : Font) (texture : FFI.Texture) : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        let rect := layout.contentRect
        let canvasW := max 1.0 windowWidth
        let canvasH := max 1.0 windowHeight
        let wF := max 1.0 w
        let hF := max 1.0 h
        let offsetX := rect.x
        let offsetY := rect.y
        let renderer ← getRenderer
        let baseHalf := 48.0 * screenScale
        let pivotX := offsetX + wF * 0.65
        let pivotY := offsetY + hF * 0.55
        let scale := 1.0 + 0.5 * Float.sin t
        let refX := offsetX + wF * 0.35
        let refY := pivotY
        let scaledHalf := baseHalf * scale
        let buf ← FFI.FloatBuffer.create 10
        FFI.FloatBuffer.setVec5 buf 0 refX refY 0.0 baseHalf 1.0
        FFI.FloatBuffer.setVec5 buf 5 pivotX pivotY 0.0 scaledHalf 1.0
        FFI.Renderer.drawSpritesInstanceBuffer renderer texture buf 2 canvasW canvasH
        FFI.FloatBuffer.destroy buf
        resetTransform
        setFillColor Color.white
        fillTextXY "Texture Scale Demo (CPU scaling) (Space to advance)"
          (20 * screenScale) (30 * screenScale) fontMedium
        fillTextXY "left: baseline  |  right: scaled"
          (20 * screenScale) (60 * screenScale) fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos
