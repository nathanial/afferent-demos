/-
  Texture Matrix Demo
  Demonstrates scaling a sprite via the u_matrix transform.
-/
import Afferent
import Afferent.Arbor
import Demos.Demo
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
        let a := 2.0 / canvasW
        let d := -2.0 / canvasH
        let tx := -1.0
        let ty := 1.0
        let a' := a * scale
        let d' := d * scale
        let tx' := tx + a * (1.0 - scale) * pivotX
        let ty' := ty + d * (1.0 - scale) * pivotY
        let refX := offsetX + wF * 0.35
        let refY := pivotY
        let refData : Array Float := #[refX, refY, 0.0, baseHalf, 1.0]
        let scaledData : Array Float := #[pivotX, pivotY, 0.0, baseHalf, 1.0]
        FFI.Renderer.drawSprites renderer texture refData 1 canvasW canvasH
        FFI.Renderer.drawSpritesMatrix renderer texture scaledData 1 canvasW canvasH a' 0.0 0.0 d' tx' ty'
        resetTransform
        setFillColor Color.white
        fillTextXY "Texture Matrix Demo (u_matrix scaling) (Space to advance)"
          (20 * screenScale) (30 * screenScale) fontMedium
        fillTextXY "left: screen-space  |  right: u_matrix scale"
          (20 * screenScale) (60 * screenScale) fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos
