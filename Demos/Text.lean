/-
  Text Demo - Cards showing font sizes, colors, and text on shapes.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Card
import Trellis

open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

private structure TextCard where
  label : String
  draw : Rect → RenderCommands
  contentSize : Float := 90
  cardWidth : Float := 140
  cardHeight : Float := 120

/-- Center a single line of text within the rect. -/
private def centeredText (text : String) (font : FontId) (color : Color) (r : Rect) : RenderCommands :=
  #[.fillTextBlock text r font color .center .middle]

/-- Text cards rendered as widgets. -/
def textWidget (fonts : DemoFonts) : WidgetBuilder := do
  let cards : Array TextCard := #[(
    { label := "Small", draw := fun r => centeredText "Small 16pt" fonts.small Afferent.Color.white r }
  ), (
    { label := "Medium", draw := fun r => centeredText "Medium 24pt" fonts.medium Afferent.Color.white r }
  ), (
    { label := "Large", draw := fun r => centeredText "Large 36pt" fonts.large Afferent.Color.white r, contentSize := 100 }
  ), (
    { label := "Huge", draw := fun r => centeredText "Huge 48pt" fonts.huge Afferent.Color.white r, contentSize := 110 }
  ), (
    { label := "Red", draw := fun r => centeredText "Red Text" fonts.medium Afferent.Color.red r }
  ), (
    { label := "Green", draw := fun r => centeredText "Green Text" fonts.medium Afferent.Color.green r }
  ), (
    { label := "Blue", draw := fun r => centeredText "Blue Text" fonts.medium Afferent.Color.blue r }
  ), (
    { label := "Yellow", draw := fun r => centeredText "Yellow Text" fonts.medium Afferent.Color.yellow r }
  ), (
    { label := "Cyan", draw := fun r => centeredText "Cyan Text" fonts.medium Afferent.Color.cyan r }
  ), (
    { label := "Magenta", draw := fun r => centeredText "Magenta Text" fonts.medium Afferent.Color.magenta r }
  ), (
    { label := "Headline",
      draw := fun r => centeredText "Afferent" fonts.large Afferent.Color.white r,
      contentSize := 110, cardWidth := 180, cardHeight := 130 }
  ), (
    { label := "Text on Shape",
      draw := fun r =>
        let rect := Rect.mk' (r.origin.x + 8) (r.origin.y + r.size.height * 0.3) (r.size.width - 16) (r.size.height * 0.4)
        #[
          .fillRect rect Afferent.Color.blue 6,
          .fillTextBlock "Text on Shape" rect fonts.small Afferent.Color.white .center .middle
        ] }
  ), (
    { label := "Labels",
      draw := fun r =>
        let center := rectCenter r
        let radius := minSide r * 0.3
        #[
          .fillPath (Path.circle center radius) Afferent.Color.red,
          .fillTextBlock "Labels" (Rect.mk' (center.x - radius) (center.y - 10) (radius * 2) 20)
            fonts.small Afferent.Color.white .center .middle
        ] }
  ), (
    { label := "Rounded Button",
      draw := fun r =>
        let rect := Rect.mk' (r.origin.x + 8) (r.origin.y + r.size.height * 0.3) (r.size.width - 16) (r.size.height * 0.4)
        #[
          .fillRect rect Afferent.Color.green 10,
          .fillTextBlock "Rounded" rect fonts.small Afferent.Color.black .center .middle
        ] }
  ), (
    { label := "Alphabet",
      draw := fun r =>
        let x := r.origin.x + 6
        let y1 := r.origin.y + r.size.height * 0.4
        let y2 := r.origin.y + r.size.height * 0.7
        #[
          .fillText "ABCDEFGHIJKLMNOPQRSTUVWXYZ" x y1 fonts.small Afferent.Color.white,
          .fillText "abcdefghijklmnopqrstuvwxyz" x y2 fonts.small Afferent.Color.white
        ],
      contentSize := 120, cardWidth := 200, cardHeight := 130 }
  ), (
    { label := "Digits",
      draw := fun r =>
        let x := r.origin.x + 6
        let y := r.origin.y + r.size.height * 0.55
        #[
          .fillText "0123456789 !@#$%^&*()" x y fonts.small Afferent.Color.white
        ],
      contentSize := 110, cardWidth := 200, cardHeight := 130 }
  ), (
    { label := "Transparent",
      draw := fun r =>
        let x := r.origin.x + 6
        let y0 := r.origin.y + r.size.height * 0.35
        let step := r.size.height * 0.22
        #[
          .fillText "Semi-transparent" x y0 fonts.small (Afferent.Color.hsva 0.0 0.0 1.0 0.7),
          .fillText "More transparent" x (y0 + step) fonts.small (Afferent.Color.hsva 0.0 0.0 1.0 0.4),
          .fillText "Very faint" x (y0 + step * 2) fonts.small (Afferent.Color.hsva 0.0 0.0 1.0 0.2)
        ] }
  ), (
    { label := "Error",
      draw := fun r =>
        let rect := Rect.mk' (r.origin.x + 8) (r.origin.y + r.size.height * 0.3) (r.size.width - 16) (r.size.height * 0.4)
        #[
          .fillRect rect (Afferent.Color.hsva 0.0 0.75 0.8 1.0) 6,
          .fillTextBlock "Error" rect fonts.small Afferent.Color.white .center .middle
        ] }
  ), (
    { label := "Success",
      draw := fun r =>
        let rect := Rect.mk' (r.origin.x + 8) (r.origin.y + r.size.height * 0.3) (r.size.width - 16) (r.size.height * 0.4)
        #[
          .fillRect rect (Afferent.Color.hsva 0.333 0.667 0.6 1.0) 6,
          .fillTextBlock "Success" rect fonts.small Afferent.Color.white .center .middle
        ] }
  ), (
    { label := "Warning",
      draw := fun r =>
        let rect := Rect.mk' (r.origin.x + 8) (r.origin.y + r.size.height * 0.3) (r.size.width - 16) (r.size.height * 0.4)
        #[
          .fillRect rect (Afferent.Color.hsva 0.119 0.875 0.8 1.0) 6,
          .fillTextBlock "Warning" rect fonts.small Afferent.Color.black .center .middle
        ] }
  )]

  let widgets := cards.map fun card =>
    demoCard fonts.label card.label card.draw card.contentSize card.cardWidth card.cardHeight
  grid 4 10 { padding := EdgeInsets.uniform 10 } widgets

/-- Render text demo content to canvas using Arbor widgets. -/
def renderTextM (reg : Afferent.FontRegistry) (fonts : DemoFonts) : Afferent.CanvasM Unit := do
  let widget := Afferent.Arbor.build (textWidget fonts)
  Afferent.Widget.renderArborWidget reg widget 1000 800

end Demos
