/-
  Font Showcase Demo
  Displays labels with different Mac fonts at various sizes.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Trellis
import Std.Data.HashMap

open Afferent.Arbor
open Trellis (FlexContainer FlexItem)

namespace Demos

/-- Get a font ID from the showcase fonts HashMap, with a fallback. -/
private def getShowcaseFont (fonts : Std.HashMap String Afferent.Arbor.FontId) (key : String)
    (fallback : Afferent.Arbor.FontId) : Afferent.Arbor.FontId :=
  fonts.getD key fallback

/-- Build an array of text widgets for all font sizes. -/
private def buildSizeWidgets (family : String)
    (fonts : Std.HashMap String Afferent.Arbor.FontId) (headerFont : Afferent.Arbor.FontId)
    (textColor : Afferent.Color) (sampleText : String) : Array WidgetBuilder :=
  #[12, 18, 24, 36, 48, 72].map fun size =>
    let fontId := getShowcaseFont fonts s!"{family}-{size}" headerFont
    text' sampleText fontId textColor .left

/-- Render a row of text samples for a font family at all sizes. -/
private def fontRowWidget (familyName : String) (family : String)
    (fonts : Std.HashMap String Afferent.Arbor.FontId) (headerFont : Afferent.Arbor.FontId)
    (screenScale : Float) : WidgetBuilder :=
  let gap := 12 * screenScale
  let textColor := Afferent.Color.white
  let sampleText := "Afferent"
  let sizeWidgets := buildSizeWidgets family fonts headerFont textColor sampleText
  let allChildren := #[text' familyName headerFont textColor .left] ++ sizeWidgets
  flexRow { FlexContainer.row gap with alignItems := .center } {} allChildren

/-- Render a header row with size labels. -/
private def sizeHeaderWidget (sizes : Array String) (headerFont : Afferent.Arbor.FontId)
    (screenScale : Float) : WidgetBuilder :=
  let gap := 12 * screenScale
  let textColor := Afferent.Color.gray 0.6
  let sizeWidgets := sizes.map fun size => text' size headerFont textColor .center
  let allChildren := #[text' "" headerFont textColor .left] ++ sizeWidgets
  flexRow { FlexContainer.row gap with alignItems := .center } {} allChildren

/-- Main font showcase widget. -/
def fontShowcaseWidget (fonts : Std.HashMap String Afferent.Arbor.FontId)
    (headerFont : Afferent.Arbor.FontId) (screenScale : Float) : WidgetBuilder :=
  let gap := 8 * screenScale
  let padding := 16 * screenScale
  let contentPadding := 24 * screenScale
  let contentGap := 16 * screenScale

  let outerStyle : BoxStyle := {
    backgroundColor := some (Afferent.Color.gray 0.12)
    flexItem := some (FlexItem.growing 1)
  }

  let headerStyle : BoxStyle := {
    padding := Trellis.EdgeInsets.uniform padding
  }

  let contentStyle : BoxStyle := {
    padding := Trellis.EdgeInsets.uniform contentPadding
    flexItem := some (FlexItem.growing 1)
  }

  let sizes := #["12pt", "18pt", "24pt", "36pt", "48pt", "72pt"]

  flexColumn (FlexContainer.column gap) outerStyle #[
    -- Header section
    flexColumn (FlexContainer.column 0) headerStyle #[
      text' "Font Showcase" headerFont Afferent.Color.white .left
    ],
    -- Content section
    flexColumn (FlexContainer.column contentGap) contentStyle #[
      sizeHeaderWidget sizes headerFont screenScale,
      fontRowWidget "Monaco" "monaco" fonts headerFont screenScale,
      fontRowWidget "Helvetica" "helvetica" fonts headerFont screenScale,
      fontRowWidget "Times" "times" fonts headerFont screenScale,
      fontRowWidget "Georgia" "georgia" fonts headerFont screenScale
    ]
  ]

end Demos
