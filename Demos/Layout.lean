/-
  Layout Demo - CSS Flexbox layout visualization (Arbor widgets)
  Using monadic do-notation for child building.
  All dimensions in logical units (DPI scaling handled by renderer).
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Demo
import Trellis
open Tincture (Color)
open Tincture.Named

open Afferent.Arbor
open Trellis

namespace Demos

/-- FlexItem alias for grow factor 1 -/
private abbrev grow1 := some (Trellis.FlexItem.growing 1)
/-- FlexItem alias for grow factor 2 -/
private abbrev grow2 := some (Trellis.FlexItem.growing 2)


/-- Convert a size to an option (0 or less becomes none). -/
def layoutOptSize (v : Float) : Option Float :=
  if v <= 0 then none else some v

/-- Style for a layout demo cell. -/
def layoutCellStyle (color : Color) (minW minH : Float)
    (flexItem : Option Trellis.FlexItem := none) : BoxStyle := {
  backgroundColor := some (color.withAlpha 0.7)
  borderColor := some Afferent.Color.white
  borderWidth := 1
  minWidth := layoutOptSize minW
  minHeight := layoutOptSize minH
  flexItem := flexItem
}

/-- Build a colored flex cell. -/
def layoutCell (color : Color) (minW minH : Float := 0)
    (flexItem : Option Trellis.FlexItem := none) : WidgetBuilder := do
  box (layoutCellStyle color minW minH flexItem)

/-- Style for layout demo sections. -/
def layoutSectionStyle (minHeight : Float) : BoxStyle := {
  backgroundColor := some ((Afferent.Color.gray 0.5).withAlpha 0.25)
  borderColor := some ((Afferent.Color.gray 0.6).withAlpha 0.35)
  borderWidth := 1
  cornerRadius := 6
  padding := EdgeInsets.uniform 8
  flexItem := grow1
  minHeight := some minHeight
  height := .percent 1.0
}

/-- Style for layout demo content containers. -/
def layoutContentStyle : BoxStyle := {
  backgroundColor := some (Afferent.Color.gray 0.12)
  borderColor := some (Afferent.Color.gray 0.3)
  borderWidth := 1
  cornerRadius := 4
  padding := EdgeInsets.uniform 4
  flexItem := grow1
  height := .percent 1.0
}

/-- Build a labeled layout demo section. -/
def layoutSection (font : FontId) (title desc : String) (minHeight : Float)
    (content : WidgetBuilder) : WidgetBuilder :=
  if desc != "" then
    vbox (gap := 4) (style := layoutSectionStyle minHeight) do
      text' title font (Afferent.Color.gray 0.95) .left
      text' desc font (Afferent.Color.gray 0.75) .left
      content
  else
    vbox (gap := 4) (style := layoutSectionStyle minHeight) do
      text' title font (Afferent.Color.gray 0.95) .left
      content

/-- Build all flexbox demos using Arbor widgets with monadic style. -/
def layoutWidgetFlex (fontTitle fontSmall : FontId) (_screenScale : Float) : WidgetBuilder :=
  let rootStyle : BoxStyle := {
    BoxStyle.fill with
    backgroundColor := some (Afferent.Color.rgba 0.1 0.1 0.15 1.0)
    padding := EdgeInsets.uniform 20
    flexItem := grow1
  }
  let sec := fun title desc minHeight content ↦ layoutSection fontSmall title desc minHeight content
  vbox (gap := 16) (style := rootStyle) do
    text' "CSS Flexbox Layout Demo (Space to advance)" fontTitle Afferent.Color.white .left
    hbox (gap := 20) (style := BoxStyle.grow) do
      -- Left column
      vbox (gap := 12) (style := BoxStyle.growFill) do
        sec "Row: flex-start" "Items packed to start" 90 <|
          hbox (gap := 10) (style := layoutContentStyle) do
            layoutCell green 80 50; layoutCell blue 100 50; layoutCell yellow 70 50
        sec "Row: center" "Items centered horizontally" 90 <|
          hcenter (gap := 10) (style := layoutContentStyle) do
            layoutCell green 60 50; layoutCell blue 60 50; layoutCell yellow 60 50
        sec "Row: space-between" "Items spread with space between" 90 <|
          hspaced (style := layoutContentStyle) do
            layoutCell cyan 50 50; layoutCell magenta 50 50; layoutCell orange 50 50
        sec "Row: flex-grow 1:2:1" "Middle item grows twice as much" 90 <|
          hbox (gap := 10) (style := layoutContentStyle) do
            layoutCell green 0 50 grow1; layoutCell blue 0 50 grow2; layoutCell yellow 0 50 grow1
        sec "Column direction" "Items stacked vertically" 180 <|
          vbox (gap := 10) (style := layoutContentStyle) do
            layoutCell green 100 40; layoutCell blue 120 48; layoutCell yellow 80 40
      -- Right column
      vbox (gap := 12) (style := BoxStyle.growFill) do
        sec "Row: align-items center" "Items centered on cross-axis" 110 <|
          hboxWith { FlexContainer.row 10 with alignItems := .center } layoutContentStyle do
            layoutCell green 60 30; layoutCell blue 60 60; layoutCell yellow 60 45
        sec "Nested containers" "Outer row with inner column" 130 <|
          hbox 10 layoutContentStyle (do
            layoutCell green 60 0
            center (style := BoxStyle.growFill) <| vbox 10 BoxStyle.growFill (do
              layoutCell cyan 0 35 grow1; layoutCell magenta 0 35 grow1)
            layoutCell blue 60 0)
        sec "Complex layout" "Header + sidebar + main" 220 <|
          vbox 10 layoutContentStyle (do
            layoutCell green 0 30 grow1
            hbox 10 BoxStyle.grow (do
              vbox 10 {} (do layoutCell blue 80 40; layoutCell yellow 80 40; layoutCell cyan 80 40)
              layoutCell magenta 0 0 grow1))
        sec "Overview layout" "Header + 2x2 grid + footer" 200 <|
          vbox (gap := 10) (style := layoutContentStyle) do
            layoutCell green 0 25 grow1
            hbox (gap := 10) (style := BoxStyle.grow) do
              layoutCell blue 0 0 grow1; layoutCell yellow 0 0 grow1
            hbox (gap := 10) (style := BoxStyle.grow) do
              layoutCell cyan 0 0 grow1; layoutCell magenta 0 0 grow1
            layoutCell orange 0 20 grow1

end Demos
