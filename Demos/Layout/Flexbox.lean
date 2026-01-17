/-
  Layout Demo - CSS Flexbox layout visualization (Arbor widgets)
  Using monadic do-notation for child building.
  All dimensions in logical units (DPI scaling handled by renderer).
  Demonstrates the css! macro for CSS-like style definitions.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Afferent.Arbor.Style.CSS
import Demos.Core.Demo
import Trellis
open Tincture (Color)
open Tincture.Named

open Afferent.Arbor
open Afferent.Arbor.CSS
open Trellis

namespace Demos

/-- Convert a size to an option (0 or less becomes none). -/
def layoutOptSize (v : Float) : Option Float :=
  if v <= 0 then none else some v

/-- Style for a layout demo cell.
    Note: Uses traditional syntax because it needs dynamic color values. -/
def layoutCellStyle (color : Color) (minW minH : Float)
    (flexItem : Option Trellis.FlexItem := none) : BoxStyle := {
  backgroundColor := some (color.withAlpha 0.7)
  borderColor := some Afferent.Color.white
  borderWidth := 1
  minWidth := layoutOptSize minW
  minHeight := layoutOptSize minH
  flexItem := flexItem
}

/-- Build a colored flex cell with optional flex item. -/
def layoutCell (color : Color) (minW minH : Float := 0)
    (flexItem : Option Trellis.FlexItem := none) : WidgetBuilder := do
  box (layoutCellStyle color minW minH flexItem)

/-- Build a colored flex cell that grows (flex-grow: 1). -/
def layoutCellGrow1 (color : Color) (minW minH : Float := 0) : WidgetBuilder := do
  box { layoutCellStyle color minW minH with
    flexItem := some (FlexItem.growing 1) }

/-- Build a colored flex cell that grows twice as much (flex-grow: 2). -/
def layoutCellGrow2 (color : Color) (minW minH : Float := 0) : WidgetBuilder := do
  box { layoutCellStyle color minW minH with
    flexItem := some (FlexItem.growing 2) }

/-- Style for layout demo sections.
    Uses css! for colors with hsva() for HSV color space. -/
def layoutSectionStyle (minHeight : Float) : BoxStyle :=
  { css! {
      background-color: hsva(0.0, 0.0, 0.5, 0.25);
      border-color: hsva(0.0, 0.0, 0.6, 0.35);
      border-width: 1;
      corner-radius: 6;
      padding: 8;
      flex-grow: 1;
      height: 100pct;
    } with
    minHeight := some minHeight
  }

/-- Style for layout demo content containers - fully CSS-defined with hsv()! -/
def layoutContentStyle : BoxStyle := css! {
  background-color: hsv(0.0, 0.0, 0.12);
  border-color: hsv(0.0, 0.0, 0.3);
  border-width: 1;
  corner-radius: 4;
  padding: 4;
  flex-grow: 1;
  height: 100pct;
}

/-- Root style for the demo - fully CSS-defined with hsv()! -/
def layoutRootStyle : BoxStyle := css! {
  background-color: hsv(0.67, 0.32, 0.15);
  padding: 20;
  flex-grow: 1;
  width: 100pct;
  height: 100pct;
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
  let sec := fun title desc minHeight content ↦ layoutSection fontSmall title desc minHeight content
  -- Styles defined with css! macro
  let growStyle := css! { flex-grow: 1; }
  let growFillStyle := css! { flex-grow: 1; height: 100pct; }
  vbox (gap := 16) (style := layoutRootStyle) do
    text' "CSS Flexbox Layout Demo (Space to advance)" fontTitle Afferent.Color.white .left
    hbox (gap := 20) (style := growStyle) do
      -- Left column
      vbox (gap := 12) (style := growFillStyle) do
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
            layoutCellGrow1 green 0 50; layoutCellGrow2 blue 0 50; layoutCellGrow1 yellow 0 50
        sec "Column direction" "Items stacked vertically" 180 <|
          vbox (gap := 10) (style := layoutContentStyle) do
            layoutCell green 100 40; layoutCell blue 120 48; layoutCell yellow 80 40
      -- Right column
      vbox (gap := 12) (style := growFillStyle) do
        sec "Row: align-items center" "Items centered on cross-axis" 110 <|
          hboxWith { FlexContainer.row 10 with alignItems := .center } layoutContentStyle do
            layoutCell green 60 30; layoutCell blue 60 60; layoutCell yellow 60 45
        sec "Nested containers" "Outer row with inner column" 130 <|
          hbox 10 layoutContentStyle (do
            layoutCell green 60 0
            center (style := growFillStyle) <| vbox 10 growFillStyle (do
              layoutCellGrow1 cyan 0 35; layoutCellGrow1 magenta 0 35)
            layoutCell blue 60 0)
        sec "Complex layout" "Header + sidebar + main" 220 <|
          vbox 10 layoutContentStyle (do
            layoutCellGrow1 green 0 30
            hbox 10 growStyle (do
              vbox 10 {} (do layoutCell blue 80 40; layoutCell yellow 80 40; layoutCell cyan 80 40)
              layoutCellGrow1 magenta 0 0))
        sec "Overview layout" "Header + 2x2 grid + footer" 200 <|
          vbox (gap := 10) (style := layoutContentStyle) do
            layoutCellGrow1 green 0 25
            hbox (gap := 10) (style := growStyle) do
              layoutCellGrow1 blue 0 0; layoutCellGrow1 yellow 0 0
            hbox (gap := 10) (style := growStyle) do
              layoutCellGrow1 cyan 0 0; layoutCellGrow1 magenta 0 0
            layoutCellGrow1 orange 0 20

end Demos
