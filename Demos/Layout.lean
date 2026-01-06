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

open Afferent.Arbor
open Trellis

namespace Demos

/-- FlexItem alias for grow factor 1 -/
private abbrev grow1 := some (Trellis.FlexItem.growing 1)
/-- FlexItem alias for grow factor 2 -/
private abbrev grow2 := some (Trellis.FlexItem.growing 2)

/-- Colors for layout cells -/
def layoutCellColors : Array Color := #[
  Afferent.Color.red,
  Afferent.Color.green,
  Afferent.Color.blue,
  Afferent.Color.yellow,
  Afferent.Color.cyan,
  Afferent.Color.magenta,
  Afferent.Color.orange,
  Afferent.Color.purple,
  Afferent.Color.hsv 0.9 0.6 1.0,   -- pink
  Afferent.Color.hsv 0.5 0.7 0.8    -- teal
]

/-- Get a color for a node ID -/
def layoutColorForId (id : Nat) : Color :=
  layoutCellColors[id % layoutCellColors.size]!

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
def layoutCell (id : Nat) (minW minH : Float := 0)
    (flexItem : Option Trellis.FlexItem := none) : WidgetBuilder := do
  box (layoutCellStyle (layoutColorForId id) minW minH flexItem)

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

/-- Standard gap between items -/
private def gap : Float := 10

/-- Demo 1: Basic flex row with items left-aligned (justify-content: flex-start) -/
def demoFlexRow : WidgetBuilder :=
  hbox (gap := gap) (style := layoutContentStyle) do
    layoutCell 1 80 50
    layoutCell 2 100 50
    layoutCell 3 70 50

/-- Demo 2: Flex row with justify-content: center -/
def demoFlexRowCenter : WidgetBuilder :=
  hcenter (gap := gap) (style := layoutContentStyle) do
    layoutCell 1 60 50
    layoutCell 2 60 50
    layoutCell 3 60 50

/-- Demo 3: Flex row with justify-content: space-between -/
def demoFlexRowSpaceBetween : WidgetBuilder :=
  hspaced (style := layoutContentStyle) do
    layoutCell 4 50 50
    layoutCell 5 50 50
    layoutCell 6 50 50

/-- Demo 4: Flex grow with 1:2:1 ratio -/
def demoFlexGrow : WidgetBuilder :=
  hbox (gap := gap) (style := layoutContentStyle) do
    layoutCell 1 0 50 grow1
    layoutCell 2 0 50 grow2
    layoutCell 3 0 50 grow1

/-- Demo 5: Flex column direction -/
def demoFlexColumn : WidgetBuilder :=
  vbox (gap := gap) (style := layoutContentStyle) do
    layoutCell 1 100 40
    layoutCell 2 120 48
    layoutCell 3 80 40

/-- Demo 6: Align items center (cross-axis alignment) -/
def demoAlignCenter : WidgetBuilder :=
  let props := { Trellis.FlexContainer.row gap with alignItems := .center }
  hboxWith props layoutContentStyle do
    layoutCell 1 60 30
    layoutCell 2 60 60
    layoutCell 3 60 45

/-- Demo 7: Nested containers (row containing column) -/
def demoNested : WidgetBuilder :=
  hbox gap layoutContentStyle (do
    layoutCell 1 60 0
    center (style := BoxStyle.growFill) <|
      vbox gap {} (do
        layoutCell 4 0 35 grow1
        layoutCell 5 0 35 grow1)
    layoutCell 2 60 0)

/-- Demo 8: Complex layout (header + sidebar + main) -/
def demoComplex : WidgetBuilder :=
  vbox gap layoutContentStyle (do
    layoutCell 1 0 30 grow1
    hbox gap BoxStyle.grow (do
      vbox gap {} (do
        layoutCell 2 80 40
        layoutCell 3 80 40
        layoutCell 4 80 40)
      layoutCell 5 0 0 grow1))

/-- Demo 9: Overview layout (header + grid + footer) -/
def demoOverview : WidgetBuilder :=
  vbox (gap := gap) (style := layoutContentStyle) do
    layoutCell 1 0 25 grow1
    hbox (gap := gap) (style := BoxStyle.grow) do
      layoutCell 2 0 0 grow1
      layoutCell 3 0 0 grow1
    hbox (gap := gap) (style := BoxStyle.grow) do
      layoutCell 4 0 0 grow1
      layoutCell 5 0 0 grow1
    layoutCell 6 0 20 grow1

/-- Build all flexbox demos using Arbor widgets with monadic style. -/
def layoutWidgetFlex (fontTitle fontSmall : FontId) (_screenScale : Float) : WidgetBuilder :=
  let rootStyle : BoxStyle := {
    BoxStyle.fill with
    backgroundColor := some (Afferent.Color.rgba 0.1 0.1 0.15 1.0)
    padding := EdgeInsets.uniform 20
    flexItem := grow1
  }
  let sec := layoutSection fontSmall
  vbox (gap := 16) (style := rootStyle) do
    text' "CSS Flexbox Layout Demo (Space to advance)" fontTitle Afferent.Color.white .left
    hbox (gap := 20) (style := BoxStyle.grow) do
      -- Left column
      vbox (gap := 12) (style := BoxStyle.growFill) do
        sec "Row: justify-content: flex-start" "Items packed to start (default)" 90 demoFlexRow
        sec "Row: justify-content: center" "Items centered horizontally" 90 demoFlexRowCenter
        sec "Row: justify-content: space-between" "Items spread with space between" 90 demoFlexRowSpaceBetween
        sec "Row: flex-grow 1:2:1" "Middle item grows twice as much" 90 demoFlexGrow
        sec "Column: flex-direction: column" "Items stacked vertically" 180 demoFlexColumn
      -- Right column
      vbox (gap := 12) (style := BoxStyle.growFill) do
        sec "Row: align-items: center" "Items centered on cross-axis" 110 demoAlignCenter
        sec "Nested: row containing column" "Outer row with inner column" 130 demoNested
        sec "Complex: header + sidebar + main" "App-like layout structure" 220 demoComplex
        sec "Overview: header + 2x2 grid + footer" "Dashboard-like layout" 200 demoOverview

end Demos
