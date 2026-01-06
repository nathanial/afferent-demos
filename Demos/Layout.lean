/-
  Layout Demo - CSS Flexbox layout visualization (Arbor widgets)
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Demo
import Trellis

open Afferent.Arbor
open Trellis

namespace Demos

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
def layoutCellStyle (color : Color) (screenScale : Float) (minW minH : Float)
    (flexItem : Option Trellis.FlexItem := none) : BoxStyle := {
  backgroundColor := some (color.withAlpha 0.7)
  borderColor := some Afferent.Color.white
  borderWidth := 1 * screenScale
  minWidth := layoutOptSize minW
  minHeight := layoutOptSize minH
  flexItem := flexItem
}

/-- Build a colored flex cell. -/
def layoutCell (color : Color) (screenScale : Float) (minW minH : Float := 0)
    (flexItem : Option Trellis.FlexItem := none) : WidgetBuilder := do
  box (layoutCellStyle color screenScale minW minH flexItem)

/-- Style for layout demo sections. -/
def layoutSectionStyle (screenScale : Float) (minHeight : Float) : BoxStyle := {
  backgroundColor := some ((Afferent.Color.gray 0.5).withAlpha 0.25)
  borderColor := some ((Afferent.Color.gray 0.6).withAlpha 0.35)
  borderWidth := 1 * screenScale
  cornerRadius := 6 * screenScale
  padding := EdgeInsets.uniform (8 * screenScale)
  flexItem := some (Trellis.FlexItem.growing 1)
  minHeight := some minHeight
  height := .percent 1.0
}

/-- Style for layout demo content containers. -/
def layoutContentStyle (screenScale : Float) : BoxStyle := {
  backgroundColor := some (Afferent.Color.gray 0.12)
  borderColor := some (Afferent.Color.gray 0.3)
  borderWidth := 1 * screenScale
  cornerRadius := 4 * screenScale
  padding := EdgeInsets.uniform (4 * screenScale)
  flexItem := some (Trellis.FlexItem.growing 1)
  height := .percent 1.0
}

/-- Build a labeled layout demo section. -/
def layoutSection (title desc : String) (fontLabel fontSmall : FontId)
    (screenScale minHeight : Float) (content : WidgetBuilder) : WidgetBuilder := do
  let gap := 4 * screenScale
  let style := layoutSectionStyle screenScale minHeight
  let mut children : Array WidgetBuilder := #[(text' title fontLabel (Afferent.Color.gray 0.95) .left)]
  if desc != "" then
    children := children.push (text' desc fontSmall (Afferent.Color.gray 0.75) .left)
  children := children.push content
  column (gap := gap) (style := style) children

/-- Demo 1: Basic flex row with items left-aligned (justify-content: flex-start) -/
def demoFlexRow (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let cellH := 50 * screenScale
  row (gap := gap) (style := layoutContentStyle screenScale) #[
    layoutCell (layoutColorForId 1) screenScale (80 * screenScale) cellH,
    layoutCell (layoutColorForId 2) screenScale (100 * screenScale) cellH,
    layoutCell (layoutColorForId 3) screenScale (70 * screenScale) cellH
  ]

/-- Demo 2: Flex row with justify-content: center -/
def demoFlexRowCenter (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let cellW := 60 * screenScale
  let cellH := 50 * screenScale
  let props := { Trellis.FlexContainer.row gap with justifyContent := .center }
  flexRow props (layoutContentStyle screenScale) #[
    layoutCell (layoutColorForId 1) screenScale cellW cellH,
    layoutCell (layoutColorForId 2) screenScale cellW cellH,
    layoutCell (layoutColorForId 3) screenScale cellW cellH
  ]

/-- Demo 3: Flex row with justify-content: space-between -/
def demoFlexRowSpaceBetween (screenScale : Float) : WidgetBuilder := do
  let cellW := 50 * screenScale
  let cellH := 50 * screenScale
  spaceBetween .row (layoutContentStyle screenScale) #[
    layoutCell (layoutColorForId 4) screenScale cellW cellH,
    layoutCell (layoutColorForId 5) screenScale cellW cellH,
    layoutCell (layoutColorForId 6) screenScale cellW cellH
  ]

/-- Demo 4: Flex grow with 1:2:1 ratio -/
def demoFlexGrow (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let cellH := 50 * screenScale
  row (gap := gap) (style := layoutContentStyle screenScale) #[
    layoutCell (layoutColorForId 1) screenScale 0 cellH (some (Trellis.FlexItem.growing 1)),
    layoutCell (layoutColorForId 2) screenScale 0 cellH (some (Trellis.FlexItem.growing 2)),
    layoutCell (layoutColorForId 3) screenScale 0 cellH (some (Trellis.FlexItem.growing 1))
  ]

/-- Demo 5: Flex column direction -/
def demoFlexColumn (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let cellW := 100 * screenScale
  let cellH := 40 * screenScale
  column (gap := gap) (style := layoutContentStyle screenScale) #[
    layoutCell (layoutColorForId 1) screenScale cellW cellH,
    layoutCell (layoutColorForId 2) screenScale (cellW * 1.2) (cellH * 1.2),
    layoutCell (layoutColorForId 3) screenScale (cellW * 0.8) cellH
  ]

/-- Demo 6: Align items center (cross-axis alignment) -/
def demoAlignCenter (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let cellW := 60 * screenScale
  let props := { Trellis.FlexContainer.row gap with alignItems := .center }
  flexRow props (layoutContentStyle screenScale) #[
    layoutCell (layoutColorForId 1) screenScale cellW (30 * screenScale),
    layoutCell (layoutColorForId 2) screenScale cellW (60 * screenScale),
    layoutCell (layoutColorForId 3) screenScale cellW (45 * screenScale)
  ]

/-- Demo 7: Nested containers (row containing column) -/
def demoNested (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let sideW := 60 * screenScale
  let innerH := 35 * screenScale
  let innerColumn := column (gap := gap) {} #[
    layoutCell (layoutColorForId 4) screenScale 0 innerH (some (Trellis.FlexItem.growing 1)),
    layoutCell (layoutColorForId 5) screenScale 0 innerH (some (Trellis.FlexItem.growing 1))
  ]
  row (gap := gap) (style := layoutContentStyle screenScale) #[
    layoutCell (layoutColorForId 1) screenScale sideW 0,
    center (style := { flexItem := some (Trellis.FlexItem.growing 1), height := .percent 1.0 }) innerColumn,
    layoutCell (layoutColorForId 2) screenScale sideW 0
  ]

/-- Demo 8: Complex layout (header + sidebar + main) -/
def demoComplex (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let headerH := 30 * screenScale
  let sidebarW := 80 * screenScale
  let itemH := 40 * screenScale

  let header := layoutCell (layoutColorForId 1) screenScale 0 headerH (some (Trellis.FlexItem.growing 1))

  let sidebar := column (gap := gap) {} #[
    layoutCell (layoutColorForId 2) screenScale sidebarW itemH,
    layoutCell (layoutColorForId 3) screenScale sidebarW itemH,
    layoutCell (layoutColorForId 4) screenScale sidebarW itemH
  ]

  let mainContent := layoutCell (layoutColorForId 5) screenScale 0 0 (some (Trellis.FlexItem.growing 1))

  let content := row (gap := gap) (style := { flexItem := some (Trellis.FlexItem.growing 1) }) #[
    sidebar,
    mainContent
  ]

  column (gap := gap) (style := layoutContentStyle screenScale) #[header, content]

/-- Demo 9: Overview layout (header + grid + footer) -/
def demoOverview (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let headerH := 25 * screenScale
  let footerH := 20 * screenScale

  let header := layoutCell (layoutColorForId 1) screenScale 0 headerH (some (Trellis.FlexItem.growing 1))

  let gridRow1 := row (gap := gap) (style := { flexItem := some (Trellis.FlexItem.growing 1) }) #[
    layoutCell (layoutColorForId 2) screenScale 0 0 (some (Trellis.FlexItem.growing 1)),
    layoutCell (layoutColorForId 3) screenScale 0 0 (some (Trellis.FlexItem.growing 1))
  ]

  let gridRow2 := row (gap := gap) (style := { flexItem := some (Trellis.FlexItem.growing 1) }) #[
    layoutCell (layoutColorForId 4) screenScale 0 0 (some (Trellis.FlexItem.growing 1)),
    layoutCell (layoutColorForId 5) screenScale 0 0 (some (Trellis.FlexItem.growing 1))
  ]

  let footer := layoutCell (layoutColorForId 6) screenScale 0 footerH (some (Trellis.FlexItem.growing 1))

  column (gap := gap) (style := layoutContentStyle screenScale) #[header, gridRow1, gridRow2, footer]

/-- Build all flexbox demos using Arbor widgets. -/
def layoutWidgetFlex (fontTitle fontSmall : FontId) (screenScale : Float) : WidgetBuilder := do
  let s := screenScale
  let rootStyle : BoxStyle := {
    backgroundColor := some (Afferent.Color.rgba 0.1 0.1 0.15 1.0)
    padding := EdgeInsets.uniform (20 * s)
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let columnStyle : BoxStyle := {
    flexItem := some (Trellis.FlexItem.growing 1)
    height := .percent 1.0
  }
  let titleWidget := text' "CSS Flexbox Layout Demo (Space to advance)" fontTitle Afferent.Color.white .left

  let leftCol := column (gap := 12 * s) (style := columnStyle) #[(
    layoutSection "Row: justify-content: flex-start" "Items packed to start (default)"
      fontSmall fontSmall s (90 * s) (demoFlexRow s)
    ),(
    layoutSection "Row: justify-content: center" "Items centered horizontally"
      fontSmall fontSmall s (90 * s) (demoFlexRowCenter s)
    ),(
    layoutSection "Row: justify-content: space-between" "Items spread with space between"
      fontSmall fontSmall s (90 * s) (demoFlexRowSpaceBetween s)
    ),(
    layoutSection "Row: flex-grow 1:2:1" "Middle item grows twice as much"
      fontSmall fontSmall s (90 * s) (demoFlexGrow s)
    ),(
    layoutSection "Column: flex-direction: column" "Items stacked vertically"
      fontSmall fontSmall s (180 * s) (demoFlexColumn s)
    )]

  let rightCol := column (gap := 12 * s) (style := columnStyle) #[(
    layoutSection "Row: align-items: center" "Items centered on cross-axis"
      fontSmall fontSmall s (110 * s) (demoAlignCenter s)
    ),(
    layoutSection "Nested: row containing column" "Outer row with inner column"
      fontSmall fontSmall s (130 * s) (demoNested s)
    ),(
    layoutSection "Complex: header + sidebar + main" "App-like layout structure"
      fontSmall fontSmall s (220 * s) (demoComplex s)
    ),(
    layoutSection "Overview: header + 2x2 grid + footer" "Dashboard-like layout"
      fontSmall fontSmall s (200 * s) (demoOverview s)
    )]

  column (gap := 16 * s) (style := rootStyle) #[(
    titleWidget
    ),(
    row (gap := 20 * s) (style := { flexItem := some (Trellis.FlexItem.growing 1) }) #[leftCol, rightCol]
    )]

end Demos
