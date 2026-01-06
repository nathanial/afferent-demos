/-
  Grid Demo - CSS Grid layout visualization (Arbor widgets)
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
def gridCellColors : Array Color := #[
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
def gridColorForId (id : Nat) : Color :=
  gridCellColors[id % gridCellColors.size]!

/-- Convert a size to an option (0 or less becomes none). -/
def optSize (v : Float) : Option Float :=
  if v <= 0 then none else some v

/-- Style for a grid demo cell. -/
def gridCellStyle (color : Color) (screenScale : Float) (minW minH : Float)
    (gridItem : Option GridItem := none) : BoxStyle := {
  backgroundColor := some (color.withAlpha 0.7)
  borderColor := some Afferent.Color.white
  borderWidth := 1 * screenScale
  minWidth := optSize minW
  minHeight := optSize minH
  gridItem := gridItem
}

/-- Build a colored grid cell. -/
def gridCell (color : Color) (screenScale : Float) (minW minH : Float := 0)
    (gridItem : Option GridItem := none) : WidgetBuilder := do
  box (gridCellStyle color screenScale minW minH gridItem)

/-- Style for grid demo sections. -/
def gridSectionStyle (screenScale : Float) (minHeight : Float) : BoxStyle := {
  backgroundColor := some ((Afferent.Color.gray 0.5).withAlpha 0.25)
  borderColor := some ((Afferent.Color.gray 0.6).withAlpha 0.35)
  borderWidth := 1 * screenScale
  cornerRadius := 6 * screenScale
  padding := EdgeInsets.uniform (8 * screenScale)
  flexItem := some (Trellis.FlexItem.growing 1)
  minHeight := some minHeight
  height := .percent 1.0
}

/-- Style for grid demo content containers. -/
def gridContentStyle (screenScale : Float) : BoxStyle := {
  backgroundColor := some (Afferent.Color.gray 0.12)
  borderColor := some (Afferent.Color.gray 0.3)
  borderWidth := 1 * screenScale
  cornerRadius := 4 * screenScale
  padding := EdgeInsets.uniform (4 * screenScale)
  flexItem := some (Trellis.FlexItem.growing 1)
  height := .percent 1.0
}

/-- Build a labeled grid demo section. -/
def gridSection (title desc : String) (fontLabel fontSmall : FontId)
    (screenScale minHeight : Float) (content : WidgetBuilder) : WidgetBuilder := do
  let gap := 4 * screenScale
  let style := gridSectionStyle screenScale minHeight
  let mut children : Array WidgetBuilder := #[(text' title fontLabel (Afferent.Color.gray 0.95) .left)]
  if desc != "" then
    children := children.push (text' desc fontSmall (Afferent.Color.gray 0.75) .left)
  children := children.push content
  column (gap := gap) (style := style) children

/-- Demo 1: Simple 3-column grid with equal fr units -/
def demoGrid3Col (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1, .fr 1, .fr 1] (gap := gap)
  gridCustom props (gridContentStyle screenScale) #[
    gridCell (gridColorForId 1) screenScale,
    gridCell (gridColorForId 2) screenScale,
    gridCell (gridColorForId 3) screenScale
  ]

/-- Demo 2: Mixed track sizes (100px, 1fr, 2fr) -/
def demoGridMixed (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let props := GridContainer.withTemplate #[.fr 1] #[.px (80 * screenScale), .fr 1, .fr 2] (gap := gap)
  gridCustom props (gridContentStyle screenScale) #[
    gridCell (gridColorForId 4) screenScale,
    gridCell (gridColorForId 5) screenScale,
    gridCell (gridColorForId 6) screenScale
  ]

/-- Demo 3: Auto-placement (6 items in 3 cols) -/
def demoGridAuto (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let props := GridContainer.withTemplate #[.fr 1, .fr 1] #[.fr 1, .fr 1, .fr 1] (gap := gap)
  gridCustom props (gridContentStyle screenScale) #[
    gridCell (gridColorForId 1) screenScale,
    gridCell (gridColorForId 2) screenScale,
    gridCell (gridColorForId 3) screenScale,
    gridCell (gridColorForId 4) screenScale,
    gridCell (gridColorForId 5) screenScale,
    gridCell (gridColorForId 6) screenScale
  ]

/-- Demo 4: Explicit item placement -/
def demoGridExplicit (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let props := GridContainer.withTemplate #[.fr 1, .fr 1] #[.fr 1, .fr 1, .fr 1] (gap := gap)
  gridCustom props (gridContentStyle screenScale) #[
    gridCell (gridColorForId 1) screenScale 0 0 (some (GridItem.atPosition 1 1)),
    gridCell (gridColorForId 2) screenScale 0 0 (some (GridItem.atPosition 2 3)),
    gridCell (gridColorForId 3) screenScale 0 0 (some (GridItem.atPosition 1 3))
  ]

/-- Demo 5: Items spanning multiple cells -/
def demoGridSpan (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let props := GridContainer.withTemplate #[.fr 1, .fr 1] #[.fr 1, .fr 1, .fr 1] (gap := gap)
  let spanItem := { GridItem.default with
    placement := { column := GridSpan.spanTracks 2 }
  }
  gridCustom props (gridContentStyle screenScale) #[
    gridCell (gridColorForId 4) screenScale 0 0 (some spanItem),
    gridCell (gridColorForId 5) screenScale,
    gridCell (gridColorForId 6) screenScale,
    gridCell (gridColorForId 7) screenScale
  ]

/-- Demo 6: Item alignment (justifySelf, alignSelf) -/
def demoGridAlign (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let props := { GridContainer.withTemplate #[.fr 1] #[.fr 1, .fr 1, .fr 1] (gap := gap) with
    justifyItems := .stretch
    alignItems := .stretch
  }
  let cellW := 50 * screenScale
  let cellH := 40 * screenScale
  let centerItem : GridItem := { placement := {}, justifySelf := some .center, alignSelf := some .center }
  let endItem : GridItem := { placement := {}, justifySelf := some .flexEnd, alignSelf := some .flexEnd }
  gridCustom props (gridContentStyle screenScale) #[
    gridCell (gridColorForId 1) screenScale cellW cellH (some GridItem.default),
    gridCell (gridColorForId 2) screenScale cellW cellH (some centerItem),
    gridCell (gridColorForId 3) screenScale cellW cellH (some endItem)
  ]

/-- Demo 7: Complex grid layout (header, sidebar, main, footer) -/
def demoGridComplex (screenScale : Float) : WidgetBuilder := do
  let gap := 10 * screenScale
  let props := GridContainer.withTemplate
    #[.px (40 * screenScale), .fr 1, .px (30 * screenScale)]  -- header, content, footer
    #[.px (80 * screenScale), .fr 1]  -- sidebar, main
    (gap := gap)

  -- Header spans both columns (row 1, cols 1-2)
  let headerPlacement := { GridItem.default with
    placement := {
      row := GridSpan.lines 1 2
      column := GridSpan.lines 1 3
    }
  }

  -- Sidebar (row 2, col 1)
  let sidebarPlacement := { GridItem.default with
    placement := GridPlacement.atPosition 2 1
  }

  -- Main content (row 2, col 2)
  let mainPlacement := { GridItem.default with
    placement := GridPlacement.atPosition 2 2
  }

  -- Footer spans both columns (row 3, cols 1-2)
  let footerPlacement := { GridItem.default with
    placement := {
      row := GridSpan.lines 3 4
      column := GridSpan.lines 1 3
    }
  }

  gridCustom props (gridContentStyle screenScale) #[
    gridCell (gridColorForId 1) screenScale 0 0 (some headerPlacement),
    gridCell (gridColorForId 2) screenScale 0 0 (some sidebarPlacement),
    gridCell (gridColorForId 3) screenScale 0 0 (some mainPlacement),
    gridCell (gridColorForId 4) screenScale 0 0 (some footerPlacement)
  ]

/-- Build all grid demos using Arbor widgets. -/
def cssGridWidget (fontTitle fontSmall : FontId) (screenScale : Float) : WidgetBuilder := do
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
  let titleWidget := text' "CSS Grid Layout Demo (Space to advance)" fontTitle Afferent.Color.white .left

  let leftCol := column (gap := 12 * s) (style := columnStyle) #[(
    gridSection "Grid: 3 equal columns (1fr 1fr 1fr)" "Expected: 3 cells of equal width"
      fontSmall fontSmall s (90 * s) (demoGrid3Col s)
    ),(
    gridSection "Grid: Mixed sizes (80px 1fr 2fr)" "Expected: fixed + 1 part + 2 parts"
      fontSmall fontSmall s (90 * s) (demoGridMixed s)
    ),(
    gridSection "Grid: Auto-placement (6 items, 3 cols)" "Expected: 2 rows x 3 cols, items flow left-to-right"
      fontSmall fontSmall s (130 * s) (demoGridAuto s)
    ),(
    gridSection "Complex: header + sidebar + main + footer" "Header and footer span 2 columns"
      fontSmall fontSmall s (220 * s) (demoGridComplex s)
    )]

  let rightCol := column (gap := 12 * s) (style := columnStyle) #[(
    gridSection "Explicit Placement" "Items placed at specific row/col positions"
      fontSmall fontSmall s (140 * s) (demoGridExplicit s)
    ),(
    gridSection "Spanning Cells" "First item spans 2 columns"
      fontSmall fontSmall s (140 * s) (demoGridSpan s)
    ),(
    gridSection "Alignment within cells" "stretch / center / end"
      fontSmall fontSmall s (90 * s) (demoGridAlign s)
    )]

  column (gap := 16 * s) (style := rootStyle) #[(
    titleWidget
    ),(
    row (gap := 20 * s) (style := { flexItem := some (Trellis.FlexItem.growing 1) }) #[leftCol, rightCol]
    )]

end Demos
