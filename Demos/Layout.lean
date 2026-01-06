/-
  Layout Cards - Flexbox layout visualization using Trellis.
  Rendered as Arbor card widgets in a 3x3 grid.
-/
import Afferent
import Afferent.Arbor
import Trellis
import Demos.Overview.Card

open Afferent.Arbor
open Trellis (EdgeInsets LayoutNode LayoutResult FlexContainer FlexItem ContentSize)

namespace Demos

/-- Colors for layout cells -/
private def cellColors : Array Color := #[
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
private def colorForId (id : Nat) : Color :=
  cellColors[id % cellColors.size]!

/-- Convert a Trellis LayoutResult to RenderCommands -/
private def layoutResultToCommands (result : LayoutResult) (offset : Point) : RenderCommands := Id.run do
  let mut cmds : RenderCommands := #[]
  for cl in result.layouts do
    let rect := cl.borderRect
    let x := rect.x + offset.x
    let y := rect.y + offset.y
    let w := rect.width
    let h := rect.height
    -- Fill rectangle with node color
    let color := colorForId cl.nodeId |>.withAlpha 0.7
    cmds := cmds.push (.fillRect (Rect.mk' x y w h) color)
    -- White border
    let path := Path.rectangle (Rect.mk' x y w h)
    cmds := cmds.push (.strokePath path Afferent.Color.white 2)
  return cmds

/-- Layout card definition -/
structure LayoutCardDef where
  label : String
  tree : Float → Float → LayoutNode

/-- Demo 1: Basic flex row with items left-aligned -/
private def flexRowTree (w h : Float) : LayoutNode :=
  let gap := min w h * 0.04
  let itemW := w * 0.25
  let itemH := h * 0.6
  LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' itemW itemH),
    LayoutNode.leaf 2 (ContentSize.mk' (itemW * 1.2) itemH),
    LayoutNode.leaf 3 (ContentSize.mk' (itemW * 0.9) itemH)
  ] (gap := gap)

/-- Demo 2: Flex row with justify-content: center -/
private def flexRowCenterTree (w h : Float) : LayoutNode :=
  let gap := min w h * 0.04
  let itemW := w * 0.2
  let itemH := h * 0.5
  let props := { FlexContainer.row with justifyContent := .center, gap := gap }
  LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' itemW itemH),
    LayoutNode.leaf 2 (ContentSize.mk' itemW itemH),
    LayoutNode.leaf 3 (ContentSize.mk' itemW itemH)
  ]

/-- Demo 3: Flex row with space-between -/
private def spaceBetweenTree (w h : Float) : LayoutNode :=
  let itemW := w * 0.18
  let itemH := h * 0.5
  let props := { FlexContainer.row with justifyContent := .spaceBetween }
  LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' itemW itemH),
    LayoutNode.leaf 2 (ContentSize.mk' itemW itemH),
    LayoutNode.leaf 3 (ContentSize.mk' itemW itemH)
  ]

/-- Demo 4: Flex grow with 1:2:1 ratio -/
private def flexGrowTree (w h : Float) : LayoutNode :=
  let gap := min w h * 0.04
  let itemH := h * 0.5
  LayoutNode.flexBox 0 (FlexContainer.row gap) #[
    LayoutNode.leaf' 1 0 itemH {} (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf' 2 0 itemH {} (.flexChild (FlexItem.growing 2)),
    LayoutNode.leaf' 3 0 itemH {} (.flexChild (FlexItem.growing 1))
  ]

/-- Demo 5: Column layout -/
private def flexColumnTree (w h : Float) : LayoutNode :=
  let gap := min w h * 0.04
  let itemW := w * 0.6
  let itemH := h * 0.2
  LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' itemW itemH),
    LayoutNode.leaf 2 (ContentSize.mk' (itemW * 1.1) (itemH * 1.2)),
    LayoutNode.leaf 3 (ContentSize.mk' (itemW * 0.8) itemH)
  ] (gap := gap)

/-- Demo 6: Align items center (cross-axis) -/
private def alignCenterTree (w h : Float) : LayoutNode :=
  let gap := min w h * 0.04
  let itemW := w * 0.2
  let props := { FlexContainer.row with alignItems := .center, gap := gap }
  LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' itemW (h * 0.3)),
    LayoutNode.leaf 2 (ContentSize.mk' itemW (h * 0.6)),
    LayoutNode.leaf 3 (ContentSize.mk' itemW (h * 0.45))
  ]

/-- Demo 7: Nested containers -/
private def nestedTree (w h : Float) : LayoutNode :=
  let gap := min w h * 0.04
  let sideW := w * 0.2
  let innerColumn := LayoutNode.column 10 #[
    LayoutNode.leaf 11 (ContentSize.mk' (w * 0.3) (h * 0.25)),
    LayoutNode.leaf 12 (ContentSize.mk' (w * 0.3) (h * 0.25))
  ] (gap := gap)
  LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' sideW (h * 0.8)),
    innerColumn.withItem (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf 2 (ContentSize.mk' sideW (h * 0.8))
  ] (gap := gap)

/-- Demo 8: Complex layout with header + sidebar + main -/
private def complexTree (w h : Float) : LayoutNode :=
  let gap := min w h * 0.03
  let headerH := h * 0.15
  let sidebarW := w * 0.25
  let itemH := (h - headerH - gap * 4) / 3

  let header := LayoutNode.row 100 #[
    LayoutNode.leaf' 101 0 headerH {} (.flexChild (FlexItem.growing 1))
  ]

  let sidebar := LayoutNode.column 200 #[
    LayoutNode.leaf 201 (ContentSize.mk' sidebarW itemH),
    LayoutNode.leaf 202 (ContentSize.mk' sidebarW itemH),
    LayoutNode.leaf 203 (ContentSize.mk' sidebarW itemH)
  ] (gap := gap)

  let mainContent := LayoutNode.flexBox 300
    { FlexContainer.column with alignItems := .stretch }
    #[LayoutNode.leaf' 301 0 0 {} (.flexChild (FlexItem.growing 1))]

  let content := LayoutNode.row 400 #[
    sidebar,
    mainContent.withItem (.flexChild (FlexItem.growing 1))
  ] (gap := gap)

  LayoutNode.column 0 #[
    header,
    content.withItem (.flexChild (FlexItem.growing 1))
  ] (gap := gap)

/-- Demo 9: Overview - header + 2x2 grid + footer -/
private def overviewTree (w h : Float) : LayoutNode :=
  let gap := min w h * 0.03
  let headerH := h * 0.12
  let footerH := h * 0.1
  let cellW := (w - gap) / 2
  let cellH := (h - headerH - footerH - gap * 4) / 2

  let header := LayoutNode.leaf' 100 0 headerH {} (.flexChild (FlexItem.growing 1))

  let gridRow1 := LayoutNode.row 200 #[
    LayoutNode.leaf 201 (ContentSize.mk' cellW cellH),
    LayoutNode.leaf 202 (ContentSize.mk' cellW cellH)
  ] (gap := gap)

  let gridRow2 := LayoutNode.row 300 #[
    LayoutNode.leaf 301 (ContentSize.mk' cellW cellH),
    LayoutNode.leaf 302 (ContentSize.mk' cellW cellH)
  ] (gap := gap)

  let footer := LayoutNode.leaf' 400 0 footerH {} (.flexChild (FlexItem.growing 1))

  LayoutNode.column 0 #[header, gridRow1, gridRow2, footer] (gap := gap)

/-- All layout cards -/
private def layoutCards : Array LayoutCardDef := #[
  { label := "Row: justify-content: flex-start",     tree := flexRowTree },
  { label := "Row: justify-content: center",         tree := flexRowCenterTree },
  { label := "Row: justify-content: space-between",  tree := spaceBetweenTree },
  { label := "Row: flex-grow ratio 1:2:1",           tree := flexGrowTree },
  { label := "Column: flex-direction: column",       tree := flexColumnTree },
  { label := "Row: align-items: center",             tree := alignCenterTree },
  { label := "Nested: row containing column",        tree := nestedTree },
  { label := "Complex: header + sidebar + main",     tree := complexTree },
  { label := "Overview: header + grid + footer",     tree := overviewTree }
]

/-- Layout demo rendered as cards in a 3x3 grid. -/
def layoutWidgetFlex (labelFont : FontId) : WidgetBuilder := do
  let cards := layoutCards.map (fun card =>
    demoCardFlex labelFont card.label (fun r =>
      let tree := card.tree r.size.width r.size.height
      let result := Trellis.layout tree r.size.width r.size.height
      layoutResultToCommands result r.origin))
  gridFlex 3 3 4 cards (EdgeInsets.uniform 6)

end Demos
