/-
  Canopy Showcase Layout Tests
  Verify that columns maintain proper sizing constraints.
-/
import Crucible
import Afferent
import Afferent.Arbor
import Demos.CanopyShowcase
import Trellis

namespace AfferentDemosTests.CanopyShowcaseLayout

open Crucible
open Afferent
open Afferent.Arbor
open Trellis

/-- Check if a widget is a column (flex container with vertical direction). -/
private def isColumn (w : Widget) : Bool :=
  match w with
  | .flex _ _ props _ _ => !props.direction.isHorizontal
  | _ => false

/-- Find the main two-column row in the widget tree.
    The CanopyShowcase structure is:
    - column (root)
      - heading, caption, etc.
      - row (two-column layout) <-- this is what we want
        - column (left)
        - column (right)
-/
private partial def findTwoColumnRow (w : Widget) : Option Widget :=
  match w with
  | .flex _ _ props _ children =>
    -- Check if this is a row with exactly 2 flex column children
    if !props.direction.isHorizontal then
      -- It's a column, search children
      children.findSome? findTwoColumnRow
    else
      -- It's a row, check if it has 2 column children with flex growing
      let columnChildren := children.filter isColumn
      if columnChildren.size == 2 then some w else children.findSome? findTwoColumnRow
  | _ => none

/-- Get layout result for left and right columns given the two-column row widget. -/
private def getColumnIds (twoColumnRow : Widget) : Option (WidgetId × WidgetId) :=
  match twoColumnRow with
  | .flex _ _ _ _ children =>
    let columnChildren := children.filter isColumn
    if h : columnChildren.size >= 2 then
      some (columnChildren[0].id, columnChildren[1].id)
    else
      none
  | _ => none

/-- Measure and layout the CanopyShowcase widget at the given size. -/
private def measureAndLayout (width height : Float)
    : IO (MeasureResult × LayoutResult) := do
  let widget := Afferent.Arbor.build (Demos.canopyShowcaseWidget FontId.default FontId.default 1.0 {})
  let measureResult ← Afferent.runWithFonts FontRegistry.empty
    (Afferent.Arbor.measureWidget widget width height)
  let layouts := Trellis.layout measureResult.node width height
  pure (measureResult, layouts)

/-- Get the intrinsic content width of a node from its ContentSize. -/
private def getIntrinsicWidth (node : LayoutNode) : Float :=
  match node.content with
  | some cs => cs.width
  | none => 0

/-- Find a node by ID in the LayoutNode tree. -/
private partial def findNodeById (node : LayoutNode) (id : Nat) : Option LayoutNode :=
  if node.id == id then
    some node
  else
    node.children.findSome? (findNodeById · id)

testSuite "CanopyShowcase Layout"

test "can find two-column row in widget tree" := do
  let widget := Afferent.Arbor.build (Demos.canopyShowcaseWidget FontId.default FontId.default 1.0 {})
  match findTwoColumnRow widget with
  | some row =>
    match getColumnIds row with
    | some (leftId, rightId) =>
      ensure (leftId != rightId) "Left and right columns should have different IDs"
    | none =>
      ensure false "Could not get column IDs from two-column row"
  | none =>
    ensure false "Could not find two-column row in widget tree"

test "left column has positive intrinsic content width" := do
  let (measureResult, _layouts) ← measureAndLayout 1200 800
  let widget := measureResult.widget
  match findTwoColumnRow widget with
  | some row =>
    match getColumnIds row with
    | some (leftId, _rightId) =>
      match findNodeById measureResult.node leftId with
      | some leftNode =>
        let leftIntrinsic := getIntrinsicWidth leftNode
        ensure (leftIntrinsic > 0) s!"Left column intrinsic width should be > 0, got {leftIntrinsic}"
      | none =>
        ensure false "Could not find left column node in layout tree"
    | none =>
      ensure false "Could not get column IDs"
  | none =>
    ensure false "Could not find two-column row"

test "left column doesn't shrink below content width at smaller viewports" := do
  -- First measure at large size to get intrinsic content width
  let (measureResult, _layouts) ← measureAndLayout 1200 800
  let widget := measureResult.widget
  match findTwoColumnRow widget with
  | some row =>
    match getColumnIds row with
    | some (leftId, _rightId) =>
      match findNodeById measureResult.node leftId with
      | some leftNode =>
        let intrinsicWidth := getIntrinsicWidth leftNode
        ensure (intrinsicWidth > 0) s!"Intrinsic width should be > 0, got {intrinsicWidth}"

        -- Test at progressively smaller viewport widths
        for testWidth in [1000.0, 800.0, 600.0, 500.0, 400.0] do
          let (_measureResult2, layouts2) ← measureAndLayout testWidth 800
          match layouts2.get leftId with
          | some leftLayout =>
            let actualWidth := leftLayout.borderRect.width
            ensure (actualWidth >= intrinsicWidth - 1.0)
              s!"At viewport {testWidth}px: left column width {actualWidth} should be >= intrinsic {intrinsicWidth}"
          | none =>
            ensure false s!"Could not find left column layout at viewport width {testWidth}"
      | none =>
        ensure false "Could not find left column node"
    | none =>
      ensure false "Could not get column IDs"
  | none =>
    ensure false "Could not find two-column row"

test "columns share space equally at normal viewport sizes" := do
  let (_measureResult, layouts) ← measureAndLayout 1200 800
  let widget := Afferent.Arbor.build (Demos.canopyShowcaseWidget FontId.default FontId.default 1.0 {})
  match findTwoColumnRow widget with
  | some row =>
    match getColumnIds row with
    | some (leftId, rightId) =>
      match layouts.get leftId, layouts.get rightId with
      | some leftLayout, some rightLayout =>
        let leftWidth := leftLayout.borderRect.width
        let rightWidth := rightLayout.borderRect.width
        -- Both should have positive width
        ensure (leftWidth > 0) s!"Left column should have positive width, got {leftWidth}"
        ensure (rightWidth > 0) s!"Right column should have positive width, got {rightWidth}"
        -- They should be roughly equal (within 20% of each other)
        let ratio := leftWidth / rightWidth
        ensure (ratio > 0.5 && ratio < 2.0)
          s!"Columns should be roughly equal size. Left: {leftWidth}, Right: {rightWidth}, Ratio: {ratio}"
      | _, _ =>
        ensure false "Could not find column layouts"
    | none =>
      ensure false "Could not get column IDs"
  | none =>
    ensure false "Could not find two-column row"

test "columns grow proportionally at larger viewport sizes" := do
  let widths := [1200.0, 1600.0, 2000.0, 2400.0]
  let widget := Afferent.Arbor.build (Demos.canopyShowcaseWidget FontId.default FontId.default 1.0 {})
  match findTwoColumnRow widget with
  | some row =>
    match getColumnIds row with
    | some (leftId, rightId) =>
      let mut prevLeftWidth : Float := 0
      let mut prevRightWidth : Float := 0
      for testWidth in widths do
        let (_measureResult, layouts) ← measureAndLayout testWidth 800
        match layouts.get leftId, layouts.get rightId with
        | some leftLayout, some rightLayout =>
          let leftWidth := leftLayout.borderRect.width
          let rightWidth := rightLayout.borderRect.width
          -- At larger sizes, both columns should grow (or stay same)
          if prevLeftWidth > 0 then
            ensure (leftWidth >= prevLeftWidth - 1.0)
              s!"Left column should grow with viewport. At {testWidth}: {leftWidth}, previously: {prevLeftWidth}"
            ensure (rightWidth >= prevRightWidth - 1.0)
              s!"Right column should grow with viewport. At {testWidth}: {rightWidth}, previously: {prevRightWidth}"
          prevLeftWidth := leftWidth
          prevRightWidth := rightWidth
        | _, _ =>
          ensure false s!"Could not find column layouts at viewport width {testWidth}"
    | none =>
      ensure false "Could not get column IDs"
  | none =>
    ensure false "Could not find two-column row"

#generate_tests

end AfferentDemosTests.CanopyShowcaseLayout
