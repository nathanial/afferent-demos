/-
  Data Panels - Tables, data grids, list boxes, virtual lists, and tree views.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive

open Reactive Reactive.Host
open Afferent CanvasM
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos.ReactiveShowcase

/-- Table panel - demonstrates tabular data display with row selection. -/
def tablePanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Table" .outlined theme do
    caption' "Click rows to select:" theme
    let columns : Array TableColumn := #[
      { header := "Name" },
      { header := "Email", width := some 160 },
      { header := "Role" },
      { header := "Status" }
    ]
    let rows : Array (Array String) := #[
      #["Alice", "alice@ex.com", "Admin", "Active"],
      #["Bob", "bob@ex.com", "User", "Active"],
      #["Carol", "carol@ex.com", "User", "Inactive"],
      #["David", "david@ex.com", "User", "Active"],
      #["Eve", "eve@ex.com", "Moderator", "Active"]
    ]
    let result ← table columns rows theme
    let _ ← performEvent_ (← Event.mapM (fun rowIdx => do
      IO.println s!"Table row selected: {rowIdx}"
    ) result.onRowSelect)
    pure ()

/-- DataGrid panel - demonstrates editable grid cells. -/
def dataGridPanel (theme : Theme) (font : Afferent.Font) : WidgetM Unit :=
  titledPanel' "DataGrid" .outlined theme do
    caption' "Click a cell to edit, press Enter to commit:" theme
    let columns : Array DataGridColumn := #[
      { header := "Item" },
      { header := "Qty", width := some 60 },
      { header := "Price", width := some 80 }
    ]
    let rows : Array (Array String) := #[
      #["Apples", "3", "$2.40"],
      #["Oranges", "5", "$4.10"],
      #["Bananas", "2", "$1.10"],
      #["Grapes", "1", "$3.25"]
    ]
    let result ← dataGrid columns rows theme font {}
    let _ ← performEvent_ (← Event.mapM (fun (r, c, v) => do
      IO.println s!"DataGrid edit: ({r}, {c}) = {v}"
    ) result.onEdit)
    pure ()

/-- ListBox panel - demonstrates scrollable list with selection. -/
def listBoxPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "ListBox" .outlined theme do
    caption' "Click items to select:" theme
    let fruits := #["Apple", "Banana", "Cherry", "Date",
                    "Elderberry", "Fig", "Grape", "Honeydew",
                    "Kiwi", "Lemon", "Mango", "Nectarine"]
    let result ← listBox fruits theme
    let _ ← performEvent_ (← Event.mapM (fun itemIdx => do
      IO.println s!"ListBox item selected: {itemIdx}"
    ) result.onSelect)
    pure ()

/-- VirtualList panel - demonstrates efficient rendering of long lists. -/
def virtualListPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "VirtualList" .outlined theme do
    caption' "Only visible rows are rendered:" theme
    let itemCount := 500
    let config : VirtualListConfig := {
      width := 220
      height := 180
      itemHeight := 28
      overscan := 3
    }
    let result ← virtualList itemCount (fun idx => do
      let isEven := idx % 2 == 0
      let bg := if isEven then theme.panel.background.withAlpha 0.2 else Color.transparent
      let rowStyle : BoxStyle := {
        backgroundColor := some bg
        padding := EdgeInsets.symmetric 8 4
        width := .percent 1.0
        minHeight := some config.itemHeight
      }
      let wid ← freshId
      let props : FlexContainer := { FlexContainer.row 0 with alignItems := .center }
      let label ← bodyText s!"Row {idx}" theme
      pure (.flex wid none props rowStyle #[label])
    ) theme config

    let _ ← dynWidget result.visibleRange fun (start, stop) =>
      caption' s!"Visible indices: [{start}, {stop})" theme

    let _ ← performEvent_ (← Event.mapM (fun idx => do
      IO.println s!"VirtualList item clicked: {idx}"
    ) result.onItemClick)
    pure ()

/-- TreeView panel - demonstrates hierarchical tree with expand/collapse. -/
def treeViewPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "TreeView" .outlined theme do
    caption' "Click arrows to expand/collapse:" theme
    let nodes : Array TreeNode := #[
      .branch "Documents" #[
        .leaf "Resume.pdf",
        .leaf "Cover Letter.docx",
        .branch "Projects" #[
          .leaf "Project A.pdf",
          .leaf "Project B.pdf"
        ]
      ],
      .branch "Pictures" #[
        .leaf "Vacation.jpg",
        .leaf "Family.png"
      ],
      .leaf "Notes.txt"
    ]
    let result ← treeView nodes theme
    let _ ← performEvent_ (← Event.mapM (fun path => do
      IO.println s!"TreeView node selected: {path}"
    ) result.onNodeSelect)
    let _ ← performEvent_ (← Event.mapM (fun path => do
      IO.println s!"TreeView node toggled: {path}"
    ) result.onNodeToggle)
    pure ()

end Demos.ReactiveShowcase
