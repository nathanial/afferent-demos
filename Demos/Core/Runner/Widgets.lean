/-
  Demo Runner - UI widgets and layout helpers.
-/
import Afferent
import Afferent.Arbor
import Demos.Core.DemoRegistry
import Demos.Core.TabBar
import Demos.Core.Runner.Types

open Afferent

namespace Demos

private def tabBarStartId : Nat := 1

/-- Build tab configurations from the demo list. -/
def buildTabConfigs (demos : Array AnyDemo) (selectedIdx : Nat) : Array TabConfig :=
  demos.mapIdx fun idx demo => {
    id := idx
    label := AnyDemo.shortName demo
    isActive := idx == selectedIdx
  }

/-- Build or rebuild the tabbar widget. -/
def rebuildTabBar (demos : Array AnyDemo) (selectedIdx : Nat)
    (fontId : Afferent.Arbor.FontId) (screenScale : Float) : TabBarResult :=
  let configs := buildTabConfigs demos selectedIdx
  buildTabBar configs fontId {} screenScale tabBarStartId

def nextWidgetId (w : Afferent.Arbor.Widget) : Nat :=
  (Afferent.Arbor.Widget.allIds w).foldl (fun acc wid => max acc wid) 0 + 1

private def buildFooterWidget (startId : Nat) (fontId : Afferent.Arbor.FontId)
    (screenScale : Float) (line1 line2 line3 line4 : String) : Afferent.Arbor.Widget :=
  Afferent.Arbor.buildFrom startId do
    let s := fun (v : Float) => v * screenScale
    let outerStyle : Afferent.Arbor.BoxStyle := {
      backgroundColor := some (Color.gray 0.08)
      padding := Trellis.EdgeInsets.symmetric (s 8) (s 4)
      height := .length (s footerBarHeight)
      flexItem := some (Trellis.FlexItem.fixed (s footerBarHeight))
    }
    let rowStyle : Afferent.Arbor.BoxStyle := {
      width := .percent 1.0
    }
    Afferent.Arbor.flexColumn (Trellis.FlexContainer.column (s 2)) outerStyle #[
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line1 fontId (Color.gray 0.7) .left none
      ],
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line2 fontId (Color.gray 0.65) .left none
      ],
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line3 fontId (Color.gray 0.6) .left none
      ],
      Afferent.Arbor.flexRow { Trellis.FlexContainer.row 0 with alignItems := .center } rowStyle #[
        Afferent.Arbor.text' line4 fontId (Color.gray 0.55) .left none
      ]
    ]

def buildRootWidget (tabBar : TabBarResult) (content : Afferent.Arbor.Widget)
    (footerLine1 footerLine2 footerLine3 footerLine4 : String) (fontId : Afferent.Arbor.FontId) (screenScale : Float) : RootBuild :=
  let footerStartId := nextWidgetId content
  let footer := buildFooterWidget footerStartId fontId screenScale footerLine1 footerLine2 footerLine3 footerLine4
  let root : Afferent.Arbor.Widget :=
    .flex 0 none (Trellis.FlexContainer.column 0)
      { width := .percent 1.0, height := .percent 1.0 }
      #[tabBar.widget, content, footer]
  { widget := root, tabBar := tabBar, contentId := Afferent.Arbor.Widget.id content }

end Demos
