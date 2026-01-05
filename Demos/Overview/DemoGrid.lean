/-
  Demo Grid - Normal demo mode showing all demos in a 2x4 grid layout
  Uses pure Arbor widgets with responsive demo content.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Overview.Shapes
import Demos.Overview.Transforms
import Demos.Overview.Strokes
import Demos.Overview.Gradients
import Demos.Overview.Text
import Demos.Overview.Animations
import Demos.Overview.Card
import Demos.Overview.Counter
import Demos.Overview.Paths

open Afferent CanvasM
open Afferent.Arbor
open Trellis

namespace Demos

/-- Cell configuration: background color, label, and content widget builder -/
structure CellConfig where
  bg : Color
  label : String
  content : Float → DemoFonts → WidgetBuilder

/- Get cell configuration by index (0-5, left-to-right, top-to-bottom). -/
def getCellConfig (idx : Nat) : CellConfig :=
  match idx with
  | 0 => ⟨Color.hsva 0.667 0.25 0.20 1.0, "Shapes",     fun _ fonts => shapesWidgetFlex fonts.label⟩
  | 1 => ⟨Color.hsva 0.0   0.25 0.20 1.0, "Transforms", fun _ fonts => transformsWidgetFlex fonts.label⟩
  | 2 => ⟨Color.hsva 0.333 0.25 0.20 1.0, "Strokes",    fun _ fonts => strokesWidgetFlex fonts.label⟩
  | 3 => ⟨Color.hsva 0.125 0.4  0.20 1.0, "Gradients",  fun _ fonts => gradientsWidgetFlex fonts.label⟩
  | 4 => ⟨Color.hsva 0.767 0.25 0.20 1.0, "Text",       fun _ fonts => textWidgetFlex fonts⟩
  | _ => ⟨Color.hsva 0.75  0.25 0.20 1.0, "Animations", fun t fonts => animationsWidgetFlex fonts.label t⟩

def cellLabelColor : Color :=
  Color.hsva 0.0 0.0 1.0 0.7

/-- Build a grid cell containing a label and demo content -/
def cellWidget (config : CellConfig) (screenScale : Float)
    (t : Float) (demoFonts : DemoFonts) : WidgetBuilder := do
  let style : BoxStyle := {
    backgroundColor := some config.bg
    padding := EdgeInsets.uniform (4 * screenScale)
    height := .percent 1.0
  }
  -- Column with label at top and demo content filling the rest
  column (gap := 4 * screenScale) (style := style) #[
    text' config.label demoFonts.label cellLabelColor .left none,
    config.content t demoFonts
  ]

/-- Build the counter overview cell using the interactive counter widget. -/
def counterCellWidget (screenScale : Float) (t : Float) (demoFonts : DemoFonts)
    (counterValue : Int) : WidgetBuilder := do
  let config : CellConfig := {
    bg := Color.hsva 0.92 0.25 0.20 1.0
    label := "Counter"
    content := fun _ fonts => counterWidget fonts.medium fonts.small counterValue screenScale
  }
  cellWidget config screenScale t demoFonts

/-- Build the paths overview cell using path feature cards. -/
def pathFeaturesCellWidget (screenScale : Float) (t : Float) (demoFonts : DemoFonts) : WidgetBuilder := do
  let config : CellConfig := {
    bg := Color.hsva 0.45 0.25 0.18 1.0
    label := "Paths"
    content := fun _ fonts => pathFeaturesWidgetFlex fonts.small
  }
  cellWidget config screenScale t demoFonts

/-- Build the normal demo mode: 2x4 grid of demo cells using Arbor widgets. -/
def demoGridWidget (screenScale : Float) (t : Float) (demoFonts : DemoFonts) (counterValue : Int)
    : WidgetBuilder := do
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1, .fr 1]  -- 4 rows, each 1fr
    #[.fr 1, .fr 1]          -- 2 columns, each 1fr
  let style : BoxStyle := {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  Afferent.Arbor.gridCustom props style #[
    cellWidget (getCellConfig 0) screenScale t demoFonts,
    cellWidget (getCellConfig 1) screenScale t demoFonts,
    cellWidget (getCellConfig 2) screenScale t demoFonts,
    cellWidget (getCellConfig 3) screenScale t demoFonts,
    cellWidget (getCellConfig 4) screenScale t demoFonts,
    cellWidget (getCellConfig 5) screenScale t demoFonts,
    pathFeaturesCellWidget screenScale t demoFonts,
    counterCellWidget screenScale t demoFonts counterValue
  ]

end Demos
