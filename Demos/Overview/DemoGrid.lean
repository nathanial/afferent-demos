/-
  Demo Grid - Normal demo mode showing all demos in a 2x3 grid layout
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

open Afferent CanvasM
open Afferent.Arbor
open Trellis

namespace Demos

/-- Cell configuration: background color, label, and content widget builder -/
structure CellConfig where
  bg : Color
  label : String
  content : Float → DemoFonts → WidgetBuilder

/-- Get cell configuration by index (0-5, left-to-right, top-to-bottom) -/
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

/-- Render the normal demo mode: 2x3 grid of demo cells using pure Arbor widgets -/
def renderDemoGridM (screenScale : Float) (screenWidth screenHeight : Float)
    (fontRegistry : Afferent.FontRegistry) (demoFonts : DemoFonts)
    (t : Float) : CanvasM Unit := do
  -- Create 2x3 grid with cells that stretch to fill viewport.
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]  -- 3 rows, each 1fr
    #[.fr 1, .fr 1]          -- 2 columns, each 1fr

  let widget := Afferent.Arbor.build do
    Afferent.Arbor.gridCustom props {} #[
      cellWidget (getCellConfig 0) screenScale t demoFonts,
      cellWidget (getCellConfig 1) screenScale t demoFonts,
      cellWidget (getCellConfig 2) screenScale t demoFonts,
      cellWidget (getCellConfig 3) screenScale t demoFonts,
      cellWidget (getCellConfig 4) screenScale t demoFonts,
      cellWidget (getCellConfig 5) screenScale t demoFonts
    ]

  -- Single-pass: measure, layout, collect, execute
  let measureResult ← Afferent.runWithFonts fontRegistry
    (Afferent.Arbor.measureWidget widget screenWidth screenHeight)
  let layouts := layout measureResult.node screenWidth screenHeight
  let commands := Afferent.Arbor.collectCommands measureResult.widget layouts
  Afferent.Widget.executeCommands fontRegistry commands

def renderDemoGridFrame (c : Canvas) (screenScale screenWidth screenHeight : Float)
    (fontRegistry : Afferent.FontRegistry) (demoFonts : DemoFonts)
    (t : Float) : IO Canvas := do
  run' c do
    resetTransform
    renderDemoGridM screenScale screenWidth screenHeight fontRegistry demoFonts t

end Demos
