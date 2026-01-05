/-
  Demo Grid - Normal demo mode showing all demos in a 2x3 grid layout
  Uses Trellis CSS Grid for proper resize handling.
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
open Trellis

namespace Demos

/-- Cell configuration: background color, label, scale factor, and render function -/
structure CellConfig where
  bg : Color
  label : String
  scale : Float
  render : Float → Afferent.FontRegistry → DemoFonts → CanvasM Unit

/-- Get cell configuration by index (0-5, left-to-right, top-to-bottom) -/
def getCellConfig (idx : Nat) : CellConfig :=
  match idx with
  | 0 => ⟨Color.hsva 0.667 0.25 0.20 1.0, "Shapes",     0.45, fun _ reg fonts => renderShapesM reg fonts.label⟩
  | 1 => ⟨Color.hsva 0.0   0.25 0.20 1.0, "Transforms", 0.60, fun _ reg fonts => renderTransformsM reg fonts.label⟩
  | 2 => ⟨Color.hsva 0.333 0.25 0.20 1.0, "Strokes",    0.51, fun _ reg fonts => renderStrokesM reg fonts.label⟩
  | 3 => ⟨Color.hsva 0.125 0.4  0.20 1.0, "Gradients",  0.51, fun _ reg fonts => renderGradientsM reg fonts.label⟩
  | 4 => ⟨Color.hsva 0.767 0.25 0.20 1.0, "Text",       0.51, fun _ reg fonts => renderTextM reg fonts⟩
  | _ => ⟨Color.hsva 0.75  0.25 0.20 1.0, "Animations", 0.45, fun t reg fonts => renderAnimationsM t reg fonts.label⟩

def cellLabelColor : Color :=
  Color.hsva 0.0 0.0 1.0 0.5

def cellSpec (config : CellConfig) (screenScale : Float)
    (labelFont : Afferent.Arbor.FontId) : Afferent.Arbor.CustomSpec :=
  { measure := fun _ _ => (0, 0)
    collect := fun layout =>
      let rect := layout.borderRect
      let x := rect.x + 10 * screenScale
      let y := rect.y + 20 * screenScale
      #[.fillText config.label x y labelFont cellLabelColor] }

def cellWidget (config : CellConfig) (screenScale : Float)
    (labelFont : Afferent.Arbor.FontId) : Afferent.Arbor.WidgetBuilder := do
  let style : Afferent.Arbor.BoxStyle := {
    backgroundColor := some config.bg
  }
  Afferent.Arbor.custom (cellSpec config screenScale labelFont) style

/-- Render the normal demo mode: 2x3 grid of demo cells using Trellis layout -/
def renderDemoGridM (screenScale : Float) (screenWidth screenHeight : Float)
    (fontRegistry : Afferent.FontRegistry) (demoFonts : DemoFonts)
    (t : Float) : CanvasM Unit := do
  -- Create 2x3 grid with cells that stretch to fill viewport.
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]  -- 3 rows, each 1fr
    #[.fr 1, .fr 1]          -- 2 columns, each 1fr
  let widget := Afferent.Arbor.build do
    Afferent.Arbor.gridCustom props {} #[
      cellWidget (getCellConfig 0) screenScale demoFonts.label,
      cellWidget (getCellConfig 1) screenScale demoFonts.label,
      cellWidget (getCellConfig 2) screenScale demoFonts.label,
      cellWidget (getCellConfig 3) screenScale demoFonts.label,
      cellWidget (getCellConfig 4) screenScale demoFonts.label,
      cellWidget (getCellConfig 5) screenScale demoFonts.label
    ]

  -- Measure + layout using Arbor/Trellis
  let measureResult ← Afferent.runWithFonts fontRegistry
    (Afferent.Arbor.measureWidget widget screenWidth screenHeight)
  let layouts := layout measureResult.node screenWidth screenHeight

  -- Render Arbor widgets (backgrounds + labels)
  let commands := Afferent.Arbor.collectCommands measureResult.widget layouts
  Afferent.Widget.executeCommands fontRegistry commands

  -- Render demo content in each cell
  for cl in layouts.layouts do
    if cl.nodeId >= 1 && cl.nodeId <= 6 then
      let rect := cl.borderRect
      let cellIdx := cl.nodeId - 1
      let config := getCellConfig cellIdx

      -- Render demo content with transform-aware clipping
      saved do
        translate rect.x rect.y
        -- Clip to cell bounds (now transform-aware - will be offset by translate)
        clip (Rect.mk' 0 0 rect.width rect.height)
        scale (config.scale * screenScale) (config.scale * screenScale)
        config.render t fontRegistry demoFonts
        popClip

def renderDemoGridFrame (c : Canvas) (screenScale screenWidth screenHeight : Float)
    (fontRegistry : Afferent.FontRegistry) (demoFonts : DemoFonts)
    (t : Float) : IO Canvas := do
  run' c do
    resetTransform
    renderDemoGridM screenScale screenWidth screenHeight fontRegistry demoFonts t

end Demos
