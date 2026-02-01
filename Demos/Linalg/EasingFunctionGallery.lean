/-
  Easing Function Gallery - grid of easing curves with animated markers.
  Includes comparison panel for side-by-side viewing.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Easing

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Easing entry for the gallery. -/
structure EasingEntry where
  name : String
  ease : Float → Float

/-- List of easing functions to display. -/
def easingEntries : Array EasingEntry := #[
  { name := "Quad In", ease := Easing.quadIn },
  { name := "Quad Out", ease := Easing.quadOut },
  { name := "Quad InOut", ease := Easing.quadInOut },
  { name := "Cubic In", ease := Easing.cubicIn },
  { name := "Cubic Out", ease := Easing.cubicOut },
  { name := "Cubic InOut", ease := Easing.cubicInOut },
  { name := "Sine In", ease := Easing.sineIn },
  { name := "Sine Out", ease := Easing.sineOut },
  { name := "Sine InOut", ease := Easing.sineInOut },
  { name := "Expo In", ease := Easing.expoIn },
  { name := "Expo Out", ease := Easing.expoOut },
  { name := "Expo InOut", ease := Easing.expoInOut },
  { name := "Circ In", ease := Easing.circIn },
  { name := "Circ Out", ease := Easing.circOut },
  { name := "Circ InOut", ease := Easing.circInOut },
  { name := "Back In", ease := Easing.backIn },
  { name := "Back Out", ease := Easing.backOut },
  { name := "Back InOut", ease := Easing.backInOut },
  { name := "Elastic In", ease := Easing.elasticIn },
  { name := "Elastic Out", ease := Easing.elasticOut },
  { name := "Elastic InOut", ease := Easing.elasticInOut },
  { name := "Bounce In", ease := Easing.bounceIn },
  { name := "Bounce Out", ease := Easing.bounceOut },
  { name := "Bounce InOut", ease := Easing.bounceInOut }
]

/-- Count of easing entries. -/
def easingEntryCount : Nat := easingEntries.size

/-- State for easing gallery. -/
structure EasingFunctionGalleryState where
  t : Float := 0.0
  animating : Bool := true
  speed : Float := 0.75
  selected : Nat := 0
  compare : Nat := 1
  compareMode : Bool := false
  lastTime : Float := 0.0
  deriving Inhabited


def easingFunctionGalleryInitialState : EasingFunctionGalleryState := {}

private def clamp01 (t : Float) : Float :=
  Linalg.Float.clamp t 0.0 1.0

/-- Rectangle helper. -/
structure EasingRect where
  x : Float
  y : Float
  w : Float
  h : Float

private def drawCurveInRect (rect : EasingRect) (ease : Float → Float) (color : Color)
    (graphMin graphMax : Float) : CanvasM Unit := do
  let samples := 48
  let mut path := Afferent.Path.empty
  for i in [:samples] do
    let t := i.toFloat / (samples - 1).toFloat
    let y := ease t
    let y' := (graphMax - y) / (graphMax - graphMin)
    let x := rect.x + t * rect.w
    let yScreen := rect.y + (Linalg.Float.clamp y' 0.0 1.0) * rect.h
    if i == 0 then
      path := path.moveTo (Point.mk x yScreen)
    else
      path := path.lineTo (Point.mk x yScreen)
  setStrokeColor color
  setLineWidth 1.6
  strokePath path

private def drawMarkerInRect (rect : EasingRect) (t value : Float) (color : Color)
    (graphMin graphMax : Float) : CanvasM Unit := do
  let t' := clamp01 t
  let y := Linalg.Float.clamp value graphMin graphMax
  let y' := (graphMax - y) / (graphMax - graphMin)
  let x := rect.x + t' * rect.w
  let yScreen := rect.y + y' * rect.h
  setFillColor color
  fillPath (Afferent.Path.circle (Point.mk x yScreen) 3.5)

/-- Render easing gallery. -/
def renderEasingFunctionGallery (state : EasingFunctionGalleryState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let entries := easingEntries
  let count := entries.size
  if count == 0 then return

  let margin := 20.0 * screenScale
  let headerH := 70.0 * screenScale
  let compareH := if state.compareMode then 160.0 * screenScale else 0.0
  let compareGap := if state.compareMode then 18.0 * screenScale else 0.0
  let gridY := headerH + compareH + compareGap
  let gridH := h - gridY - margin
  let columns := 4
  let rows := (count + columns - 1) / columns
  let cellW := (w - margin * 2.0) / columns.toFloat
  let cellH := gridH / rows.toFloat
  let graphMin := -0.25
  let graphMax := 1.25

  -- Title
  setFillColor VecColor.label
  fillTextXY "EASING FUNCTION GALLERY" (margin) (32.0 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Tab/Shift+Tab: select | C: compare | X: cycle compare | Space: pause | ↑/↓ speed"
    margin (55.0 * screenScale) fontSmall

  -- Comparison panel
  if state.compareMode then
    let panelW := (w - margin * 3.0) / 2.0
    let panelY := headerH
    let rectA : EasingRect := EasingRect.mk margin panelY panelW compareH
    let rectB : EasingRect := EasingRect.mk (margin * 2.0 + panelW) panelY panelW compareH
    let selIdx := state.selected % count
    let cmpIdx := state.compare % count
    let sel := entries.getD selIdx (entries.getD 0 { name := "", ease := fun t => t })
    let cmp := entries.getD cmpIdx (entries.getD 0 { name := "", ease := fun t => t })

    setStrokeColor (Color.gray 0.3)
    setLineWidth 1.0
    strokePath (Afferent.Path.rectangleXYWH rectA.x rectA.y rectA.w rectA.h)
    strokePath (Afferent.Path.rectangleXYWH rectB.x rectB.y rectB.w rectB.h)

    let innerPad := 10.0 * screenScale
    let graphA : EasingRect := EasingRect.mk (rectA.x + innerPad) (rectA.y + innerPad)
      (rectA.w - innerPad * 2.0) (rectA.h - innerPad * 2.0)
    let graphB : EasingRect := EasingRect.mk (rectB.x + innerPad) (rectB.y + innerPad)
      (rectB.w - innerPad * 2.0) (rectB.h - innerPad * 2.0)

    drawCurveInRect graphA sel.ease (Color.rgba 0.2 0.9 1.0 0.9) graphMin graphMax
    drawCurveInRect graphB cmp.ease (Color.rgba 1.0 0.7 0.3 0.9) graphMin graphMax
    let vA := sel.ease state.t
    let vB := cmp.ease state.t
    drawMarkerInRect graphA state.t vA (Color.white) graphMin graphMax
    drawMarkerInRect graphB state.t vB (Color.white) graphMin graphMax

    setFillColor VecColor.label
    fillTextXY sel.name (rectA.x + innerPad) (rectA.y + 16.0 * screenScale) fontSmall
    fillTextXY cmp.name (rectB.x + innerPad) (rectB.y + 16.0 * screenScale) fontSmall

  -- Grid of easing functions
  for idx in [:count] do
    let entry := entries.getD idx (entries.getD 0 { name := "", ease := fun t => t })
    let row := idx / columns
    let col := idx % columns
    let x := margin + col.toFloat * cellW
    let y := gridY + row.toFloat * cellH
    let rect : EasingRect := EasingRect.mk x y cellW cellH

    let isSelected := idx == (state.selected % count)
    let borderColor := if isSelected then Color.yellow else Color.gray 0.35
    setStrokeColor borderColor
    setLineWidth (if isSelected then 2.0 else 1.0)
    strokePath (Afferent.Path.rectangleXYWH rect.x rect.y rect.w rect.h)

    let pad := 10.0 * screenScale
    let graphRect : EasingRect := EasingRect.mk (rect.x + pad) (rect.y + pad)
      (rect.w - pad * 2.0) (rect.h - pad * 2.0 - 14.0 * screenScale)

    drawCurveInRect graphRect entry.ease (Color.rgba 0.2 0.9 1.0 0.85) graphMin graphMax
    let eased := entry.ease state.t
    drawMarkerInRect graphRect state.t eased (Color.white) graphMin graphMax

    -- Animated box along the bottom
    let boxT := Linalg.Float.clamp eased 0.0 1.0
    let boxX := graphRect.x + boxT * graphRect.w
    let boxY := rect.y + rect.h - 18.0 * screenScale
    setFillColor (Color.rgba 0.3 0.8 1.0 0.9)
    fillPath (Afferent.Path.rectangleXYWH (boxX - 6.0) (boxY - 6.0) 12.0 12.0)

    setFillColor VecColor.label
    fillTextXY entry.name (rect.x + pad) (rect.y + rect.h - 6.0 * screenScale) fontSmall

/-- Create the easing gallery widget. -/
def easingFunctionGalleryWidget (env : DemoEnv) (state : EasingFunctionGalleryState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderEasingFunctionGallery state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
