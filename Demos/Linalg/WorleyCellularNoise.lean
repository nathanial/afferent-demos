/-
  Worley Cellular Noise - Visualize Worley F1/F2 distances and feature points.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Noise

open Afferent CanvasM Linalg
open Afferent.Widget

namespace Demos.Linalg

inductive WorleyMode where
  | f1
  | f2
  | f2f1
  | f3f1
  deriving BEq, Inhabited

inductive WorleyDrag where
  | none
  | slider
  deriving BEq, Inhabited

structure WorleyCellularState where
  mode : WorleyMode := .f1
  jitter : Float := 1.0
  showEdges : Bool := true
  showPoints : Bool := true
  showConnections : Bool := false
  dropdownOpen : Bool := false
  dragging : WorleyDrag := .none
  deriving Inhabited

def worleyCellularInitialState : WorleyCellularState := {}

def worleyCellularMathViewConfig (screenScale : Float) : MathView2D.Config := {
  style := { flexItem := some (Trellis.FlexItem.growing 1) }
  scale := 80.0 * screenScale
  showGrid := false
  showAxes := false
  showLabels := false
}

structure WorleySliderLayout where
  x : Float
  y : Float
  width : Float
  height : Float

structure WorleyToggleLayout where
  x : Float
  y : Float
  size : Float

structure WorleyDropdownLayout where
  x : Float
  y : Float
  width : Float
  height : Float

def worleyModeOptions : Array WorleyMode := #[.f1, .f2, .f2f1, .f3f1]

private def panelWidth (screenScale : Float) : Float :=
  250.0 * screenScale

private def panelX (w screenScale : Float) : Float :=
  w - panelWidth screenScale

def worleyDropdownLayout (w h screenScale : Float) : WorleyDropdownLayout :=
  let x := panelX w screenScale + 20.0 * screenScale
  let y := 90.0 * screenScale
  let width := panelWidth screenScale - 40.0 * screenScale
  let height := 28.0 * screenScale
  { x := x, y := y, width := width, height := height }

def worleyDropdownOptionLayout (base : WorleyDropdownLayout) (idx : Nat) : WorleyDropdownLayout :=
  { x := base.x, y := base.y + base.height + idx.toFloat * base.height,
    width := base.width, height := base.height }

def worleySliderLayout (w h screenScale : Float) : WorleySliderLayout :=
  let x := panelX w screenScale + 20.0 * screenScale
  let y := 150.0 * screenScale
  let width := panelWidth screenScale - 40.0 * screenScale
  let height := 8.0 * screenScale
  { x := x, y := y, width := width, height := height }

def worleyToggleLayout (w h screenScale : Float) (idx : Nat) : WorleyToggleLayout :=
  let x := panelX w screenScale + 20.0 * screenScale
  let y := 190.0 * screenScale + idx.toFloat * 26.0 * screenScale
  let size := 16.0 * screenScale
  { x := x, y := y, size := size }

private def clamp01 (t : Float) : Float :=
  Float.clamp t 0.0 1.0

def worleyJitterFromSlider (t : Float) : Float :=
  clamp01 t

private def jitterToSlider (v : Float) : Float :=
  clamp01 v

def WorleyMode.label : WorleyMode -> String
  | .f1 => "F1"
  | .f2 => "F2"
  | .f2f1 => "F2 - F1"
  | .f3f1 => "F3 - F1"

private def renderSlider (label value : String) (t : Float) (layout : WorleySliderLayout)
    (fontSmall : Font) (active : Bool := false) : CanvasM Unit := do
  let t := clamp01 t
  let knobX := layout.x + t * layout.width
  let knobY := layout.y + layout.height / 2.0
  let knobRadius := layout.height * 0.75
  let trackHeight := layout.height * 0.5

  setFillColor (if active then Color.gray 0.7 else Color.gray 0.5)
  fillPath (Afferent.Path.rectangleXYWH layout.x
    (layout.y + layout.height / 2.0 - trackHeight / 2.0)
    layout.width trackHeight)

  setFillColor (if active then Color.yellow else Color.gray 0.85)
  fillPath (Afferent.Path.circle (Point.mk knobX knobY) knobRadius)

  setFillColor (Color.gray 0.75)
  fillTextXY s!"{label}: {value}" layout.x (layout.y - 6.0) fontSmall

private def renderToggle (label : String) (value : Bool) (layout : WorleyToggleLayout)
    (fontSmall : Font) : CanvasM Unit := do
  let box := Afferent.Path.rectangleXYWH layout.x layout.y layout.size layout.size
  setStrokeColor (Color.gray 0.6)
  setLineWidth 1.5
  strokePath box
  if value then
    setFillColor (Color.rgba 0.3 0.9 0.6 1.0)
    fillPath (Afferent.Path.rectangleXYWH (layout.x + 3) (layout.y + 3)
      (layout.size - 6) (layout.size - 6))
  setFillColor (Color.gray 0.8)
  fillTextXY label (layout.x + layout.size + 8.0) (layout.y + layout.size - 3.0) fontSmall

private def renderDropdown (label : String) (layout : WorleyDropdownLayout) (isOpen : Bool)
    (fontSmall : Font) : CanvasM Unit := do
  setFillColor (Color.gray 0.15)
  fillPath (Afferent.Path.rectangleXYWH layout.x layout.y layout.width layout.height)
  setStrokeColor (Color.gray 0.4)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH layout.x layout.y layout.width layout.height)
  setFillColor (Color.gray 0.85)
  fillTextXY label (layout.x + 8.0) (layout.y + layout.height - 8.0) fontSmall
  let arrowX := layout.x + layout.width - 16.0
  let arrowY := layout.y + layout.height / 2.0
  let arrow := if isOpen then "^" else "v"
  fillTextXY arrow arrowX (arrowY + 5.0) fontSmall

-- Permutation table (copied from Linalg.Noise to match feature points).
private def permTable : Array UInt8 := #[
  151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
  140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148,
  247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32,
  57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175,
  74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122,
  60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54,
  65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169,
  200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64,
  52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212,
  207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213,
  119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
  129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104,
  218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241,
  81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157,
  184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93,
  222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180
]

@[inline]
private def perm (i : Int) : UInt8 :=
  let idx := (i % 256 + 256) % 256
  permTable.getD idx.toNat 0

@[inline]
private def worleyHash (i j : Int) (n : Nat) : Float :=
  let h := perm (i + (perm j).toNat * 17 + n * 31)
  h.toFloat / 255.0

@[inline]
private def intToFloat (i : Int) : Float :=
  if i >= 0 then i.toNat.toFloat else -((-i).toNat.toFloat)

private def floatToInt (f : Float) : Int :=
  if f >= 0.0 then Int.ofNat (f.toUInt64.toNat)
  else -Int.ofNat ((-f).toUInt64.toNat)

private def featurePoint (ci cj : Int) (jitter : Float) : Vec2 :=
  let px := intToFloat ci + worleyHash ci cj 0 * jitter
  let py := intToFloat cj + worleyHash ci cj 1 * jitter
  Vec2.mk px py

structure WorleyPoint where
  cellX : Int
  cellY : Int
  pos : Vec2

def renderWorleyCellular (state : WorleyCellularState)
    (view : MathView2D.View) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  let w := view.width
  let h := view.height
  let panelW := panelWidth screenScale
  let plotW := w - panelW
  let plotH := h

  setFillColor (Color.gray 0.08)
  fillPath (Afferent.Path.rectangleXYWH 0 0 plotW plotH)

  let scale := 3.4
  let cellSize := 6.0 * screenScale
  let resX := Float.floor (plotW / cellSize)
  let resY := Float.floor (plotH / cellSize)
  let res := Nat.max 28 (Nat.min resX.toUInt64.toNat resY.toUInt64.toNat)
  let cellW := plotW / res.toFloat
  let cellH := plotH / res.toFloat

  for yi in [:res] do
    for xi in [:res] do
      let u := xi.toFloat / res.toFloat - 0.5
      let v := yi.toFloat / res.toFloat - 0.5
      let sx := u * scale
      let sy := v * scale
      let r := Noise.worley2D sx sy state.jitter
      let f1 := Noise.worley2DF1 sx sy state.jitter
      let value := match state.mode with
        | .f1 => f1
        | .f2 => r.f2
        | .f2f1 => r.f2 - r.f1
        | .f3f1 => r.f3 - r.f1
      let mut n01 := Float.clamp (1.0 - value / 1.6) 0.0 1.0
      if state.showEdges then
        let edge := Noise.worley2DEdge sx sy state.jitter
        let edge01 := Float.clamp (edge * 3.0) 0.0 1.0
        n01 := Float.clamp (n01 + edge01 * 0.7) 0.0 1.0
      setFillColor (Color.gray n01)
      fillPath (Afferent.Path.rectangleXYWH (xi.toFloat * cellW) (yi.toFloat * cellH)
        (cellW + 0.5) (cellH + 0.5))

  -- Feature points and connections
  let minX := -0.5 * scale
  let maxX := 0.5 * scale
  let minCell := floatToInt (Float.floor minX) - 1
  let maxCell := floatToInt (Float.floor maxX) + 1
  let mut points : Array WorleyPoint := #[]
  let mut ci := minCell
  while ci <= maxCell do
    let mut cj := minCell
    while cj <= maxCell do
      let p := featurePoint ci cj state.jitter
      points := points.push { cellX := ci, cellY := cj, pos := p }
      cj := cj + 1
    ci := ci + 1

  let worldToScreen := fun (p : Vec2) =>
    let u := p.x / scale + 0.5
    let v := p.y / scale + 0.5
    (u * plotW, v * plotH)

  if state.showConnections then
    setStrokeColor (Color.rgba 0.8 0.6 0.2 0.4)
    setLineWidth (1.0 * screenScale)
    for i in [:points.size] do
      let p := points.getD i { cellX := 0, cellY := 0, pos := Vec2.zero }
      let mut bestDist := Float.infinity
      let mut best : Option WorleyPoint := none
      for j in [:points.size] do
        if i != j then
          let q := points.getD j { cellX := 0, cellY := 0, pos := Vec2.zero }
          let d := (p.pos - q.pos).length
          if d < bestDist then
            bestDist := d
            best := some q
      match best with
      | some q =>
          if bestDist < 1.8 then
            let a := worldToScreen p.pos
            let b := worldToScreen q.pos
            let path := Afferent.Path.empty
              |>.moveTo (Point.mk a.1 a.2)
              |>.lineTo (Point.mk b.1 b.2)
            strokePath path
      | none => pure ()

  if state.showPoints then
    for p in points do
      let s := worldToScreen p.pos
      setFillColor (Color.rgba 0.95 0.4 0.2 0.9)
      fillPath (Afferent.Path.circle (Point.mk s.1 s.2) (3.0 * screenScale))

  setStrokeColor (Color.gray 0.35)
  setLineWidth 1.0
  strokePath (Afferent.Path.rectangleXYWH 0 0 plotW plotH)

  let pX := panelX w screenScale
  setFillColor (Color.rgba 0.08 0.08 0.1 0.95)
  fillPath (Afferent.Path.rectangleXYWH pX 0 panelW h)

  setFillColor VecColor.label
  fillTextXY "WORLEY CELLULAR" (pX + 20 * screenScale) (36 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "worley2D + Voronoi" (pX + 20 * screenScale) (58 * screenScale) fontSmall

  let drop := worleyDropdownLayout w h screenScale
  renderDropdown s!"Mode: {state.mode.label}" drop state.dropdownOpen fontSmall
  if state.dropdownOpen then
    for i in [:worleyModeOptions.size] do
      let opt := worleyModeOptions.getD i .f1
      let optLayout := worleyDropdownOptionLayout drop i
      setFillColor (if opt == state.mode then Color.rgba 0.2 0.5 0.8 0.6 else Color.gray 0.18)
      fillPath (Afferent.Path.rectangleXYWH optLayout.x optLayout.y optLayout.width optLayout.height)
      setFillColor (if opt == state.mode then Color.white else Color.gray 0.75)
      fillTextXY opt.label (optLayout.x + 8.0) (optLayout.y + optLayout.height - 8.0) fontSmall

  let layout := worleySliderLayout w h screenScale
  let t := jitterToSlider state.jitter
  let active := match state.dragging with | .slider => true | _ => false
  renderSlider "Jitter" (formatFloat state.jitter) t layout fontSmall active

  let toggleA := worleyToggleLayout w h screenScale 0
  let toggleB := worleyToggleLayout w h screenScale 1
  let toggleC := worleyToggleLayout w h screenScale 2
  renderToggle "Show Edges" state.showEdges toggleA fontSmall
  renderToggle "Show Points" state.showPoints toggleB fontSmall
  renderToggle "Connections" state.showConnections toggleC fontSmall

  setFillColor (Color.gray 0.6)
  fillTextXY "R: reset" (pX + 20 * screenScale) (h - 30 * screenScale) fontSmall

/-- Create worley cellular widget. -/
def worleyCellularNoiseWidget (env : DemoEnv) (state : WorleyCellularState)
    : Afferent.Arbor.WidgetBuilder := do
  let config := worleyCellularMathViewConfig env.screenScale
  MathView2D.mathView2D config env.fontSmall (fun view => do
    renderWorleyCellular state view env.screenScale env.fontMedium env.fontSmall
  )

end Demos.Linalg
