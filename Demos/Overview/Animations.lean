/-
  Animations Demo - Animated cards showing dynamic shapes.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Overview.Card
import Trellis

open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Spinning star cluster. -/
private def spinningStarsCommands (r : Rect) (t : Float) : RenderCommands := Id.run do
  let center := rectCenter r
  let baseRadius := minSide r * 0.18
  let mut cmds : RenderCommands := #[.pushTranslate center.x center.y]
  for i in [:5] do
    let angle := t * 2.0 + i.toFloat * (Path.twoPi / 5.0)
    let dist := minSide r * 0.25 + minSide r * 0.06 * Float.sin (t * 3.0 + i.toFloat)
    let x := dist * Float.cos angle
    let y := dist * Float.sin angle
    let hue := (t * 0.5 + i.toFloat / 5.0) - (t * 0.5 + i.toFloat / 5.0).floor
    cmds := cmds ++ #[
      .pushTranslate x y,
      .pushRotate (t * 3.0 + i.toFloat),
      .fillPath (Path.star ⟨0, 0⟩ baseRadius (baseRadius * 0.5) 5) (Afferent.Color.hsv hue 1.0 1.0),
      .popTransform,
      .popTransform
    ]
  cmds := cmds.push .popTransform
  return cmds

/-- Pulsing rainbow circles. -/
private def pulsingCirclesCommands (r : Rect) (t : Float) : RenderCommands := Id.run do
  let center := rectCenter r
  let orbit := minSide r * 0.28
  let mut cmds : RenderCommands := #[]
  for i in [:10] do
    let angle := i.toFloat * (Path.twoPi / 10.0)
    let pulse := 0.5 + 0.5 * Float.sin (t * 4.0 + i.toFloat * 0.5)
    let radius := minSide r * 0.08 + minSide r * 0.08 * pulse
    let x := center.x + orbit * Float.cos (angle + t)
    let y := center.y + orbit * Float.sin (angle + t)
    let hue := (i.toFloat / 10.0 + t * 0.3) - (i.toFloat / 10.0 + t * 0.3).floor
    cmds := cmds.push (.fillPath (Path.circle ⟨x, y⟩ radius) (Afferent.Color.hsv hue 1.0 1.0))
  return cmds

/-- Wiggling line path. -/
private def wigglingLineCommands (r : Rect) (t : Float) : RenderCommands := Id.run do
  let x0 := r.origin.x + r.size.width * 0.08
  let x1 := r.origin.x + r.size.width * 0.92
  let y0 := r.origin.y + r.size.height * 0.5
  let amp := r.size.height * 0.25
  let steps := 16
  let mut path := Path.empty.moveTo ⟨x0, y0⟩
  for i in [:steps] do
    let x := x0 + (x1 - x0) * (i.toFloat / steps.toFloat)
    let y := y0 + amp * Float.sin (t * 6.0 + x * 0.05)
    path := path.lineTo ⟨x, y⟩
  let hue := (t * 0.2) - (t * 0.2).floor
  return #[
    .strokePath path (Afferent.Color.hsv hue 1.0 1.0) 3.0
  ]

/-- Morphing polygon. -/
private def morphingPolygonCommands (r : Rect) (t : Float) : RenderCommands :=
  let center := rectCenter r
  let sides := 3 + ((t * 0.5).floor.toUInt32 % 6).toNat
  let radius := minSide r * 0.35 + minSide r * 0.08 * Float.sin t
  let hue := (t * 0.4) - (t * 0.4).floor
  #[
    .pushTranslate center.x center.y,
    .pushRotate (t * 1.5),
    .fillPath (Path.polygon ⟨0, 0⟩ radius sides) (Afferent.Color.hsv hue 0.8 0.9),
    .popTransform,
    .popTransform
  ]

/-- Orbiting hearts with trail effect. -/
private def orbitingHeartsCommands (r : Rect) (t : Float) : RenderCommands := Id.run do
  let center := rectCenter r
  let radius := minSide r * 0.25
  let mut cmds : RenderCommands := #[]
  for i in [:6] do
    let trailT := t - i.toFloat * 0.08
    let angle := trailT * 2.0
    let x := center.x + radius * Float.cos angle
    let y := center.y + radius * Float.sin angle * 0.7
    let alpha := 1.0 - i.toFloat * 0.12
    let hue := (trailT * 0.3) - (trailT * 0.3).floor
    let color := Afferent.Color.hsv hue 1.0 1.0
    cmds := cmds ++ #[
      .pushTranslate x y,
      .pushScale (0.25 + 0.1 * Float.sin (t * 3.0)) (0.25 + 0.1 * Float.sin (t * 3.0)),
      .fillPath (Path.heart ⟨0, 0⟩ (minSide r * 0.6)) (Afferent.Color.rgba color.r color.g color.b alpha),
      .popTransform,
      .popTransform
    ]
  return cmds

/-- Bouncing rectangles. -/
private def bouncingRectsCommands (r : Rect) (t : Float) : RenderCommands := Id.run do
  let startX := r.origin.x + r.size.width * 0.15
  let baseY := r.origin.y + r.size.height * 0.75
  let spacing := r.size.width * 0.14
  let size := minSide r * 0.12
  let mut cmds : RenderCommands := #[]
  for i in [:5] do
    let phase := i.toFloat * 0.8
    let bounce := Float.abs (Float.sin (t * 3.0 + phase)) * r.size.height * 0.35
    let x := startX + i.toFloat * spacing
    let y := baseY - bounce
    let hue := (t * 0.5 + i.toFloat / 5.0) - (t * 0.5 + i.toFloat / 5.0).floor
    cmds := cmds ++ #[
      .pushTranslate x y,
      .pushRotate (t * 2.0 + phase),
      .fillRect (Rect.mk' (-size / 2) (-size / 2) size size) (Afferent.Color.hsv hue 0.9 1.0),
      .popTransform,
      .popTransform
    ]
  return cmds

/-- Animated cards rendered as widgets. -/
def animationsWidget (labelFont : FontId) (t : Float) : WidgetBuilder := do
  let cards : Array (String × (Rect → RenderCommands)) := #[(
    "Spinning Stars", fun r => spinningStarsCommands r t
  ), (
    "Pulsing Circles", fun r => pulsingCirclesCommands r t
  ), (
    "Wiggling Line", fun r => wigglingLineCommands r t
  ), (
    "Morphing Poly", fun r => morphingPolygonCommands r t
  ), (
    "Orbiting Hearts", fun r => orbitingHeartsCommands r t
  ), (
    "Bouncing Rects", fun r => bouncingRectsCommands r t
  )]
  let widgets := cards.map fun (label, draw) => demoCardFlex labelFont label draw
  gridFlex 3 10 4 widgets (EdgeInsets.uniform 10)

/-- Responsive animations widget that fills available space. -/
def animationsWidgetFlex (labelFont : FontId) (t : Float) : WidgetBuilder := do
  let cards : Array (String × (Rect → RenderCommands)) := #[
    ("Spinning Stars", fun r => spinningStarsCommands r t),
    ("Pulsing Circles", fun r => pulsingCirclesCommands r t),
    ("Wiggling Line", fun r => wigglingLineCommands r t),
    ("Morphing Poly", fun r => morphingPolygonCommands r t),
    ("Orbiting Hearts", fun r => orbitingHeartsCommands r t),
    ("Bouncing Rects", fun r => bouncingRectsCommands r t)
  ]
  let widgets := cards.map fun (label, draw) => demoCardFlex labelFont label draw
  gridFlex 2 3 4 widgets

/-- Render animations demo content to canvas using Arbor widgets. -/
def renderAnimationsM (t : Float) (reg : Afferent.FontRegistry) (labelFont : FontId) : Afferent.CanvasM Unit := do
  let widget := Afferent.Arbor.build (animationsWidget labelFont t)
  Afferent.Widget.renderArborWidget reg widget 1000 800

end Demos
