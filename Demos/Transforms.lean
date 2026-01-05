/-
  Transforms Demo - Cards showing translation, scaling, rotation, and nesting.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Card
import Trellis

open Afferent.Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Base reference shapes (rect + circle). -/
private def referenceCommands (r : Rect) (color : Color) : RenderCommands :=
  let rect := Rect.mk' (r.origin.x + r.size.width * 0.15)
    (r.origin.y + r.size.height * 0.3)
    (r.size.width * 0.3) (r.size.height * 0.2)
  let center := Point.mk' (r.origin.x + r.size.width * 0.7)
    (r.origin.y + r.size.height * 0.4)
  let radius := minSide r * 0.18
  #[
    .fillRect rect color,
    .fillPath (Path.circle center radius) color
  ]

/-- Translation example. -/
private def translateCommands (r : Rect) : RenderCommands :=
  let dx := r.size.width * 0.12
  let dy := r.size.height * 0.08
  #[
    .pushTranslate dx dy
  ] ++ referenceCommands r Afferent.Color.red ++ #[
    .popTransform
  ]

/-- Scaling example. -/
private def scaleCommands (r : Rect) : RenderCommands :=
  let center := rectCenter r
  let rectW := r.size.width * 0.32
  let rectH := r.size.height * 0.2
  let circleR := minSide r * 0.16
  #[
    .pushTranslate center.x center.y,
    .pushScale 1.3 1.3,
    .fillRect (Rect.mk' (-rectW / 2) (-rectH / 2) rectW rectH) Afferent.Color.green,
    .fillPath (Path.circle ⟨rectW * 0.6, 0⟩ circleR) Afferent.Color.green,
    .popTransform,
    .popTransform
  ]

/-- Rotation fan example. -/
private def rotateFanCommands (r : Rect) : RenderCommands := Id.run do
  let center := rectCenter r
  let rectW := r.size.width * 0.35
  let rectH := r.size.height * 0.1
  let mut cmds : RenderCommands := #[.pushTranslate center.x center.y]
  for i in [:8] do
    let angle := i.toFloat * (Path.pi / 4.0)
    let color := Afferent.Color.rgba
      (0.5 + 0.5 * Float.cos angle)
      (0.5 + 0.5 * Float.sin angle)
      0.5
      0.8
    cmds := cmds ++ #[
      .pushRotate angle,
      .fillRect (Rect.mk' (rectW * 0.1) (-rectH / 2) rectW rectH) color,
      .popTransform
    ]
  cmds := cmds.push .popTransform
  return cmds

/-- Scaled circles example. -/
private def scaledCircleCommands (r : Rect) : RenderCommands := Id.run do
  let center := rectCenter r
  let spacing := r.size.width * 0.2
  let startX := center.x - spacing * 1.5
  let baseRadius := minSide r * 0.18
  let mut cmds : RenderCommands := #[]
  for i in [:4] do
    let s := 0.6 + i.toFloat * 0.25
    let x := startX + i.toFloat * spacing
    let color := Afferent.Color.rgba
      (1.0 - i.toFloat * 0.15)
      (i.toFloat * 0.2)
      (0.5 + i.toFloat * 0.1)
      1.0
    cmds := cmds ++ #[
      .pushTranslate x center.y,
      .pushScale s s,
      .fillPath (Path.circle ⟨0, 0⟩ baseRadius) color,
      .popTransform,
      .popTransform
    ]
  return cmds

/-- Combined rotate + scale star. -/
private def combinedCommands (r : Rect) : RenderCommands :=
  let center := rectCenter r
  let radius := minSide r * 0.35
  #[
    .pushTranslate center.x center.y,
    .pushRotate (Path.pi / 6.0),
    .pushScale 1.2 0.8,
    .fillPath (Path.star ⟨0, 0⟩ radius (radius * 0.5) 5) Afferent.Color.yellow,
    .popTransform,
    .popTransform,
    .popTransform
  ]

/-- Nested transforms (concentric circles). -/
private def nestedCommands (r : Rect) : RenderCommands :=
  let center := rectCenter r
  let radius := minSide r * 0.35
  #[
    .pushTranslate center.x center.y,
    .fillPath (Path.circle ⟨0, 0⟩ radius) Afferent.Color.blue,
    .pushScale 0.6 0.6,
    .fillPath (Path.circle ⟨0, 0⟩ radius) Afferent.Color.cyan,
    .pushScale 0.5 0.5,
    .fillPath (Path.circle ⟨0, 0⟩ radius) Afferent.Color.white,
    .popTransform,
    .popTransform,
    .popTransform
  ]

/-- Alpha blending demo. -/
private def alphaCommands (r : Rect) : RenderCommands :=
  let center := rectCenter r
  let w := r.size.width * 0.45
  let h := r.size.height * 0.3
  let base := Rect.mk' (center.x - w / 2) (center.y - h / 2) w h
  #[
    .fillRect base (Afferent.Color.rgba 1.0 0.0 0.0 1.0),
    .fillRect (Rect.mk' (base.origin.x + w * 0.2) (base.origin.y + h * 0.2) w h)
      (Afferent.Color.rgba 0.0 0.0 1.0 0.5),
    .fillRect (Rect.mk' (base.origin.x + w * 0.4) (base.origin.y + h * 0.4) w h)
      (Afferent.Color.rgba 0.0 1.0 0.0 0.3)
  ]

/-- Orbiting squares demo. -/
private def orbitCommands (r : Rect) : RenderCommands := Id.run do
  let center := rectCenter r
  let radius := minSide r * 0.32
  let size := minSide r * 0.12
  let mut cmds : RenderCommands := #[.pushTranslate center.x center.y]
  for i in [:6] do
    let angle := i.toFloat * (Path.twoPi / 6.0)
    let color := Afferent.Color.rgba
      (if i % 2 == 0 then 1.0 else 0.5)
      (if i % 3 == 0 then 1.0 else 0.3)
      (if i % 2 == 1 then 1.0 else 0.2)
      1.0
    cmds := cmds ++ #[
      .pushRotate angle,
      .pushTranslate radius 0,
      .fillRect (Rect.mk' (-size / 2) (-size / 2) size size) color,
      .popTransform,
      .popTransform
    ]
  cmds := cmds.push .popTransform
  return cmds

/-- Skew-like effect via rotate + scale. -/
private def skewCommands (r : Rect) : RenderCommands :=
  let center := rectCenter r
  let rectW := r.size.width * 0.5
  let rectH := r.size.height * 0.3
  #[
    .pushTranslate center.x center.y,
    .pushRotate (Path.pi / 12.0),
    .pushScale 1.5 0.7,
    .fillRect (Rect.mk' (-rectW / 2) (-rectH / 2) rectW rectH) Afferent.Color.magenta,
    .popTransform,
    .popTransform,
    .popTransform
  ]

/-- Hearts with different transforms. -/
private def heartCommands (r : Rect) : RenderCommands :=
  let center := rectCenter r
  let radius := minSide r * 0.35
  #[
    .pushTranslate (center.x - radius * 0.6) center.y,
    .fillPath (Path.heart ⟨0, 0⟩ radius) Afferent.Color.red,
    .popTransform,
    .pushTranslate (center.x + radius * 0.6) center.y,
    .pushRotate (Path.pi / 8.0),
    .pushScale 0.7 0.7,
    .fillPath (Path.heart ⟨0, 0⟩ radius) Afferent.Color.magenta,
    .popTransform,
    .popTransform,
    .popTransform
  ]

/-- Transform cards rendered as widgets. -/
def transformsWidget (labelFont : FontId) : WidgetBuilder := do
  let cards : Array (String × (Rect → RenderCommands)) := #[(
    "Reference", fun r => referenceCommands r Afferent.Color.white
  ), (
    "Translate", translateCommands
  ), (
    "Scale", scaleCommands
  ), (
    "Rotate Fan", rotateFanCommands
  ), (
    "Scale Series", scaledCircleCommands
  ), (
    "Rotate+Scale", combinedCommands
  ), (
    "Nested", nestedCommands
  ), (
    "Alpha", alphaCommands
  ), (
    "Orbit", orbitCommands
  ), (
    "Skew", skewCommands
  ), (
    "Hearts", heartCommands
  )]
  let widgets := cards.map fun (label, draw) => demoCard labelFont label draw
  grid 4 10 { padding := EdgeInsets.uniform 10 } widgets

/-- Render transforms demo content to canvas using Arbor widgets. -/
def renderTransformsM (reg : Afferent.FontRegistry) (labelFont : FontId) : Afferent.CanvasM Unit := do
  let widget := Afferent.Arbor.build (transformsWidget labelFont)
  Afferent.Widget.renderArborWidget reg widget 1000 800

end Demos
