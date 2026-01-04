/-
  Animations Demo - Psychedelic animated shapes ("Disco Party")
-/
import Afferent

open Afferent CanvasM Linalg

namespace Demos

/-- Render psychedelic animation cell using CanvasM -/
def renderAnimationsM (t : Float) : CanvasM Unit := do
  -- Spinning star cluster
  saved do
    translate 150 150
    for i in [:7] do
      saved do
        let angle := t * 2.0 + i.toFloat * (Float.twoPi / 7.0)
        let dist := 60 + 20 * Float.sin (t * 3.0 + i.toFloat)
        translate (dist * Float.cos angle) (dist * Float.sin angle)
        rotate (t * 4.0 + i.toFloat)
        let hue := (t * 0.5 + i.toFloat / 7.0) - (t * 0.5 + i.toFloat / 7.0).floor
        setFillColor (Color.hsv hue 1.0 1.0)
        fillPath (Path.star ⟨0, 0⟩ (20 + 10 * Float.sin (t * 5.0)) 10 5)

  -- Pulsing rainbow circles
  saved do
    translate 400 150
    for i in [:12] do
      let angle := i.toFloat * (Float.pi / 6.0)
      let pulse := 0.5 + 0.5 * Float.sin (t * 4.0 + i.toFloat * 0.5)
      let radius := 20 + 30 * pulse
      let x := 80 * Float.cos (angle + t)
      let y := 80 * Float.sin (angle + t)
      let hue := (i.toFloat / 12.0 + t * 0.3) - (i.toFloat / 12.0 + t * 0.3).floor
      setFillColor (Color.hsv hue 1.0 1.0)
      fillCircle ⟨x, y⟩ radius

  -- Wiggling lines (sine wave with moving phase)
  saved do
    translate 650 100
    for row in [:5] do
      let rowOffset := row.toFloat * 0.5
      setLineWidth (2 + row.toFloat)
      let hue := (row.toFloat / 5.0 + t * 0.2) - (row.toFloat / 5.0 + t * 0.2).floor
      setStrokeColor (Color.hsv hue 1.0 1.0)
      let mut path := Path.empty
      path := path.moveTo ⟨0, row.toFloat * 50⟩
      for i in [:20] do
        let x := i.toFloat * 15
        let y := row.toFloat * 50 + 20 * Float.sin (t * 6.0 + x * 0.05 + rowOffset)
        path := path.lineTo ⟨x, y⟩
      strokePath path

  -- Morphing polygon (changing number of sides smoothly via rotation)
  saved do
    translate 150 300
    rotate (t * 1.5)
    let sides := 3 + ((t * 0.5).floor.toUInt32 % 6).toNat
    let hue := (t * 0.4) - (t * 0.4).floor
    setFillColor (Color.hsv hue 0.8 0.9)
    fillPath (Path.polygon ⟨0, 0⟩ (40 + 20 * Float.sin t) sides)

  -- Orbiting hearts with trail effect
  saved do
    translate 400 320
    for i in [:8] do
      let trailT := t - i.toFloat * 0.05
      let angle := trailT * 2.0
      let x := 60 * Float.cos angle
      let y := 40 * Float.sin angle
      let alpha := 1.0 - i.toFloat * 0.12
      let hue := (trailT * 0.3) - (trailT * 0.3).floor
      let color := Color.hsv hue 1.0 1.0
      setFillColor (Color.rgba color.r color.g color.b alpha)
      saved do
        translate x y
        scale (0.3 + 0.1 * Float.sin (t * 3.0)) (0.3 + 0.1 * Float.sin (t * 3.0))
        fillPath (Path.heart ⟨0, 0⟩ 80)

  -- Bouncing rectangles with color cycling
  saved do
    translate 650 280
    for i in [:6] do
      let phase := i.toFloat * 0.8
      let bounce := Float.abs (Float.sin (t * 3.0 + phase)) * 60
      let x := i.toFloat * 45
      let rotation := t * 2.0 + phase
      saved do
        translate x (-bounce)
        rotate rotation
        let hue := (t * 0.5 + i.toFloat / 6.0) - (t * 0.5 + i.toFloat / 6.0).floor
        setFillColor (Color.hsv hue 0.9 1.0)
        fillRectXYWH (-15) (-15) 30 30

end Demos
