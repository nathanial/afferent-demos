/-
  Shared utilities for Linalg vector visualization demos.
  Includes arrow drawing, coordinate grids, and transforms.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Color constants for vector visualization -/
structure VecColor where
private mk ::

namespace VecColor
  def vectorA : Color := Color.cyan
  def vectorB : Color := Color.magenta
  def result : Color := Color.green
  def projection : Color := Color.green
  def perpendicular : Color := Color.red
  def reflection : Color := Color.yellow
  def grid : Color := Color.gray 0.3
  def axis : Color := Color.gray 0.5
  def label : Color := Color.white
  def interpolated : Color := Color.orange
  def xAxis : Color := Color.red
  def yAxis : Color := Color.green
  def zAxis : Color := Color.rgba 0.3 0.5 1.0 1.0  -- Blue
end VecColor

/-- Float modulo operation -/
def floatMod (a b : Float) : Float :=
  a - b * Float.floor (a / b)

/-- Check if a float is approximately a multiple of another -/
def isMultipleOf (a b : Float) : Bool :=
  let rem := floatMod (Float.abs a) (Float.abs b)
  rem < 0.001 || (b - rem) < 0.001

/-- Configuration for arrow drawing -/
structure ArrowConfig where
  color : Color := Color.white
  lineWidth : Float := 2.0
  headLength : Float := 12.0
  headAngle : Float := 0.4  -- Radians from shaft

/-- Configuration for coordinate grid -/
structure GridConfig where
  origin : Float × Float
  scale : Float  -- Pixels per unit
  minorSpacing : Float := 1.0
  majorSpacing : Float := 5.0
  minorColor : Color := Color.gray 0.2
  majorColor : Color := Color.gray 0.4
  axisColor : Color := Color.gray 0.6
  labelColor : Color := Color.white
  width : Float
  height : Float

/-- Convert world coordinates to screen coordinates -/
def worldToScreen (v : Vec2) (origin : Float × Float) (scale : Float) : Float × Float :=
  (origin.1 + v.x * scale, origin.2 - v.y * scale)

/-- Convert screen coordinates to world coordinates -/
def screenToWorld (screen : Float × Float) (origin : Float × Float) (scale : Float) : Vec2 :=
  Vec2.mk ((screen.1 - origin.1) / scale) ((origin.2 - screen.2) / scale)

/-- Check if a point is near another point (for drag detection) -/
def nearPoint (pos : Vec2) (target : Vec2) (threshold : Float) : Bool :=
  Vec2.distanceSquared pos target < threshold * threshold

/-- Draw an arrow from start to end in screen coordinates -/
def drawArrow2D (start finish : Float × Float) (config : ArrowConfig := {}) : CanvasM Unit := do
  let (x1, y1) := start
  let (x2, y2) := finish

  -- Draw the shaft
  setStrokeColor config.color
  setLineWidth config.lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk x1 y1)
    |>.lineTo (Point.mk x2 y2)
  strokePath path

  -- Calculate arrowhead
  let dx := x2 - x1
  let dy := y2 - y1
  let len := Float.sqrt (dx * dx + dy * dy)
  if len > config.headLength then
    let ux := dx / len
    let uy := dy / len
    let cosA := Float.cos config.headAngle
    let sinA := Float.sin config.headAngle
    -- Left side of head
    let lx := x2 - config.headLength * (ux * cosA - uy * sinA)
    let ly := y2 - config.headLength * (uy * cosA + ux * sinA)
    -- Right side of head
    let rx := x2 - config.headLength * (ux * cosA + uy * sinA)
    let ry := y2 - config.headLength * (uy * cosA - ux * sinA)

    setFillColor config.color
    let headPath := Afferent.Path.empty
      |>.moveTo (Point.mk x2 y2)
      |>.lineTo (Point.mk lx ly)
      |>.lineTo (Point.mk rx ry)
      |>.closePath
    fillPath headPath

/-- Draw an arrow from origin to a vector in world coordinates -/
def drawVectorArrow (origin : Vec2) (vec : Vec2) (screenOrigin : Float × Float)
    (scale : Float) (config : ArrowConfig := {}) : CanvasM Unit := do
  let start := worldToScreen origin screenOrigin scale
  let finish := worldToScreen (origin + vec) screenOrigin scale
  drawArrow2D start finish config

/-- Draw a coordinate grid -/
def drawGrid2D (config : GridConfig) (font : Font) : CanvasM Unit := do
  let (ox, oy) := config.origin
  let halfW := config.width / 2
  let halfH := config.height / 2

  -- Calculate visible range in world units
  let minX := -halfW / config.scale
  let maxX := halfW / config.scale
  let minY := -halfH / config.scale
  let maxY := halfH / config.scale

  -- Draw minor grid lines
  setStrokeColor config.minorColor
  setLineWidth 1.0

  -- Vertical minor lines
  let mut x := Float.floor minX
  while x <= maxX do
    if !isMultipleOf x config.majorSpacing then
      let sx := ox + x * config.scale
      let path := Afferent.Path.empty
        |>.moveTo (Point.mk sx (oy - halfH))
        |>.lineTo (Point.mk sx (oy + halfH))
      strokePath path
    x := x + config.minorSpacing

  -- Horizontal minor lines
  let mut y := Float.floor minY
  while y <= maxY do
    if !isMultipleOf y config.majorSpacing then
      let sy := oy - y * config.scale
      let path := Afferent.Path.empty
        |>.moveTo (Point.mk (ox - halfW) sy)
        |>.lineTo (Point.mk (ox + halfW) sy)
      strokePath path
    y := y + config.minorSpacing

  -- Draw major grid lines
  setStrokeColor config.majorColor
  setLineWidth 1.0

  -- Vertical major lines
  x := Float.floor (minX / config.majorSpacing) * config.majorSpacing
  while x <= maxX do
    if x != 0 then
      let sx := ox + x * config.scale
      let path := Afferent.Path.empty
        |>.moveTo (Point.mk sx (oy - halfH))
        |>.lineTo (Point.mk sx (oy + halfH))
      strokePath path
    x := x + config.majorSpacing

  -- Horizontal major lines
  y := Float.floor (minY / config.majorSpacing) * config.majorSpacing
  while y <= maxY do
    if y != 0 then
      let sy := oy - y * config.scale
      let path := Afferent.Path.empty
        |>.moveTo (Point.mk (ox - halfW) sy)
        |>.lineTo (Point.mk (ox + halfW) sy)
      strokePath path
    y := y + config.majorSpacing

  -- Draw axes
  setStrokeColor config.axisColor
  setLineWidth 2.0

  -- X axis
  let xAxisPath := Afferent.Path.empty
    |>.moveTo (Point.mk (ox - halfW) oy)
    |>.lineTo (Point.mk (ox + halfW) oy)
  strokePath xAxisPath

  -- Y axis
  let yAxisPath := Afferent.Path.empty
    |>.moveTo (Point.mk ox (oy - halfH))
    |>.lineTo (Point.mk ox (oy + halfH))
  strokePath yAxisPath

  -- Draw axis labels
  setFillColor config.labelColor

  -- X axis labels
  x := Float.floor (minX / config.majorSpacing) * config.majorSpacing
  while x <= maxX do
    if x != 0 then
      let sx := ox + x * config.scale
      let label := if x == Float.floor x then s!"{x.toInt32}" else s!"{x}"
      let (tw, _) ← font.measureText label
      fillTextXY label (sx - tw / 2) (oy + 18) font
    x := x + config.majorSpacing

  -- Y axis labels
  y := Float.floor (minY / config.majorSpacing) * config.majorSpacing
  while y <= maxY do
    if y != 0 then
      let sy := oy - y * config.scale
      let label := if y == Float.floor y then s!"{y.toInt32}" else s!"{y}"
      let (tw, _) ← font.measureText label
      fillTextXY label (ox - tw - 8) (sy + 4) font
    y := y + config.majorSpacing

/-- Draw a circle marker at a world position -/
def drawMarker (pos : Vec2) (screenOrigin : Float × Float) (scale : Float)
    (color : Color) (radius : Float := 6.0) : CanvasM Unit := do
  let (sx, sy) := worldToScreen pos screenOrigin scale
  setFillColor color
  fillPath (Afferent.Path.circle (Point.mk sx sy) radius)

/-- Draw a dashed line between two screen points -/
def drawDashedLine (start finish : Float × Float) (color : Color)
    (dashLength : Float := 8.0) (gapLength : Float := 4.0)
    (lineWidth : Float := 1.5) : CanvasM Unit := do
  setStrokeColor color
  setLineWidth lineWidth
  let (x1, y1) := start
  let (x2, y2) := finish
  let dx := x2 - x1
  let dy := y2 - y1
  let len := Float.sqrt (dx * dx + dy * dy)
  if len < 1.0 then return
  let ux := dx / len
  let uy := dy / len
  let mut d : Float := 0
  while d < len do
    let startD := d
    let endD := Float.min (d + dashLength) len
    let sx := x1 + ux * startD
    let sy := y1 + uy * startD
    let ex := x1 + ux * endD
    let ey := y1 + uy * endD
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk sx sy)
      |>.lineTo (Point.mk ex ey)
    strokePath path
    d := d + dashLength + gapLength

/-- Draw a right angle marker at a point -/
def drawRightAngleMarker (corner : Float × Float) (dir1 dir2 : Float × Float)
    (color : Color) (size : Float := 10.0) : CanvasM Unit := do
  let (cx, cy) := corner
  let (d1x, d1y) := dir1
  let (d2x, d2y) := dir2
  -- Normalize directions
  let len1 := Float.sqrt (d1x * d1x + d1y * d1y)
  let len2 := Float.sqrt (d2x * d2x + d2y * d2y)
  if len1 < 0.001 || len2 < 0.001 then return
  let u1x := d1x / len1
  let u1y := d1y / len1
  let u2x := d2x / len2
  let u2y := d2y / len2
  -- Draw right angle symbol
  let p1x := cx + u1x * size
  let p1y := cy + u1y * size
  let p2x := cx + u1x * size + u2x * size
  let p2y := cy + u1y * size + u2y * size
  let p3x := cx + u2x * size
  let p3y := cy + u2y * size
  setStrokeColor color
  setLineWidth 1.5
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk p1x p1y)
    |>.lineTo (Point.mk p2x p2y)
    |>.lineTo (Point.mk p3x p3y)
  strokePath path

/-- Format a float for display (2 decimal places) -/
def formatFloat (f : Float) : String :=
  let scaled := Float.floor (f * 100 + 0.5) / 100
  s!"{scaled}"

/-- Format a Vec2 for display -/
def formatVec2 (v : Vec2) : String :=
  s!"({formatFloat v.x}, {formatFloat v.y})"

/-- Format a Vec3 for display -/
def formatVec3 (v : Vec3) : String :=
  s!"({formatFloat v.x}, {formatFloat v.y}, {formatFloat v.z})"

end Demos.Linalg
