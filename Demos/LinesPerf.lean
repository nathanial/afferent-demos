/-
  Lines Performance Test - 100k GPU-extruded line segments
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Line grid dimensions for the perf demo. -/
def linePerfCols : Nat := 400
/-- Line grid dimensions for the perf demo. -/
def linePerfRows : Nat := 250

/-- Total line count for the perf demo. -/
def linePerfCount : Nat := linePerfCols * linePerfRows

/-- Build packed stroke segments for a 100k-line grid. -/
def buildLineSegments (screenWidth screenHeight : Float) : Array Float × Nat := Id.run do
  let cols := linePerfCols
  let rows := linePerfRows
  let total := linePerfCount

  let margin : Float := 20.0
  let safeWidth := max 1.0 (screenWidth - margin * 2.0)
  let safeHeight := max 1.0 (screenHeight - margin * 2.0)
  let spacingX := if cols > 1 then safeWidth / (cols.toFloat - 1.0) else 0.0
  let spacingY := if rows > 1 then safeHeight / (rows.toFloat - 1.0) else 0.0
  let dx := spacingX * 0.8
  let dy := spacingY * 0.8

  let mut segments : Array Float := Array.mkEmpty (total * Afferent.Tessellation.strokeSegmentStride)

  for r in [:rows] do
    for c in [:cols] do
      let x0 := margin + c.toFloat * spacingX
      let y0 := margin + r.toFloat * spacingY
      let horizontal := (r + c) % 2 == 0
      let x1 := x0 + (if horizontal then dx else 0.0)
      let y1 := y0 + (if horizontal then 0.0 else dy)
      let dirX := x1 - x0
      let dirY := y1 - y0
      let len := Float.sqrt (dirX * dirX + dirY * dirY)
      let inv := if len < 0.0001 then 0.0 else 1.0 / len
      let dirX := dirX * inv
      let dirY := dirY * inv

      -- p0, p1
      segments := segments
        |>.push x0 |>.push y0
        |>.push x1 |>.push y1
        -- c1, c2 (unused for line segments)
        |>.push x0 |>.push y0
        |>.push x1 |>.push y1
        -- prevDir, nextDir
        |>.push dirX |>.push dirY
        |>.push dirX |>.push dirY
        -- startDist, length
        |>.push 0.0 |>.push len
        -- hasPrev, hasNext
        |>.push 0.0 |>.push 0.0
        -- kind, padding
        |>.push 0.0 |>.push 0.0

  return (segments, total)

/-- Render 100k line segments using GPU stroke extrusion (single draw call). -/
def renderLinesPerfM (t : Float) (buffer : FFI.Buffer) (lineCount : Nat) (lineWidth : Float)
    (font : Font) (screenWidth screenHeight : Float) : CanvasM Unit := do
  setFillColor Color.white
  fillTextXY s!"Lines: {lineCount} GPU stroke segments (Space to advance)" 20 30 font

  let renderer ← getRenderer
  let centerX := screenWidth / 2.0
  let centerY := screenHeight / 2.0
  let rotation := t * 0.35
  let tform :=
    Transform.concat
      (Transform.concat (Transform.translate (-centerX) (-centerY)) (Transform.rotate rotation))
      (Transform.translate centerX centerY)
  renderer.drawStrokePath
    buffer
    lineCount.toUInt32
    1
    (lineWidth / 2.0)
    screenWidth screenHeight
    10.0
    0 0
    tform.a tform.b tform.c tform.d tform.tx tform.ty
    #[]
    0
    0.0
    0.85 0.9 1.0 1.0

def renderLinesPerfFrame (c : Canvas) (t : Float) (buffer : FFI.Buffer) (lineCount : Nat)
    (lineWidth : Float) (font : Font) (screenWidth screenHeight : Float) : IO Canvas := do
  run' c do
    resetTransform
    renderLinesPerfM t buffer lineCount lineWidth font screenWidth screenHeight

end Demos
