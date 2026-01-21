/-
  Demo Runner - Main orchestration for all demos
-/
import Demos.Core.Runner.Unified

set_option maxRecDepth 1024

namespace Demos

/-- Main entry point - runs all demos -/
def main : IO Unit := do
  IO.println "Afferent - 2D Vector Graphics Library"
  IO.println "======================================"
  IO.println ""

  -- Run unified visual demo (single window with all demos)
  unifiedDemo

  IO.println ""
  IO.println "Done!"

end Demos
