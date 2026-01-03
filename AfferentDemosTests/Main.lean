/-
  Afferent Demos Tests
-/
import Crucible
import Demos.Runner

namespace AfferentDemosTests

open Crucible

testSuite "AfferentDemos"

test "smoke" := do
  1 ≡ 1

#generate_tests

end AfferentDemosTests

def main : IO UInt32 := do
  runAllSuites
