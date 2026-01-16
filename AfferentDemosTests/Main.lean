/-
  Afferent Demos Tests
-/
import Crucible
import Demos.Runner
import AfferentDemosTests.Smoke
import AfferentDemosTests.StrokeCards
import AfferentDemosTests.RegistryTests
import AfferentDemosTests.WidgetPerfBench

def main : IO UInt32 := do
  runAllSuites
