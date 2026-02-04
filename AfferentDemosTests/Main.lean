/-
  Afferent Demos Tests
-/
import Crucible
import Demos.Core.Runner
import AfferentDemosTests.Smoke
import AfferentDemosTests.StrokeCards
import AfferentDemosTests.RegistryTests
import AfferentDemosTests.WidgetPerfBench
import Wisp

def main : IO UInt32 := do
  Wisp.FFI.globalInit
  try
    runAllSuites
  finally
    Wisp.FFI.globalCleanup
    Wisp.HTTP.Client.shutdown
