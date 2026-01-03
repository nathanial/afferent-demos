import Lake
open Lake DSL

package afferentDemos where
  version := v!"0.1.0"

require afferent from git "https://github.com/nathanial/afferent" @ "v0.0.3"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.1"

-- Link arguments for Metal/macOS (inherited from Afferent)
def commonLinkArgs : Array String := #[
  "-framework", "Metal",
  "-framework", "Cocoa",
  "-framework", "QuartzCore",
  "-framework", "Foundation",
  "-framework", "Security",
  "-framework", "SystemConfiguration",
  "-lobjc",
  "-L/opt/homebrew/lib",
  "-L/usr/local/lib",
  "-lfreetype",
  "-lassimp",
  "-lz",
  "-lc++"
]

@[default_target]
lean_lib AfferentDemos where
  roots := #[`AfferentDemos]

lean_lib Demos where
  roots := #[`Demos]

lean_lib AfferentDemosTests where
  roots := #[`AfferentDemosTests]
  globs := #[.submodules `AfferentDemosTests]

lean_exe afferent_demos where
  root := `AfferentDemos.Main
  moreLinkArgs := commonLinkArgs

@[test_driver]
lean_exe afferent_demos_tests where
  root := `AfferentDemosTests.Main
  moreLinkArgs := commonLinkArgs
