import Lake
open Lake DSL

package afferentDemos where
  version := v!"0.1.0"

require afferent from git "https://github.com/nathanial/afferent" @ "v0.0.16"
require «afferent-math» from git "https://github.com/nathanial/afferent-math" @ "v0.0.1"
require «afferent-chat» from git "https://github.com/nathanial/afferent-chat" @ "v0.0.1"
require afferent_charts from git "https://github.com/nathanial/afferent-charts" @ "v0.0.1"
require «afferent-worldmap» from git "https://github.com/nathanial/afferent-worldmap" @ "v0.0.1"
require linalg from git "https://github.com/nathanial/linalg" @ "v0.0.5"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.10"
require wisp from git "https://github.com/nathanial/wisp" @ "v0.0.2"
require cellar from git "https://github.com/nathanial/cellar" @ "v0.0.2"
require reactive from git "https://github.com/nathanial/reactive" @ "v0.2.2"
require tileset from git "https://github.com/nathanial/tileset" @ "v0.0.2"

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
  "-lc++",
  "-lcurl"
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
