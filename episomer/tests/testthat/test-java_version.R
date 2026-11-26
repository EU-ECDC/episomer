# Tests for get_java_version() and check_java_version() (R/check.R:60 and :78, roadmap item T1-7).
#
# Both are pure string handling once get_java_version_vector() is mocked -- that is the only
# function here that shells out to the `java` binary, so nothing below needs a JVM installed.
#
# What they gate: setup_can_run() (R/check.R:562) chains
#
#   check_64() && check_java_64() && check_java_present() && check_java_version()
#
# inside a tryCatch that turns ANY warning into FALSE, and check_all() (R/check.R:593) lists
# check_java_version among the sanity checks shown to the user. So the return value decides whether
# episomer will start at all, and the warning text is the only explanation the user gets.
#
# The min/max thresholds move with every Java release, which is the reason to pin them: the tests
# on the real bounds below are meant to fail when someone bumps java_max_version(), so that the
# branch geometry gets re-read rather than silently shifted.

# ---- helpers -----------------------------------------------------------------

# the mock lives for the duration of the helper call, which is exactly as long as it is needed
java_version_of <- function(lines) {
  local_mocked_bindings(
    get_java_version_vector = function() lines,
    .package = "episomer"
  )
  get_java_version()
}

# against the thresholds actually shipped
check_installed_version <- function(java_version) {
  local_mocked_bindings(
    get_java_version = function() java_version,
    .package = "episomer"
  )
  check_java_version()
}

# against arbitrary thresholds, to exercise the branch geometry independently of the release cycle
check_version_against <- function(java_version, min, max, tested) {
  local_mocked_bindings(
    get_java_version = function() java_version,
    java_min_version = function() min,
    java_max_version = function() max,
    java_tested_version = function() tested,
    .package = "episomer"
  )
  check_java_version()
}

# the warning message of a call that is expected to warn
warning_of <- function(expr) {
  tryCatch(expr, warning = function(w) conditionMessage(w))
}

# check_java_64() reads the same output; it takes no argument either
java_64_of <- function(lines) {
  local_mocked_bindings(
    get_java_version_vector = function() lines,
    .package = "episomer"
  )
  check_java_64()
}

# ---- parsing the version, modern scheme --------------------------------------

test_that("a modern version string is read as its feature number", {
  # since Java 9 the first number IS the version, and the rest is the update
  expect_identical(
    java_version_of(c(
      'openjdk version "21.0.1" 2023-10-17',
      "OpenJDK Runtime Environment (build 21.0.1+12-29)",
      "OpenJDK 64-Bit Server VM (build 21.0.1+12-29, mixed mode, sharing)"
    )),
    21L
  )
  expect_identical(java_version_of('java version "17.0.7" 2023-04-18 LTS'), 17L)
  expect_identical(java_version_of('openjdk version "11.0.20" 2023-07-18'), 11L)
  # the current maximum, i.e. the string a supported installation actually prints
  expect_identical(java_version_of('java version "25.0.1" 2025-10-21'), 25L)
})

test_that("a version with no update part is read too", {
  # what Java 9 and 10 printed before the date was appended to the line
  expect_identical(java_version_of('java version "9"'), 9L)
  expect_identical(java_version_of('java version "10"'), 10L)
})

test_that("an early access build is read as its feature number", {
  # `22-ea`: every non-digit is turned into a dot before splitting, so the suffix simply vanishes
  expect_identical(java_version_of('openjdk version "22-ea" 2024-03-19'), 22L)
})

# ---- parsing the version, legacy 1.x scheme ----------------------------------

test_that("a legacy 1.x version is read as its SECOND number", {
  # `1.8.0_301` is Java 8: before Java 9 the feature number was the minor. The parser special
  # cases a leading 1 for exactly this.
  expect_identical(
    java_version_of(c(
      'openjdk version "1.8.0_301"',
      "OpenJDK Runtime Environment (build 1.8.0_301-b09)",
      "OpenJDK 64-Bit Server VM (build 25.301-b09, mixed mode)"
    )),
    8L
  )
  expect_identical(java_version_of('java version "1.7.0_80"'), 7L)
  # the underscore of the update number is not a separator in the version scheme, but it is turned
  # into one here -- harmless, the update number is dropped either way
  expect_identical(java_version_of('java version "1.8.0_401"'), 8L)
})

test_that("the leading 1 is only special when something follows it", {
  # the guard is `length(parts) > 1 && parts[[1]] == 1`, so a bare "1" stays 1 rather than
  # indexing past the end
  expect_identical(java_version_of('java version "1"'), 1L)
  # Java 1.9 was renamed 9 before release, but the scheme still parses
  expect_identical(java_version_of('java version "1.9.0"'), 9L)
})

test_that("the vendor's own digits do not disturb the parse", {
  # Zulu puts its build number in the runtime line, Temurin and GraalVM do the same. Only the
  # first line matching "version" is looked at, so those never come into play.
  expect_identical(
    java_version_of(c(
      'openjdk version "17.0.7" 2023-04-18 LTS',
      "OpenJDK Runtime Environment Zulu17.42+19-CA (build 17.0.7+7-LTS)",
      "OpenJDK 64-Bit Server VM Zulu17.42+19-CA (build 17.0.7+7-LTS, mixed mode)"
    )),
    17L
  )
})

# ---- no version to parse -----------------------------------------------------

test_that("output with no version line gives NULL", {
  # a JVM that fails to start prints errors and no version at all; NULL is the signal
  # check_java_version() turns into "cannot identify your Java version"
  expect_null(java_version_of(c(
    "Error: Could not create the Java Virtual Machine.",
    "Error: A fatal exception has occurred. Program will exit."
  )))
  expect_null(java_version_of("Unable to locate a Java Runtime."))
})

test_that("no output at all gives NULL", {
  # grep() on an empty vector is integer(0), so the guard holds
  expect_null(java_version_of(character(0)))
})

# ---- the version window ------------------------------------------------------

test_that("the supported Java window is 21 only", {
  # pinned on purpose: bumping a threshold should make this fail, so that the branches of
  # check_java_version() below get re-read against the new window (see B-11)
  expect_identical(java_min_version(), 21)
  expect_identical(java_max_version(), 21)
  expect_identical(java_tested_version(), 21)
})

test_that("a version inside the window passes without a warning", {
  # the only path through check_java_version() that returns TRUE, and the only one that lets
  expect_true(check_installed_version(21L))
  expect_true(check_installed_version(21L))
  # bounds are inclusive on both ends
  expect_silent(expect_true(check_installed_version(java_min_version())))
  expect_silent(expect_true(check_installed_version(java_max_version())))
})

test_that("a version above the window is refused, naming the window", {
  expect_warning(
    expect_false(check_installed_version(22L)),
    "Java version is 22"
  )
  expect_match(
    warning_of(check_installed_version(30L)),
    "needs Java 21\\."
  )
})

test_that("an unidentifiable version is refused with its own message", {
  # NULL from get_java_version(), i.e. java ran but printed no version line
  expect_warning(
    expect_false(check_installed_version(NULL)),
    "cannot identify your Java version"
  )
})

# ---- the branch geometry, independent of the shipped thresholds ---------------

test_that("the intermediate branch covers ]max, tested]", {
  # what the branch was written for: a release newer than the one episomer is built against, that
  # users have reported working anyway. Reachable only when tested > max.
  expect_match(
    warning_of(check_version_against(26L, min = 21, max = 25, tested = 27)),
    "also works"
  )
  expect_match(
    warning_of(check_version_against(27L, min = 21, max = 25, tested = 27)),
    "also works"
  )
  # one past `tested` falls through to the plain refusal
  expect_false(grepl(
    "also works",
    warning_of(check_version_against(28L, min = 21, max = 25, tested = 27)),
    fixed = TRUE
  ))
  # and it still returns FALSE: the message is softer, the verdict is not
  expect_warning(
    expect_false(check_version_against(26L, min = 21, max = 25, tested = 27)),
    "also works"
  )
})

test_that("the intermediate window is empty as the thresholds are shipped", {
  # java_tested_version() == java_max_version() == 21 today, so ]max, tested] is empty and the
  # branch cannot be reached from above -- 22 gets the plain refusal. Everything it does catch
  # today comes from BELOW the minimum, which is B-11.
  expect_identical(java_max_version(), java_tested_version())
  expect_false(grepl(
    "also works",
    warning_of(check_installed_version(java_max_version() + 1)),
    fixed = TRUE
  ))
})

# ---- bugs and quirks frozen here ---------------------------------------------

test_that("KNOWN BUG: a version below the minimum is told it probably works (B-11)", {
  # The intermediate branch is `java_version <= java_tested_version()` with NO lower bound. The
  # in-window case has already returned TRUE, so what is left is everything at or below `tested`
  # -- which includes every version BELOW the minimum.
  #
  # A user on Java 8 is therefore told "Some users have reported that your version also works",
  # about a release fourteen versions below the minimum. The verdict is still FALSE, so nothing
  # breaks; the user is simply sent looking in the wrong direction, and this is the message shown
  # in the check_all() table (R/check.R:593).
  #
  # Note it is not a consequence of tested == max: it happens with tested > max too, so raising
  # java_tested_version() would not fix it. The branch needs its lower bound.
  expect_match(warning_of(check_installed_version(8L)), "also works")
  expect_match(warning_of(check_installed_version(17L)), "also works")
  expect_match(warning_of(check_installed_version(20L)), "also works")
  # ...and with a tested version genuinely above the maximum, showing the missing lower bound
  expect_match(
    warning_of(check_version_against(11L, min = 21, max = 25, tested = 27)),
    "also works"
  )
  # the corrected assertion, once the branch reads
  # `java_version > java_max_version() && java_version <= java_tested_version()`:
  # expect_false(grepl("also works", warning_of(check_installed_version(8L)), fixed = TRUE))
})

test_that("KNOWN BUG: a matched line with no digits raises a subscript error (B-12)", {
  # gsub("[^0-9]", ".", line) leaves nothing but dots, the split gives a single empty string, the
  # digit filter empties the vector, and `j_version_parts[[1]]` is then out of bounds. The word
  # "version" is matched anywhere in the line and is not required to be followed by a number.
  #
  # The error escapes get_java_version() as an error, not a warning -- and setup_can_run() only
  # catches warnings (R/check.R:563), so this propagates out of setup_can_run() to the user.
  expect_error(
    java_version_of("Unrecognized option: -version"),
    "subscript out of bounds"
  )
  expect_error(java_version_of("version"), "subscript out of bounds")
  # the corrected behaviour would be NULL, as for a line that never matched at all:
  # expect_null(java_version_of("Unrecognized option: -version"))
})

test_that("KNOWN QUIRK: the first line holding the word 'version' wins (B-13)", {
  # The JVM echoes "Picked up JAVA_TOOL_OPTIONS: ..." on stderr BEFORE its own version output, and
  # get_java_version_vector() merges stderr into stdout (`2>&1`, R/check.R:32 and :39). Any such
  # line holding the substring "version" is matched first and parsed instead of the real one.
  #
  # Here a perfectly good Java 21 is reported as version 3, which check_java_version() then
  # refuses -- and the warning names a version the user does not have installed, so the message
  # sends them to reinstall a JDK that is already correct.
  expect_identical(
    java_version_of(c(
      "Picked up JAVA_TOOL_OPTIONS: -Dapp.version=3",
      'openjdk version "21.0.1" 2023-10-17'
    )),
    3L
  )
  # a JAVA_TOOL_OPTIONS line without the substring is harmless, which is why this is rare
  expect_identical(
    java_version_of(c(
      "Picked up JAVA_TOOL_OPTIONS: -Dfile.encoding=UTF-8",
      'openjdk version "21.0.1" 2023-10-17'
    )),
    21L
  )
  # the corrected behaviour would anchor on the version line, e.g. grep("^\\S* ?version ")
})

# ---- check_java_64(), the other reader of the same output ---------------------

test_that("check_java_64 looks for 64-Bit in the whole output", {
  # third consumer of get_java_version_vector(), and pure parsing for the same reason. It reads
  # the VM line, not the version line.
  expect_true(java_64_of(c(
    'openjdk version "21.0.1" 2023-10-17',
    "OpenJDK Runtime Environment (build 21.0.1+12-29)",
    "OpenJDK 64-Bit Server VM (build 21.0.1+12-29, mixed mode, sharing)"
  )))
  expect_warning(
    expect_false(java_64_of(c(
      'openjdk version "21.0.1" 2023-10-17',
      "OpenJDK Server VM (build 21.0.1+12-29, mixed mode)"
    ))),
    "does not appear to be 64-bit"
  )
  expect_warning(expect_false(java_64_of(character(0))))
})

test_that("KNOWN QUIRK: the 64-Bit test is case sensitive", {
  # `grepl("64-Bit", ...)` without ignore.case, while the warning it prints says "64-bit". Any JVM
  # spelling the VM line differently is reported as 32-bit and blocks setup_can_run() entirely.
  expect_warning(expect_false(java_64_of("OpenJDK 64-bit Server VM")))
  expect_true(java_64_of("OpenJDK 64-Bit Server VM"))
})
