# Tests for ears_t_reweighted() (R/alert.R:112, roadmap item T1-1).
#
# This is the statistical core of the signal detection: for each time point of a daily count
# series it recomputes a baseline out of the `no_historic` previous days and decides whether
# today is above the upper limit of the prediction interval. It is a pure function -- no I/O, no
# configuration, no randomness -- so everything below is exact arithmetic.
#
# It returns SEVEN columns, and production reads only three of them
# (`get_alerts_topic_counts()`, R/alert.R:439):
#
#   counts$alert    <- alerts$alarm0_dw    # the REWEIGHTED alarm, not alarm0
#   counts$limit    <- alerts$U0_dw
#   counts$baseline <- alerts$y0bar        # the UN-reweighted baseline
#
# So `alarm0` / `U0` are the plain EARS reference and are never shown to a user; `*_dw` is what
# actually raises an alert. The tests below therefore treat the gap between the two as the point
# of the function rather than as an implementation detail: downweighting a past spike is the whole
# reason this extension of EARS exists, and nothing checked it until now.
#
# Vocabulary, from the roxygen block:
#   y0     the monitored value                    U0     upper limit, no reweighting
#   y0bar  the baseline (mean of the history)     U0_dw  upper limit, Farrington downweighting
#   alarm0 y0 > U0                                alarm0_dw  y0 > U0_dw

# the series of the roxygen example: 7 quiet days, then a plateau three times higher
example_ts <- c(150, 130, 122, 160, 155, 128, 144, 125, 300, 319, 289, 277, 500)

# the upper limit of the prediction interval, recomputed from base R only -- the oracle the
# implementation is checked against, rather than a value copied out of a previous run
plain_limit <- function(historic, alpha = 0.025) {
  n <- length(historic)
  mean(historic) +
    stats::qt(1 - alpha, df = n - 1) * stats::sd(historic) * sqrt(1 + 1 / n)
}

# ---- shape of the result -----------------------------------------------------

test_that("the result is one row per time point, with the seven documented columns", {
  res <- ears_t_reweighted(example_ts)

  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), length(example_ts))
  expect_identical(
    names(res),
    c("time_monitored", "y0", "U0", "alarm0", "y0bar", "U0_dw", "alarm0_dw")
  )
  # the monitored point is carried through untouched in both branches, so the caller can bind the
  # result to its own data frame positionally -- which is exactly what R/alert.R:439 does
  expect_identical(res$time_monitored, as.numeric(seq_along(example_ts)))
  expect_identical(res$y0, as.numeric(example_ts))
})

test_that("KNOWN QUIRK: the alarms come back as numeric 0/1, not logical", {
  # `c(time_monitored = t, ..., alarm0 = TRUE, ...)` builds an atomic vector, so the logical is
  # coerced to a double. Harmless downstream -- R/alert.R:694 filters on `alert == 1` -- but the
  # empty branch two lines above the assignment (`counts$alert <- logical()`, R/alert.R:446) hands
  # back a LOGICAL column for an empty series, so the type of `counts$alert` depends on whether
  # any count was collected.
  res <- ears_t_reweighted(example_ts)

  expect_type(res$alarm0, "double")
  expect_type(res$alarm0_dw, "double")
  expect_identical(res$alarm0[[13]], 1)
  expect_identical(res$alarm0[[12]], 0)
})

# ---- the warm-up rows --------------------------------------------------------

test_that("the first no_historic points have no baseline to compare against", {
  # `t < no_historic * step + 1` -> everything but the monitored value is NA. With the defaults
  # that is the first 7 rows, and the 8th is the first one actually monitored.
  res <- ears_t_reweighted(example_ts)
  computed <- c("U0", "alarm0", "y0bar", "U0_dw", "alarm0_dw")

  expect_true(all(is.na(res[1:7, computed])))
  expect_false(any(is.na(res[8:13, computed])))
})

test_that("no_historic moves the first monitored point", {
  # a shorter baseline starts monitoring sooner -- and is noisier, cf. the df of the t quantile
  expect_true(all(is.na(ears_t_reweighted(example_ts, no_historic = 3)$U0[
    1:3
  ])))
  expect_false(is.na(ears_t_reweighted(example_ts, no_historic = 3)$U0[[4]]))

  expect_true(all(is.na(ears_t_reweighted(example_ts, no_historic = 10)$U0[
    1:10
  ])))
  expect_false(is.na(ears_t_reweighted(example_ts, no_historic = 10)$U0[[11]]))
})

test_that("a series shorter than the baseline is all NA rather than an error", {
  # a brand new topic: the collection has a few days of counts and no alert can be computed yet
  res <- ears_t_reweighted(c(1, 2, 3))

  expect_identical(nrow(res), 3L)
  expect_true(all(is.na(res$U0)))
  expect_true(all(is.na(res$alarm0_dw)))
  # exactly no_historic points is still one short: the 8th point is the first one monitored
  expect_true(all(is.na(ears_t_reweighted(1:7)$U0)))
})

# ---- the plain EARS baseline and threshold -----------------------------------

test_that("y0bar is the plain mean of the no_historic previous values", {
  res <- ears_t_reweighted(example_ts)

  # the history of t = 8 is t = 1..7, and the monitored value itself is NOT part of it
  expect_equal(res$y0bar[[8]], mean(example_ts[1:7]))
  expect_equal(res$y0bar[[13]], mean(example_ts[6:12]))
  # this is the column shown as the baseline on the charts, and it is the UN-reweighted one:
  # the line drawn under a plateau follows the plateau, even where the limit above it does not
  expect_equal(res$y0bar[[12]], mean(example_ts[5:11]))
})

test_that("U0 is the upper limit of the t prediction interval on the history", {
  res <- ears_t_reweighted(example_ts)

  expect_equal(res$U0[[8]], plain_limit(example_ts[1:7]))
  expect_equal(res$U0[[13]], plain_limit(example_ts[6:12]))
})

test_that("alpha lowers the threshold, so a bigger alpha means more alarms", {
  # alpha is the false positive rate: U0 is the (1 - alpha) quantile, so it DECREASES as alpha
  # grows. It is divided by the Bonferroni factor before reaching this function (R/alert.R:433),
  # which is the same knob read the other way round: more regions monitored, higher threshold.
  limits <- vapply(
    c(0.001, 0.01, 0.025, 0.1, 0.4),
    function(a) ears_t_reweighted(example_ts, alpha = a)$U0[[13]],
    numeric(1)
  )

  expect_identical(order(limits, decreasing = TRUE), seq_along(limits))
  expect_equal(limits[[3]], plain_limit(example_ts[6:12], alpha = 0.025))
})

test_that("alarm0 is a strict comparison of the monitored value to its limit", {
  res <- ears_t_reweighted(example_ts)

  expect_identical(as.logical(res$alarm0[8:13]), res$y0[8:13] > res$U0[8:13])
  # sitting exactly on the limit is not an alarm
  flat <- ears_t_reweighted(c(10, 11, 9, 10, 12, 10, 11, 20))
  expect_identical(flat$alarm0[[8]], as.numeric(flat$y0[[8]] > flat$U0[[8]]))
})

test_that("a stable series with a single spike alarms on the spike only", {
  ts <- rep(c(10, 11, 9, 10, 12, 10, 11), 4)
  ts[[20]] <- 200
  res <- ears_t_reweighted(ts)

  expect_identical(which(res$alarm0 == 1), 20L)
  expect_identical(which(res$alarm0_dw == 1), 20L)
  # and the day AFTER the spike does not alarm on the spike being in its own history: the
  # threshold is raised by it, not lowered
  expect_identical(res$alarm0[[21]], 0)
})

# ---- the Farrington reweighting, i.e. the reason this function exists ---------

test_that("a past spike in the history raises the plain limit but not the reweighted one", {
  # THE case the extension is built for. Same monitored value, same six quiet days; the only
  # difference is a single outbreak day sitting in the baseline window.
  quiet <- c(100, 110, 95, 105, 98, 102)
  today <- 130

  clean <- ears_t_reweighted(c(quiet, 101, today))
  spiked <- ears_t_reweighted(c(quiet, 400, today))

  # plain EARS: the past outbreak inflates both the mean and the standard deviation, the limit
  # jumps far above today's count and the signal is LOST
  expect_gt(spiked$U0[[8]], clean$U0[[8]])
  expect_identical(clean$alarm0[[8]], 1)
  expect_identical(spiked$alarm0[[8]], 0)

  # reweighted: the outlier is downweighted out of the baseline and the signal comes back
  expect_lt(spiked$U0_dw[[8]], spiked$U0[[8]])
  expect_identical(spiked$alarm0_dw[[8]], 1)
  # and it lands close to the limit computed from a history that never held the outbreak
  expect_equal(spiked$U0_dw[[8]], clean$U0_dw[[8]], tolerance = 0.05)
})

test_that("the same thing happens on the roxygen example, at t = 10", {
  # the documented series is a plateau: at t = 10 the 300 of t = 9 is already in the history.
  # Plain EARS misses the day, the reweighting catches it -- the single row of that data frame
  # where the two halves of the result disagree.
  res <- ears_t_reweighted(example_ts)

  expect_identical(which(res$alarm0 != res$alarm0_dw), 10L)
  expect_identical(res$alarm0[[10]], 0)
  expect_identical(res$alarm0_dw[[10]], 1)
  expect_lt(res$U0_dw[[10]], res$U0[[10]])
})

test_that("a history with no flagged outlier is left exactly alone", {
  # all weights are 1, so the two thresholds are not merely close but identical
  res <- ears_t_reweighted(example_ts)

  expect_identical(res$U0[[8]], res$U0_dw[[8]])
  expect_identical(res$alarm0[[8]], res$alarm0_dw[[8]])
})

test_that("only HIGH outliers are downweighted", {
  # the weights come from `rstar > reweight_threshold`, on the signed residual and with no abs():
  # a day far BELOW the baseline is kept at weight 1. Deliberate for an outbreak detector -- but
  # it means a collection outage (a day of zeros in the middle of a normal week) inflates the
  # standard deviation, raises both limits and masks the following days, with nothing to undo it.
  quiet <- c(100, 110, 95, 105, 98, 102)

  low <- ears_t_reweighted(c(quiet, 10, 130))
  high <- ears_t_reweighted(c(quiet, 400, 130))

  expect_identical(low$U0[[8]], low$U0_dw[[8]])
  expect_lt(high$U0_dw[[8]], high$U0[[8]])
})

test_that("k_decay = 0 disables the reweighting entirely", {
  # the weight is (threshold / rstar)^k_decay, so k_decay = 0 makes every weight 1 whatever the
  # residual: the reweighted half of the result collapses onto the plain one, for the whole series
  res <- ears_t_reweighted(example_ts, k_decay = 0)

  expect_identical(res$U0, res$U0_dw)
  expect_identical(res$alarm0, res$alarm0_dw)
})

test_that("a bigger k_decay downweights harder, so U0_dw decreases", {
  ts <- c(100, 110, 95, 105, 98, 102, 400, 130)
  limits <- vapply(
    c(0, 1, 2, 4, 8),
    function(k) ears_t_reweighted(ts, k_decay = k)$U0_dw[[8]],
    numeric(1)
  )

  expect_identical(order(limits, decreasing = TRUE), seq_along(limits))
  # it saturates: past a point the outlier is already worth nothing and more k changes nothing
  expect_equal(limits[[4]], limits[[5]], tolerance = 1e-3)
})

test_that("a bigger alpha_outlier flags more points as outliers, so U0_dw decreases", {
  # alpha_outlier sets the studentised residual above which a day is treated as an outlier: the
  # larger it is, the lower that bar, and the more of the history is discounted
  ts <- c(100, 110, 95, 105, 98, 102, 400, 130)
  limits <- vapply(
    c(0.001, 0.01, 0.05, 0.2, 0.4),
    function(a) ears_t_reweighted(ts, alpha_outlier = a)$U0_dw[[8]],
    numeric(1)
  )

  expect_identical(order(limits, decreasing = TRUE), seq_along(limits))
})

# ---- same_weekday_baseline ---------------------------------------------------

test_that("same_weekday_baseline = TRUE pushes the first monitored point to t = 50", {
  # the step becomes 7, so the condition is `t < 7 * 7 + 1`: SEVEN WEEKS of history are needed
  # before the first alert. A topic collected for a month and a half raises nothing at all.
  ts <- seq_len(60) * 1.0
  res <- ears_t_reweighted(ts, same_weekday_baseline = TRUE)

  expect_identical(sum(is.na(res$U0)), 49L)
  expect_true(all(is.na(res$U0[1:49])))
  expect_false(is.na(res$U0[[50]]))
})

test_that("same_weekday_baseline = TRUE builds the baseline out of the same weekday only", {
  # a series with a hard weekly shape: one day of the week is quiet, the six others are ten times
  # higher. Comparing a quiet day against the whole preceding week is what the flag exists to
  # avoid -- the weekend of a work-related topic.
  ts <- rep(1000, 50)
  same_weekday <- c(10, 12, 11, 9, 13, 10, 11)
  ts[seq(1, 43, by = 7)] <- same_weekday
  ts[[50]] <- 40

  weekly <- ears_t_reweighted(ts, same_weekday_baseline = TRUE)
  daily <- ears_t_reweighted(ts)

  # the history of t = 50 is t = 1, 8, 15, ..., 43 -- the same weekday, seven weeks back
  expect_equal(weekly$y0bar[[50]], mean(same_weekday))
  expect_equal(weekly$U0[[50]], plain_limit(same_weekday))
  # 40 posts on a day that normally sees 11 is an alarm against its own weekday...
  expect_identical(weekly$alarm0[[50]], 1)
  # ...and invisible against the preceding seven days, six of which are the busy ones
  expect_equal(daily$y0bar[[50]], mean(ts[43:49]))
  expect_identical(daily$alarm0[[50]], 0)
})

# ---- numeric lock ------------------------------------------------------------

test_that("the roxygen example series keeps its exact values", {
  # a change of formula anywhere -- the t quantile, the weight normalisation, the residual
  # denominator -- moves these numbers. The tests above pin the properties; this pins the values.
  expect_snapshot_value(
    round(as.matrix(ears_t_reweighted(example_ts)), 6),
    style = "json2"
  )
})

# ---- bugs and quirks frozen here ---------------------------------------------

test_that("KNOWN BUG: a perfectly flat baseline produces no alert at all", {
  # `sigmai` is the standard deviation of the history with one point removed, and `rstar` is the
  # residual divided by it. On a flat history both are 0, so rstar is 0/0 = NaN, `ifelse(NaN >
  # threshold, ...)` is NA, and every weight -- hence y0bar_dw, U0_dw and alarm0_dw -- is NA.
  #
  # Production reads alarm0_dw (R/alert.R:439) and R/alert.R:694 filters on
  # `!is.na(alert) & alert == 1`, so the NA is dropped silently: the topic raises NOTHING.
  #
  # This is not an exotic input. A low-volume topic sitting at zero for a week has a flat
  # baseline, and the day it is finally spoken about is exactly the day an alert is wanted. Note
  # the plain half of the result gets it right (U0 = 0, alarm0 = 1) -- it is only the reweighted
  # half, the one actually used, that goes NA.
  quiet <- ears_t_reweighted(c(rep(0, 7), 5))

  expect_identical(quiet$U0[[8]], 0)
  expect_identical(quiet$alarm0[[8]], 1)
  expect_true(is.na(quiet$U0_dw[[8]]))
  expect_true(is.na(quiet$alarm0_dw[[8]]))
  # expect_identical(quiet$alarm0_dw[[8]], 1) # <- the corrected assertion

  # any constant history does it, not just zeros
  constant <- ears_t_reweighted(c(rep(10, 7), 100))
  expect_true(is.na(constant$alarm0_dw[[8]]))
  # expect_identical(constant$alarm0_dw[[8]], 1) # <- the corrected assertion

  # one single day differing anywhere in the window is enough to make it work again
  almost <- ears_t_reweighted(c(10, 10, 10, 10, 10, 10, 11, 100))
  expect_identical(almost$alarm0_dw[[8]], 1)
})

test_that("KNOWN BUG: an empty series raises a subscript error", {
  # `1:length(ts)` again -- the pattern of B-4 and B-8. On an empty series 1:0 is c(1, 0), so the
  # body runs on t = 1 and `ts[[1]]` is out of bounds. `seq_along()` would return an empty result.
  #
  # Not reachable through get_alerts_topic_counts(), which guards on `nrow(counts) > 0`
  # (R/alert.R:418), but the function is exported and documented as taking "a numeric vector".
  expect_error(ears_t_reweighted(numeric(0)), "subscript out of bounds")
  # expect_identical(nrow(ears_t_reweighted(numeric(0))), 0L) # <- the corrected assertion
})

test_that("KNOWN QUIRK: a single NA count blanks the next no_historic rows", {
  # no na.rm anywhere, so a missing count propagates into every window that contains it -- seven
  # rows here -- and the series recovers on its own afterwards. Counts are zero-filled before
  # reaching this function (R/alert.R:425), so this should not happen in the pipeline; it is
  # worth knowing that it degrades locally rather than poisoning the whole series.
  ts <- rep(c(10, 11, 9, 10, 12, 10, 11), 3)
  ts[[5]] <- NA
  res <- ears_t_reweighted(ts)

  expect_identical(which(is.na(res$U0)), 1:12)
  expect_false(any(is.na(res$U0[13:21])))
})
