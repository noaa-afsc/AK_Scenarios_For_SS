\name{Do_AK_TIER_3_Scenarios}
\alias{Do_AK_TIER_3_Scenarios}
\title{Run Alaska Tier 3 Stock Synthesis Projection Scenarios}
\usage{
Do_AK_TIER_3_Scenarios(
  DIR = "Model_24.1.25/PROJ", CYR = 2025, SYR = 1977,
  SEXES = 1, FLEETS = 1, Scenario2 = 1, S2_F = 0.4,
  s4_F = 0.75, Fyr_ts = 2, do_fig = TRUE, init_dir = NULL,
  exe = NULL, cores = 8L, catch_csv = "Scenario2_catch.csv",
  skip_run = FALSE, no_hessian_late = TRUE
)
}
\arguments{
  \item{DIR}{Directory containing SS3 model input files and a forecast specification.}
  \item{CYR}{Current assessment year.}
  \item{SYR}{First year of the assessment.}
  \item{SEXES}{Number of sexes; original single-sex SSB halving is retained.}
  \item{FLEETS}{Integer fleet IDs.}
  \item{Scenario2}{Alternative scenario: 1 default, 2 F multiplier, or 3 CSV catches.}
  \item{S2_F}{Fishing mortality multiplier for Scenario 2 option 2.}
  \item{s4_F}{Fishing mortality scaling fraction for scenario 4.}
  \item{Fyr_ts}{Scenario (1 through 7) supplying projection catches and the 9--11 catch source.}
  \item{do_fig}{Create plots for scenarios 1--7 only.}
  \item{init_dir}{Unused; retained for backwards-compatible calls.}
  \item{exe}{SS3 executable filename/path; autodetected when NULL.}
  \item{cores}{Maximum parallel workers.}
  \item{catch_csv}{Path to specified scenario 2 catches CSV.}
  \item{skip_run}{Reuse outputs instead of executing SS3; forecast inputs may still be rewritten.}
  \item{no_hessian_late}{Skip Hessian estimates for scenarios 9--15 when running SS3.}
}
\description{Runs Stock Synthesis 3 projections for 15 Alaska Tier 3 harvest scenarios and returns catch, F, and spawning biomass summaries.}
\details{
Scenarios 2--8 and 9--11 can run in parallel. Scenarios 9--11 independently use the catch forecasts selected by \code{Fyr_ts}. Scenarios 12--15 are chained for five-year projections. When \code{Scenario2 == 3} and \code{Fyr_ts == 2}, only supplied rows from \code{Scenario2_catch.csv} are written for the applicable horizon; unspecified years and fleets are not filled. Figures are generated only for scenarios 1--7. The function overwrites scenario directories in \code{DIR}. Numerical equivalence to the original model has not been verified.
}
\value{A list containing \code{SSB}, \code{CATCH}, \code{Five_year}, \code{Tables}, and optionally \code{FIGS}; plus a \code{scenario_map} attribute.}
\examples{
\dontrun{
result <- Do_AK_TIER_3_Scenarios(
  DIR = "Model_26_1_2/PROJ", CYR = 2026, SYR = 1977,
  SEXES = 1, FLEETS = 1:2, Scenario2 = 3, Fyr_ts = 2)
result$Five_year
}
}
\author{Steve Barbeaux}
