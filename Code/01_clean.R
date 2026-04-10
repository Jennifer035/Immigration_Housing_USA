# =============================================================================
#  01_cleaning.R
#  Project:  Immigration and Wages in the USA — State-Level Analysis
#  Data:     Current Population Survey (CPS) IPUMS Extract
#  Course:   ECNS 560 | Montana State University
#  Author:   [Jennifer]
# =============================================================================
#
#  BEST PRACTICES (from lecture):
#    1. Record every cleaning step in a script — never edit raw data by hand.
#    2. Never overwrite the original raw data file.
#    3. Identify observations by logical conditions, not row numbers.
#    4. Look at your data every step of the way.
# =============================================================================


# -----------------------------------------------------------------------------
# 0.  PACKAGES
# -----------------------------------------------------------------------------

library(tidyverse)
library(skimr)        
library(dplyr)



# -----------------------------------------------------------------------------
# 1.  IMPORT RAW DATA
#     Checklist Step 1: Import the data.
# -----------------------------------------------------------------------------

# read_csv() from readr is preferred over base read.csv():
#   - Returns a tibble (safe printing, no silent string-to-factor conversion)
#   - Shows a column-type message on load (useful sanity check)
#   - Much faster on large files (986k rows × 21 columns here)
 
raw <- read_csv("cps_00004.csv", col_types = cols())

# Always start by eyeballing the structure before touching anything:
glimpse(raw)        # variable names, types, and first values
dim(raw)            # 986,061 rows × 21 columns
skim(raw)           # rich summary: missing %, histograms, quantiles


# -----------------------------------------------------------------------------
# 2.  UNDERSTAND THE VARIABLES
#     Checklist Step 6: Know the definition, origin, and units of each variable.
#
#   KEY VARIABLE CODEBOOK (source: https://cps.ipums.org):
#   ─────────────────────────────────────────────────────────────────────────
#   YEAR      Survey year (1970–2020 in this extract; decennial waves)
#   SERIAL    Household serial number
#   PERNUM    Person number within household
#   ASECFLAG  1 = ASEC supplement respondent; 2 = basic monthly CPS only.
#             EARNWEEK is asked ONLY of ASEC respondents.
#   ASECWT    Person-level ASEC survey weight — use for all statistics.
#   STATEFIP  State FIPS code: 1–56 = 50 states + DC; 70–80 = territories.
#   AGE       Age in years (0–99)
#   SEX       1 = Male; 2 = Female
#   NATIVITY  1=Native-born US; 2=Native born abroad (US parents);
#             3=Native born in US outlying area; 4=Foreign-born, citizen;
#             5=Foreign-born, non-citizen; 0=N/A (not collected pre-2000).
#             NOTE: Usable values (1–5) only exist for years 2000–2020.
#   EMPSTAT   Employment status codes (see Step 6b below).
#   EDUC      Education code (1–125 + 999=NIU; see crosswalk in Step 6d).
#   UHRSWORKT Usual weekly hours: 0–997 = actual; 999 = N/A; 997 = varies.
#   EARNWEEK  Usual weekly earnings ($): positive values are real;
#             9999.99 = top-code OR not applicable. Available: 2000, 2010, 2020.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 3.  SELECT RELEVANT VARIABLES
#     Checklist Step 3: Remove irrelevant / garbage columns.
# -----------------------------------------------------------------------------

cps <- raw |>
  select(
    YEAR, SERIAL, PERNUM,   # person identifiers
    ASECFLAG,               # supplement flag — needed to understand coverage
    ASECWT,                 # person survey weight (critical for weighted stats)
    STATEFIP,               # state FIPS code
    AGE, SEX,               # demographics
    NATIVITY,               # immigration status
    EMPSTAT,                # employment status
    EDUC,                   # educational attainment
    UHRSWORKT,              # usual weekly hours
    EARNWEEK                # usual weekly earnings
  )

names(cps)   # confirm selection


# -----------------------------------------------------------------------------
# 4.  RENAME VARIABLES
#     Checklist Step 7: Use succinct, descriptive snake_case names.
# -----------------------------------------------------------------------------

cps <- cps |>
  rename(
    year       = YEAR,
    serial     = SERIAL,
    pernum     = PERNUM,
    asec_flag  = ASECFLAG,
    weight     = ASECWT,
    state_fips = STATEFIP,
    age        = AGE,
    sex        = SEX,
    nativity   = NATIVITY,
    empstat    = EMPSTAT,
    educ       = EDUC,
    hrs_work   = UHRSWORKT,
    earn_week  = EARNWEEK
  )

glimpse(cps)


# -----------------------------------------------------------------------------
# 5.  IDENTIFY THE PRIMARY KEY
#     Checklist Step 4: Verify the primary key is unique.
#
#     Each row should uniquely represent one person in one survey year.
#     Primary key = (year, serial, pernum).
# -----------------------------------------------------------------------------

n_dupes <- cps |>
  count(year, serial, pernum) |>
  filter(n > 1) |>
  nrow()

cat("Duplicate key combinations:", n_dupes, "\n")
# Should print 0. 


# -----------------------------------------------------------------------------
# 6.  CONVERT VARIABLE FORMATS
#     Checklist Step 8: Binary = 0/1 integer; ordered factors for categories;
#                       ID variables stay as integer/character.
# -----------------------------------------------------------------------------

# --- 6a. IMMIGRATION STATUS binary ----------------------------------------
# NATIVITY:
#   0 → N/A (not collected before 2000 in this extract) → NA
#   1,2,3 → native-born (in US, abroad to US parents, or US territory) → 0
#   4,5   → foreign-born (citizen or non-citizen) → 1
#
# This native vs. foreign-born distinction is the standard binary used in the
# immigration economics literature  

cps <- cps |>
  mutate(
    immigrant = case_when(
      nativity %in% c(1, 2, 3) ~ 0L,         # native-born
      nativity %in% c(4, 5)    ~ 1L,         # foreign-born
      nativity == 0            ~ NA_integer_  # not collected (pre-2000)
    )
  )

# Verify counts match the raw NATIVITY distribution:
count(cps, nativity, immigrant)


# --- 6b. EMPLOYMENT STATUS binary -----------------------------------------
# EMPSTAT codes relevant here:
#   0  = N/A (children / suppressed)      → NA
#   10 = At work                           → 1 (employed)
#   12 = Has job, not at work last week    → 1 (employed)
#   All other codes (armed forces, unemployed, not in labor force) → 0

cps <- cps |>
  mutate(
    employed = case_when(
      empstat %in% c(10, 12) ~ 1L,
      empstat == 0           ~ NA_integer_,
      TRUE                   ~ 0L
    )
  )

count(cps, empstat, employed)   # verify mapping is correct


# --- 6c. SEX → female binary (0/1 coding) --
cps <- cps |>
  mutate(
    female = if_else(sex == 2, 1L, 0L)  # 1 = female, 0 = male
  )


# --- 6d. EDUCATION — collapse to 4 ordered categories ---------------------
# Raw EDUC codes are granular integers (1–125, 999=NIU/not applicable).
#  collapse to four tiers following standard labor economics practice
# (e.g., high school dropouts, HS diploma, some college, BA or more).


cps <- cps |>
  mutate(
    educ_cat = case_when(
      educ %in% c(1, 2, 10:14, 20:22, 30:32, 40, 50:60, 70) ~ "less_hs",
      educ %in% c(71, 72, 73)                                 ~ "hs",
      educ %in% c(80, 81, 90:92, 100)                        ~ "some_college",
      educ %in% c(110, 111, 120:125)                         ~ "college_plus",
      educ == 999                                             ~ NA_character_
    ),
    # Ordered factor: controls sort order in plots and ensures logical comparisons
    educ_cat = factor(educ_cat,
                      levels = c("less_hs", "hs", "some_college", "college_plus"),
                      ordered = TRUE)
  )

count(cps, educ_cat)   # distribution — does it look plausible?


# --- 6e. STATE FIPS → add human-readable state abbreviation and name ------
# Joining a crosswalk adds text labels so plots have proper axis text.
# FIPS codes > 56 are US territories and they are excluded from the analysis.

state_xwalk <- tribble(
  ~state_fips, ~state_abb, ~state_name,
   1,"AL","Alabama",        2,"AK","Alaska",        4,"AZ","Arizona",
   5,"AR","Arkansas",       6,"CA","California",     8,"CO","Colorado",
   9,"CT","Connecticut",   10,"DE","Delaware",      11,"DC","Dist. of Columbia",
  12,"FL","Florida",       13,"GA","Georgia",       15,"HI","Hawaii",
  16,"ID","Idaho",         17,"IL","Illinois",      18,"IN","Indiana",
  19,"IA","Iowa",          20,"KS","Kansas",        21,"KY","Kentucky",
  22,"LA","Louisiana",     23,"ME","Maine",         24,"MD","Maryland",
  25,"MA","Massachusetts", 26,"MI","Michigan",      27,"MN","Minnesota",
  28,"MS","Mississippi",   29,"MO","Missouri",      30,"MT","Montana",
  31,"NE","Nebraska",      32,"NV","Nevada",        33,"NH","New Hampshire",
  34,"NJ","New Jersey",    35,"NM","New Mexico",    36,"NY","New York",
  37,"NC","North Carolina",38,"ND","North Dakota",  39,"OH","Ohio",
  40,"OK","Oklahoma",      41,"OR","Oregon",        42,"PA","Pennsylvania",
  44,"RI","Rhode Island",  45,"SC","South Carolina",46,"SD","South Dakota",
  47,"TN","Tennessee",     48,"TX","Texas",         49,"UT","Utah",
  50,"VT","Vermont",       51,"VA","Virginia",      53,"WA","Washington",
  54,"WV","West Virginia", 55,"WI","Wisconsin",     56,"WY","Wyoming"
)

cps <- cps |>
  left_join(state_xwalk, by = "state_fips")

# Rows with no state match = territories — will be dropped in sample restriction
filter(cps, is.na(state_abb)) |> count(state_fips)


# -----------------------------------------------------------------------------
# 7.  HANDLE MISSING VALUES
#     Checklist Step 9: Convert sentinel values → NA; understand why values
#                       are missing; and not confuse missing with true zeros.
# -----------------------------------------------------------------------------

# --- 7a. EARNWEEK: 9999.99 is both top-code and "not applicable" ----------
# IPUMS uses 9999.99 for two distinct situations we cannot separate:
#   (i)  The person is NOT an ASEC supplement respondent (no wages asked).
#   (ii) The person IS an ASEC respondent but has very high earnings
#        (top-coded at $2,885/week in 1990 → $2,885/week in 2020; the
#        number changes; 9999.99 appears to be a blanket sentinel here).
# Because we cannot separate (i) from (ii), we convert all 9999.99 to NA
# and restrict to workers with positive earnings in Step 8.

summary(cps$earn_week)   # before: median = 9999.99 (most rows are N/A)

cps <- cps |>
  mutate(earn_week = if_else(earn_week >= 9999.99, NA_real_, earn_week))

summary(cps$earn_week)   # after: sensible range; only ~3% of rows have wages


# --- 7b. UHRSWORKT: 999 = not applicable; 997 = hours vary ---------------
# We set both to NA. 997 ("hours vary") is not actually missing, but we
# cannot use it to compute a meaningful hourly wage. Since we use weekly
# earnings (not hourly) as our primary outcome, this is a minor loss.

cps <- cps |>
  mutate(
    hrs_work = case_when(
      hrs_work %in% c(997, 999) ~ NA_real_,
      TRUE                      ~ hrs_work
    )
  )

summary(cps$hrs_work)   # now shows actual hours worked (mostly 40 hrs/week)


# --- 7c. Overall missingness summary ---------------------------------------
cps |>
  summarise(across(
    c(immigrant, employed, earn_week, hrs_work, educ_cat, state_abb),
    ~ round(mean(is.na(.)) * 100, 1)
  )) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") |>
  arrange(desc(pct_missing))
#
# Expected results:
#   earn_week  ~94%  — only ASEC respondents 2000+ report wages
#   hrs_work   ~55%  — many workers have varying or unreported hours
#   immigrant  ~0.1% — only code-0 NATIVITY (pre-2000 surveys)
#   employed   ~24%  — code-0 EMPSTAT (children, suppressed)


# -----------------------------------------------------------------------------
# 8.  RESTRICT THE ANALYSIS SAMPLE
#
#     For immigration × wages study at the state level, we need:
#     (a) Years 2000, 2010, 2020 — these are the only years where BOTH earn_week
#         AND nativity are available simultaneously in this extract.
#     (b) Working-age adults (25–64) — excludes students and near-retirees
#         whose wages are driven by life-cycle factors, not immigration.
#     (c) Employed civilians (empstat 10 or 12).
#     (d) Known immigration status (nativity ≠ 0).
#     (e) Non-missing, positive weekly earnings.
#     (f) Positive survey weight (a handful have weight = 0).
#     (g) 50 states + DC — drop territories (FIPS > 56).
#
# -----------------------------------------------------------------------------

n_before <- nrow(cps)
cat("Observations before restrictions:", n_before, "\n")

cps_samp <- cps |>
  filter(
    year %in% c(2000, 2010, 2020),  # usable years
    between(age, 25, 64),           # working age (between() is tidyverse)
    employed == 1,                   # employed
    !is.na(immigrant),              # known nativity
    !is.na(earn_week),              # has wage data
    earn_week > 0,                  # positive earnings
    weight > 0,                     # positive survey weight
    !is.na(state_abb)               # 50 states + DC
  )

cat("Observations after restrictions:", nrow(cps_samp), "\n")
cat("Dropped:", n_before - nrow(cps_samp), "\n")
cat("Years in sample:", paste(sort(unique(cps_samp$year)), collapse = ", "), "\n")
cat("States in sample:", n_distinct(cps_samp$state_abb), "\n")

# Confirm immigrant share looks reasonable (it should be ~14-21% by year):
cps_samp |>
  group_by(year) |>
  summarise(
    n           = n(),
    imm_share   = weighted.mean(immigrant, weight) |> round(3)
  )


# -----------------------------------------------------------------------------
# 9.  CONSTRUCT ANALYSIS VARIABLES
#     Checklist Step 10: Make scales consistent; apply transformations.
# -----------------------------------------------------------------------------

# Log weekly earnings:
#   - Wages are right-skewed; the log transform addresses this.
#   - Coefficients have a proportional (%) interpretation.
#   - More resistant to outliers than raw dollar levels.
cps_samp <- cps_samp |>
  mutate(ln_earn = log(earn_week))

# Verify: no -Inf or NaN (would occur if earn_week ≤ 0; already filtered)
sum(is.infinite(cps_samp$ln_earn))
sum(is.nan(cps_samp$ln_earn))


# -----------------------------------------------------------------------------
# 10. HANDLE EXTREME VALUES
#     Checklist Step 11: Enforce logical conditions; create a flag variable.
#
#     extreme earners will not be dropped because of this would introduce selection bias.
#     Instead, we:
#       (a) Flag them so we can check sensitivity of results.
#       (b) Use log earnings (which compresses extreme values) as our
#           primary outcome.
# -----------------------------------------------------------------------------

p99 <- quantile(cps_samp$earn_week, 0.99)
cat("99th percentile of weekly earnings: $", round(p99), "\n")

cps_samp <- cps_samp |>
  mutate(
    earn_extreme = if_else(earn_week > p99, 1L, 0L)
  )

count(cps_samp, earn_extreme)
# The flagged observations remain in the data
# For this project we retain them and use log earnings as the primary metric.


# -----------------------------------------------------------------------------
# 11. AGGREGATE TO STATE × YEAR PANEL
#
#     Unit of analysis = one row per state × year combination.
#     We collapse using ASEC survey weights so that statistics represent
#     the working population of each state, not just the sampled individuals.
#
#     Key aggregations:
#       immigrant_share — share of employed workers who are foreign-born
#       mean_ln_earn    — average log weekly earnings (weighted)
#       median_earn     — median weekly earnings (robust to extreme values)
# -----------------------------------------------------------------------------

state_yr <- cps_samp |>
  group_by(state_fips, state_abb, state_name, year) |>
  summarise(

    # Sample quality
    n_obs          = n(),                                   # unweighted obs in cell
    n_weighted     = sum(weight),                           # weighted worker count

    # Immigration
    n_immigrants   = sum(weight * immigrant),
    immigrant_share = n_immigrants / n_weighted,            # weighted share

    # Wages
    mean_earn      = weighted.mean(earn_week, weight),      # mean weekly earnings
    mean_ln_earn   = weighted.mean(ln_earn,   weight),      # mean log earnings
    median_earn    = median(earn_week),                     # median (unweighted)

    # Control variables for context
    share_college  = weighted.mean(educ_cat == "college_plus", weight),
    share_female   = weighted.mean(female, weight),
    mean_age       = weighted.mean(age, weight),

    .groups = "drop"
  )

glimpse(state_yr)
cat("State × year cells:", nrow(state_yr), "\n")   # max 51 × 3 = 153

# Flag cells with very few observations (unreliable estimates)
filter(state_yr, n_obs < 25) |> select(state_abb, year, n_obs)


# -----------------------------------------------------------------------------
# 12. FINAL CHECKS AND SAVE
#     Checklist Step 13: Save clean data before any further analysis.
# -----------------------------------------------------------------------------

# Any remaining missingness in the panel?
state_yr |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything()) |>
  filter(value > 0)

# Range sanity checks:
stopifnot(all(between(state_yr$immigrant_share, 0, 1)))
stopifnot(all(state_yr$mean_earn > 0))

cat("All range checks passed.\n")

# Save both the individual sample and the state-year panel:
write_csv(cps_samp,  "cps_individual_clean.csv")
write_csv(state_yr,  "cps_state_year.csv")

cat("Files saved:\n")
cat("  cps_individual_clean.csv — ", nrow(cps_samp), "rows\n")
cat("  cps_state_year.csv       — ", nrow(state_yr), "rows\n")

# =============================================================================
# END OF CLEANING SCRIPT
# =============================================================================
