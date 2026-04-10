# Load cleaned data produced by 01_clean.R:
ind      <- read_csv("cps_individual_clean.csv", col_types = cols())
state_yr <- read_csv("cps_state_year.csv",        col_types = cols())

# Convenience: year and educ_cat as factors for categorical plotting
ind <- ind |>
  mutate(
    year_f    = factor(year),
    educ_cat  = factor(educ_cat,
                       levels = c("less_hs","hs","some_college","college_plus"),
                       ordered = TRUE),
    imm_label = if_else(immigrant == 1, "Immigrant", "Native-born")
  )

state_yr <- state_yr |>
  mutate(year_f = factor(year))



# A shared theme we apply to every figure for a consistent look:
theme_project <- theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    plot.caption  = element_text(color = "gray50", size = 8, hjust = 0),
    legend.position = "bottom",
    strip.text    = element_text(face = "bold")
  )


# ── Figure 1: Overlapping Density — wages by immigration status over time ───
# Density plots (rather than histograms) are better for comparing two groups.
# Faceting by year shows whether the gap has grown, shrunk, or persisted.
#  using alpha transparency so both groups are visible where they overlap.

fig1 <- ggplot(ind, aes(x = ln_earn, fill = imm_label, color = imm_label)) +
  geom_density(alpha = 0.30, adjust = 1.2, linewidth = 0.8) +
  scale_fill_manual(
    values = c("Immigrant" = "#D6604D", "Native-born" = "#2166AC"),
    name = NULL
  ) +
  scale_color_manual(
    values = c("Immigrant" = "#D6604D", "Native-born" = "#2166AC"),
    name = NULL
  ) +
  facet_wrap(~ year_f, ncol = 3) +
  labs(
    title    = "Figure 1. Distribution of Log Weekly Earnings by Immigration Status",
    subtitle = "Employed civilians aged 25–64. Vertical spread = density, not count.",
    x        = "Log Weekly Earnings",
    y        = "Density",
    caption  = paste("Source: IPUMS CPS.",
                     "Distributions are unweighted; survey weights used in quantitative summaries.")
  ) +
  theme_project +
  theme(legend.position = "top")

print(fig1)
ggsave("fig1_earnings_density_by_status.png", fig1, width = 10, height = 5, dpi = 150)



#── Figure 2: Cleveland Dot Plot — earnings gap by education level ──────────
#  Cleveland dot plots are cleaner than bar charts when
# the baseline does not have a meaningful zero.
#
# This figure shows the earnings gap conditional on education — a first,
# visual "control" that helps assess whether any gap is just because
# immigrants and natives have different education compositions.

educ_wage <- ind |>
  filter(!is.na(educ_cat)) |>
  group_by(year, educ_cat, imm_label) |>
  summarise(
    mean_ln = weighted.mean(ln_earn, weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    educ_label = recode(educ_cat,
                        less_hs       = "< High School",
                        hs            = "High School",
                        some_college  = "Some College",
                        college_plus  = "College+")
  )

fig2 <- ggplot(educ_wage,
               aes(x = mean_ln, y = educ_label, color = imm_label,
                   group = imm_label)) +
  geom_line(aes(group = educ_label), color = "gray70", linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Immigrant" = "#D6604D", "Native-born" = "#2166AC"),
    name = NULL
  ) +
  facet_wrap(~ year, ncol = 3) +
  labs(
    title    = "Figure 2. Mean Log Earnings by Education Level and Immigration Status",
    subtitle = "Connected lines show the within-education earnings gap (native vs. immigrant).",
    x        = "Mean Log Weekly Earnings",
    y        = NULL,
    caption  = "Source: IPUMS CPS. Survey-weighted means. Employed civilians aged 25–64."
  ) +
  theme_project +
  theme(legend.position = "top")

print(fig2)
ggsave("fig2_educ_earnings_gap.png", fig2, width = 11, height = 6, dpi = 150)



# =============================================================================
# THE CORE RELATIONSHIP — IMMIGRANT SHARE AND WAGES ACROSS STATES
# =============================================================================
# This is the central figure of my topic: do states with higher immigrant shares tend to have higher or lower wages?

#NOT a causal effect.

# ── Figure 3: Scatterplot — immigrant share vs. mean log wages by state ─────
# Size = workforce so large states don't get equal visual weight as small ones.
# OLS line added to summarize the central tendency.

fig3 <- ggplot(state_yr,
               aes(x = immigrant_share, y = mean_ln_earn, color = year_f)) +
  geom_point(aes(size = n_weighted / 1e5), alpha = 0.65) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  geom_text_repel(
    data = state_yr |>
      filter(state_abb %in% c("CA","TX","NY","FL","MT","ND","MS","WV","NJ","HI")),
    aes(label = state_abb),
    size = 2.8, show.legend = FALSE, max.overlaps = 20
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Set1", name = "Year") +
  scale_size_continuous(
    name   = "Workforce\n(100k workers)",
    range  = c(1, 8),
    guide  = guide_legend(override.aes = list(alpha = 1))
  ) +
  facet_wrap(~ year, ncol = 3) +
  labs(
    title    = "Figure 3. Immigrant Workforce Share vs. Mean Log Wages Across States",
    subtitle  = paste("Each point = one state. Size = relative workforce.",
                      "OLS line summarizes the cross-sectional association."),
    x        = "Immigrant Share of Employed Workers",
    y        = "Mean Log Weekly Earnings",
    caption  = paste("Source: IPUMS CPS. Survey-weighted. Employed civilians aged 25–64.",
                     "\nNote: A positive association does not imply that immigration raises wages;",
                     "immigrants may sort into high-wage labor markets.")
  ) +
  theme_project +
  theme(legend.position = "right")

print(fig3)
ggsave("fig3_scatter_share_wages.png", fig3, width = 12, height = 5, dpi = 150)


# =============================================================================
# Change in immigrant share 2000→2020, by state
# =============================================================================
# Showing *change* rather than levels is useful for panel analyses.
# This plot visualizes which states experienced the biggest shifts.

share_change <- state_yr |>
  select(state_abb, year, immigrant_share) |>
  pivot_wider(names_from = year, values_from = immigrant_share,
              names_prefix = "share_") |>
  mutate(
    change_00_20 = share_2020 - share_2000,
    state_abb    = reorder(state_abb, change_00_20)
  ) |>
  filter(!is.na(change_00_20))

fig_bonus <- ggplot(share_change,
                    aes(x = change_00_20, y = state_abb,
                        fill = change_00_20 > 0)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("TRUE"  = "#2166AC",
                               "FALSE" = "#D6604D"),
                    guide  = "none") +
  labs(
    title   = "Figure 4. Change in Immigrant Workforce Share, 2000–2020 (by State)",
    subtitle = "Blue = increase; Red = decrease. All states saw immigrant share grow.",
    x       = "Percentage-Point Change in Immigrant Share",
    y       = NULL,
    caption = "Source: IPUMS CPS. Survey-weighted. Employed civilians aged 25–64."
  ) +
  theme_project +
  theme(axis.text.y = element_text(size = 7))

print(fig_bonus)
ggsave("fig4_change_immigrant_share.png", fig_bonus, width = 8, height = 10, dpi = 150)
