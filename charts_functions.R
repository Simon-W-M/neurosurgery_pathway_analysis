# charts tables and functions

txt_size <- 20

# los density
dist_los <- data |>
  # filter(admit_flag == 1) |>
  ggplot(aes(x = spell_adj_lo_s)) +
  geom_density() +
  facet_wrap(~neuro_centre) +
  theme_minimal() +
  labs(
    title = "Density plot showing distribution of LOS",
    caption = "Data taken from SUS",
    x = "Spell adjusted length of stay",
    y = "Density"
  ) +
  theme(text = element_text(size = txt_size))


# los box
los_box <- ggbetweenstats(data,
  x = neuro_centre,
  y = spell_adj_lo_s,
  type = "nonparametric",
  boxplot.args = list(alpha = 0.01)
) +
  scale_y_log10() +
  coord_flip() +
  annotation_logticks(
    sides = "b",
    base = 10
  ) +
  labs(
    x = NULL,
    y = "Length of stay (LOG scale)"
  ) +
  theme(text = element_text(size = txt_size))


options(scipen = 5)

# pay density
dist_pay <- data |>
  # filter(admit_flag == 1) |>
  ggplot(aes(x = total_payment_with_mff)) +
  geom_density() +
  facet_wrap(~neuro_centre) +
  theme_minimal() +
  labs(
    title = "Density plot showing distribution of total payment",
    caption = "Data taken from SUS",
    x = "Total adjusted payment",
    y = "Density"
  ) +
  theme(text = element_text(size = txt_size))

# cost box
pay_box <- ggbetweenstats(data,
  x = neuro_centre,
  y = total_payment_with_mff,
  type = "nonparametric"
) +
  scale_y_log10() +
  coord_flip() +
  annotation_logticks(
    sides = "b",
    base = 10
  ) +
  labs(
    x = NULL,
    y = "Total payment with MFF (LOG scale)"
  ) +
  theme(text = element_text(size = txt_size))


# los vs payment
los_pay <- data |>
  filter(
    spell_adj_lo_s > 0,
    hrg_tot > 500
  ) |>
  ggplot(aes(x = total_payment_with_mff, y = spell_adj_lo_s, colour = hrg_code)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  # theme (legend.position = 'none') +
  facet_wrap(~neuro_centre, ncol = 1) +
  labs(
    title = "Scatter plot showing LOS against Payment",
    subtitle = "Showing the 5 most common HRG codes by service provision",
    caption = "Data taken from SUS",
    x = "Total payment with MFF",
    y = "Spell adjusted LOS"
  ) +
  theme(
    text = element_text(size = txt_size),
    legend.position = "bottom"
  )

# values
min_date <- format(min(data$admission_date), "%B %Y")
max_date <- format(max(data$admission_date), "%B %Y")
no_ccgs <- length(unique(data$final_derived_ccg))

# create table one of categorical
tab_one <- data |>
  dplyr::select(
    "Admission Method" = ad_desc,
    "Discharge Destination" = dis_desc,
    "Management" = man_desc,
    setting,
    "National Programme of Care" = npoc_category,
    neuro_centre,
    cost_type,
    primary_diagnosis
  ) |>
  tbl_summary(by = neuro_centre) |>
  as_gt() |>
  tab_style(
    style = list(
      cell_fill(color = "#E8EDEE")
    ),
    locations = cells_body(
      rows = c(5:12, 27, 30, 55, 75)
    )
  ) |>
  opt_table_font(
    size = 10
  ) |>
  tab_options(data_row.padding = px(1))

# chi test admissions

chi_t <- function(vec) {
  chi_d <- data_frame(table(vec, data$neuro_centre))
  chi_t <- chisq.test(chi_d)
  chi_t_p <- if_else(chi_t$p.value < 0.05, "<0.05", as.character(chi_t$p.value))
  chi_t_p
}

# admissions
chi_ad_t <- chi_t(data$admission_method)

# discharges
chi_dis_t <- chi_t(data$discharge_destination)

# discharge LOS by discharge
discharge_los <- data |>
  filter(admit_flag == 1) |>
  rename("Setting" = neuro_centre) |>
  ggplot(aes(x = spell_adj_lo_s, y = dis_desc, fill = Setting)) +
  geom_density_ridges(alpha = 0.3, panel_scaling = FALSE) +
  labs(
    title = "Density plot comparison of length of stay by discharge destination",
    x = "Spell adjusted length of stay",
    y = "Discharge destination"
  ) +
  theme_minimal() +
  theme(text = element_text(size = txt_size))


sd_rate_plot <- function(setting) {
  data_mth <- data |>
    dplyr::select(admission_date,
      neuro_centre,
      final_derived_ccg,
      pops = total_weighted_populations_uplifted_by_ons_population_growth_to_2020
    ) |>
    mutate(mth = floor_date(admission_date, "month")) |>
    summarise(
      mth_admits = n(),
      .by = c(
        mth, final_derived_ccg, pops,
        neuro_centre
      )
    ) |>
    mutate(rate_per_100k = round((mth_rate_pop <- mth_admits / pops) * 100000, 3))

  da <- data_mth |>
    filter(neuro_centre == setting,
      mth == max(mth),
      .by = final_derived_ccg
    ) |>
    arrange(rate_per_100k)

  da <- da |>
    mutate(dev_col = case_when(rate_per_100k > (mean(da$rate_per_100k, na.rm = T) + (2 * sd(da$rate_per_100k, na.rm = T))) ~ 1,
      rate_per_100k > (mean(da$rate_per_100k, na.rm = T) + sd(da$rate_per_100k, na.rm = T)) ~ 2,
      rate_per_100k > mean(da$rate_per_100k, na.rm = T) ~ 3,
      rate_per_100k > (mean(da$rate_per_100k, na.rm = T) - sd(da$rate_per_100k, na.rm = T)) ~ 4,
      rate_per_100k > (mean(da$rate_per_100k, na.rm = T) - (2 * sd(da$rate_per_100k, na.rm = T))) ~ 5,
      .default = 0
    )) |>
    filter(
      !is.na(rate_per_100k),
      dev_col == c(1, 5)
    )


  da |> ggplot(aes(
    x = rate_per_100k, y = reorder(final_derived_ccg, rate_per_100k),
    fill = dev_col
  )) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(y = "CCG") +
    geom_vline(
      xintercept = mean(da$rate_per_100k, na.rm = T),
      linetype = "dashed"
    ) +
    labs(
      title = paste0(setting, ": Chart showing rates per 100k 2 standard deviations away from mean"),
      subtitle = "Dashed line shows sample mean"
    ) +
    theme(text = element_text(size = 15))
}

# trend

data_mth <- data |>
  dplyr::select(admission_date,
    neuro_centre,
    final_derived_ccg,
    pops = total_weighted_populations_uplifted_by_ons_population_growth_to_2020
  ) |>
  mutate(mth = floor_date(admission_date, "month")) |>
  summarise(
    mth_admits = n(),
    .by = c(mth, neuro_centre, final_derived_ccg, pops)
  ) |>
  mutate(rate_per_100k = round((mth_rate_pop <- mth_admits / pops) * 100000), 3)


spc_plot_t <- function(setting) {
  da <- data_mth |>
    filter(neuro_centre == setting)

  # get last 18mths of data
  da <- da |>
    filter(mth > max(da$mth) %m-% months(18)) |>
    mutate(
      data_points = n(),
      .by = final_derived_ccg
    ) |>
    filter(data_points > 12)

  # spc chart based on numbers
  p_spc <- ptd_spc(da,
    value_field = mth_admits,
    date_field = mth,
    facet_field = final_derived_ccg,
    improvement_direction = "decrease"
  )

  ptd_create_ggplot(p_spc,
    icons_position = "none",
    point_size = 1.5,
    x_axis_date_format = "%b %y",
    x_axis_breaks = "2 months",
    main_title = paste0(setting, ": Statistical process control charts to identify trend by CCG"),
    x_axis_label = "",
    y_axis_label = "Number of monthly admissions"
  )
}
