library(tidyverse)
library(here)
library(scales)
library(kjhmisc)
library(nyc)

setup_helvetica()
theme_set(theme_subway())


varnames <- tibble(
  raw_names = c("tsa_volume",
                "subways_total_estimated_ridership",
                "buses_total_estimated_ridership",
                "lirr_total_estimated_ridership",
                "metro_north_total_estimated_ridership",
                "access_a_ride_total_scheduled_trips",
                "bridges_and_tunnels_total_traffic",
                "staten_island_railway_total_estimated_ridership"),
  clean_names = c(
    "Every TSA Airport In America",
    "Subway",
    "Buses",
    "LIRR",
    "Metro North",
    "Access-a-Ride",
    "Cars: Bridges & Tunnels",
    "Staten Island Railway"))


new_varnames <- tibble(
  mode = c(
    "tsa_volume",
    "AAR",
    "SIR",
    "MNR",
    "Subway",
    "LIRR",
    "Bus",
    "BT",
    "CRZ Entries",
    "CBD Entries"),
  clean_names = c(
    "Every TSA Airport In America",
    "Access-a-Ride",
    "Staten Island Railway",
    "Metro North",
    "Subway",
    "LIRR",
    "MTA Buses",
    "Bridges & Tunnels",
    "CRZ Entries",
    "CBD Entries"
    ))

tsa <- readxl::read_xlsx(here("raw", "tsa_volumes.xlsx")) |>
  janitor::clean_names() |>
  mutate(date = as.Date(date)) |>
  mutate(mode = "tsa_volume") |>
  rename(count = tsa_volume) |>
select(date, mode, count)

tsa

# MTA_Daily_Ridership_and_Traffic__Beginning_2020_20250219.csv
# MTA_Daily_Ridership_and_Traffic__Beginning_2020_20250405.csv

mta <- read_csv(here("raw", "MTA_Daily_Ridership_and_Traffic__Beginning_2020_20250405.csv")) |>
  janitor::clean_names() |>
  mutate(date = mdy(date))

mta

tsa_mta <- tsa |>
  bind_rows(mta) |>
  mutate(mode_rc = case_when(mode %in% "tsa_volume" ~ "Every TSA Airport in the US",
                             mode %in% "Subway" ~ "The Subway",
                             mode %in% c("Bus", "AAR") ~ "MTA Buses & Access-a-Rides",
                             mode %in% c("LIRR",
                                              "MNR",
                                              "SIR") ~ "LIRR/Metro North/SIR",
                             mode %in% "BT" ~ "Bridge & Tunnel Vehicles",
                             .default = "Other")) |>
  filter(mode_rc != "Other") |>
  group_by(date, mode_rc) |>
  summarize(count = sum(count)) |>
  mutate(week = epiweek(date),
         year = epiyear(date),
         yrwk = ymd(paste0(year, "01", "01")) + weeks(week - 1))

avg_weekly_volume <-  tsa_mta |>
  group_by(yrwk, mode_rc) |>
  summarize(avg_vol = mean(count))

level_order <- avg_weekly_volume |>
  group_by(mode_rc) |>
  summarize(avg_vol = mean(avg_vol)) |>
  arrange(desc(avg_vol)) |>
  pull(mode_rc)

## Fudge factors for the in-plot labels
weekly_volume_labs <- avg_weekly_volume |>
  filter(yrwk == ymd("2024-12-16")) |>
  arrange(desc(avg_vol)) |>
  mutate(mode_rc_ord = factor(mode_rc, levels = level_order, ordered = TRUE),
         adj = c(3.2e5, -6e5, 2.8e5, -2.3e5, -2e5),
         avg_adj = avg_vol + adj)

out <- avg_weekly_volume |>
  mutate(mode_rc_ord = factor(mode_rc, levels = rev(level_order), ordered = TRUE)) |>
  filter(yrwk < ymd("2025-04-01") & yrwk > ymd("2022-01-01")) |>
  ggplot(aes(x = yrwk, y = avg_vol, color = mode_rc_ord)) +
  geom_line(linewidth = 1.75) +
  ggrepel::geom_text_repel(
    data = weekly_volume_labs,
    mapping = aes(x = yrwk,
                  y = avg_adj,
                  color = mode_rc_ord,
                  label = mode_rc,
                  fontface = "bold",
                  family = "Helvetica LT Pro Base"),
    size = 8,
    force = 0.01,
    force_pull = 0.01) +
  scale_color_manual(values = rev(c("#00add0", "#ff6319", "#6cbe45",
                                "#fccc0a", "#ee352e"))) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_date(labels = scales::label_date_short()) +
  guides(color = "none") +
  labs(x = "Weeks", y = "Average Volume (Millions)",
       color = "",
       title = "New York MTA Average Daily Ridership and Traffic, 2022-2025",
       subtitle = "With National TSA Airport Passenger Volumes for Comparison",
       caption = "Data: tsa.gov and data.ny.gov. Figure: Kieran Healy / @kjhealy") +
  theme(legend.text = element_text(size = rel(1.4)),
        plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(margin = margin(b = 24)),
        plot.margin = margin(0.25, 0.75, 0.15, 0.25, "in"),
        axis.text = element_text(size = rel(1.4)),
        axis.title = element_text(size = rel(1.6)))

out

kjhmisc::save_figure(here("figures", "mta_volumes"), out, height = 11, width = 18)

### Further plots and aggregations
tsa_mta |>
  filter(mode_rc == "The Subway") |>
  mutate(weekday = weekdays(date)) |>
  filter(yrwk < ymd("2024-12-20") & yrwk > ymd("2022-01-01")) |>
  filter(weekday %nin% c("Saturday", "Sunday")) |>
  group_by(week) |>
  summarize(avg_wkly = mean(count)) |>
  summarize(tot_avg = mean(avg_wkly))

tsa_mta |>
  filter(mode_rc %in% c("MTA Buses & Access-a-Rides",
                        "The Subway", "LIRR/Metro North/SIR")) |>
  filter(date == ymd("2024-03-03"))

tsa_mta |>
  filter(mode_rc %in% c("MTA Buses & Access-a-Rides",
                        "The Subway", "LIRR/Metro North/SIR")) |>
  mutate(weekday = weekdays(date)) |>
  filter(date < ymd("2024-12-20") & date > ymd("2022-01-01")) |>
  filter(weekday %nin% c("Saturday", "Sunday")) |>
  group_by(year, week, weekday) |>
  summarize(tot_wkday = sum(count)) |>
  group_by(year) |>
  summarize(tot_avg = mean(tot_wkday))

out_tsa_daily <- tsa |>
  filter(date > ymd("2018-12-31") & date < ymd("2025-02-01")) |>
  ggplot(aes(x = date, y = count)) +
  geom_line(linewidth = 0.3) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_date(date_breaks = "3 months",
               labels = scales::label_date_short(),
               expand = expansion(mult = c(0.005, 0))) +
  coord_cartesian(xlim = c(ymd("2019-01-01"), ymd("2025-02-09"))) +
  labs(x = NULL, y = "Count",
       title = "TSA Checkpoint Travel Volume",
       subtitle = "Daily Number of Screened Individuals, Jan 2019 to Jan 2025",
       caption = "Data: tsa.gov. Figure: Kieran Healy / @kjhealy") +
  theme_broadway()

kjhmisc::save_figure(here("figures", "tsa_volumes_daily"), out_tsa_daily, height = 5, width = 40, dpi = 600)

out_tsa_daily_sample <- tsa |>
  filter(date > ymd("2019-09-30") & date < ymd("2019-11-01")) |>
  ggplot(aes(x = date, y = count)) +
  geom_line(linewidth = 0.85) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_date(date_breaks = "1 day",
               date_labels = "%a",
               expand = expansion(mult = c(0.006, 0.008))) +
  labs(x = NULL, y = "Count",
       title = "TSA Checkpoint Daily Travel Volume",
       subtitle = "October 1st 2019 to October 30th 2019",
       caption = "Data: tsa.gov. Figure: Kieran Healy / @kjhealy") +
  theme_broadway()

kjhmisc::save_figure(here("figures", "tsa_volumes_daily_sample"),
                     out_tsa_daily_sample,
                     height = 5, width = 18, dpi = 300)

out_tsa_wkly <- tsa |>
  filter(date > ymd("2018-12-31") & date < ymd("2025-02-02")) |>
  mutate(week = epiweek(date),
         year = epiyear(date),
         yrwk = ymd(paste0(year, "01", "01")) + weeks(week - 1)) |>
  group_by(yrwk) |>
  summarize(wkly_vol = sum(count)) |>
  ggplot(aes(x = yrwk, y = wkly_vol)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_date(date_breaks = "3 months",
               labels = scales::label_date_short(),
               expand = expansion(mult = c(0.005, 0))) +
  labs(x = NULL, y = "Count",
       title = "TSA Checkpoint Travel Volume",
       subtitle = "Weekly Number of Screened Individuals, Jan 2019 to Jan 2025",
       caption = "Data: tsa.gov. Figure: Kieran Healy / @kjhealy") +
  theme_broadway()

kjhmisc::save_figure(here("figures", "tsa_volumes_wkly"),
                     out_tsa_wkly, height = 5, width = 20, dpi = 600)



out_tsa_noepi_wkly <- tsa |>
  filter(date > ymd("2018-12-31") & date < ymd("2025-02-02")) |>
  mutate(week = week(date),
         year = year(date),
         yrwk = ymd(paste0(year, "01", "01")) + weeks(week - 1)) |>
  group_by(yrwk) |>
  summarize(wkly_vol = sum(count)) |>
  ggplot(aes(x = yrwk, y = wkly_vol)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_date(date_breaks = "3 months",
               labels = scales::label_date_short(),
               expand = expansion(mult = c(0.005, 0))) +
  labs(x = NULL, y = "Count",
       title = "TSA Checkpoint Travel Volume",
       subtitle = "Weekly Number of Screened Individuals, Jan 2019 to Jan 2025",
       caption = "Data: tsa.gov. Figure: Kieran Healy / @kjhealy") +
  theme_broadway()

kjhmisc::save_figure(here("figures", "tsa_volumes_noepi_wkly"),
                     out_tsa_noepi_wkly, height = 5, width = 20, dpi = 600)



tsa |>
  filter(date > ymd("2019-01-05") & date < ymd("2025-01-31")) |>
  mutate(week = epiweek(date),
         year = epiyear(date)) |>
  arrange(date) |>
  group_by(year, week) |>
  tally() |>
  pull(n) |>
  table()

## Write out some CSVs
tsa_weekly_df <- tsa |>
    filter(date > ymd("2018-12-31") & date < ymd("2025-02-02")) |>
    mutate(week = epiweek(date),
           year = epiyear(date),
           yrwk = ymd(paste0(year, "01", "01")) + weeks(week - 1)) |>
    group_by(yrwk) |>
    summarize(wkly_vol = sum(count))

write_csv(tsa_weekly_df, here("data", "tsa_weekly_count.csv"))
saveRDS(tsa_weekly_df, file = here("data", "tsa_weekly_df.rda"))

subway_weekly_df <- mta |>
  filter(mode == "Subway") |>
  mutate(week = epiweek(date),
         year = epiyear(date),
         yrwk = ymd(paste0(year, "01", "01")) + weeks(week - 1)) |>
  group_by(yrwk) |>
  summarize(count = sum(count))

write_csv(subway_weekly_df, here("data", "subway_weekly_ridership.csv"))
saveRDS(subway_weekly_df, file = here("data", "subway_weekly_df.rda"))

tail(avg_weekly_volume)

