library(readr)
library(tidyverse)

results2020 <- read_csv("2020results.csv", col_types = cols(
  state = col_character(),
  trump_2020 = col_double(),
  biden_2020 = col_double()
))

results2016 <- read_csv("1976-2016-president.csv", col_types = cols(
  year = col_double(),
  state = col_character(),
  state_po = col_character(),
  state_fips = col_double(),
  state_cen = col_double(),
  state_ic = col_double(),
  office = col_character(),
  candidate = col_character(),
  party = col_character(),
  writein = col_logical(),
  candidatevotes = col_double(),
  totalvotes = col_double(),
  version = col_double(),
  notes = col_logical()
)) %>%
  filter(year == 2016) %>%
  filter(candidate %in% c("Trump, Donald J.", "Clinton, Hillary")) %>%
  group_by(state_po, candidate) %>%
  summarize(totalvotes = mean(totalvotes),
            candidatevotes = sum(candidatevotes),
            .groups = "drop") %>%
  pivot_wider(id_cols = c("state_po", "totalvotes"),
              names_from = "candidate",
              values_from = "candidatevotes") %>%
  rename(state = state_po)

results_2016_2020 <-
  inner_join(results2016, results2020, by = "state") %>%
  rename(clinton_2016 = "Clinton, Hillary",
         trump_2016 = "Trump, Donald J.",
         total_2016 = "totalvotes") %>%
  mutate(clinton_2016 = clinton_2016 * 100 / total_2016,
         trump_2016 = trump_2016 * 100 / total_2016,
         margin_2016 = clinton_2016 - trump_2016,
         margin_2020 = biden_2020 - trump_2020,
         shift = margin_2020 - margin_2016) %>%
  select(-total_2016)

cases <- read_rds("cases_state.rds")

results_cases <- results_2016_2020 %>%
  inner_join(cases, by = "state")

write_rds(results_cases, "results_cases.rds")


approval <- read_csv("covid_approval_toplines.csv") 

approval
View(approval %>%
       pivot_wider(id_cols = "modeldate",
                   values_from = "approve_estimate",
                   names_from = "party"))

approval1 <- approval %>%
  pivot_wider(id_cols = "modeldate",
              values_from = "approve_estimate",
              names_from = "party") %>%
  ggplot(aes(x = as.Date(modeldate, "%m/%d/%Y"), y = approve_estimate, color = party)) +
  geom_line()
