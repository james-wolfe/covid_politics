library(readr)
library(tidyverse)

results2020 <- read_csv("raw_data/2020results.csv", col_types = cols(
  state = col_character(),
  trump_2020 = col_double(),
  biden_2020 = col_double()
))

results2016 <- read_csv("raw_data/1976-2016-president.csv", col_types = cols(
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
  
  # I only care about the 2016 results and Trump and Clinton.
  
  filter(year == 2016) %>%
  filter(candidate %in% c("Trump, Donald J.", "Clinton, Hillary")) %>%
  group_by(state_po, candidate) %>%
  
  # Totalvotes is the same for all, so we use mean, but for some reason
  # candidatevotes has a couple different entries for each candidate...
  
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
  
  # I'm putting these into percent format so I can get percent margins.
  
  mutate(clinton_2016 = clinton_2016 * 100 / total_2016,
         trump_2016 = trump_2016 * 100 / total_2016,
         margin_2016 = clinton_2016 - trump_2016,
         margin_2020 = biden_2020 - trump_2020,
         shift = margin_2020 - margin_2016) %>%
  select(-total_2016)

cases <- read_rds("shiny_app/clean_data/cases_state.rds")

results_cases <- results_2016_2020 %>%
  inner_join(cases, by = "state")

# Finally, an rds with results and cases.

write_rds(results_cases, "results_cases.rds")

approval <- read_csv("raw_data/covid_approval_toplines.csv") 

# Just experimenting with this approval data. I will use this in the app.

approval1 <- approval %>%
  pivot_wider(id_cols = "modeldate",
              values_from = "approve_estimate",
              names_from = "party") %>%
  ggplot(aes(x = as.Date(modeldate, "%m/%d/%Y"), y = approve_estimate, color = party)) +
  geom_line()

approval_all <- approval %>%
  pivot_wider(id_cols = "modeldate",
              values_from = "approve_estimate",
              names_from = "party") %>%
  select(modeldate, all)

natl_polls <- read_csv("raw_data/presidential_poll_averages_2020 copy.csv", col_types = cols(
  cycle = col_double(),
  state = col_character(),
  modeldate = col_character(),
  candidate_name = col_character(),
  pct_estimate = col_double(),
  pct_trend_adjusted = col_double()
)) %>%
  filter(state == "National") %>%
  filter(candidate_name %in% c("Joseph R. Biden Jr.", "Donald Trump")) %>%
  select(modeldate, candidate_name, pct_estimate) %>%
  pivot_wider(id_cols = modeldate, 
              names_from = "candidate_name",
              values_from = "pct_estimate") %>%
  rename(biden_est = "Joseph R. Biden Jr.",
         trump_est = "Donald Trump") %>%
  
  # Here I join national polling data with Covid approval data for the plot in
  # the Election section.
  
  inner_join(approval_all, by = "modeldate") %>%
  rename(covid_approve = all) %>%
  pivot_longer(cols = -modeldate,
               names_to = "type", 
               values_to = "estimate")

# This is the final code I used for this plot.

natl_polls %>%
  ggplot(aes(x = as.Date(modeldate, "%m/%d/%Y"), y = estimate, color = type)) +
  geom_line(size = 1.05) +
  scale_color_manual(breaks = c("biden_est", "trump_est", "covid_approve"),
                     values = c("navyblue", "red", "darkgray"),
                     name = "Estimate",
                     labels = c("Biden Polling Avg", "Trump Polling Avg", 
                                "Trump Covid Approval")) + 
  geom_hline(yintercept = 51.3, color = "navyblue", alpha = 0.7, lty = "dashed") +
  geom_hline(yintercept = 47, color = "red", alpha = 0.7, lty = "dashed") +
  annotate(geom = "text", x = as.Date("8/15/20", "%m/%d/%y"), y = 47.35, 
           label = "Actual Trump Vote Share", color = "firebrick4", size = 3) +
  annotate(geom = "text", x = as.Date("8/15/20", "%m/%d/%y"), y = 51.65, 
           label = "Actual Biden Vote Share", color = "navyblue", size = 3) +
  theme_minimal() +
  labs(x = "Date",
       y = "Percent",
       title = "Presidential Election National Polling Average",
       subtitle = "Compared with Trump's Covid Approval")

state_polls <- read_csv("presidential_poll_averages_2020 copy.csv", col_types = cols(
  cycle = col_double(),
  state = col_character(),
  modeldate = col_character(),
  candidate_name = col_character(),
  pct_estimate = col_double(),
  pct_trend_adjusted = col_double()
)) %>%
  
  # I only want to look at swing states for my swing state bar plot.
  
  filter(state %in% c("Arizona", "Florida", "Georgia", "Michigan", "Pennsylvania",
                      "Wisconsin", "Nevada", "North Carolina")) %>%
  filter(candidate_name %in% c("Joseph R. Biden Jr.", "Donald Trump")) %>%
  select(state, modeldate, candidate_name, pct_estimate) %>%
  pivot_wider(id_cols = c(modeldate, state), 
              names_from = "candidate_name",
              values_from = "pct_estimate") %>%
  rename(biden_est = "Joseph R. Biden Jr.",
         trump_est = "Donald Trump") %>%
  filter(modeldate %in% c("11/3/2020", "3/3/2020")) %>%
  pivot_wider(id_cols = state, 
              names_from = "modeldate",
              values_from = c("biden_est", "trump_est")) %>%
  rename(biden_nov = "biden_est_11/3/2020",
         biden_mar = "biden_est_3/3/2020",
         trump_nov = "trump_est_11/3/2020",
         trump_mar = "trump_est_3/3/2020") %>%
  mutate(margin_mar = biden_mar - trump_mar,
         margin_nov = biden_nov - trump_nov) %>%
  
  # One dataset has state names, the other has state abbreviations. 
  
  mutate(state = state.abb[match(state, state.name)]) %>%
  inner_join(results_2016_2020 %>% select(state, margin_2020), by = "state") %>%
  pivot_longer(cols = c(margin_mar, margin_nov, margin_2020),
               names_to = "type",
               values_to = "value") %>%
  
  # I create a logical column so that I can set color to it in the bar plot.
  # When it's positive (Biden won), it's blue. When it's negative (Biden lost),
  # it's red.
  
  mutate(positive = ifelse(value >= 0, TRUE, FALSE)) %>%
  mutate(type = factor(type, levels = c("margin_mar", "margin_nov", "margin_2020")))

write_rds(state_polls, "swingstate_polls.rds")

# Experimenting. Will copy and paste this code into the app.

state_polls %>%
  filter(state == "WI") %>%
  ggplot(aes(x = type, y = value, fill = positive)) +
  geom_col() +
  scale_fill_manual(breaks = c(TRUE, FALSE),
                    values = c("navyblue", "firebrick")) +
  scale_x_discrete(labels = c("Polling Avg \n in March",
                              "Polling Avg \n in Nov.",
                              "Actual Margin")) +
  labs(x = "Biden Margin", 
       y = "Percent", 
       title = "Swing State Polling Averages \n Compared to 2020 Election Outcomes") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

state_results <- read_rds("states.rds") %>%
  mutate(state = NAME) %>%
  select(state, geometry) %>%
  inner_join(results_2016_2020 %>% 
               select(state, margin_2020) %>%
               mutate(state = state.name[match(state, state.abb)]),
             by = "state") 

# Here I create the final electoral map. 

state_results %>%
  ggplot(aes(fill = margin_2020)) +
  geom_sf(color = "lightgray") +
  scale_color_gradientn(colors = c("firebrick", "firebrick", "pink", 
                                   "lightblue", "navyblue", "navyblue"),
                        values = c(0, 0.51, 0.550190599, 0.5501906, 
                                   .59, 1),
                        breaks = c(-20, -10, 0, 10, 20),
                        labels = c(-20, -10, 0, 10, 20)) +
  scale_fill_gradientn(colors = c("firebrick", "firebrick", "pink", 
                                  "lightblue", "navyblue", "navyblue"),
                        values = c(0, 0.49, 0.550190599, 0.5501906, 
                                   .61, 1),
                        breaks = c(-30, -20, -10, 0, 10, 20, 30),
                        labels = c(-30, -20, -10, 0, 10, 20, 30),
                       name = "Vote Margin \n (D)") +
  theme_void() +
  labs(title = "The 2020 U.S. Presidential Election")

# Writing the files for the concern graphs. Not much cleaning to do, thankfully.

concern_economy <- read_csv("covid_concern_toplines.csv") %>%
  filter(subject == "concern-economy") %>%
  pivot_longer(cols = very_estimate:not_at_all_estimate,
               names_to = "type",
               values_to = "estimate") %>%
  select(modeldate, type, estimate) %>%
  mutate(modeldate = as.Date(modeldate, "%m/%d/%Y"))

write_rds(concern_economy, "concern_economy.rds")

concern_infected <- read_csv("covid_concern_toplines.csv") %>%
  filter(subject == "concern-infected") %>%
  pivot_longer(cols = very_estimate:not_at_all_estimate,
               names_to = "type",
               values_to = "estimate") %>%
  select(modeldate, type, estimate) %>%
  mutate(modeldate = as.Date(modeldate, "%m/%d/%Y"))

write_rds(concern_infected, "concern_infected.rds")
