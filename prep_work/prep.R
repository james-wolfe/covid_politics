library(tidyverse)
library(tidycensus)
library(haven)
library(ggthemes)
library(gganimate)
library(sf)
library(spData)
library(transformr)
library(corrplot)
library(RColorBrewer)
library(janitor)
library(viridis)
library(ggrepel)
library(directlabels)
library(rstanarm)


ns319 <- read_dta("./raw_data/ns20200319.dta")
ns326 <- read_dta("./raw_data/ns20200326.dta")
ns402 <- read_dta("./raw_data/ns20200402.dta")
ns409 <- read_dta("./raw_data/ns20200409.dta")
ns416 <- read_dta("./raw_data/ns20200416.dta")
ns423 <- read_dta("./raw_data/ns20200423.dta")
ns430 <- read_dta("./raw_data/ns20200430.dta")
ns507 <- read_dta("./raw_data/ns20200507.dta")
ns514 <- read_dta("./raw_data/ns20200514.dta")
ns521 <- read_dta("./raw_data/ns20200521.dta")
ns528 <- read_dta("./raw_data/ns20200528.dta")
ns604 <- read_dta("./raw_data/ns20200604.dta")
ns611 <- read_dta("./raw_data/ns20200611.dta")
ns618 <- read_dta("./raw_data/ns20200618.dta")
ns625 <- read_dta("./raw_data/ns20200625.dta")


# Here I full_join all these together because I didn't really know how else to get them all in one place

march <- full_join(ns319, ns326) %>%
  select(state, census_region, extra_trump_corona, trump_biden, pid3, congress_district, news_sources_fox, extra_covid_close_schools, deportation, medicare_for_all, raise_upper_tax, minwage, environment, weight)
april <- full_join(full_join(full_join(ns402, ns409), full_join(ns416, ns423)), ns430) %>% 
  select(state, census_region, extra_trump_corona, trump_biden, pid3, congress_district, news_sources_fox, extra_covid_close_schools, deportation, medicare_for_all, raise_upper_tax, minwage, environment, weight)
may <- full_join(full_join(ns507, ns514), full_join(ns521, ns528)) %>%
  select(state, census_region, extra_trump_corona, trump_biden, pid3, congress_district, news_sources_fox, extra_covid_socialize_no_dist, extra_covid_close_schools, deportation, medicare_for_all, raise_upper_tax, minwage, environment, weight)
june <- full_join(full_join(ns604, ns611), full_join(ns618, ns625)) %>%
  select(state, census_region, extra_trump_corona, trump_biden, pid3, congress_district, news_sources_fox, extra_covid_socialize_no_dist, extra_covid_close_schools, deportation, medicare_for_all, raise_upper_tax, minwage, environment, weight)

write_rds(march, "march.rds")
write_rds(april, "april.rds")
write_rds(may, "may.rds")
write_rds(june, "june.rds")

states_population <- get_decennial(geography = "state",
                                   variables = "P001001",
                                   year = 2010,
                                   geometry = TRUE, 
                                   shift_geo = TRUE) 

counties_population <- get_decennial(geography = "county",
                                     variables = "P001001",
                                     year = 2010,
                                     geometry = TRUE, 
                                     shift_geo = TRUE) 

write_rds(states_population, "states.rds")
write_rds(counties_population, "counties.rds")



march_3_biden <- read_csv("presidential_poll_averages_2020 copy.csv") %>%
  filter(modeldate == "3/3/2020") %>%
  rename(pct_march = "pct_estimate") %>%
  pivot_wider(names_from = "candidate_name",
              values_from = "pct_march") %>%
  slice(1:33) %>%
  rename(biden_mar = `Joseph R. Biden Jr.`) %>%
  select(state, modeldate, biden_mar)

march_3_trump <- read_csv("presidential_poll_averages_2020 copy.csv") %>%
  filter(modeldate == "3/3/2020") %>%
  rename(pct_march = "pct_estimate") %>%
  pivot_wider(names_from = "candidate_name",
              values_from = "pct_march") %>%
  slice(34:66) %>%
  rename(trump_mar = `Donald Trump`) %>%
  select(state, modeldate, trump_mar)

march_3 <- inner_join(march_3_biden, march_3_trump, by = c("state", "modeldate"))


nov_3_biden <- read_csv("presidential_poll_averages_2020 copy.csv") %>%
  filter(modeldate == "11/3/2020") %>%
  rename(pct_nov = "pct_estimate") %>%
  filter(candidate_name == "Joseph R. Biden Jr.") %>%
  pivot_wider(names_from = "candidate_name",
              values_from = "pct_nov") %>%
  rename(biden_nov = `Joseph R. Biden Jr.`) %>%
  select(state, modeldate, biden_nov)

nov_3_trump <- read_csv("presidential_poll_averages_2020 copy.csv") %>%
  filter(modeldate == "11/3/2020") %>%
  rename(pct_nov = "pct_estimate") %>%
  filter(candidate_name == "Donald Trump") %>%
  pivot_wider(names_from = "candidate_name",
              values_from = "pct_nov") %>%
  rename(trump_nov = `Donald Trump`) %>%
  select(state, modeldate, trump_nov)

nov_3 <- inner_join(nov_3_biden, nov_3_trump, by = c("state", "modeldate"))

pct_changes <- inner_join(march_3, nov_3, by = "state")

state_pop <- 
  states_population %>%
  mutate(state = state.abb[match(NAME, state.name)]) %>%
  select(state, value, -geometry)

cases

cases_state <- cases %>%
  filter(date == "11/3/20") %>%
  group_by(state) %>%
  summarize(mean = mean(cases))

cases_state
write_rds(cases_state, "cases_state.rds")
poll_vs_cases <- pct_changes %>%
  filter(state != "National") %>%
  mutate(biden_diff_mar = biden_mar - trump_mar,
         biden_diff_nov = biden_nov - trump_nov,
         biden_diff_overall = biden_diff_nov - biden_diff_mar) %>%
  mutate(state = state.abb[match(state, state.name)]) %>%
  inner_join(cases_state, by = "state") %>%
  inner_join(state_pop, by = "state")


poll_vs_cases %>%
  ggplot(aes(x = mean / value, y = biden_diff_overall, color = biden_diff_nov, size = value)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "navyblue", se = FALSE) +
  scale_color_gradientn(values = c(0, 0.4744088, 1),
                        colors = c("red", "purple", "blue")) + 
  theme_minimal()

poll_vs_cases %>%
  ggplot(aes(x = mean, y = biden_diff_overall, size = value, color = biden_diff_nov)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


total %>%
  filter(extra_trump_corona != 999) %>%
  group_by(congress_district) %>%
  summarize(approval_covid = mean(extra_trump_corona %in% c(1, 2), na.rm = TRUE),
            mean_democrat = mean(pid3 == 1, na.rm = TRUE), 
            region = as.factor(round(mean(census_region, na.rm = TRUE))),
            number = n(),
            .groups = "drop") %>%
  slice(-1) %>%
  filter(approval_covid != 0) %>%
  ggplot(aes(x = mean_democrat, y = approval_covid)) + 
  geom_point(aes(color = region, size = number),
             alpha = 0.7) + 
  geom_smooth(method = lm, se = FALSE, color = "royalblue", alpha = 0.8) +
  theme_minimal() + 
  labs(x = "Democrats",
       y = "Approval on Covid",
       title = "Party ID Makeup Predicts Trump's Approval on Covid",
       subtitle = "By congressional district") + 
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_color_discrete(name = "Region",
                       labels = c("South", "West", "Northeast", "Midwest")) +
  scale_size_continuous(name = "Number of \n Respondents")

total_model <- total %>%
  filter(extra_trump_corona != 999) %>%
  group_by(congress_district) %>%
  summarize(approval_covid = mean(extra_trump_corona %in% c(1, 2), na.rm = TRUE),
            mean_democrat = mean(pid3 == 1, na.rm = TRUE), 
            region = as.factor(round(mean(census_region, na.rm = TRUE))),
            number = n(),
            .groups = "drop") %>%
  slice(-1)

total_model

write_rds(total_model, "total_filt.rds")

fit <- stan_glm(data = total_model,
                formula = approval_covid ~ mean_democrat,
                refresh = 0)

print(fit, digits = 3)


population <- get_decennial(geography = "state",
                            variables = "P001001",
                            year = 2010,
                            geometry = TRUE, 
                            shift_geo = TRUE) 

pop <- population %>%
  rename(state = NAME) %>%
  mutate(state = state.abb[match(state, state.name)]) %>%
  filter(! GEOID %in% c(11, 72)) %>%
  select(state, geometry) 

cor(total_new, method = c("pearson", "kendall", "spearman"))


pop <- states_population %>%
  rename(state = NAME) %>%
  mutate(state = state.abb[match(state, state.name)]) %>%
  filter(! GEOID %in% c(11, 72)) %>%
  select(state, geometry)

survey_march <- march %>%
  filter(extra_trump_corona != 999) %>%
  mutate(trump_covid = ifelse(extra_trump_corona %in% c(1,2), 1, 0)) %>%
  mutate(trump_covid_weights = trump_covid * weight) %>%
  group_by(state) %>%
  summarize(mean = sum(trump_covid_weights) / sum(weight), .groups = "drop") 

survey_march <- inner_join(survey_march, pop, by = "state") %>%
  mutate(month = 3)

survey_april <- april %>%
  filter(extra_trump_corona != 999) %>%
  mutate(trump_covid = ifelse(extra_trump_corona %in% c(1,2), 1, 0)) %>%
  mutate(trump_covid_weights = trump_covid * weight) %>%
  group_by(state) %>%
  summarize(mean = sum(trump_covid_weights) / sum(weight), .groups = "drop") 

survey_april <- inner_join(survey_april, pop, by = "state") %>%
  mutate(month = 4)

survey_may <- may %>%
  filter(extra_trump_corona != 999) %>%
  mutate(trump_covid = ifelse(extra_trump_corona %in% c(1,2), 1, 0)) %>%
  mutate(trump_covid_weights = trump_covid * weight) %>%
  group_by(state) %>%
  summarize(mean = sum(trump_covid_weights) / sum(weight), .groups = "drop") 

survey_may <- inner_join(survey_may, pop, by = "state") %>%
  mutate(month = 5)

survey_june <- june %>%
  filter(extra_trump_corona != 999) %>%
  mutate(trump_covid = ifelse(extra_trump_corona %in% c(1,2), 1, 0)) %>%
  mutate(trump_covid_weights = trump_covid * weight) %>%
  group_by(state) %>%
  summarize(mean = sum(trump_covid_weights) / sum(weight), .groups = "drop") 

survey_june <- inner_join(survey_june, pop, by = "state") %>%
  mutate(month = 6)


survey <- full_join(full_join(survey_march, survey_april), full_join(survey_may, survey_june)) %>%
  filter(state != "DC")


q <- inner_join(pop, survey, by = "state") %>%
  arrange(month) %>%
  rename(geometry = geometry.x) %>%
  ggplot(aes(fill = mean * 100)) +
  geom_sf() + 
  scale_fill_viridis_c(direction = -1,
                       option = "plasma") +
  labs(title = "Trump's Approval on Covid-19",
       fill = "Approval \n (in %)") +
  theme_void() +
  transition_states(month, transition_length = 1, state_length = 0.5) +
  labs(subtitle = 'Month: 0{closest_state}/2020')

anim_save("trump_covid.gif", q)

survey %>%
  ggplot(aes(fill = mean * 100)) +
  geom_sf(geometry = "geometry") + 
  scale_fill_viridis_c(direction = -1,
                       option = "plasma") +
  labs(title = "Trump's Approval on Covid in May/June",
       fill = "Mean Approval (in %)") +
  theme_void() + 
  transition_states(month, transition_length = 1, state_length = 1)


total_cov %>%
  pivot_longer(cols = c(approval_covid, mean_trump), values_to = "percent", names_to = "type") %>%
  filter(census_region == 4) %>%
  ggplot(aes(x = survey, y = percent, color = type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 15, 2),
                     labels = c("3/19", "4/02", "4/16", "4/30", "5/14", "5/28", "6/11", "6/25")) + 
  theme_minimal() +
  transition_reveal(survey)


march <- read_rds("march.rds")
april <- read_rds("april.rds")
may <- read_rds("may.rds")
june <- read_rds("june.rds")
total <- full_join(full_join(march, april), full_join(may, june))

total_new <- total %>%
  select(extra_trump_corona, 
         news_sources_fox, 
         extra_covid_socialize_no_dist,
         extra_covid_close_schools,
         deportation,
         medicare_for_all,
         raise_upper_tax,
         minwage,
         environment,
         congress_district,
         pid3) 

total_new[total_new == 999] <- NA
total_new[total_new == 888] <- NA
total_new[total_new == 2] <- 0

total_new1 <- total_new %>%
  group_by(congress_district) %>%
  summarize(covid_trump = mean(extra_trump_corona %in% c(0, 1), na.rm = TRUE),
            close_school = mean(extra_covid_close_schools %in% c(0, 1), na.rm = TRUE),
            fox_news = mean(news_sources_fox, na.rm = TRUE),
            social_dist = mean(extra_covid_socialize_no_dist, na.rm = TRUE),
            deportation = mean(deportation, na.rm = TRUE),
            medicare4all = mean(medicare_for_all, na.rm = TRUE),
            upper_tax = mean(raise_upper_tax, na.rm = TRUE),
            minwage = mean(minwage, na.rm = TRUE), 
            environment = mean(environment, na.rm = TRUE),
            perc_dem = mean(pid3 == 1, na.rm = TRUE),
            .groups = "drop") %>%
  slice(-1)

total_new1 <- total_new1 %>%
  select(-congress_district)

total_new <- total_new %>%
  mutate(extra_trump_corona = case_when(
    extra_trump_corona == 1 ~ 3,
    extra_trump_corona == 2 ~ 4,
    extra_trump_corona == 3 ~ 2,
    extra_trump_corona == 4 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  mutate(extra_covid_close_schools = case_when(
    extra_covid_close_schools == 1 ~ 3,
    extra_covid_close_schools == 2 ~ 4,
    extra_covid_close_schools == 3 ~ 2,
    extra_covid_close_schools == 4 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  rename(covid_trump = extra_trump_corona,
         social_dist = extra_covid_socialize_no_dist,
         fox_news = news_sources_fox,
         close_school = extra_covid_close_schools,
         upper_tax = raise_upper_tax,
         medicare4all = medicare_for_all)

total_new

corre1 <- cor(total_new1, use = "complete.obs",
              method = c("pearson", "kendall", "spearman"))

corrplot(corre)

corrplot(corre, order = "FPC",
         col = r, bg = "white")

corrplot.mixed(corre1, 
               lower = "number", 
               upper = "color", 
               lower.col = "black", 
               order = "FPC", 
               number.cex = .7,
               cl.align = "r",
               tl.pos = "lt", diag = "u",
               tl.col = "black")

counties_pop <- read_csv("covid_county_population_usafacts.csv")
counties_cases <- read_csv("covid_confirmed_usafacts.csv")

counties_cases <- counties_cases %>%
  rename(name = "County Name",
         state = "State") 

state.name[match(state, state.abb)]
counties_cases

counties_cases$NAME <- paste(counties_cases$name, counties_cases$state, sep=", ")
counties_cases

inner_join(counties_population, by = "NAME")

cases_plot <- counties_cases %>%
  pivot_longer(cols = "1/22/20":"11/3/20",
               values_to = "cases",
               names_to = "date")


cases <- cases_plot %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases),
            .groups = "drop") 

cases

cases %>%
  filter(date == "11/3/20") %>%
  group_by(state) %>%
  summarize(mean = mean(cases))

read_csv()

p <- cases %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line() +
  scale_color_discrete(breaks = c("CA", "TX", "FL", "NY", "IL"),
                       name = "Top 5 States \n by Case Count") +
  theme_minimal() +
  labs(x = "Date",
       y = "Cases",
       title = "Total cases by state") +
  transition_reveal(as.Date(date, "%m/%d/%y")) +
  view_follow() 

anim_save("states_cases.gif", p)

?scale_color_discrete

cases_plot %>%
  mutate(cases_per_cap = cases/value) %>%
  summarize(max = 100000 * max(cases_per_cap))

cases_plot <- counties_cases %>%
  inner_join(counties_population, by = c("NAME", "name")) %>%
  select(-"11/4/20":"11/12/20")

cases_plot
write_rds(cases_plot, "cases_plot.rds")

cases_plot %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  filter(date == "2020-11-03") %>%
  ggplot(aes(fill = cases * 100000 / value, color = cases * 100000 / value)) + 
  geom_sf(aes(geometry = geometry)) + 
  labs(title = "Covid Cases Per Capita. By County",
       fill = "Cases per \n 100,000") +
  theme_void() + 
  scale_colour_gradientn(colors = c("black", "red", "lightpink"),
                         values = c(0, 0.5, 1)) +
  scale_fill_gradientn(colors = c("black", "red", "lightpink"),
                       values = c(0, 0.5, 1))

new_cases <- read_csv("new_cases.csv")

r <- new_cases %>%
  rename(us = "United States") %>%
  select(us, date) %>%
  filter(date >= "2020-01-15") %>%
  ggplot(aes(x = date, y = us)) +
  geom_col(color = "firebrick4") +
  theme_minimal() +
  labs(y = "New Cases",
       x = "Date",
       title = "New cases by day in the U.S., 2020") +
  transition_time(as.Date(date)) +
  shadow_mark()

anim_save("new_cases.gif", r)



cases_plot %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  filter(date == "2020-11-03") %>%
  ggplot(aes(fill = cases, color = cases)) + 
  geom_sf(aes(geometry = geometry)) + 
  labs(title = "Covid Cases. By County",
       fill = "Cases") +
  theme_void() + 
  scale_colour_gradientn(colors = c("black","gray1", "red", "white", "white"),
                         values = c(0, 0.4, 0.9, 0.95, 1),
                         trans = "log") +
  scale_fill_gradientn(colors = c("black","gray1", "red", "white", "white"),
                       values = c(0, 0.4, 0.9, 0.95, 1),
                       trans = "log")
# transition_time(date)

gganimate(p, "cases.gif")


cases <- read_csv("covid_confirmed_usafacts.csv") %>%
  group_by(State) %>%
  rename(election_day = `11/3/20`,
         county = "County Name",
         state = State) %>%
  summarize(cases = sum(election_day))

read_rds("states.rds")

cases
popu <- read_csv("covid_county_population_usafacts.csv") %>%
  group_by(State) %>%
  rename(county = "County Name",
         state = State) %>%
  summarize(population = sum(population))

cases_per_cap_state <- inner_join(cases, popu, by = "state") %>%
  mutate(cases_per_100000 = 100000 * cases / population)

write_rds(cases_per_cap_state, "cases_state.rds")

averages <- read_csv("presidential_poll_averages_2020.csv", col_types = cols(
  cycle = col_double(),
  state = col_character(),
  modeldate = col_character(),
  candidate_name = col_character(),
  pct_estimate = col_double(),
  pct_trend_adjusted = col_double()
)) %>%
  filter(state == "Wyoming")

averages_new <- averages %>%
  filter(modeldate %in% c("8/3/2020", "11/3/2020")) %>%
  filter(candidate_name %in% c("Joseph R. Biden Jr.", "Donald Trump"))

averages

cases_plot2
  
  state_avgs <- read_csv("presidential_poll_averages_2020 copy.csv",
                         col_types = cols(
                           cycle = col_double(),
                           state = col_character(),
                           modeldate = col_character(),
                           candidate_name = col_character(),
                           pct_estimate = col_double(),
                           pct_trend_adjusted = col_double()
                         )
  )

natl_model <- read_csv("presidential_national_toplines_2020.csv", 
                       col_types = cols(
                         .default = col_double(),
                         branch = col_character(),
                         model = col_character(),
                         modeldate = col_character(),
                         candidate_inc = col_character(),
                         candidate_chal = col_character(),
                         candidate_3rd = col_logical(),
                         ecwin_3rd = col_logical(),
                         popwin_3rd = col_logical(),
                         ev_3rd = col_logical(),
                         ev_3rd_hi = col_logical(),
                         ev_3rd_lo = col_logical(),
                         national_voteshare_3rd = col_logical(),
                         national_voteshare_3rd_hi = col_logical(),
                         national_voteshare_3rd_lo = col_logical(),
                         timestamp = col_character()
                       ))

covid_approval <- read_csv("covid_approval_toplines.csv", col_types = cols(
  subject = col_character(),
  modeldate = col_character(),
  party = col_character(),
  approve_estimate = col_double(),
  disapprove_estimate = col_double(),
  timestamp = col_character()
))


inner_join(covid_approval, natl_model, by = "modeldate") %>%
  filter(party == "all") %>%
  select(modeldate, national_voteshare_inc, national_voteshare_chal,
         national_voteshare_inc_lo, national_voteshare_chal_lo,
         national_voteshare_inc_hi, national_voteshare_chal_hi,
         approve_estimate, disapprove_estimate) %>%
  pivot_longer(cols = c(national_voteshare_inc, national_voteshare_chal, 
                        approve_estimate),
               names_to = "poll_type",
               values_to = "estimate") %>%
  ggplot(aes(x = as.Date(modeldate, "%m/%d/%Y"), y = estimate, color = poll_type)) +
  geom_line(size = 1.3) +
  geom_ribbon(aes(ymin = national_voteshare_inc_lo, ymax = national_voteshare_inc_hi),
              fill = "red", alpha = 0.3, color = NA) +
  geom_ribbon(aes(ymin = national_voteshare_chal_lo, ymax = national_voteshare_chal_hi),
              fill = "blue", alpha = 0.3, color = NA) +
  scale_color_manual(breaks = c("approve_estimate", "national_voteshare_chal", 
                                "national_voteshare_inc"),
                     values = c("darkgreen", "blue", "red"),
                     labels = c("Trump Covid Approval", "Biden Nat'l Vote Share", "Trump Nat'l Vote Share"),
                     name = "Estimate") +
  theme_minimal() +
  labs(title = "Projected 2020 Nat'l Vote Shares Over Time",
       subtitle = "Compared with Trump's approval on Covid",
       y = "Percent",
       x = "Date") +
  geom_vline(xintercept = as.Date("9/22/20", "%m/%d/%y"),
             lty = "dashed",
             alpha = 0.5) +
  annotate("text",
           x = as.Date("10/10/20", "%m/%d/%y"),
           y = 58,
           label = "U.S. Deaths \n Surpass 200,000", size = 3)



counties_population <- get_decennial(geography = "county",
                                     variables = "P001001",
                                     year = 2010,
                                     geometry = TRUE, 
                                     shift_geo = TRUE) 

write_rds(counties_population, "counties.rds")

cases_plot <- cases_plot2 %>%
  select(date, name, state, geometry, value, cases)

write_rds(cases_plot, "cases_plot.rds")

read_dta("total_1.dta")



total <- read_dta("total_1.dta") %>%
  filter(extra_trump_corona != 999) %>%
  group_by(congress_district) %>%
  summarize(approval_covid = mean(extra_trump_corona %in% c(1, 2), na.rm = TRUE),
            mean_democrat = mean(pid3 == 1, na.rm = TRUE), 
            region = as.factor(round(mean(census_region, na.rm = TRUE))),
            number = n(),
            .groups = "drop") %>%
  slice(-1) %>%
  filter(approval_covid != 0)



write_rds(total_model, "total_filt.rds")

counties <- read_rds("counties.rds")

counties_cases <- 
  read_csv("covid_confirmed_usafacts.csv") %>%
  rename(name = "County Name",
         state = "State") %>%
  mutate(state = state.name[match(state, state.abb)]) %>%
  filter(name != "Statewide Unallocated")

counties_cases$NAME <- paste(counties_cases$name, counties_cases$state, sep=", ")

counties_cases$NAME[counties_cases$NAME == "Municipality of Anchorage, Alaska"] <- 
  "Anchorage Municipality, Alaska"
counties_cases$NAME[counties_cases$NAME == "City and Borough of Juneau, Alaska"] <- 
  "Juneau City and Borough, Alaska"
counties_cases$NAME[counties_cases$NAME == "Washington, NA"] <- 
  "District of Columbia, District of Columbia"
counties_cases$NAME[counties_cases$NAME == "Baltimore City, Maryland"] <- 
  "Baltimore city, Maryland"
counties_cases$NAME[counties_cases$NAME == "Lac Qui Parle County, Minnesota"] <- 
  "Lac qui Parle County, Minnesota"
counties_cases$NAME[counties_cases$NAME == "Jackson County (including other portions of Kansas City), Missouri"] <- 
  "Jackson County, Missouri"
counties_cases$NAME[counties_cases$NAME == "City of St. Louis, Missouri"] <- 
  "St. Louis city, Missouri"
counties_cases$NAME[counties_cases$NAME == "Dona Ana County, New Mexico"] <- 
  "Do?a Ana County, New Mexico"
counties_cases$NAME[counties_cases$NAME == "Oglala Lakota County, South Dakota"] <- 
  "Shannon County, South Dakota"
counties_cases$NAME[counties_cases$NAME == "Matthews County, Virginia"] <- 
  "Mathews County, Virginia"
counties_cases$NAME[counties_cases$NAME == "Alexandria City, Virginia"] <- 
  "Alexandria city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Charlottesville City, Virginia"] <- 
  "Charlottesville city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Chesapeake City, Virginia"] <- 
  "Chesapeake city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Danville City, Virginia"] <- 
  "Danville city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Fredericksburg City, Virginia"] <- 
  "Fredericksburg city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Harrisonburg City, Virginia"] <- 
  "Harrisonburg city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Manassas City, Virginia"] <- 
  "Manassas city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Newport News City, Virginia"] <- 
  "Newport News city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Norfolk City, Virginia"] <- 
  "Norfolk city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Portsmouth city, Virginia"] <- 
  "Portsmouth city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Richmond City, Virginia"] <- 
  "Richmond city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Suffolk City, Virginia"] <- 
  "Suffolk city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Virginia Beach City, Virginia"] <- 
  "Virginia Beach city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Williamsburg City, Virginia"] <- 
  "Williamsburg city, Virginia"
counties_cases$NAME[counties_cases$NAME == "Broomfield County and City, Colorado"] <- 
  "Broomfield County, Colorado"
counties_cases$NAME[counties_cases$NAME == "Portsmouth City, Virginia"] <- 
  "Portsmouth city, Virginia"

county_cases <- inner_join(counties, counties_cases, by = "NAME") %>%
  select(-GEOID, -variable, -name, -state, -stateFIPS, -countyFIPS)

write_rds(county_cases, "county_cases.rds")


