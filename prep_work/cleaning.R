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
library(readr)

# I'm reading in all the Nationscape data here. What a mess.

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


# Here I full_join all these together because I didn't really know how else to
# get them all in one place. It's messy, but it works. I select only some 
# variables because the data set is massive.

march <- full_join(ns319, ns326, by = c("state", "census_region", 
"extra_trump_corona", "trump_biden", "pid3", "congress_district", "news_sources_fox",
"deportation", "medicare_for_all", "raise_upper_tax",
"minwage", "environment", "weight")) %>%
  select(state, census_region, extra_trump_corona, trump_biden, pid3, congress_district, 
         news_sources_fox, extra_covid_close_schools, deportation, medicare_for_all, 
         raise_upper_tax, minwage, environment, weight)

april <- full_join(full_join(full_join(ns402, ns409, 
                                       by = c("state", 
                                              "census_region",
                                              "extra_trump_corona", 
                                              "trump_biden", 
                                              "pid3", 
                                              "congress_district", 
                                              "news_sources_fox", 
                                              "deportation", 
                                              "medicare_for_all", 
                                              "raise_upper_tax",
                                              "minwage", 
                                              "environment", 
                                              "weight")), 
                             full_join(ns416, ns423, 
                                       by = c("state", 
                                              "census_region",
                                              "extra_trump_corona", 
                                              "trump_biden", 
                                              "pid3", 
                                              "congress_district", 
                                              "news_sources_fox",
                                              "deportation", 
                                              "medicare_for_all", 
                                              "raise_upper_tax",
                                              "minwage", 
                                              "environment", 
                                              "weight")), 
                             by = c("state", 
                                    "census_region",
                                    "extra_trump_corona", 
                                    "trump_biden", 
                                    "pid3", 
                                    "congress_district", 
                                    "news_sources_fox",
                                    "deportation", 
                                    "medicare_for_all", 
                                    "raise_upper_tax",
                                    "minwage", 
                                    "environment", 
                                    "weight")), ns430, 
                   by = c("state", 
                          "census_region",
                          "extra_trump_corona", 
                          "trump_biden", 
                          "pid3", 
                          "congress_district", 
                          "news_sources_fox", 
                          "deportation", 
                          "medicare_for_all", 
                          "raise_upper_tax",
                          "minwage", 
                          "environment", 
                          "weight")) %>% 
  select(state, census_region, extra_trump_corona, trump_biden, pid3, congress_district, 
         news_sources_fox, extra_covid_close_schools, deportation, medicare_for_all, 
         raise_upper_tax, minwage, environment, weight)

may <- full_join(full_join(ns507, ns514, 
                           by = c("state", 
                                  "census_region",
                                  "extra_trump_corona", 
                                  "trump_biden", 
                                  "pid3", 
                                  "congress_district", 
                                  "news_sources_fox",
                                  "extra_covid_close_schools", 
                                  "deportation", 
                                  "medicare_for_all", 
                                  "raise_upper_tax",
                                  "minwage", 
                                  "environment", 
                                  "weight")), full_join(ns521, ns528, 
                                                        by = c("state", 
                                                               "census_region",
                                                               "extra_trump_corona", 
                                                               "trump_biden", 
                                                               "pid3", 
                                                               "congress_district", 
                                                               "news_sources_fox",
                                                               "extra_covid_close_schools", 
                                                               "deportation", 
                                                               "medicare_for_all", 
                                                               "raise_upper_tax",
                                                               "minwage", 
                                                               "environment", 
                                                               "weight")), 
                 by = c("state", 
                        "census_region",
                        "extra_trump_corona", 
                        "trump_biden", 
                        "pid3", 
                        "congress_district", 
                        "news_sources_fox",
                        "extra_covid_close_schools", 
                        "deportation", 
                        "medicare_for_all", 
                        "raise_upper_tax",
                        "minwage", 
                        "environment", 
                        "weight")) %>%
  select(state, census_region, extra_trump_corona, trump_biden, pid3, congress_district, 
         news_sources_fox, extra_covid_socialize_no_dist, extra_covid_close_schools, 
         deportation, medicare_for_all, raise_upper_tax, minwage, environment, weight)

june <- full_join(full_join(ns604, ns611, 
                            by = c("state", 
                                   "census_region",
                                   "extra_trump_corona", 
                                   "trump_biden", 
                                   "pid3", 
                                   "congress_district", 
                                   "news_sources_fox",
                                   "extra_covid_close_schools", 
                                   "deportation", 
                                   "medicare_for_all", 
                                   "raise_upper_tax",
                                   "minwage", 
                                   "environment", 
                                   "weight")), 
                  full_join(ns618, ns625, 
                            by = c("state", 
                                   "census_region",
                                   "extra_trump_corona", 
                                   "trump_biden", 
                                   "pid3", 
                                   "congress_district", 
                                   "news_sources_fox",
                                   "extra_covid_close_schools", 
                                   "deportation", 
                                   "medicare_for_all", 
                                   "raise_upper_tax",
                                   "minwage", 
                                   "environment", 
                                   "weight")), 
                  by = c("state", 
                         "census_region",
                         "extra_trump_corona", 
                         "trump_biden", 
                         "pid3", 
                         "congress_district", 
                         "news_sources_fox",
                         "extra_covid_close_schools", 
                         "deportation", 
                         "medicare_for_all", 
                         "raise_upper_tax",
                         "minwage", 
                         "environment", 
                         "weight")) %>%
  select(state, census_region, extra_trump_corona, trump_biden, pid3, congress_district, 
         news_sources_fox, extra_covid_close_schools, 
         deportation, medicare_for_all, raise_upper_tax, minwage, environment, weight)

# Writing my rds files early on so I can just read them when I need in this prep
# file or the other. I separate by month here because the Trump approval plot is
# an animation that depends on time changing.

write_rds(march, "march.rds")
write_rds(april, "april.rds")
write_rds(may, "may.rds")
write_rds(june, "june.rds")

total <- full_join(full_join(march, april,
                             by = c("state", 
                                    "census_region",
                                    "extra_trump_corona", 
                                    "trump_biden", 
                                    "pid3", 
                                    "congress_district", 
                                    "news_sources_fox",
                                    "extra_covid_close_schools", 
                                    "deportation", 
                                    "medicare_for_all", 
                                    "raise_upper_tax",
                                    "minwage", 
                                    "environment", 
                                    "weight")), 
                   full_join(may, june, 
                             by = c("state", 
                                    "census_region",
                                    "extra_trump_corona", 
                                    "trump_biden", 
                                    "pid3", 
                                    "congress_district", 
                                    "news_sources_fox",
                                    "extra_covid_close_schools", 
                                    "deportation", 
                                    "medicare_for_all", 
                                    "raise_upper_tax",
                                    "minwage", 
                                    "environment", 
                                    "weight")), 
                   by = c("state", 
                          "census_region",
                          "extra_trump_corona", 
                          "trump_biden", 
                          "pid3", 
                          "congress_district", 
                          "news_sources_fox",
                          "extra_covid_close_schools", 
                          "deportation", 
                          "medicare_for_all", 
                          "raise_upper_tax",
                          "minwage", 
                          "environment", 
                          "weight"))


# Here I read in the data for states' and counties' geometry -- crucial for
# getting the maps I'm using.

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

# Just saving these as rds files so I can access them without having to retrieve
# them from tidycensus.

write_rds(states_population, "states.rds")
write_rds(counties_population, "counties.rds")

# I prepare the data with geometry info to be joined.

pop <- states_population %>%
  rename(state = NAME) %>%
  mutate(state = state.abb[match(state, state.name)]) %>%
  filter(! GEOID %in% c(11, 72)) %>%
  select(state, geometry)

# Here I attempt to manually weight the data, by counting each person's support
# vote (which would otherwise be 1) as their weight, then summing all weights.
# I'm summing up the number of people who approved of Trump's handling of Covid
# and dividing by the number of responses, to get his approval percentage.

survey_march <- march %>%
  filter(extra_trump_corona != 999) %>%
  mutate(trump_covid = ifelse(extra_trump_corona %in% c(1,2), 1, 0)) %>%
  mutate(trump_covid_weights = trump_covid * weight) %>%
  group_by(state) %>%
  summarize(mean = sum(trump_covid_weights) / sum(weight), .groups = "drop") 

# I do this next step for each month individually because I want a graphic
# displaying the approval in different months. 

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

# Joining the results together with the month added to each for the animation.

survey <- full_join(full_join(survey_march, survey_april, 
                              by = c("state", "mean", "month")), 
                    full_join(survey_may, survey_june, 
                              by = c("state", "mean", "month")),
                    by = c("state", "mean", "month")) %>%
  filter(state != "DC")

# Here I finally save the gif of Trump's approval by state for use in the Shiny
# app! Had to try anim_save because the other functions I knew of weren't
# working.

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




# I'm making a modified, clean dataset for the correlation matrix.

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

# Getting rid of non-responses and also changing each variable into binary so
# that I can take the mean of a variable and it will give me the percent of
# people in the district who answered yes or some variant of yes.

total_new[total_new == 999] <- NA
total_new[total_new == 888] <- NA
total_new[total_new == 2] <- 0

total_new1 <- total_new %>%
  group_by(congress_district) %>%
  
  # These first two questions asked for levels of support, where 0 and 1 are
  # both some level of support (rather than non-support). The rest are just 0 or
  # 1, where 1 is support, hence the different methods.
  
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
  slice(-1) %>%
  select(-congress_district)

corre1 <- cor(total_new1, use = "complete.obs",
              method = c("pearson", "kendall", "spearman"))

# I Googled for some of these arguments. I wanted black numbers on the lower
# diagonal and colors on the top diagonal.

corrplot.mixed(corre1, 
               lower = "number", 
               upper = "color", 
               lower.col = "black", 
               order = "FPC", 
               number.cex = .7,
               cl.align = "r",
               tl.pos = "lt", diag = "u",
               tl.col = "black")

# I exported the above into a .png and put it into my app.




# I had to get a little crafty here, because I forgot some pivot_wider
# techniques. I'm basically trying to get point margins for both March and
# November from polling averages. This is for the polling averages plot.

march_3_biden <- 
  read_csv("raw_data/presidential_poll_averages_2020 copy.csv") %>%
  filter(modeldate == "3/3/2020") %>%
  rename(pct_march = "pct_estimate") %>%
  pivot_wider(names_from = "candidate_name",
              values_from = "pct_march") %>%
  slice(1:33) %>%
  rename(biden_mar = `Joseph R. Biden Jr.`) %>%
  select(state, modeldate, biden_mar)

# I replicate this technique for all further ones.

march_3_trump <- 
  read_csv("raw_data/presidential_poll_averages_2020 copy.csv") %>%
  filter(modeldate == "3/3/2020") %>%
  rename(pct_march = "pct_estimate") %>%
  pivot_wider(names_from = "candidate_name",
              values_from = "pct_march") %>%
  slice(34:66) %>%
  rename(trump_mar = `Donald Trump`) %>%
  select(state, modeldate, trump_mar)

march_3 <- inner_join(march_3_biden, march_3_trump, by = c("state", "modeldate"))

nov_3_biden <- read_csv("raw_data/presidential_poll_averages_2020 copy.csv") %>%
  filter(modeldate == "11/3/2020") %>%
  rename(pct_nov = "pct_estimate") %>%
  filter(candidate_name == "Joseph R. Biden Jr.") %>%
  pivot_wider(names_from = "candidate_name",
              values_from = "pct_nov") %>%
  rename(biden_nov = `Joseph R. Biden Jr.`) %>%
  select(state, modeldate, biden_nov)

nov_3_trump <- read_csv("raw_data/presidential_poll_averages_2020 copy.csv") %>%
  filter(modeldate == "11/3/2020") %>%
  rename(pct_nov = "pct_estimate") %>%
  filter(candidate_name == "Donald Trump") %>%
  pivot_wider(names_from = "candidate_name",
              values_from = "pct_nov") %>%
  rename(trump_nov = `Donald Trump`) %>%
  select(state, modeldate, trump_nov)

nov_3 <- inner_join(nov_3_biden, nov_3_trump, by = c("state", "modeldate"))

pct_changes <- inner_join(march_3, nov_3, by = "state")







# Here I combine county geometry data with cases data to visualize each county's
# situation. Later, I combine this data with 2016 and 2020 election results.

counties <- read_rds("shiny_app/clean_data/counties.rds")

counties_cases <- 
  read_csv("raw_data/covid_confirmed_usafacts.csv", col_types = cols(
    .default = col_double(),
    `County Name` = col_character(),
    State = col_character()
  )) %>%
  rename(name = "County Name",
         state = "State") %>%
  mutate(state = state.name[match(state, state.abb)]) %>%
  filter(name != "Statewide Unallocated")

# This was pretty brutal. Over 30 counties didn't align in the inner_join, so I
# had to manually change names for them to match. Really tough.

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
  select(-GEOID, -variable, -name, -state, -stateFIPS)

write_rds(county_cases, "county_cases.rds")

# Reading in county election results here.

county_results <- read_csv("raw_data/CountyResults2020.csv", col_types = cols(
  .default = col_character()
)) %>%
  slice(-1) %>%
  rename(biden = `Joseph R. Biden Jr.`,
         trump = `Donald J. Trump`,
         total = `Total Vote`) %>%
  mutate(countyFIPS = as.numeric(FIPS))

# Reading in 2016 results so I can get the shift for each county.

county2016 <- read_csv("raw_data/countyresults2016.csv", col_types = cols(
  .default = col_character()
)) %>%
  slice(-1) %>%
  rename(clinton_2016 = `Hillary Clinton`,
         trump_2016 = `Donald J. Trump`,
         total_2016 = `Total Vote`) %>%
  mutate(countyFIPS = as.numeric(FIPS),
         
         # Getting each candidate's totals in the form of percent.
         
         clinton_perc_2016 = as.numeric(clinton_2016) / as.numeric(total_2016),
         trump_perc_2016 = as.numeric(trump_2016) / as.numeric(total_2016),
         margin_2016 = clinton_perc_2016 - trump_perc_2016)

# Combining the election results data with the cases data, by county.

county_results_cases <- 
  inner_join(county_cases, county_results, by = "countyFIPS") %>%
  mutate(biden_perc_2020 = as.numeric(biden) / as.numeric(total),
         trump_perc_2020 = as.numeric(trump) / as.numeric(total),
         margin_2020 = biden_perc_2020 - trump_perc_2020,
         cases_per_100000 = 100000 * `11/3/20` / value) %>%
  inner_join(county2016, by = "countyFIPS") %>%
  select(NAME, value, margin_2016, margin_2020, `11/3/20`, cases_per_100000) %>%
  mutate(shift = margin_2020 - margin_2016)

write_rds(county_results_cases, "county_results_cases.rds")



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
  
  # I only care about the 2016 results and Trump and Clinton. There are other
  # years and other candidates in this. I ultimately didn't use this data but in
  # previous versions of the project I did, so I leave it here.
  
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






# Here I'm cleaning the Nationscape data for use in the scatterplot (the one in
# the model section)

total_model <- total %>%
  
  # Here I exclude non-responses to the question.
  
  filter(extra_trump_corona != 999) %>%
  group_by(congress_district) %>%
  
  # Here I add up the number of times a respondent said they strongly or
  # somewhat approve of Trump's handling, corresponds to 1 or 2.
  
  summarize(approval_covid = mean(extra_trump_corona %in% c(1, 2), 
                                  na.rm = TRUE),
            mean_democrat = mean(pid3 == 1, na.rm = TRUE), 
            
            # Region is numerical, so I take the mean, then set it to a factor
            # for later use.
            
            region = as.factor(round(mean(census_region, na.rm = TRUE))),
            number = n(),
            .groups = "drop") %>%
  slice(-1)

write_rds(total_model, "total_filt.rds")









# Here I fiddle around with county case count and population data to create the
# state case count animation.

counties_cases <- read_csv("raw_data/covid_confirmed_usafacts.csv")

counties_cases <- counties_cases %>%
  rename(name = "County Name",
         state = "State") 

# Adding a NAME column with counties' names and the state they're in.

counties_cases$NAME <- paste(counties_cases$name, counties_cases$state, sep=", ")

inner_join(counties_population, by = "NAME")

cases_plot <- counties_cases %>%
  pivot_longer(cols = "1/22/20":"11/3/20",
               values_to = "cases",
               names_to = "date")

cases <- cases_plot %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases),
            .groups = "drop") 

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








new_cases <- read_csv("raw_data/new_cases.csv")

# Here I create the new cases animation, the first one in the app.

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





approval <- read_csv("raw_data/covid_approval_toplines.csv", col_types = cols(
  subject = col_character(),
  modeldate = col_character(),
  party = col_character(),
  approve_estimate = col_double(),
  disapprove_estimate = col_double(),
  timestamp = col_character()
)
) 

write_rds(approval, "covid_approval.rds")

# Just experimenting with this approval data. I will use this in the app.

approval_all <- approval %>%
  pivot_wider(id_cols = "modeldate",
              values_from = "approve_estimate",
              names_from = "party") %>%
  select(modeldate, all)

natl_polls <- read_csv("raw_data/presidential_poll_averages_2020 copy.csv", 
                       col_types = cols(
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

# I exported into a .png and put this image into my app.











state_polls <- read_csv("presidential_poll_averages_2020 copy.csv", 
                        col_types = cols(
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




# Writing the files for the concern graphs. Not much cleaning to do, thankfully.

concern_economy <- read_csv("raw_data/covid_concern_toplines.csv", col_types = 
                              cols(
                                subject = col_character(),
                                modeldate = col_character(),
                                party = col_character(),
                                very_estimate = col_double(),
                                somewhat_estimate = col_double(),
                                not_very_estimate = col_double(),
                                not_at_all_estimate = col_double(),
                                timestamp = col_character()
                              )) %>%
  filter(subject == "concern-economy") %>%
  pivot_longer(cols = very_estimate:not_at_all_estimate,
               names_to = "type",
               values_to = "estimate") %>%
  select(modeldate, type, estimate) %>%
  mutate(modeldate = as.Date(modeldate, "%m/%d/%Y"))

write_rds(concern_economy, "concern_economy.rds")



concern_infected <- read_csv("covid_concern_toplines.csv", col_types = cols(
  subject = col_character(),
  modeldate = col_character(),
  party = col_character(),
  very_estimate = col_double(),
  somewhat_estimate = col_double(),
  not_very_estimate = col_double(),
  not_at_all_estimate = col_double(),
  timestamp = col_character()
)) %>%
  filter(subject == "concern-infected") %>%
  pivot_longer(cols = very_estimate:not_at_all_estimate,
               names_to = "type",
               values_to = "estimate") %>%
  select(modeldate, type, estimate) %>%
  mutate(modeldate = as.Date(modeldate, "%m/%d/%Y"))

write_rds(concern_infected, "concern_infected.rds")










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










