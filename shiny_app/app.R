library(shiny)
library(haven)
library(tidyverse)
library(tidycensus)
library(shinythemes)
library(sf)
library(ggrepel)
library(gt)
library(gtsummary)
library(rstanarm)
library(broom.mixed)

# Here I'm reading in all my clean data. The data cleaning is done in the
# cleaning.R file. 

population <- read_rds("./clean_data/states.rds")
counties_population <- read_rds("./clean_data/counties.rds")
march <- read_rds("./clean_data/march.rds")
april <- read_rds("./clean_data/april.rds")
may <- read_rds("./clean_data/may.rds")
june <- read_rds("./clean_data/june.rds")
total <- read_rds("./clean_data/total_filt.rds")
counties_cases <- read_rds("./clean_data/county_cases.rds")
covid_approval <- read_rds("./clean_data/covid_approval.rds")
swing_state <- read_rds("./clean_data/swingstate_polls.rds")
economic_concern <- read_rds("./clean_data/concern_economy.rds")
results_cases <- read_rds("./clean_data/results_cases.rds")
infection_concern <- read_rds("./clean_data/concern_infected.rds")
county_results_cases <- read_rds("./clean_data/county_results_cases.rds")

# There is a warning that appears when this app runs and I simply cannot figure
# it out. I've Googled and fiddled around to no avail. 

ui <- navbarPage(
    "Covid & Politics",
    tabPanel("Virus",
             fluidPage(theme = shinytheme("flatly"),
                       titlePanel("The Spread of COVID-19 in the U.S."),
                       fluidRow(style = 'padding:30px;',
                           column(width = 7,
                                  
                                  # Inputting a gif of new cases. I had to
                                  # ensure all my images were in a folder called
                                  # www.
                                  
                                  img(src = "new_cases.gif", height = 420)),
                           column(width = 5,
                                  h3("Number of confirmed cases per day 
                                     increasing sharply"),
                                  p("As the animation on the left shows, the 
                                  number of cases per day has been increasing 
                                  sharply as of late. The first peak wave hit 
                                  almost 50,000 cases per day, and the second 
                                  peak hit about 75,000 cases per day, whereas 
                                  the new peak is reaching almost 200,000. But, 
                                  an important thing to note is that the graph
                                  shows the number of confirmed cases per day. 
                                  For the first few months of the pandemic, the 
                                  United States had very limited testing 
                                  capacity. It's estimated that a great deal of 
                                  cases were never recorded during those first 
                                  two peaks (and even that a great, but lesser, 
                                  numb34 of cases are still not being recorded 
                                  now). The number of cases that have been 
                                  missed is a subject of much debate.")
                                  )),
                       fluidRow(style = 'padding:30px;',
                            column(width = 5,
                                   h3("Most populous states have the most 
                                      cases"),
                                   p("New York's case count (and New Jersey's, 
                                   to a lesser extent) exploded toward the 
                                   beginning of the pandemic, but in the summer,
                                   California, Texas, and Florida flew by New 
                                   York's total case count. Also during the 
                                   summer, several states saw their curves 
                                   flatten, possibly in part due to more people 
                                   spending time outdoors. But the weather isn't 
                                   more hospitable in the summer in all states 
                                   -- Florida and Texas, for example, experience 
                                   dreadfully hot summers that force many 
                                   indoors, possibly worsening their spreads.")
                            ),
                            
                            # I wanted to give some space in between columns,
                            # since when the columns were right next to each
                            # other, sometimes they'd overlap when viewing. I'm
                            # not totally sure if this does anything, to be
                            # perfectly honest.
                            
                            column(width = 2),
                           column(width = 5,
                                  img(src = "states_cases.gif", height = 420)))
                       ),
             
             fluidRow(style = 'padding:30px;',
                      column(width = 12,
             h3("Number of cases per capita is another story"),
             p("As we can see in the graph below, some regions have been hit 
             particularly hard when looking at cases per 100,000 residents. 
             North Dakota and South Dakota, which have relatively low 
             populations, have extremely high numbers of cases per capita. Some 
             less populous counties in the South, too, have high numbers of 
             cases per capita. Adjust to choose which date you'd like to look 
             at, and whether you'd like to look at total cases or cases per 
             100,000 residents."))), 
                       sidebarPanel(
                           sliderInput("dateInput",
                                       "Dates:",
                                       
                                       # I figured it would be easier to put
                                       # both the sliders and the actual
                                       # variables in date format.
                                       
                                       min = as.Date("1/22/20", "%m/%d/%y"),
                                       max = as.Date("11/3/20", "%m/%d/%y"),
                                       value = as.Date("11/3/20", "%m/%d/%y"),
                                       timeFormat = "%m/%d/%y"),
                           selectInput(inputId = "percap", 
                                       label = ("Select one:"), 
                                       choices = list("Cases per 100,000", 
                                                      "Total Cases"), 
                                       selected = "Cases per 100,000")),
                       mainPanel(
                           plotOutput("VirusPlot")
                       )
                     
                       
             ),
    tabPanel("Public Opinion",
             fluidPage(
                 titlePanel("Public Opinion"),
                 p("Concern about getting coronavirus has fluctuated a great 
                 deal. As we can see from the graph below, few people were very 
                 concerned about being infected in mid-February, but by 
                 mid-April, a majority of the country was at least somewhat 
                 concerned. A slightly concerning sign is that despite seeing 
                 surges in people who are very concerned about infection when 
                 there are outbreaks, we see that concern subsides right after, 
                 even though the threat is still there. Concern may be a product 
                 of several things: raw case numbers, warnings from public 
                 health experts, and statements and actions from leaders like 
                 President Donald Trump, who repeatedly urged citizens to not 
                 worry about contracting COVID-19."),
                 
                 sidebarPanel(
                     selectInput(inputId = "concern", 
                                 label = ("Select one:"), 
                                 choices = list("Economy", 
                                                "Infection"), 
                                 selected = "Infection"),
                     dateInput(inputId = "datepub", 
                               label = "Choose end date:", 
                               value = "2020-06-15", 
                               min = "2020-02-20",
                               max = "2020-11-15", 
                               format = "yyyy-mm-dd")),
                 mainPanel(plotOutput("ConcernPlot")),
                 fluidRow(style = 'padding:30px;',
                          column(width = 7,
                                 img(src = "trump_covid.gif", height = 500)),
                          column(width = 4,
                                 HTML(
                                     paste(
                                         
                                         # Adding some extra space here, just
                                         # for aesthetic reasons.
                                         
                                         p(" "),'<br/>',
                                         p(" "),'<br/>' )),
                                 h3("Trump's response hasn't aged well"),
                                 p("Despite giving high approval ratings at the 
                                 beginning of the crisis, by June, Americans 
                                 largely did not approve of President Trump's
                                 coronavirus response, which has been widely 
                                 panned by public health experts. However, we 
                                 still notice partisan differences in approval 
                                 within this graph: more liberal states like 
                                 Vermont strongly disapprove of Trump's 
                                 response, while more conservative states like 
                                 Wyoming strongly approve.")
                          )),
                 fluidRow(style = 'padding:30px;',
                          column(width = 12,
                h3("Partisan differences in grading Trump's response"),
                 p("To investigate these aforementioned partisan differences, 
                 here I look at how people who identify as Democrats, 
                 Republicans, and Independents approve of Trump's handling of 
                 the coronavirus. Trump's approval from Republicans has been 
                 remarkably static, while he has seen steadily declining 
                 approval from Democrats, and a more slight decline from 
                 Independents."),
                 HTML(
                     paste(
                         p(" "),'<br/>')))),
                 mainPanel(
                            plotOutput("ApprovalPlot")
                            ),
                sidebarPanel(h4("Choose Groups"),
                            checkboxInput("toggleAll", label = "All", 
                                          value = FALSE),
                            checkboxInput("toggleDem", label = "Democrats", 
                                          value = TRUE),
                            checkboxInput("toggleRep", label = "Republicans", 
                                          value = TRUE),
                            checkboxInput("toggleInd", label = "Independents", 
                                          value = FALSE)),
                 
             )
    ),
    tabPanel("Model",
             fluidPage(
                 titlePanel("Partisanship & Covid"),
                 mainPanel(
                            plotOutput("ScatterPlot",
                                       
                                       # I had to Google for quite a while to
                                       # figure out how to add this hover
                                       # feature. I finally found something and
                                       # basically copied and pasted from Google
                                       # and changed up the names to match my
                                       # data and variables. Here I'm allowing
                                       # the user to hover their mouse over a
                                       # point and see which district it is.
                                       
                                       hover = hoverOpts(id = "plot_hover"))
                 ),
                 sidebarPanel(h3("Approval tracks closely with partisan 
                                 makeup"),
                              p("Looking now at congressional districts (hover 
                              your mouse over any point in the plot on the left 
                              to see which congressional district you are 
                              looking at), we see that  the proportion of 
                              Democrats in a district is highly predictive of 
                              the district's approval rating of the President on 
                              Covid. Even though districts have experienced 
                              wildly different Covid outbreaks, most seem to 
                              follow the trend pretty closely. I build a model 
                              describing this relationship in more detail in the 
                              next subsection.")),
                 fluidRow(style = 'padding:30px;',
                     column(width = 5,
                            verbatimTextOutput("hover_info")
                     )
                 ),
                 fluidRow(style = 'padding:30px;',
                          column(width = 7,
                                 h3("The Model"),
                                 p("On the right is a table representing my 
                                 predictive model correlating the partisan 
                                 makeup of a congressional district 
                                 (specifically, the percent of survey 
                                 respondents in the district who identify as 
                                 Democrats) with the percent of respondents in
                                 the district who approve of Trump's handling of
                                 the coronavirus. The Beta value for 
                                 (Intercept), 0.72, is the median of our 
                                 posterior distribution for the intercept, which 
                                 indicates that our model predicts a district 
                                 with no Democrats will have a 72% approval 
                                 rating on Trump's response to Covid. The Beta 
                                 value for mean_democrat, -0.66, is the median 
                                 of the posterior distribution for the slope. 
                                 It indicates that for an increase of 1% in 
                                 district's proportion of Democrats, the model 
                                 predicts the Trump Covid response approval of 
                                 that district will go down by 0.66%. 
                                 Alternatively, we can also interpret it simply 
                                 by saying we expect a district of all Democrats 
                                 to have a 6% approval rating of Trump's 
                                 response to Covid.")),
                          column(width = 5,
                                 
                                 # Here I add the table for my model -- had to
                                 # familiarize myself with gt_output.
                                 
                                 gt_output("model_table"))
                          ),
                 fluidRow(style = 'padding:30px;',
                    column(width = 5,
                           img(src = "correlation.png", height = 400)
                    ),
                    
                    # Adding some space between columns because for some reason
                    # they're overlapping a lot.
                    
                    column(width = 2
                    ),
                    column(width = 5,
                           h3("Correlations"),
                           p("On the left is a figure showing the correlations 
                           between different survey responses. Note that these 
                           are correlations between the percentages of people in 
                           the district who responded one way or another, rather 
                           than correlations between individual survey 
                           responses. We see the relationship between the 
                           percent of Democrats in a district and Trump's Covid 
                           approval is quite strongly negative, as detailed 
                           above. Democratic ideals, like a higher minimum wage,
                           stronger investments in the environment, and Medicare 
                           for All, generally correlate with the number of 
                           Democrats in a district and each other. Support for 
                           mass deportation is highly correlated with Trump's 
                           approval on Covid. However, there are some murkier 
                           correlations, or lack thereof, that are less 
                           intuitive. The number of people in a district who 
                           watch Fox News isn't very correlated with the number 
                           of people who support Medicare for All or investments
                           in the environment, for example."))
                    
                 ),
                 
                 # Creating a reference for my variables in the correlation
                 # matrix. They are not totally clear just from the names.
                 
                 fluidRow(style = "background-color:#CCCCCC",
                          column(width = 6,
                                 h3("Variable Reference"),
                                 HTML(
                                     paste(
                                         p(strong("perc_dem"), " = Percent of 
                                           district that identifies as 
                                           Democratic"),
                                         p(strong("minwage"), " = Percent of 
                                           district that supports a $15 minimum 
                                           wage"),
                                         p(strong("environment"), " = Percent of 
                                           district that supports large 
                                           financial investment in the 
                                           environment"),
                                         p(strong("medicare4all"), " = Percent 
                                           of district that supports Medicare 
                                           for All"),
                                         p(strong("close_school"), " = Percent 
                                           of district that supports closing 
                                           schools")
                                     )
                                 )),
                          column(width = 6,
                                 HTML(
                                     paste(
                                 p(" "),'<br/>',
                                 p(" "),'<br/>',
                                 p(strong("upper_tax"), " = Percent of district 
                                   that supports raising taxes on those with 
                                   incomes over $600,000"),
                                 p(strong("fox_news"), " = Percent of district 
                                   that has heard news about politics on Fox 
                                   News within last week"),
                                 p(strong("deportation"), " = Percent of 
                                   district that supports deporting all 
                                   undocumented immigrants"),
                                 p(strong("covid_trump"), " = Percent of 
                                   district that approves of Trump's handling of 
                                   the coronavirus")
                                     ))
                                 )
    ))),
    tabPanel("Election",
             fluidPage(
                 titlePanel("The 2020 U.S. Election"),
                 fluidRow(style = 'padding:30px;',
                     column(width = 7,
                            
                            # I figured just loading in an image would be easier
                            # than rendering a plot -- the less burden on the
                            # system to render plots at once, I figure, the
                            # better.
                            
                            img(src = "538avg.png", height = 420)),
                     column(width = 5,
                            h3("The Polls"),
                            p("Here we can see polling averages for Biden and 
                            Trump from about March on, compared with their 
                            actual vote margins. There is a noticeable shift 
                            from March onwards in Trump's support. Several 
                            theories have been peddled on why this polling shift
                            occured, and why Trump's support was underestimated 
                            in the end. Some have suggested that coronavirus 
                            might have made Democrats more likely to be at home 
                            and ready to answer surveys, due maybe to geographic 
                            and socioeconomic factors, while Republicans 
                            weren't. It's also possible that Trump supporters 
                            are somehow inherently less likely to answer a poll.
                            Of course, though Trump's support was 
                            underestimated, it's still very possible Trump lost 
                            votes because of his handling of the pandemic."))
                 ),
                 fluidRow(style = 'padding:30px;',
                     column(width = 7,
                            h3("The Election"),
                            p("On the right is a visualization of the 2020 
                            Presidential Election Results by state, where a red 
                            state indicates a Trump victory, and a blue state 
                            indicates a Biden victory, and light red and light 
                            blue indicate close Trump and Biden wins, 
                            respectively. The eight closest states -- Arizona, 
                            Florida, Georgia, Michigan, Nevada, North Carolina, 
                            Pennsylvania, and Wisconsin -- decided the outcome 
                            of the national election and were (for the most 
                            part) the center of attention for each campaign. 
                            Whether Covid and, specifically, Trump's handling of 
                            Covid impacted the outcomes of these states is a 
                            question crucial to understanding how this election 
                            unfolded.")),
                     column(width = 5,
                            
                            # Again, same thing. I loaded an image instead of
                            # rendering a plot here for the same reason.
                            
                            img(src = "results.png", height = 420))),
                 fluidRow(style = 'padding:30px;',
                          column(width = 12,
                                 h3("Swing States"),
                                 p("The plots below show, for each of the eight 
                                 aforementioned swing states, its margin 
                                 according to polling averages on March 3 and 
                                 November 3, and its actual margin for Biden. In
                                 some cases, like in Michigan, the polling 
                                 average in March, before coronavirus really 
                                 took root in the U.S., was quite close to the 
                                 actual margin of victory for Biden. In fact, in
                                 the majority of these states, the polling 
                                 average in November was not distinctly more 
                                 accurate than the polling average in March, 
                                 which is probably not to be expected. The 
                                 further out from an election a poll is, the 
                                   less accurate it should (theoretically) be.")
                                 )),
                 sidebarPanel(
                     selectInput(inputId = "swing", 
                                 label = ("Select swing state:"), 
                                 choices = list("Arizona" = "AZ", 
                                                "Florida" = "FL",
                                                "Georgia" = "GA",
                                                "Michigan" = "MI",
                                                "Nevada" = "NV",
                                                "North Carolina" = "NC",
                                                "Pennsylvania" = "PA",
                                                "Wisconsin" = "WI"), 
                                 selected = "Arizona")),
                 mainPanel(
                     plotOutput("SwingStatePlot")
                 ),
                 
                 fluidRow(style = 'padding:30px;',
                          column(width = 12,
                                 h3("Covid Cases and Election Results"),
                                 p("The plots below show how the number of cases 
                                 per 100,000 residents in each county correlates 
                                 with that county's vote margin in 2020 and that
                                 county's shift in vote margin from 2016. I 
                                 create two more predictive models here. 
                                 Overall, we estimate that as the number of 
                                 cases per capita in a county goes up, a county 
                                 votes very slightly more Democratic. And yet, 
                                 our other model suggests that as a county's 
                                 cases per capita increases, it shifts more 
                                 Republican. This second model isn't very 
                                 intuitive: why did counties with higher cases 
                                 per capita shift more Republican than those 
                                 with lower cases per capita? The more technical 
                                 explanations for each variable mirror those of 
                                 the model in the last section, so I will just 
                                 interpret each variable a little more 
                                 informally here. First, we predict that a 
                                 theoretical county with 0 cases per 100,000 
                                 residents voted +32 points Republican and had a
                                 +1.3 point shift toward Democrats in 2020 from 
                                 2016. For each additional case per 100,000, we 
                                 expect this theoretical county's Democratic 
                                 point margin to go up by about 0.00006 and its 
                                 shift from 2016 to go down by about 0.0005. 
                                 This model, importantly, suggests that a county 
                                 with a very high number of Covid cases per 
                                 capita likely voted more Republican than it did
                                 in 2016."))),
                 mainPanel(
                     plotOutput("ElectionPlot")
                 ),
                 sidebarPanel(
                     selectInput(inputId = "shift", 
                                 label = ("Select your y variable:"), 
                                 choices = list("Democratic Shift from 2016" = 
                                                    "shift", 
                                                "2020 Democratic Margin" = 
                                                    "margin_2020"), 
                                 selected = "Democratic Shift"),
                     gt_output("model_table2")
                     )
             )),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background"),
             p("I sought to better understand the national political effects 
             of a virus that has ravaged the whole globe. I did this because I 
             am deeply interested in U.S. politics, but I recognize and want to 
             reinforce that the scope of Covid's effects extends far beyond U.S. 
             politics: its more serious effects are much more violent and 
             tragic. Cases and deaths are not just numbers or plots."),
             p(
             "I got survey data from Nationscape and FiveThirtyEight, 
             data on Covid from USAFacts, and data on elections from MIT."),
             h3("About Me"),
             p("My name is James Wolfe and I study Mathematics. 
             You can reach me at jameswolfe@college.harvard.edu."),
             h3("Repo"),
             p("The GitHub repository for this project can be found",
               a("here.",
             href = "https://github.com/james-wolfe/covid_politics"))))


server <- function(input, output, session){
    
    output$VirusPlot <- renderPlot({
        cases_plot <- counties_cases %>%
            
            # I originally tried doing pivot_longer in my data cleaning and then
            # loading the longer dataset into the app, but the longer dataset
            # was over 100 MB, so I figured I'd save my computer and GitHub the
            # space and pivot_longer within the app. It definitely makes for a
            # slower map, but it's manageable.
            
            pivot_longer(cols = "1/22/20":"11/3/20",
                         values_to = "cases",
                         names_to = "date")    
        
        # I know ifelse statements are probably not the neatest way to do
        # determine what is shown to the user based on their input, but I
        # figured it was relatively efficient, and at the time, it made a lot of
        # sense to me. Based on what they choose, I assign a variable and
        # display that variable after the ifelse.
        
        ifelse(input$percap == "Cases per 100,000",
               cases_output <- cases_plot %>%
                   
                   # I had to change the date here rather than earlier; for some
                   # reason I can't explain I kept running into an error when I
                   # set date to a date column earlier.
                   
                   mutate(date = as.Date(date, "%m/%d/%y")) %>%
                   filter(date == input$dateInput) %>%
                   ggplot(aes(fill = cases * 100000 / value, 
                              color = cases * 100000 / value)) + 
                   geom_sf(aes(geometry = geometry)) + 
                   labs(title = "Covid cases per 100,000 residents, by county",
                        fill = "Cases per \n 100,000") +
                   theme_void() + 
                   
                   # The top limit is just the max cases per 100,000 of any
                   # county.
                   
                   scale_colour_gradientn(colors = c("black", "red", "lightpink"),
                                          limits = c(0, 22986.02),
                                          values = c(0, 0.3, 1),
                                          
                                          # I just want one legend, since color
                                          # and fill are using the same color
                                          # gradient.
                                          
                                          guide = FALSE) +
                   scale_fill_gradientn(colors = c("black", "red", "lightpink"),
                                        limits = c(0, 22986.02),
                                        values = c(0, 0.3, 1)),
               cases_output <- cases_plot %>% 
                   mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
                   filter(date == input$dateInput) %>%
                   ggplot(aes(fill = cases, color = cases)) + 
                   geom_sf(aes(geometry = geometry)) + 
                   labs(title = "Total Covid cases, by county",
                        fill = "Cases") +
                   theme_void() + 
                   scale_colour_gradientn(colors = c("black", "firebrick4", "red", 
                                                     "lightpink", "white"),
                                          
                                          # The values are chosen somewhat
                                          # arbitrarily, just to show the
                                          # differentiation in colors.
                                          
                                          values = c(0, 0.5, 0.7, 0.99, 1),
                                          
                                          # This is an upper bound on cases in a
                                          # county.
                                          
                                          limits = c(0, 325000),
                                          
                                          # Regular log wasn't working with very
                                          # low case counts. I found
                                          # "pseudo_log," which worked well!
                                          
                                          trans = "pseudo_log",
                                          guide = FALSE) +
                   scale_fill_gradientn(colors = c("black", "firebrick4", "red", 
                                                   "lightpink", "white"),
                                        values = c(0, 0.5, 0.7, 0.99, 1),
                                        limits = c(0, 325000),
                                        breaks = c(0, 25000, 100000, 300000),
                                        labels = c(0, 25000, 100000, 300000),
                                        trans = "pseudo_log"))
        cases_output
        
    })
    
    output$ConcernPlot <- renderPlot({
        
        # Here I try if statements, which work well. I wasn't familiar. This is
        # for the selection tool allowing you to choose between the plot
        # demonstrating the concern people have felt about the economy and about
        # getting Covid.
        
        if(input$concern == "Infection")
               p <- infection_concern %>%
                   filter(modeldate <= input$datepub) %>%
                   ggplot(aes(x = modeldate, y = estimate, color = type)) +
                    geom_line()+
                theme_minimal() +
                scale_color_manual(values = c("not_at_all_estimate" = "navyblue",
                                              "not_very_estimate" = "blue",
                                              "somewhat_estimate" = "purple",
                                              "very_estimate" = "red"),
                                   labels = c("Not at all",
                                              "Not very",
                                              "Somewhat",
                                              "Very"),
                                   name = "Level of Concern") +
                labs(x = "Date",
                     y = "Percent",
                     title = "How concerned are you about being infected with Covid?",
                     subtitle = "Polling averages")
        if(input$concern == "Economy")
           p <- economic_concern %>%
            filter(modeldate <= input$datepub) %>%
            ggplot(aes(x = modeldate, y = estimate, color = type)) +
        geom_line() +
                theme_minimal() +
                scale_color_manual(values = c("not_at_all_estimate" = "navyblue",
                                              "not_very_estimate" = "blue",
                                              "somewhat_estimate" = "purple",
                                              "very_estimate" = "red"),
                                   labels = c("Not at all",
                                              "Not very",
                                              "Somewhat",
                                              "Very"),
                                   name = "Level of Concern") +
                labs(x = "Date",
                     y = "Percent",
                     title = "How concerned are you about the economy?",
                     subtitle = "Polling averages")
        
        p
    })
    
    output$ScatterPlot <- renderPlot({
        total %>%
            
            # Here's a slight change for the visualization, but not for the
            # model. Some districts with very few respondents had 0% approval. I
            # leave them out of the graph because they make it look a little
            # worse, but I leave them in the model. This means the slope in the
            # model is a little different, but just a tiny bit.
            
            filter(approval_covid != 0) %>%
            ggplot(aes(x = mean_democrat, y = approval_covid)) + 
            geom_point(aes(color = region, size = number),
                       alpha = 0.7) + 
            geom_smooth(method = lm, 
                        se = FALSE, 
                        formula = y ~ x, 
                        color = "royalblue", 
                        alpha = 0.8) +
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
        
    })
    
    output$ElectionPlot <- renderPlot({
        
        # This is the plot showing election results vs. cases per capita. The
        # user determines whether they want to look at 2020 results or shifts
        # from 2016.
        
        ifelse(input$shift == "shift",
               results_cases_plot <- 
                   county_results_cases %>%
                   filter(cases_per_100000 != 0) %>%
                   filter(!is.na(margin_2020)) %>%
                   ggplot(aes(x = cases_per_100000, 
                              
                              # Getting these values from a proportion to a
                              # percent.
                              
                              y = 100 * shift, 
                              color = 100 * margin_2020, 
                              size = value)) + 
                   geom_point(alpha = 0.7) +
                   geom_smooth(method = lm, se = FALSE, formula = y ~ x) +
                   scale_x_log10() +
                   scale_color_gradientn(colors = c("firebrick", "firebrick", 
                                                    "pink", "skyblue", 
                                                    "navyblue", "navyblue"),
                                         values = c(0, 0.2, 0.517622389, 
                                                    0.517622389000001, .8, 1),
                                         name = "Margin (D)") +
                   scale_size_continuous(name = "Population") +
                   theme_minimal() +
                   labs(x = "Cases per 100,000",
                        y = "Percent Shift to Democrats",
                        title = "Counties' Electoral Margin Shifts from 2016 vs. Cases per Capita"),
               results_cases_plot <- 
                   county_results_cases %>%
                   filter(cases_per_100000 != 0) %>%
                   filter(!is.na(margin_2020)) %>%
                   ggplot(aes(x = cases_per_100000,
                              y = 100 * margin_2020, 
                              color = 100 * margin_2020, 
                              size = value)) + 
                   geom_point(alpha = 0.7) +
                   geom_smooth(method = lm, se = FALSE, formula = y ~ x) +
                   scale_x_log10() +
                   scale_color_gradientn(colors = c("firebrick", "firebrick", 
                                                    "pink", "skyblue", 
                                                    "navyblue", "navyblue"),
                                         values = c(0, 0.2, 0.517622389, 
                                                    0.517622389000001, .8, 1),
                                         name = "Margin (D)") +
                   scale_size_continuous(name = "Population") +
                   theme_minimal() +
                   labs(x = "Cases per 100,000",
                        y = "Vote Margin (in Pct)",
                        title = "Counties' Vote Margins vs. Cases per Capita"))
        
        results_cases_plot
    })
    
    output$model_table <- render_gt({
        fit_model <- stan_glm(formula = approval_covid ~ mean_democrat,
                          refresh = 0,
                          data = total)
        
        # I had to use the broom.mixed library to get this to work -- kept
        # running into errors.
        
        tbl_regression(fit_model, intercept = TRUE) %>%
            as_gt() %>%
            tab_header(title = "Regression of Trump's Covid Approval",
                       subtitle = "The Connection between Party ID and Approval") %>%
            tab_source_note("Source: Democracy Fund + UCLA Nationscape")
    })
    
    output$model_table2 <- render_gt({
        
        # Here is the set of regression models for the last page.
        
        fit_model_1 <- stan_glm(formula = shift ~ cases_per_100000,
                                refresh = 0,
                                data = county_results_cases %>%
                                    filter(cases_per_100000 != 0) %>%
                                    filter(!is.na(margin_2020)) %>%
                                    mutate(shift = 100 * shift))
        
        fit_model_2 <- stan_glm(formula = margin_2020 ~ cases_per_100000,
                                refresh = 0,
                                data = county_results_cases %>%
                                    filter(cases_per_100000 != 0) %>%
                                    filter(!is.na(margin_2020)) %>%
                                    mutate(margin_2020 = 100 * margin_2020))
        
        ifelse(input$shift == "shift",
               y <- tbl_regression(fit_model_1, 
                                   intercept = TRUE, 
                                   
                                   # A classmate on Slack posted this neat way
                                   # to get more digits -- very useful for me.
                                   
                                   estimate_fun = function(x)
                                        style_sigfig(x, digits = 5)) %>%
                   as_gt() %>%
                   tab_header(title = "Regression of Democratic Vote Shift",
                              subtitle = "How Predictive is Number of Cases per Capita?"),
               y <- tbl_regression(fit_model_2,  
                                   intercept = TRUE, 
                                   estimate_fun = function(x)
                                       style_sigfig(x, digits = 5)) %>%
                   as_gt() %>%
                   tab_header(title = "Regression of 2020 County Vote Margins",
                              subtitle = "How Predictive is Number of Cases per Capita?"))
        y
        
    })
    
    output$SwingStatePlot <- renderPlot({
        
        swing_state %>%
            filter(state == input$swing) %>%
            ggplot(aes(x = type, y = as.numeric(value), fill = positive)) +
            geom_col() +
            scale_fill_manual(breaks = c(TRUE, FALSE),
                              values = c("navyblue", "firebrick")) +
            scale_x_discrete(labels = c("Polling Avg \n in March",
                                        "Polling Avg \n in Nov.",
                                        "Actual Margin")) +
            labs(x = "Measure", 
                 y = "Biden Margin (Percent)", 
                 title = "Swing State Polling Averages \n Compared to 2020 Election Outcomes") +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_y_continuous(labels = function(x) paste0(x, '%'))
        
    })
    
    output$ApprovalPlot <- renderPlot({
        
        p <- covid_approval %>% 
            ggplot(aes(x = as.Date(modeldate, "%m/%d/%Y"), y = all))
        
        # I figured manually adding lines was the easiest way to go about this.
        # I think it turned out okay, even though the y-axis shifts depending on
        # inputs (which I kind of like). I let users look at approval for any
        # group they select.
        
        if(input$toggleDem)
            p <- p + geom_line(aes(x = as.Date(modeldate, "%m/%d/%Y"),
                                   y = D),
                               color = "navyblue", 
                               size = 1.2) +
                geom_text(x = as.Date("10/1/2020", "%m/%d/%Y"),
                          y = 14,
                          label = "Democrats",
                          color = "navyblue")
        
        if(input$toggleRep)
            p <- p + geom_line(aes(x = as.Date(modeldate, "%m/%d/%Y"),
                                   y = R),
                               color = "firebrick", 
                               size = 1.2) +
                geom_text(x = as.Date("4/22/2020", "%m/%d/%Y"),
                          y = 81.2,
                          label = "Republicans",
                          color = "firebrick")
        
        if(input$toggleInd)
            p <- p + geom_line(aes(x = as.Date(modeldate, "%m/%d/%Y"),
                                   y = I),
                               color = "darkgray",
                               size = 1.2) +
                geom_text(x = as.Date("4/22/2020", "%m/%d/%Y"),
                          y = 36,
                          label = "Independents",
                          color = "darkgray")
        
        if(input$toggleAll)
            p <- p + geom_line(aes(x = as.Date(modeldate, "%m/%d/%Y"),
                                   y = all),
                               color = "forestgreen",
                               size = 1.2) +
                geom_text(x = as.Date("10/1/2020", "%m/%d/%Y"),
                          y = 43,
                          label = "All",
                          color = "forestgreen")
        
        p +
            labs(x = "Date",
                 y = "Percent",
                 title = "Trump Approval on Covid by Party",
                 subtitle = "Partisan differences are significant") +
            theme_minimal()
        
    })
    
    output$hover_info <- renderPrint({
        
        # Again, this hover stuff is largely taken from Google, with names
        # changed to my data. I'm incredibly grateful to the person who posted
        # this.
        
        if(!is.null(input$plot_hover)){
            hover = input$plot_hover
            dist = sqrt((hover$x - total$mean_democrat)^2 + 
                            (hover$y - total$approval_covid)^2)
            cat("Congressional District \n")
            if(min(dist) < 3)
                total$congress_district[which.min(dist)]
        }
    })
}


shinyApp(ui = ui, server = server)
