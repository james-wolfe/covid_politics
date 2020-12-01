library(shiny)
library(haven)
library(tidyverse)
library(tidycensus)
library(shinythemes)
library(sf)
library(ggrepel)
population <- read_rds("~/Projects/covid_politics/clean_data/states.rds")
counties_population <- read_rds("~/Projects/covid_politics/clean_data/counties.rds")
march <- read_rds("~/Projects/covid_politics/clean_data/march.rds")
april <- read_rds("~/Projects/covid_politics/clean_data/april.rds")
may <- read_rds("~/Projects/covid_politics/clean_data/may.rds")
june <- read_rds("~/Projects/covid_politics/clean_data/june.rds")
total <- read_rds("~/Projects/covid_politics/clean_data/total_filt.rds")
counties_cases <- read_rds("~/Projects/covid_politics/clean_data/county_cases.rds")
covid_approval <- read_rds("~/Projects/covid_politics/clean_data/covid_approval.rds")
swing_state <- read_rds("~/Projects/covid_politics/clean_data/swingstate_polls.rds")
economic_concern <- read_rds("~/Projects/covid_politics/clean_data/concern_economy.rds")
results_cases <- read_rds("~/Projects/covid_politics/clean_data/results_cases.rds")
infection_concern <- read_rds("~/Projects/covid_politics/clean_data/concern_infected.rds")

ui <- navbarPage(
    "Public Opinion, Now",
    tabPanel("Virus",
             fluidPage(theme = shinytheme("flatly"),
                       titlePanel("The Spread of COVID-19 in the U.S."),
                       fluidRow(style = 'padding:30px;',
                           column(width = 7,
                                  img(src = "new_cases.gif", height = 420)),
                           column(width = 5,
                                  h3("Number of confirmed cases per day increasing sharply"),
                                  p("As the animation on the left shows, the number of cases
                                    per day has been increasing sharply as of late. The first peak
                                    wave hit almost 50,000 cases per day, and the second peak hit
                                    about 75,000 cases per day, whereas the new peak is reaching 
                                    almost 200,000. But, an important thing 
                                    to note is that the graph shows the number of confirmed cases
                                    per day. For the first few months of the pandemic, the United
                                    States had very limited testing capacity. It's estimated that a 
                                    great deal of cases were never recorded during those first two peaks
                                    (and even that a great, but lesser, amount of cases are still not
                                    being recorded now). The number of cases that have been missed is a subject of
                                    much debate.")
                                  )),
                       fluidRow(style = 'padding:30px;',
                            column(width = 5,
                                   h3("Most populous states have the most cases"),
                                   p("New York's (and New Jersey's, to a lesser extent) case count 
                                     exploded toward the beginning of the pandemic, but in the summer,
                                     California, Texas, and Florida surpassed New York's total case count.
                                    Also during the summer, several states saw their curves flatten, 
                                     possibly in part due to more people spending time outdoors. But the weather
                                     isn't more hospitable in the summer in all states -- Florida and Texas, for example,
                                     experience dreadfully hot summers that force many indoors.")
                            ),
                            column(width = 2),
                           column(width = 5,
                                  img(src = "states_cases.gif", height = 420)))
                       ),
             h3("Number of cases per capita is another story"),
             p("As we can see in the graph below, some regions have been hit particularly hard
             when analyzing cases per 100,000 residents. North Dakota and South Dakota, which 
             have relatively low populations, have extremely high numbers of cases per capita. 
             Some less populous counties in the South, too, have high numbers of cases per capita.
               Adjust to choose which date you'd like to look at, and whether you'd 
               like to look at total cases or cases per 100,000 residents."),  
             p(" "),
                       sidebarPanel(
                           sliderInput("dateInput",
                                       "Dates:",
                                       min = as.Date("1/22/20", "%m/%d/%y"),
                                       max = as.Date("11/3/20", "%m/%d/%y"),
                                       value = as.Date("11/3/20", "%m/%d/%y"),
                                       timeFormat="%m/%d/%y"),
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
                 p("Concern about getting coronavirus has fluctuated a great deal.
                   As we can see from the graph below, few people were very concerned about 
                   being infected in mid-February, but by mid-April, a majority of the country was at
                   least somewhat concerned. A slightly concerning sign is that despite seeing surges
                   in people who are very concerned about infection when there are outbreaks, 
                   we see that concern subsides right after, even though the threat is still there.
                   Concern may be a product of several things: raw case numbers, warnings from public health
                   experts, and statements and actions from leaders like President Donald Trump, 
                   who repeatedly urged citizens to not worry about contracting COVID-19.
                   "),
                 
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
                                 h3("Trump's response hasn't aged well"),
                                 p("Despite seeing high approval ratings at the beginning of the crisis,
                                   by June, Americans largely did not approve of President Trump's
                                   coronavirus response, which has been widely panned by 
                                   public health experts. However, we still notice partisan differences in
                                   approval within this graph: more liberal states, like Vermont, strongly
                                   disapprove of Trump's response, while more conservative states, like Wyoming
                                   strongly approve.")
                          )),
                 h3("Partisan differences in grading Trump's response"),
                 p("To investigate these aforementioned partisan differences, here I look
                   at how people who identify as Democrats, Republicans, and Independents
                   approve of Trump's handling of the coronavirus. Trump's approval from Republicans 
                   has been remarkably static, while he has seen steadily declining approval from Democrats, and a
                   more slight decline from Independents."),
                 mainPanel(
                            plotOutput("ApprovalPlot")
                            ),
                sidebarPanel(h4("Choose Groups"),
                            checkboxInput("toggleAll", label = "All", value = FALSE),
                            checkboxInput("toggleDem", label = "Democrats", value = TRUE),
                            checkboxInput("toggleRep", label = "Republicans", value = TRUE),
                            checkboxInput("toggleInd", label = "Independents", value = FALSE)),
                 
             )
    ),
    tabPanel("Model",
             fluidPage(
                 titlePanel("Modeling Partisanship"),
                 mainPanel(
                            plotOutput("ScatterPlot",
                                       hover = hoverOpts(id = "plot_hover"))
                 ),
                 sidebarPanel(h3("Approval tracks closely with partisan makeup"),
                              p("Looking now at congressional districts, we see that 
                                the proportion of Democrats in a district is highly
                                predictive of the district's approval rating of the
                                President on Covid. Even though districts have experienced
                                wildly different Covid outbreaks, most seem to follow the trend
                                pretty closely. I build a model describing this relationship
                                in more detail in the next section. Put your mouse over any
                                point in the plot on the left to see which congressional district
                                you are looking at.")),
                 fluidRow(style = 'padding:30px;',
                     column(width = 5,
                            verbatimTextOutput("hover_info")
                     )
                 ),
                 fluidRow(style = 'padding:30px;',
                          width = 10,
                          h3("The Model"),
                          p(" ~ Stuff About the Model ~" )),
                 fluidRow(style = 'padding:30px;',
                    column(width = 7,
                           h3("Correlations"),
                           p("On the right is a figure showing the correlations between different
                             survey responses. ~ More stuff about the figure ~ ")),
                     column(width = 5,
                            img(src = "correlation.png", height = 400))
                 )
    )),
    tabPanel("Election",
             fluidPage(
                 titlePanel("The 2020 U.S. Election"),
                 fluidRow(style = 'padding:30px;',
                     column(width = 7,
                            img(src = "538avg.png", height = 420)),
                     column(width = 5,
                            h3("The Polls"),
                            p("Here we can see polling averages for Biden and Trump from about
                              March on, compared with their actual vote margins. There is a 
                              noticeable shift from March onwards in Trump's support.
                              Several theories have been peddled on why this polling shift occured,
                              and why Trump's support was underestimated in the end. The coronavirus
                              likely had something to do with it. "))
                 ),
                 fluidRow(style = 'padding:30px;',
                     column(width = 7,
                            h3("The Election"),
                            p("On the right is a depiction of the 2020 Presidential
                              Election Results by state, where a red state indicates a Trump
                              victory, and a blue state indicates a Biden victory, where light red
                              and light blue indicate close Trump and Biden wins, respectively.
                              The eight closest states -- Arizona, Florida, Georgia, Michigan, Nevada,
                              North Carolina, Pennsylvania, and Wisconsin -- decided the outcome of the 
                              national election and were (for the most part) the center of attention for 
                              each campaign. Whether Covid impacted the outcomes of these states
                              is a question crucial to understanding how this election went down.")),
                     column(width = 5,
                            img(src = "results.png", height = 420))),
                 sidebarPanel(
                     selectInput(inputId = "swing", 
                                 label = ("Select Swing State:"), 
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
                 sidebarPanel(
                     selectInput(inputId = "shift", 
                                          label = ("Select one:"), 
                                          choices = list("Democratic Shift" = "shift", 
                                                         "2020 Democratic Margin" = "margin_2020"), 
                                          selected = "Democratic Shift")),
                 mainPanel(
                     plotOutput("ElectionPlot")
                 ),
                 
             )),
    tabPanel("About", 
             titlePanel("About"),
             h3("Background"),
             p("I sought to better understand the United States' national political effects 
             of a virus that has ravaged the whole globe. I did this because I am
             deeply interested in U.S. politics, but I recognize and want to reinforce 
             that the scope of Covid's effects extends far beyond U.S. politics: its more serious
             effects are much more violent and tragic. Cases and deaths are not just numbers or plots."),
             p(
             "I got survey data from Nationscape and FiveThirtyEight, 
             data on Covid from USAFacts, and data on elections from MIT."),
             h3("About Me"),
             p("My name is James Wolfe and I study Mathematics. 
             You can reach me at jameswolfe@college.harvard.edu."),
             h3("Repo"),
             p("The link to my repo is 
               https://github.com/james-wolfe/covid_politics.")))


server <- function(input, output, session){
    
    output$VirusPlot <- renderPlot({
        cases_plot <- counties_cases %>%
            pivot_longer(cols = "1/22/20":"11/3/20",
                         values_to = "cases",
                         names_to = "date")    
        
        ifelse(input$percap == "Cases per 100,000",
               cases_output <- cases_plot %>%
                   mutate(date = as.Date(date, "%m/%d/%y")) %>%
                   filter(date == input$dateInput) %>%
                   ggplot(aes(fill = cases * 100000 / value, 
                              color = cases * 100000 / value)) + 
                   geom_sf(aes(geometry = geometry)) + 
                   labs(title = "Covid cases per 100,000 residents, by county",
                        fill = "Cases per \n 100,000") +
                   theme_void() + 
                   scale_colour_gradientn(colors = c("black", "red", "lightpink"),
                                          limits = c(0, 22986.02),
                                          values = c(0, 0.3, 1),
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
                                          values = c(0, 0.5, 0.7, 0.99, 1),
                                          limits = c(0, 325000),
                                          trans = "pseudo_log",
                                          guide = FALSE) +
                   scale_fill_gradientn(colors = c("black", "firebrick4", "red", 
                                                   "lightpink", "white"),
                                        values = c(0, 0.5, 0.7, 0.99, 1),
                                        limits = c(0, 325000),
                                        breaks = c(0, 25000, 100000, 300000),
                                        labels = c(0, 25000, "100000", "300000"),
                                        trans = "pseudo_log"))
        cases_output
        
    })
    
    output$ConcernPlot <- renderPlot({
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
        
    })
    
    output$ElectionPlot <- renderPlot({
        
        ifelse(input$shift == "shift",
               results_cases_plot <- 
                   results_cases %>%
                   ggplot(aes(x = cases_per_100000, 
                              y = shift, 
                              color = margin_2020,
                              size = population)) +
                   geom_point(alpha = 0.7) +
                   geom_text_repel(aes(label = state), 
                                    alpha = 1, 
                                    size = 3,
                                    color = "black",
                                    min.segment.length = 1) +
                   geom_smooth(method = lm, se = FALSE, formula = y ~ x) +
                   scale_color_gradientn(colors = c("red", "red", "pink", 
                                                    "lightblue", "blue", "blue"),
                                         values = c(0, 0.25, 0.333076899, 0.3330769, 
                                                    .4166, 1),
                                         breaks = c(-25, 0, 25, 50, 75),
                                         labels = c(-25, 0, 25, 50, 75)) +
                   scale_size_continuous(range = c(2, 8)) +
                   theme_minimal(),
               results_cases_plot <- 
                   results_cases %>%
                   ggplot(aes(x = cases_per_100000, 
                              y = margin_2020, 
                              color = margin_2020,
                              size = population)) +
                   geom_point(alpha = 0.7) +
                   geom_text_repel(aes(label = state), 
                                    alpha = 1, 
                                    size = 3,
                                    color = "black",
                                    min.segment.length = 0.8) +
                   geom_smooth(method = lm, se = FALSE, formula = y ~ x) +
                   scale_color_gradientn(colors = c("red", "red", "pink", "lightblue", "blue", "blue"),
                                         values = c(0, 0.25, 0.333076899, 0.3330769, .4166, 1),
                                         breaks = c(-25, 0, 25, 50, 75),
                                         labels = c(-25, 0, 25, 50, 75)) +
                   scale_size_continuous(range = c(2, 8)) +
                   theme_minimal())
        
        results_cases_plot
    })
    
    output$SwingStatePlot <- renderPlot({
        
        swing_state %>%
            filter(state == input$swing) %>%
            ggplot(aes(x = type, y = value, fill = positive)) +
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
            theme(legend.position = "none")
        
    })
    
    output$ApprovalPlot <- renderPlot({
        
        p <- covid_approval %>% 
            ggplot(aes(x = as.Date(modeldate, "%m/%d/%Y"), y = all))
        
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
        if(!is.null(input$plot_hover)){
            hover = input$plot_hover
            dist = sqrt((hover$x-total$mean_democrat)^2+(hover$y-total$approval_covid)^2)
            cat("Congressional District \n")
            if(min(dist) < 3)
                total$congress_district[which.min(dist)]
        }
    })
}


shinyApp(ui = ui, server = server)
