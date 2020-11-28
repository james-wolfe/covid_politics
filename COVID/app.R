library(shiny)
library(haven)
library(tidyverse)
library(tidycensus)
library(shinythemes)
library(sf)
population <- read_rds("~/Projects/covid_politics/states.rds")
counties_population <- read_rds("~/Projects/covid_politics/counties.rds")
march <- read_rds("~/Projects/covid_politics/march.rds")
april <- read_rds("~/Projects/covid_politics/april.rds")
may <- read_rds("~/Projects/covid_politics/may.rds")
june <- read_rds("~/Projects/covid_politics/june.rds")
total <- read_rds("~/Projects/covid_politics/total_filt.rds")
counties_pop <- read_csv("~/Projects/covid_politics/covid_county_population_usafacts.csv")
counties_cases <- read_rds("~/Projects/covid_politics/county_cases.rds")

ui <- navbarPage(
    "Public Opinion, Now",
    tabPanel("Virus",
             fluidPage(theme = shinytheme("flatly"),
                       titlePanel("The Spread of COVID-19"),
                       fluidRow(
                           column(width = 6,
                                  img(src = "new_cases.gif")),
                           column(width = 6,
                                  img(src = "states_cases.gif"))
                       ),
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
                     
                       
             )),
    tabPanel("Public Opinion",
             fluidPage(
                 titlePanel("Public Opinion"),
                 mainPanel(
                     img(src = "trump_covid.gif")
                 ),
                 fluidRow(
                     column(width = 6,
                            img(src = "natlvote.png", width = 728, height = 457)
                 ))
             )
    ),
    tabPanel("Model",
             fluidPage(
                 titlePanel("Approval & Party ID"),
                 mainPanel(
                            plotOutput("ScatterPlot", height = 350,
                                       hover = hoverOpts(id = "plot_hover"))
                 )    
                 ),
                 fluidRow(
                     column(width = 5,
                            verbatimTextOutput("hover_info")
                     )
                 ),
                 fluidRow(
                     column(width = 5,
                            img(src = "correlation.png", width = 555, height = 504))
                 )
    ),
    tabPanel("Election",
             fluidPage(
                 titlePanel("The 2020 U.S. Election"),
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
             h3("Project Background and Motivations"),
             p("I'm getting my data from Nationscape, 
               a massive survey of Americans on social and political sentiment,
               and from the U.S. Census, through the tidycensus package. I hope
               to find some interesting results and correlations in the 
               Nationscape dataset and create some visualizations with tidycensus."),
             h3("About Me"),
             p("My name is James Wolfe and I study Mathematics. 
             You can reach me at jameswolfe@college.harvard.edu."),
             h3("Repo"),
             p("The link to my repo is 
               https://github.com/james-wolfe/FinalProject.")))


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
                   labs(title = "Covid cases per papita by county",
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
                   labs(title = "Covid cases by county",
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
                                    min.segment.length = 0.7) +
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
