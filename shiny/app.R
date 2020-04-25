options(scipen = 999)

# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(lubridate)

Canada <- read.csv("Canada.csv")
Canada$Date <- ymd(Canada$Date)
Canada$prname <- as.character(Canada$prname)

Canada_grouped <- read.csv("Canada_grouped.csv")
Canada_grouped$Date <- ymd(Canada_grouped$Date)

df4 <- read.csv("df4.csv")
df4$Date <- ymd(df4$Date)
df4$country_region <- as.character(df4$country_region)

df5 <- read.csv("df5.csv")
df5$Date <- ymd(df5$Date)

pullDate <- read.table(header = FALSE, "pullDate.txt")

#Theme for plots
lightTheme =
    theme(
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "black", size = 0.2),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_blank(),
        legend.position="",
        text = element_text(size=14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

darkTheme =
theme(
        panel.border = element_blank(),
        panel.background = element_rect(fill = "grey10"),
        plot.background = element_rect(fill = "grey10"),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        plot.title = element_text(hjust = 0.5, color="grey90"),
        legend.title=element_blank(),
        legend.position="",
        legend.text = element_text(color="grey60"),
        legend.background = element_rect(fill="grey10"),
        text = element_text(size=14),
        axis.title.x = element_text(color="grey90"),
        axis.title.y = element_text(color="grey90"),
        axis.text.y = element_text(color="grey60"),
        axis.text.x = element_text(angle = 45, hjust = 1, color="grey60"),
        axis.line.x = element_line(color="grey30"),
        axis.line.y = element_line(color="grey30"))

ui <- fluidPage(theme = shinytheme("darkly"),


    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
               
    ),

    titlePanel("COVID-19 Cases and Deaths"),
    print(pullDate),
    p(),
    sidebarLayout(
        sidebarPanel(
            radioButtons("countryInput", "Country",
                         choices = c('Global', 'Canada (grouped by province/territory)', unique(df4$country_region)), 
                         selected = "Canada (separate plots)"),

            uiOutput("Province_List"),
            sliderInput("dates",
                        "Date range",
                        min = ymd("2020-01-22"),
                        max = ymd(Sys.Date()),
                        value = c(ymd(today() - 30), ymd(Sys.Date())),
                        step = 7,
                        timeFormat ='%B-%d')
        ),
        mainPanel(
            # Output: Three plots
            tabsetPanel(id="tabs", type = "tabs", selected = "Cumulative Cases",
                        tabPanel("Incident Cases", plotlyOutput("incPlot")),
                        tabPanel("Cumulative Cases", plotlyOutput("cumIncPlot")),
                        tabPanel("Cumulative Deaths", plotlyOutput("deathsPlot"))
                        
            ),
            hr(),
        fluidRow(
        column(width = 3,
               checkboxInput("standardize", "Standardize Per 100,000", FALSE)),
        column(width = 2,
               # conditionalPanel(
               # condition = "input.tabs != 'Incident Cases'",
               checkboxInput("scaleType", "Log Scale", FALSE)),
         column(width = 3,
                checkboxInput("darkLight", "Plot Darkmode", TRUE))
        ) 
        )
    ),
    hr(),
    HTML(print("<em>Note</em>. The reported numbers of cases underestimate the true numbers (substantially in some regions). Reported numbers depend on the availability of testing and do not account for the lag between infection and symptom onset, or between symptom onset and confirmed test results.")),
    p(),
    print("Canadian data provided by"),
    tags$a(href="https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html", "Public Health Agency of Canada"),
    p(),
    print("Global data provided by"),
    tags$a(href="https://github.com/CSSEGISandData/COVID-19", "Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"),
    p(),
    HTML("Developed by Dylan Ermacora <a href='https://github.com/Dermacora/COVID-19-Cases-and-Deaths/'><img src='GitHub-Mark-Light-32px.png' ></a>"),
    p(),
    p()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$Province_List <- renderUI({
        conditionalPanel(
            condition = "input.countryInput == 'Canada (separate plots)'",
            selectizeInput('provinceInput', 'Select a Province/Territory',
                           choices = c("All", "Alberta", "British Columbia", "Manitoba", "New Brunswick",
                                       "Newfoundland and Labrador", "Northwest Territories", "Nova Scotia", 
                                       "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"),
                           selected = "All"))
    })

    datasetInput = reactive({
        if(input$countryInput == 'Canada (separate plots)') {
            df4 %>%
                subset(., prname == input$provinceInput) %>%
                mutate(mycolour = NA, myshowlegend = FALSE, myLabel == "Canada")
        } else if(input$countryInput == 'Canada (grouped by province/territory)') {
            Canada_grouped %>%
                mutate(mysize = NA, myshowlegend = TRUE)
        } else if(input$countryInput == 'Global') {
            df5
        } else {
            df4 %>%
                subset(., country_region == input$countryInput) %>%
                mutate(mycolour = NA, myshowlegend = FALSE)
        }
        
    })
    
    output$incPlot <- renderPlotly({
        
        x <- datasetInput()
        
        if(input$standardize == 0) {
          x
        } else {
          x <- x %>% mutate(incident_cases = inc_per_100000)
        }
        
        inc_p <-
            x %>%
            filter(between(Date, ymd(input$dates[1]), ymd(input$dates[2]))) %>%
            ggplot(aes(y = incident_cases, x = Date, color = mycolour)) + 
            geom_line(size = 0.7) +
            geom_point(size = 1) +
            ggtitle(paste("Cases Reported /Day", unique(x$myLabel), sep = " ")) +
            labs(x = "Date", y = "New Cases Reported", color = "Country") + 
            scale_colour_brewer(palette="Paired", na.value="purple") +
            scale_x_date(date_breaks = "2 day", date_labels = "%b %d") #+
        
        if(input$scaleType == 0) {
          inc_p <- inc_p + scale_y_continuous(label=comma, breaks = extended_breaks(n = 8))
        } else {
          inc_p <- inc_p + scale_y_log10(label=comma)
        }
        
        if(input$darkLight == 1) {
          inc_p <- inc_p + darkTheme
        } else {
          inc_p <- inc_p + lightTheme
        }
        
        ggplotly(inc_p, tooltip = c("incident_cases", "Date")) %>% config(displayModeBar = F)  %>%
            layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) %>% layout(showlegend = unique(x$myshowlegend)) %>%
            layout(legend = list(font = list(size = 8, orientation = 'h', x = min(x$Date), y = max(x$incident_cases)))) %>%
            layout(legend = list(bgcolor = 'rgba(0, 0, 0, 0)', font=list(size = 8)))
    })
    

    output$cumIncPlot <- renderPlotly({
        
        x <- datasetInput()
        
        if(input$standardize == 0) {
          x
        } else {
          x <- x %>% mutate(cumulative_cases = infected_per_100000)
        }
        
        cum_inc_p <-        
            x %>%
            filter(between(Date, ymd(input$dates[1]), ymd(input$dates[2]))) %>%
            ggplot(aes(y = cumulative_cases, x = Date, color = mycolour)) +
            geom_line(size = 0.7)+
            geom_point(size = 1) +
            ggtitle(paste("Cumulative Cases", unique(x$myLabel), sep = " ")) +
            labs(x = "Date", y = "Cumulative Cases", color = "Country") + 
            scale_colour_brewer(palette="Paired", na.value="royalblue") +
            scale_x_date(date_breaks = "2 day", date_labels = "%b %d")
        
        if(input$scaleType == 0) {
            cum_inc_p <- cum_inc_p + scale_y_continuous(label=comma, breaks = extended_breaks(n = 8))
        } else {
            cum_inc_p <- cum_inc_p + scale_y_log10(label=comma)
        }
        
        if(input$darkLight == 1) {
          cum_inc_p <- cum_inc_p + darkTheme
        } else {
          cum_inc_p <- cum_inc_p + lightTheme
        }
        
        ggplotly(cum_inc_p, tooltip = c("cumulative_cases", "Date")) %>% config(displayModeBar = F)  %>%
            layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) %>% layout(showlegend = unique(x$myshowlegend)) %>%
            layout(legend = list(font = list(size = 8, orientation = 'h', x = min(x$Date), y = max(x$cumulative_cases)))) %>%
            layout(legend = list(bgcolor = 'rgba(0, 0, 0, 0)', font=list(size = 8)))
    })
    
    output$deathsPlot <- renderPlotly({ 
        
        x <- datasetInput()
        
        if(input$standardize == 0) {
            x
        } else {
            x <- x %>% mutate(deaths = deaths_per_100000)
        }
        
        deaths_p <-
            x %>%
            filter(between(Date, ymd(input$dates[1]), ymd(input$dates[2]))) %>%
            mutate(Date = date(Date)) %>%
            ggplot(aes(y = deaths, x = Date, color = mycolour)) + 
            geom_line(size = 0.7) +
            geom_point(size = 1) +
            ggtitle(paste("Cumulative Deaths", unique(x$myLabel), sep = " ")) +
            labs(x = "Date", y = "Cumulative Deaths", color = "Country") + 
            scale_colour_brewer(palette="Paired", na.value="firebrick") +
            scale_x_date(date_breaks = "2 days", date_labels = "%b %d") #+
        
        if(input$scaleType == 0) {
            deaths_p <- deaths_p + scale_y_continuous(label=comma, breaks = extended_breaks(n = 8))
        } else {
            deaths_p <- deaths_p + scale_y_log10(label=comma)
        }
        
        if(input$darkLight == 1) {
          deaths_p <- deaths_p + darkTheme
        } else {
          deaths_p <- deaths_p + lightTheme
        }
        
        ggplotly(deaths_p, tooltip = c("deaths", "Date")) %>% config(displayModeBar = F)  %>%
            layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) %>% layout(showlegend = unique(x$myshowlegend)) %>%
            layout(legend = list(font = list(size = 8, orientation = 'h', x = min(x$Date), y = max(x$deaths)))) %>%
            layout(legend = list(bgcolor = 'rgba(0, 0, 0, 0)', font=list(size = 8)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
