# Analysis and Prediction of Flight Delays at Chicago O'Hare International Airport (ORD)

![delay_pic](https://github.com/user-attachments/assets/68321052-e852-4bfe-a61a-37ec012811cb)


## Authors - Abhib Lal Amatya and Zain Shrestha



## Project Overview
This project analyzes 2023–2024 flight delays at Chicago O'Hare International Airport (ORD) for three major US airline operators - American Airlines (AA), Delta Airlines (DL), and United Airlines (UA) over reasons of the delay, major holidays, and distribution of delays by hour, weekday and month. In addition, it also explores the possibility of relationship between aircraft age and flight delays. Lastly, a prediction model based on 2023-2024 data attempts to predict flight delays on a particular date and time for 2025. All the analysis and visualizations are conviniently displayed on an interactive [Shiny Application](https://amatya02.shinyapps.io/Ohare_flight_delays/). 

## Things to keep in mind
- The three biggest airlines in the U.S - American, United and Delta are chosen for this project to represent the nation's commercial flights since these airlines have a combined market share of 51.3% (Voronoi 2024 )
- Only flights departing out of ORD are analyzed
- AA - American Airlines, DL - Delta Airlines, UA - United Airlines

## Source of Data
- **Carrier and Delay Dataset**: Dataset filtered by three airlines between 2023 and 2024 at ORD and imported as a CSV file from Bureau of Transportation Statistics Airline On-Time Statistics. The CSV files were uploaded to and pulled from this Git repository for coding on R Studio
  
- <img width="1438" alt="Screenshot 2025-05-13 at 8 51 49 AM" src="https://github.com/user-attachments/assets/13ef3651-6ba1-4914-9a28-759ccdfdb29d" />

- **Holiday Calendar**: Major US holidays extracted from NYSE/Federal holidays function in the timeDate R package
```
# Holiday lookup (major U.S. holidays)
years    <- unique(year(combined$FlightDate))
hol_funcs <- list(
  "New Year's Day"   = USNewYearsDay,
  "Memorial Day"     = USMemorialDay,
  "Independence Day" = USIndependenceDay,
  "Labor Day"        = USLaborDay,
  "Thanksgiving Day" = USThanksgivingDay,
  "Christmas Day"    = USChristmasDay
)
holiday_df <- imap_dfr(hol_funcs, ~{
  tibble(Date = as.Date(.x(years)), Holiday = .y)
})
```
- **Aircraft Age**: Joined the dataset from nycflights13 R package and combined carrier and delay dataset via the aircraft registration code table to calculate aircraft age. Note that nycflight13 package only has data of flights flying out of NYC airports in 2013
- ```
  # Join aircraft age info (rename ‘Tail Number’ to ‘tailnum’ first)
  combined_age <- combined %>%
    rename(tailnum = `Tail Number`) %>%
    filter(!is.na(tailnum)) %>%
    left_join(
      planes %>% select(tailnum, manufacturer, model, year),
      by = "tailnum"
    ) %>%
    mutate(
      # assume data ends in 2024
      aircraft_age = 2024 - year,
      age_group   = cut(
        aircraft_age,
        breaks = c(-1, 5, 10, 20, 50, Inf),
        labels = c("<5y", "5–10y", "10–20y", "20–50y", ">50y")
      )
    )
  ```

## The interactive Shiny application allows you to view:
- Delay counts by airline, day-of-week, month, and hour  
- Heatmaps for carrier vs. month & day vs. hour  
- Delay causes and total minutes lost  
- Delay patterns on major U.S. holidays  
- Aircraft-age insights (via `nycflights13::planes`)  
- Simple seasonal-average delay predictor for any date & hour in 2025
  
## Project Files
- `aa.csv`: Raw dataset of American Airlines flight data
- `delta.csv`: Raw dataset of Delta Airlines flight data
- `united.csv`: Raw dataset of United Airlines flight data
- `new_final.R`: R script for loading, cleaning, analyzing and building shiny app

## Requirements

Make sure the following R packages are installed before running the script
```
install.packages(c(
  "tidyverse",
  "here",
  "readr",
  "dplyr",
  "lubridate",
  "scales",
  "DT",
  "randomForest",
  "shiny",
  "hms",
  "timeDate",
  "tidymodels",
  "ranger",
  "nycflights13"
))
```
## Loading Data
```  
  urls <- c(
  AA = "https://raw.githubusercontent.com/amatya02/ord_flight_delays/main/final_project/datasets/aa.csv",
  DL = "https://raw.githubusercontent.com/amatya02/ord_flight_delays/main/final_project/datasets/delta.csv",
  UA = "https://raw.githubusercontent.com/amatya02/ord_flight_delays/main/final_project/datasets/united.csv"
)

```

## Tidying Data

```
# Read each, skip the first 7 rows, tag with carrier, then row-bind
raw <- imap_dfr(urls, ~
                  read_csv(.x, skip = 7) %>%
                  mutate(Carrier = .y)
)
# Tidy & feature‐engineer for charts
combined <- raw %>%
  rename(
    FlightDate      = `Date (MM/DD/YYYY)`,
    DepDelayMinutes = `Departure delay (Minutes)`
  ) %>%
  mutate(
    FlightDate = mdy(FlightDate),
    Month      = month(FlightDate,   label = TRUE, abbr = TRUE),
    DayOfWeek  = wday(FlightDate,    label = TRUE, abbr = TRUE, week_start = 1),
    Hour       = hour(as_hms(`Scheduled departure time`))
  ) %>%
  filter(DepDelayMinutes > 0)
  
  ```
## Clean Data Summary

![Screenshot 2025-05-14 at 8 04 28 AM](https://github.com/user-attachments/assets/f5d4df7d-2ea6-4930-b0fa-29a867af810d)

## Planes Data Summary 

`library(nycflights13)`

![Screenshot 2025-05-14 at 8 06 54 AM](https://github.com/user-attachments/assets/6d417607-6d9e-4a2b-bedb-599abe71fea5)

## Joining Two Tables by Tail Number
```
# Join aircraft age info (rename ‘Tail Number’ to ‘tailnum’ first)
combined_age <- combined %>%
  rename(tailnum = `Tail Number`) %>%
  filter(!is.na(tailnum)) %>%
  left_join(
    planes %>% select(tailnum, manufacturer, model, year),
    by = "tailnum"
  ) %>%
  mutate(
    # assume data ends in 2024
    aircraft_age = 2024 - year,
    age_group   = cut(
      aircraft_age,
      breaks = c(-1, 5, 10, 20, 50, Inf),
      labels = c("<5y", "5–10y", "10–20y", "20–50y", ">50y")
    )
  )
```
## Age portion of final data summary

<img width="716" alt="Screenshot 2025-05-14 at 8 12 02 AM" src="https://github.com/user-attachments/assets/7f1aa787-76df-4222-a6d9-a6ec962a2ea7" />

**Note**: All the `NA` values are those aircrafts that flew out of a NYC airport in 2013 but did not fly out of ORD in 2023-2024. The `nycflights13` package contains only those flights that flew out of NYC in 2013 . We could not find a similar dataset for ORD 2023-2024 so we still used NYC 2013 data to calculate aircraft age.

## Flight Delay Predictor
```
# Seasonal‐average table (2023 & 2024) by Month/Day/Hour
seasonal_tbl <- raw %>%
  rename(
    FlightDate      = `Date (MM/DD/YYYY)`,
    DepDelayMinutes = `Departure delay (Minutes)`
  ) %>%
  mutate(
    FlightDate = mdy(FlightDate),
    Year       = year(FlightDate),
    Month      = month(FlightDate),
    Day        = day(FlightDate),
    Hour       = hour(as_hms(`Scheduled departure time`))
  ) %>%
  filter(Year %in% c(2023, 2024)) %>%
  group_by(Month, Day, Hour) %>%
  summarize(AvgDelay = mean(DepDelayMinutes, na.rm = TRUE), .groups = "drop")

predict_delay <- function(date2025, hour2025) {
  d  <- ymd(date2025)
  m  <- month(d); dy <- day(d); hr <- hour2025
  out <- seasonal_tbl %>% filter(Month == m, Day == dy, Hour == hr) %>% pull(AvgDelay)
  if (length(out) == 0) out <- mean(seasonal_tbl$AvgDelay, na.rm = TRUE)
  round(out, 1)
}
```
## Building Shiny App

### UI
```
ui <- navbarPage(
  "Flight Delays at O'Hare International",
  
  tabPanel(
    "Home",
    titlePanel("Flight Delays at O'Hare International"),
    tags$p("This app analyzes 2023–2024 flight delays at Chicago O'Hare International Airport (ORD) for American Airlines (AA), Delta Airlines (DL), and United Airlines (UA) —showing when delays occur (by airline, time, cause, and holidays) via interactive charts—and uses those patterns to predict average delays by date and hour in 2025. It combines clear, tabbed visualizations with a simple seasonal‐average forecasting tool to reveal operational and seasonal factors affecting on-time performance."),
    tags$p("The objective is to understand seasonal and operational factors affecting delays and to build a date/hour predictor for 2025 at O'Hare."),
    fluidRow(
      column(12,
             tags$h4("Research Scope & Goals"),
             tags$p("\u2022 Describe delay distributions by airline, time, cause, and holidays."),
             tags$p("\u2022 Provide clear visualizations with explanations and aesthetics."),
             tags$p("\u2022 Offer a baseline forecasting method."),
             tags$h4("Requirements"),
             tags$ul(
               tags$li("Delay counts by airline, day, month, hour."),
               tags$li("Heatmaps: carrier vs month; day vs hour."),
               tags$li("Delay causes and minutes lost."),
               tags$li("Seasonal-average predictor."),
               tags$li("Documentation: scope, backlog, explanations.")),
             tags$h4("Backlog of Ideas"),
             tags$ul(
               tags$li("Interactive filters by carrier/month/hour."),
               tags$li("Add more advanced ML models."),
               tags$li("Map-based view of airport delays."),
               tags$li("Downloadable report module."))
      )
    )
  ),
  
  tabPanel(
    "Delay Counts",
    fluidRow(
      column(6, tags$h4("Number of Delays by Airline"), plotOutput("byAirline")),
      column(6, tags$h4("Delays by Day of Week (by Carrier)"), plotOutput("byDayCarrier"))
    ),
    fluidRow(
      column(6, tags$h4("Delays by Month (by Carrier)"), plotOutput("byMonthCarrier")),
      column(6, tags$h4("Total Delays by Month"), plotOutput("byMonth"))
    ),
    fluidRow(
      column(6, tags$h4("Total Delays by Day of Week"), plotOutput("byDay")),
      column(6, tags$h4("Total Delays by Hour"), plotOutput("byHour"))
    )
  ),
  
  tabPanel(
    "Heatmaps",
    fluidRow(
      column(6, tags$h4("Carrier vs Month Heatmap"), plotOutput("byCarrierMonthHeat")),
      column(6, tags$h4("Day vs Hour Heatmap"), plotOutput("byHourDayHeat"))
    )
  ),
  
  tabPanel(
    "Causes & Distribution",
    fluidRow(
      column(12, tags$h4("Hours Delayed by Cause (by Carrier)"), plotOutput("byDelayCause"))
    ),
    fluidRow(
      column(6, tags$h4("Delay Distribution by Carrier"), plotOutput("delayDistCarrier")),
      column(6, tags$h4("Average Delay by Hour per Carrier"), plotOutput("avgDelayHourCarrier"))
    ),
    fluidRow(
      column(12, tags$h4("Daily Delay Trend"), plotOutput("trendDaily"))
    )
  ),
  
  tabPanel(
    "Holidays",
    fluidRow(
      column(12, tags$h4("Delays on Major U.S. Holidays"), plotOutput("byHolidayCarrier"))
    )
  ),
  tabPanel(
    "Aircraft Age",
    fluidRow(
      column(
        8,
        tags$h4("Avg Departure Delay by Aircraft Age Group"),
        plotOutput("ageDelayPlot")
      ),
      column(
        4,
        tags$h5("Behind the scenes"),
        tags$p("Aircraft age is computed as 2024 minus the plane's manufacture year (from nycflights13::planes).")
      )
    )
  ),
  
  tabPanel(
    "Prediction",
    fluidRow(
      column(4,
             tags$h4("Predict Average Delay (min) for 2025"),
             dateInput("pred_date","Flight Date:",value="2025-01-01",min="2025-01-01",max="2025-12-31"),
             sliderInput("pred_hour","Departure Hour:",min=0,max=23,value=12,step=1)
      ),
      column(8,
             tags$h4("Predicted Average Delay"),
             verbatimTextOutput("pred_minutes")
      )
    )
  )
)
```
### Server
```
server <- function(input, output, session) {
  # 1) Delays by airline
  output$byAirline <- renderPlot({
    combined %>%
      count(Carrier) %>%
      ggplot(aes(x=Carrier, y=n, fill=Carrier)) +
      geom_col(show.legend=FALSE) +
      labs(title="Number of Delays by Airline", x="Carrier", y="Count") +
      theme_minimal()
  })
  
  # 2) Delays by day of week, by carrier
  output$byDayCarrier <- renderPlot({
    combined %>%
      count(Carrier, DayOfWeek) %>%
      ggplot(aes(x=DayOfWeek, y=n, fill=Carrier)) +
      geom_col(position="dodge") +
      labs(title="Delays by Day of Week (by Carrier)", x="Day", y="Count") +
      theme_minimal()
  })
  
  # 3) Delays by month, by carrier
  output$byMonthCarrier <- renderPlot({
    combined %>%
      count(Carrier, Month) %>%
      ggplot(aes(x=Month, y=n, fill=Carrier)) +
      geom_col(position="dodge") +
      labs(title="Delays by Month (by Carrier)", x="Month", y="Count") +
      theme_minimal()
  })
  
  # 4) Total delays by month
  output$byMonth <- renderPlot({
    combined %>%
      count(Month) %>%
      ggplot(aes(x=Month, y=n)) +
      geom_col(fill="#4C72B0") +
      labs(title="Total Delays by Month", x="Month", y="Count") +
      theme_minimal()
  })
  
  # 5) Total delays by day of week
  output$byDay <- renderPlot({
    combined %>%
      count(DayOfWeek) %>%
      ggplot(aes(x=DayOfWeek, y=n)) +
      geom_col(fill="#55A868") +
      labs(title="Total Delays by Day of Week", x="Day", y="Count") +
      theme_minimal()
  })
  
  # 6) Total delays by hour
  output$byHour <- renderPlot({
    combined %>%
      count(Hour) %>%
      ggplot(aes(x=Hour, y=n)) +
      geom_col(fill="#C44E52") +
      scale_x_continuous(breaks=0:23) +
      labs(title="Total Delays by Hour", x="Hour", y="Count") +
      theme_minimal()
  })
  
  # 7) Heatmap: carrier vs month
  output$byCarrierMonthHeat <- renderPlot({
    combined %>%
      count(Carrier, Month) %>%
      ggplot(aes(x=Month, y=Carrier, fill=n)) +
      geom_tile() +
      scale_fill_gradient(low="#F7FBFF", high="#08306B") +
      labs(title="Carrier vs Month Heatmap", x="Month", y="Carrier", fill="Count") +
      theme_minimal()
  })
  
  # 8) Heatmap: day vs hour
  output$byHourDayHeat <- renderPlot({
    combined %>%
      count(DayOfWeek, Hour) %>%
      ggplot(aes(x=Hour, y=DayOfWeek, fill=n)) +
      geom_tile() +
      scale_x_continuous(breaks=0:23) +
      scale_fill_gradient(low="#FFF7EC", high="#7F2704") +
      labs(title="Day vs Hour Heatmap", x="Hour", y="Day", fill="Count") +
      theme_minimal()
  })
  
  # 9) Delays by carrier on holidays
  output$byHolidayCarrier <- renderPlot({
    combined %>%
      inner_join(holiday_df, by=c("FlightDate"="Date")) %>%
      count(Carrier, Holiday) %>%
      ggplot(aes(x=Holiday, y=n, fill=Carrier)) +
      geom_col(position="dodge") +
      labs(title="Delays on Major U.S. Holidays", x="Holiday", y="Count") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1))
  })
  
  # 10) Delay causes & distribution
  output$byDelayCause <- renderPlot({
    combined %>%
      pivot_longer(cols=starts_with("Delay"), names_to="Cause", values_to="Min") %>%
      mutate(Cause=str_remove_all(Cause, "Delay | \\(Minutes\\)")) %>%
      group_by(Carrier, Cause) %>%
      summarise(TotalHours=sum(Min, na.rm=TRUE)/60, .groups="drop") %>%
      ggplot(aes(x=Cause, y=TotalHours, fill=Carrier)) +
      geom_col(position="dodge") +
      scale_y_continuous(labels=comma_format(accuracy=0.1)) +
      labs(title="Hours Delayed by Cause", x="Cause", y="Hours") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1))
  })
  
  # 11) Delay distribution boxplot
  output$delayDistCarrier <- renderPlot({
    combined %>%
      ggplot(aes(x=Carrier, y=DepDelayMinutes, fill=Carrier)) +
      geom_boxplot(show.legend=FALSE) +
      coord_cartesian(ylim=c(0,200)) +
      labs(title="Delay Distribution by Carrier", x="Carrier", y="Delay (min)") +
      theme_minimal()
  })
  
  # 12) Avg delay by hour per carrier
  output$avgDelayHourCarrier <- renderPlot({
    combined %>%
      group_by(Carrier, Hour) %>%
      summarise(AvgDelay=mean(DepDelayMinutes, na.rm=TRUE), .groups="drop") %>%
      ggplot(aes(x=Hour, y=AvgDelay, color=Carrier)) +
      geom_line(size=1) +
      scale_x_continuous(breaks=0:23) +
      labs(title="Avg Delay by Hour per Carrier", x="Hour", y="Avg Delay (min)") +
      theme_minimal()
  })
  
  # 13) Daily delay trend
  output$trendDaily <- renderPlot({
    combined %>%
      count(FlightDate) %>%
      ggplot(aes(x=FlightDate, y=n)) +
      geom_line(color="#4C72B0") +
      labs(title="Daily Delay Trend", x="Date", y="Count") +
      theme_minimal()
  })
  # 14) Avg delay by aircraft age group
  output$ageDelayPlot <- renderPlot({
    combined_age %>%
      group_by(age_group) %>%
      summarise(
        avg_delay = mean(DepDelayMinutes, na.rm = TRUE),
        count     = n(),
        .groups   = "drop"
      ) %>%
      ggplot(aes(x = age_group, y = avg_delay, fill = age_group)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(round(avg_delay,1), "m")), 
                vjust = -0.5, size = 3) +
      labs(
        x     = "Aircraft Age Group",
        y     = "Avg Departure Delay (min)",
        title = "Delay vs. Aircraft Age"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Prediction panel
  output$pred_minutes <- renderText({
    req(input$pred_date, input$pred_hour)
    paste0(predict_delay(input$pred_date, input$pred_hour), " minutes")
  })
}
```
## Running the App
```
shinyApp(ui, server)
```
## Click the link below to visit the interactive shiny app 

https://amatya02.shinyapps.io/Ohare_flight_delays/



  
