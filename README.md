# Analysis and Prediction of Flight Delays at Chicago O'Hare International Airport (ORD)

![delay_pic](https://github.com/user-attachments/assets/68321052-e852-4bfe-a61a-37ec012811cb)


## Authors - Abhib Lal Amatya and Zain Shrestha



## Project Overview
This project analyzes 2023–2024 flight delays at Chicago O'Hare International Airport (ORD) for three major US airline operators - American Airlines (AA), Delta Airlines (DL), and United Airlines (UA) over reasons of the delay, major holidays, and distribution of delays by hour, weekday and month. In addition, it also explores the possibility of relationship between aircraft age and flight delays. Lastly, a prediction model based on 2023-2024 data attempts to predict flight delays on a particular date and time for 2025.

## Things to keep in mind
- The three biggest airlines in the U.S - American, United and Delta are chosen for this project to represent the nation's commercial flights since these airlines have a combined market share of 51.3% (Voronoi 2024 )
- Only flights departing out of ORD are analyzed
- AA - American Airlines, DL - Delta Airlines, UA - United Airlines

## Source of Data
- **Carrier and Delay Dataset**: Dataset filtered by three airlines between 2023 and 2024 at ORD and imported as a CSV file from Bureau of Transportation Statistics Airline On-Time Statistics. The CSV files were uploaded to and pulled from this Git repository for coding on R Studio
  
- <img width="1438" alt="Screenshot 2025-05-13 at 8 51 49 AM" src="https://github.com/user-attachments/assets/13ef3651-6ba1-4914-9a28-759ccdfdb29d" />
```  
  urls <- c(
  AA = "https://raw.githubusercontent.com/amatya02/ord_flight_delays/main/final_project/datasets/aa.csv",
  DL = "https://raw.githubusercontent.com/amatya02/ord_flight_delays/main/final_project/datasets/delta.csv",
  UA = "https://raw.githubusercontent.com/amatya02/ord_flight_delays/main/final_project/datasets/united.csv"
)
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
  

  
