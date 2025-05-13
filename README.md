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
- **Carrier and Delay Dataset**: Dataset filtered by three airlines between 2023 and 2024 at ORD and imported as a CSV file from Bureau of Transportation Statistics Airline On-Time Statistics. The CSV files were uploaded to and pulled from a shared Git repository for coding on R Studio
  ```
  
- **Holiday Calenda**: Major US holidays extracted from NYSE/Federal holidays function in the timeDate R package
- **Aircraft Age**: Joined the dataset from nycflights13 R package and combined carrier and delay dataset via the aircraft registration code table to calculate aircraft age. Note that nycflight13 package only has data of flights flying out of NYC airports in 2013
  
