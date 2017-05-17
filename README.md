# BalloonDataAnalysis
`R` scripts for analyzing and visualizing balloon data

`R` (https://www.r-project.org/) is a versatile scripting language made for statistical applications, data management, and visualization.
This R package performs data parsing and anaylsis tools useful to the University of Maryland Balloon Payload Program (UMDBPP).

### Installation
Make sure `R` is installed, and open a new `R` session (console). 

Install `devtools` by running 

`install.packages("devtools")`,

then you can run 

`devtools::install_github("UMDBPP/BalloonDataAnalysis")`

 to install directly from GitHub.

### Usage

In an `R` session, load the package using

`library(BalloonDataAnalysis)`

In `R`, use `help()` to view documentation and usage instructions.

For instance, to see documentation on parsing payload data, type

`help(read.payload, package = "BalloonDataAnalysis")`

### Functions

`BalloonDataAnalysis::read.payload(logfile, data_source, flight_number, flight_date = NULL, start_time = NULL, end_time = NULL, timezone = Sys.timezone())`

`BalloonDataAnalysis::join.interpolate(data_1, data_2, by = c("DateTime", "Data_Source", "Flight"), exclude = NULL)`

`BalloonDataAnalysis::map.points(location_data, zoom = 10)`
