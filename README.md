# BalloonDataAnalysis
R scripts for analyzing and visualizing balloon data

R (https://www.r-project.org/) is a versatile scripting language made for statistical applications, data analysis, and visualization. To compare R to MATLAB would be like the difference between Python and C++.
This R package performs data parsing and anaylsis tools useful to the University of Maryland Balloon Payload Program (UMDBPP).

### Installation
Make sure R is installed, and open a new R session (console). 

Install the "devtools" package: `install.packages("devtools")`

Now you can use devtools to install BalloonDataAnalysis from GitHub: `devtools::install_github("UMDBPP/BalloonDataAnalysis")`

### Usage

In an R session, load the package: `library(BalloonDataAnalysis)`

In R, use `help()` to view documentation and usage instructions (or precede the commande with a question mark `?`).

To view all documentation for this package, run `help(package = "BalloonDataAnalysis")`

To view documentation for a specific function once the package is loaded, either run `help(read.payload)`, or just `?read.payload`

### Functions

`BalloonDataAnalysis::read.payload(logfile, data_source, flight_number, flight_date = NULL, start_time = NULL, end_time = NULL, timezone = Sys.timezone())`

`BalloonDataAnalysis::join.interpolate(data_1, data_2, by = c("DateTime", "Data_Source", "Flight"), exclude = NULL)`

`BalloonDataAnalysis::map.points(location_data, zoom = 10)`
