# BalloonDataAnalysis
R scripts for analyzing and visualizing balloon data

R (https://www.r-project.org/) is a versatile scripting language made for statistical applications, data analysis, and visualization. To compare R to MATLAB would be like the difference between Python and C++.
This R package performs data parsing and anaylsis tools useful to the University of Maryland Balloon Payload Program (UMDBPP).

### Installation
Make sure R is installed, and open a new R session (console). 

Install the "devtools" package: 

`install.packages("devtools")`

Now you can use devtools to install BalloonDataAnalysis from GitHub: 

`devtools::install_github("UMDBPP/BalloonDataAnalysis")`

### Usage and Examples

In an R session, load the package using the following command: 

`library(BalloonDataAnalysis)`

In R, to view documentation and usage instructions of a specific command or package, use `help(name_of_command)` (or precede the command with a question mark `?name_of_command`). For example, to view all documentation for this package, run `help(package = "BalloonDataAnalysis")`, and to view documentation for a specific function once the package is loaded, either run `help(read.payload)`, or just `?read.payload`

The main function of BalloonDataAnalysis is `read.payload`. It imports data from the log formats of several payloads (currently LINK-TLM, CellTracker, APRS, and IRENE) into useable and standardized data structures in R, adjusts timestamps to local time, and also calculates ascent rates, ground speeds, and downrange distances for CellTracker logs. It also recalulates LINK-TLM ascent rates and ground speeds by callsign to improve accuracy. 

The function takes a string file path to the logfile, and also requires that you specify the source (payload) of the log, as well as the flight number, if it is not already in the name of the file. You can optionally specify start and end times, and `read.payload` will discard entries outside those times and return a truncated dataset.

In the case of LINK-TLM, whose logs can be either `.json` or `.txt`, either format will work.

For example, to import LINK-TLM data from NS57, run 

`NS57.Coord <- read.payload(logfile = "NS57LaunchData.txt", data_source = "LINK-TLM", flight_number = "NS57", start_time = "07:50:31", end_time = "09:27:34")`

### Functions

`BalloonDataAnalysis::read.payload(logfile, data_source, flight_number, flight_date = NULL, start_time = NULL, end_time = NULL, timezone = Sys.timezone())`

`BalloonDataAnalysis::join.interpolate(data_1, data_2, by = c("DateTime", "Data_Source", "Flight"), exclude = NULL)`

`BalloonDataAnalysis::map.points(location_data, zoom = 10)`
