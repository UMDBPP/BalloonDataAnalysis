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

`help(read.payload)`

### Functions

BalloonDataAnalysis::read.payload

BalloonDataAnalysis::join.interpolate

BalloonDataAnalysis::map.points
