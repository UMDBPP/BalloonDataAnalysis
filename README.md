# BalloonDataAnalysis
R scripts for analyzing and visualizing balloon data

R (https://www.r-project.org/) is a versatile scripting language made for statistical applications, data management, and visualization.
This R package performs data parsing and anaylsis tools useful to the University of Maryland Balloon Payload Program (UMDBPP).

### Installation
This package is not currently on CRAN. 

In order to install you will need the "devtools" package:

`install.packages("devtools")`

After devtools installs you can then install from this repository: 

`devtools::install_github("UMDBPP/BalloonDataAnalysis")`

### Usage

Type a question mark before function names to view their documentation and usage instructions. For instance, to see documentation on parsing payload data, type

`?parsePayloadData`

### Functions

BalloonDataAnalysis::parsePayloadData
BalloonDataAnalysis::joinData_interpolate
BalloonDataAnalysis::plotmultiSeries_sameUnits
BalloonDataAnalysis::plotMultiSeries_differentUnits
BalloonDataAnalysis::coordinatePointsMap_static
BalloonDataAnalysis::coordinatePointsMap_html
