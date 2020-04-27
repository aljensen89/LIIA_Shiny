# LIIA_Shiny -- A `shiny` app to Accompany the LIIA REDCap Database

The LIIA `shiny` application provides a streamlined way to access and summarize data collected via the accompanying REDCap database. While REDCap offers a variety of download options, its ability to produce summary statistics or conditional datasets is limited.

![App Screenshot](figures/app_screenshot.PNG)

# Getting Started

To run the app locally, it is straightforward to initiate an instance of LIIA_Shiny using the 3 steps below (it is assumed that R is installed and an internet connection is available):

1) Open an R session
2) Make sure `shiny` is installed and loaded:

`if(!require(shiny)) install.packages('shiny')`  
`library(shiny)`

3) Run the code: 

`
runGitHub("aljensen89/LIIA_Shiny")
`

Note that the app is optimized for Google Chrome, and some features may be limited in other browsers. Additionally, the first time this is run on a new machine may take a minute to install the required packages.
