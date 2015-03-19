require(shiny); require(shinythemes)

# Define list of options
choices.list <- list("Overall Rank" = "overall_rank",
                     "Income" = "income_deprivation_rank",
                     "Employment" = "employment_deprivation_rank",
                     "Health" = "health_deprivation_rank",
                     "Education" = "education_deprivation_rank",
                     "Access to Services" = "access_deprivation_rank",
                     "Housing" = "housing_deprivation_rank",
                     "Crime" = "crime_deprivation_rank"
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("flatly"),

                  # Application title
                  titlePanel("Scottish Index of Multiple Deprivation 2012 Open Data Shiny App"),

                  # Sidebar with a slider input for the number of bins
                  sidebarLayout(
                      sidebarPanel(
                          h4("Analysis"),
                          selectInput(inputId = "firstvar",
                                      label = h5("Index domain to map"),
                                      choices = choices.list,
                                      selected = "income_deprivation_rank"),
                          sliderInput("quannum", label = h5("Number of quantiles"),
                                      min = 0,
                                      max = 50,
                                      value = 10),
                          radioButtons("radioanalysis", label = h5("Table Type"),
                                       choices = list("Group Min/Max" = 1,
                                                      "Linear Regression" = 2),
                                       selected = 1),
                          tags$p(HTML("<p style='font-size:10px'>Summary table shows the min/max values for the generated quantiles. Linear regression shows the results of regression model on quantile variable and the remaining SIMD indicators.")),
                          h4("Aesthetics"),
                          sliderInput("polborder", label = h5("Polygon border size"),
                                      min = 0, max = 1, value = 0.25),
                          selectInput(inputId = "polcolor",
                                      label = h5("Polygon colour"),
                                      choices = list("Black" = "black",
                                                     "Red" = "red",
                                                     "White" = "white",
                                                     "Green" = "green",
                                                     "Pink" = "pink"),
                                      selected = "black")
                      ),

                      # Show a plot of the generated distribution and table
                      mainPanel(
                          tabsetPanel(
                              tabPanel("Plot", plotOutput("distPlot")),
                              tabPanel("Table", tableOutput("sum.simd"))),
                          br(),
                          # Provide notes
                          h5("Notes"),
                          tags$p(HTML("<p style='font-size:10px'>The following application was developed for the educational purposes. The application sources the SIMD data from the <a href='http://www.opendatascotland.org/', target='_blank'>Open Data Scotland</a> website. The code for the app is available via <a href='https://github.com/konradedgar/ShinySimd', target='_blank'>GitHub</a>. The following application was also contributed to the <a href='https://www.coursera.org/specialization/jhudatascience/1?utm_medium=listingPage', target='_blank'>Data Science Specialisation</a> delivered via Coursera.<b>The app was developed for the demonstrative purposes only.</b></p>")),
                          tags$p(HTML("<p align='right', style='font-size:10px'>Konrad Zdeb @ <a href='https://culturedsoftwareandcuriousresearch.wordpress.com/', target='_blank'>Cultured Software and Curious Research</a></p>"))
                      )
                  )
))
