# Libs --------------------------------------------------------------------

require(shiny); require(SPARQL); require(ggplot2); require(rgeos); require(maptools); require(plyr);
require(RCurl); require(RColorBrewer); require(stringr); require(grid)

# Server ------------------------------------------------------------------

shinyServer(function(input, output) {

# Data Sourcing -----------------------------------------------------------

  ## Source the SPARQL data
  ### Define endpoint URL.
  endpoint <- "http://data.opendatascotland.org/sparql.csv"

  ### Create Query and download table for the SIMD rank
  query.simd <- "PREFIX stats: <http://statistics.data.gov.uk/id/statistical-geography/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX simd: <http://data.opendatascotland.org/def/simd/>
      PREFIX cube: <http://purl.org/linked-data/cube#>
      PREFIX stats_dim: <http://data.opendatascotland.org/def/statistical-dimensions/>
      PREFIX year: <http://reference.data.gov.uk/id/year/>

      SELECT DISTINCT
      ?dz_label
      ?overall_rank
      ?income_deprivation_rank
      ?employment_deprivation_rank
      ?health_deprivation_rank
      ?education_deprivation_rank
      ?access_deprivation_rank
      ?housing_deprivation_rank
      ?crime_deprivation_rank

      WHERE {

      GRAPH <http://data.opendatascotland.org/graph/simd/rank> {
      ?overall_rank_observation stats_dim:refArea ?dz .
      ?overall_rank_observation stats_dim:refPeriod year:2012 .
      ?overall_rank_observation simd:rank ?overall_rank .
      }

      GRAPH <http://data.opendatascotland.org/graph/simd/income-rank> {
      ?income_rank_observation stats_dim:refArea ?dz .
      ?income_rank_observation stats_dim:refPeriod year:2012 .
      ?income_rank_observation simd:incomeRank ?income_deprivation_rank .
      }

      GRAPH <http://data.opendatascotland.org/graph/simd/employment-rank> {
      ?employment_rank_observation stats_dim:refArea ?dz .
      ?employment_rank_observation stats_dim:refPeriod year:2012 .
      ?employment_rank_observation simd:employmentRank ?employment_deprivation_rank .
      }

      GRAPH <http://data.opendatascotland.org/graph/simd/health-rank> {
      ?health_rank_observation stats_dim:refArea ?dz .
      ?health_rank_observation stats_dim:refPeriod year:2012 .
      ?health_rank_observation simd:healthRank ?health_deprivation_rank .
      }

      GRAPH <http://data.opendatascotland.org/graph/simd/education-rank> {
      ?education_rank_observation stats_dim:refArea ?dz .
      ?education_rank_observation stats_dim:refPeriod year:2012 .
      ?education_rank_observation simd:educationRank ?education_deprivation_rank .
      }

      GRAPH <http://data.opendatascotland.org/graph/simd/geographic-access-rank> {
      ?access_rank_observation stats_dim:refArea ?dz .
      ?access_rank_observation stats_dim:refPeriod year:2012 .
      ?access_rank_observation simd:geographicAccessRank ?access_deprivation_rank .
      }

      GRAPH <http://data.opendatascotland.org/graph/simd/housing-rank> {
      ?housing_rank_observation stats_dim:refArea ?dz .
      ?housing_rank_observation stats_dim:refPeriod year:2012 .
      ?housing_rank_observation simd:housingRank ?housing_deprivation_rank .
      }

      GRAPH <http://data.opendatascotland.org/graph/simd/crime-rank> {
      ?crime_rank_observation stats_dim:refArea ?dz .
      ?crime_rank_observation stats_dim:refPeriod year:2012 .
      ?crime_rank_observation simd:crimeRank ?crime_deprivation_rank .
      }

    {
      SELECT ?dz ?dz_label WHERE
    {
      ?dz a <http://data.opendatascotland.org/def/geography/DataZone> .
      ?dz rdfs:label ?dz_label .
    }
    }
  }"

  ## Make the data table for SIMD
  dta.simd <- SPARQL(url = endpoint, query = query.simd, format = "csv")$results
  ### Clean the data frame
  dta.simd$dz_label <- gsub("Data Zone","", dta.simd$dz_label)

  # Readings shapefiles (pass only file that ends with shp)
  shps.dzs2001 <- readShapeSpatial("./data/SG_DataZone_Bdry_2001.shp")

  ## Prepare the shapefile for the map
  shps.dzs2001 <- fortify(shps.dzs2001, region = "DZ_CODE")

  # Switch on the gclibpermit
  gpclibPermit()


# Chart -------------------------------------------------------------------

# Make the plot
output$distPlot <- renderPlot({

    ### Get the column index fumber
    col.ind <- match(input$firstvar, colnames(dta.simd))

    #### Calculate the breaks
    ##### Take tha quantilies value from the UI
    dta.simd$column <- as.numeric(as.character(dta.simd[,col.ind]))
    ##### Obtain the breaks
    dta.simd$overall.quantiles <- ceiling(sapply(dta.simd$column,function(x)
      sum(x-dta.simd$column>=0))/(length(dta.simd$column)/input$quannum))

    #### Add breaks to the map data frame for th ease of plotting
    dta.simd$dz_label <- str_trim(dta.simd$dz_label, side = "both")
    shps.dzs2001$quantiles = dta.simd[match(shps.dzs2001$id, dta.simd$dz_label),"overall.quantiles"]

    ##### Draw the map
    ggplot(data = shps.dzs2001) +
        aes(long,lat,group=group, fill = as.factor(quantiles)) +
        # Take border size from input
        geom_polygon(size = input$polborder, colour = input$polcolor) +
        coord_equal(ratio=1) +
        scale_fill_discrete(name="Quantile") +
        theme(plot.title = element_text(size = 14, face="bold"),
            plot.background = element_rect(fill = 'white', colour = 'white'),
            panel.background = element_rect(fill = 'white'),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title=element_blank(),
            axis.ticks = element_blank(),
            legend.position="right",
            legend.title=element_text(size = 12, colour = "black", face = "bold"),
            legend.background = element_rect(colour = 'black', fill = 'white'),
            legend.key.width=unit(1,"cm"))
  }, height = 500)

# Analysis ----------------------------------------------------------------

# Generate summary table using the summarise function from the ddply package
output$sum.simd <- renderTable({
    ### Get the column index fumber
    col.ind <- match(input$firstvar, colnames(dta.simd))
    #### Calculate the breaks
    ##### Take tha quantilies value from the UI
    dta.simd$column <- as.numeric(as.character(dta.simd[,col.ind]))
    ##### Obtain the breaks
    dta.simd$overall.quantiles <- ceiling(sapply(dta.simd$column,function(x)
        sum(x-dta.simd$column>=0))/(length(dta.simd$column)/input$quannum))
    # Check what type of table to generate
    if(input$radioanalysis == 1) {
        # Generate summary table
        ddply(.data = dta.simd, .variables = .(overall.quantiles),
              summarize, Minimum=min(column), Maximum=max(column))
    } else
    {
        # Conduct regression results on quantiles in relation to other
        # Available indicators
        mod <- lm(formula = overall.quantiles ~ overall_rank +
                      income_deprivation_rank +
                      employment_deprivation_rank +
                      health_deprivation_rank +
                      education_deprivation_rank +
                      access_deprivation_rank +
                      housing_deprivation_rank +
                      crime_deprivation_rank, data = dta.simd)
    }
})
})