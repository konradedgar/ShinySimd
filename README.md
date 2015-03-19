# README
The following repo contains Shiny application developed for the purposes of illustrating use of Open/Linked Data architecture in publicly available indicators.

## Description
The application is fairly simple and was developed for the purposes of illustrating how the Open/Linked Data technologies can be used to rapidly develop analytical products concerned with use of publicly-available data.

## Architecture
As any other Shiny application, this application consists of the two files that source the publicly available data from the [Open Data Scotland](http://www.opendatascotland.org) website. 

### server.R
The `server.R` file sources the data from the [Open Data Scotland](http://www.opendatascotland.org) website using the following `SPARQL` query:

```
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

```

The data is subsequently translated into a data frame using the following code:

```
  ## Make the data table for SIMD
  dta.simd <- SPARQL(url = endpoint, query = query.simd, format = "csv")$results
  ### Clean the data frame
  dta.simd$dz_label <- gsub("Data Zone","", dta.simd$dz_label)
```

Shapefiles that are download from the SNS website are read:

```
  # Readings shapefiles (pass only file that ends with shp)
  shps.dzs2001 <- readShapeSpatial("./data/SG_DataZone_Bdry_2001.shp")

  ## Prepare the shapefile for the map
  shps.dzs2001 <- fortify(shps.dzs2001, region = "DZ_CODE")

  # Switch on the gclibpermit
  gpclibPermit()
```

As final step the quantiles are computed and the map is generated via the ggplot:

```
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
    ggplot(data = shps.dzs2001) +
      aes(long,lat,group=group, fill = as.factor(quantiles)) +
      geom_polygon() +
      coord_equal() +
    scale_fill_discrete(name="Quantile")
    # Theme defintion available in the source code
    
```

### ui.R
The `ui.R` has fairly straightforward structure and consists of the 
# Additional info
* The information on designing `SPARQL` is hosted on the [Open Data Scotland]() website.
* [Shiny Apps]() website provides the information on the development of web-based applications with use of the ShinyApps architecture
* Shape files can be sources from numerous sources, including [Scottish Neighbourhood Statistics](http://www.sns.gov.uk/), [National Records of Scotland](http://www.nrscotland.gov.uk/), [Open Data Scotland](http://www.opendatascotland.org/) as well as number of other websites. The app was contributed to Coursera.