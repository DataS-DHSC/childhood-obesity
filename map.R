# Map

makemap <- function() {
  # Packages required

  (library(sf)) # st_read function to read in the data
  (library(tm)) # removeWords function
  (library(fingertipsR)) # import in Fingertips data using R
  (library(tidyverse)) # data tidying (filter, select, rename)
  (library(viridis)) # map colour
  (library(viridisLite)) #map colour

  # This is also available to download here: https://opendata.arcgis.com/datasets/62774ee52e08477f8b65c8b2f15cecb8_0.geojson
  mapdata <- st_read("Counties_and_Unitary_Authorities_December_2018_Boundaries_EW_BUC.geojson", quiet = TRUE)

  # removing regions in Wales
  mapdata <- mapdata %>%
    filter(grepl("^[E].*", mapdata$ctyua18cd)) %>%    # Only Selecting 
    select(-ctyua18nmw)

  # Converting the Region names from factors to character values
  mapdata$ctyua18nm <- as.character(mapdata$ctyua18nm)

  # Removing "City of" and "County of" so the names match the  fingertips data
  mapdata$ctyua18nm <- removeWords(mapdata$ctyua18nm, ", City of")
  mapdata$ctyua18nm <- removeWords(mapdata$ctyua18nm, ", County of")

  # Importing the Year 6 2017/17 Childhood Obesity Data from Fingertips

  Y6Obesity <- fingertips_data(IndicatorID = 90323, AreaTypeID = 102) %>%
    filter(AreaType == "County & UA", Timeperiod == "2017/18") %>%
    select(AreaName, Value) %>%
    rename(ctyua18nm = AreaName)
  # Removing (Cty) from Dorset
  Y6Obesity[131, 1] <- "Dorset"

  # Rounding the Data
  Y6Obesity[2] <- round(Y6Obesity[2], 10)

  # Joining the Map Data and the Y6 Obesity Data
  mapdata2 <- left_join(mapdata, Y6Obesity, by = "ctyua18nm")

  # Generating the plot
  ggplotly(
    ggplot(data = mapdata2, aes(fill = Value, text = mapdata$ctyua18nm)) +
      geom_sf(alpha = 0.8, colour = "white", size = 0.3) +
      scale_fill_viridis(discrete = F, name = "Childhood Obesity (%)", direction = -1) +
      labs(x = NULL, y = NULL, title = "Regional Variation in Year 6 Childhood Obesity Values 2017/18") +
      theme(line = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.background = element_blank())
  )
}
