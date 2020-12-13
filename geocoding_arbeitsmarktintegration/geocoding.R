am.integrations.daten <- read.csv("data/Hack4SocialGood_data.csv")
am.integrations.daten$adresse <- paste0(am.integrations.daten$Strasse, ", ", am.integrations.daten$Ort)
am.integrations.daten$adresse[am.integrations.daten$adresse == "NA, NA"] <- NA

## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler ; many thanks!

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)) | is.na(address))
    return(c(lon = NA, lat = NA))
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(c(lon = NA, lat = NA))
  )
  try(if(length(d) == 0) return(c(lon = NA, lat = NA)))
  return(c(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}
nominatim_osm <- Vectorize(nominatim_osm) # vectorizing the function to feed vector of addresses

am.integrations.daten$lon <- NA
am.integrations.daten$lat <- NA
for(i in 1:nrow(am.integrations.daten)) {
  try(am.integrations.daten[i, c("lon", "lat")] <- nominatim_osm(am.integrations.daten$adresse[i]))
}

write.csv(am.integrations.daten, "data/am.integration.csv")