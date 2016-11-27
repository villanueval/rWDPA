#' Get the data available for a country in the ProtectedPlanet.net WDPA database
#'
#' This function returns the data for a country in the ProtectedPlanet database. The country is identified by its ISO code, which can be obtained using the function \code{getWDPACountries()}.
#'
#' @param country_iso The ISO code for the country to search for
#' @param api_key ProtectedPlanet.net API key 
#' @param with_geometry Boolean to return the geometry of the country
#' @param filename If \code{with_geometry} is \code{TRUE}, the filename to use when saving the geometry to a GeoJSON file. By default, the filename is the ISO (\emph{e.g.} USA.geojson)
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' regions <- getWDPACountry(country_iso = 'USA', api_key='xxxxxxxxxxx')
#' }

getWDPACountry <- function(country_iso, with_geometry = TRUE, filename = NA, api_key) {
  if (nchar(country_iso) != 3){
    stop('The country_iso has to be a 3-character code.')
  }
  
  cat(paste("Searching for country:", country_iso))
  
  results.json <- fromJSON(paste('http://api.protectedplanet.net/v3/countries/', country_iso, '?token=', api_key, sep=""), flatten = TRUE)$country
  
  if (length(results.json) == 0){
    stop(paste('Could not find a country with the ISO code:', country_iso))
  }
  
  results.df <- data.frame(results.json$name, results.json$iso_3, results.json$id, results.json$pas_count, results.json$pas_national_count, results.json$pas_regional_count, results.json$pas_international_count, results.json$pas_with_iucn_category_count, results.json$pas_with_iucn_category_percentage)
  if (with_geometry == TRUE){
    if (is.na(filename)){
      filename = paste(country_iso, ".geojson", sep="")
      geojson_write(toJSON(results.json$geojson), file = filename)
    }else{
      geojson_write(toJSON(results.json$geojson), file = filename)
    }
  }  
  invisible(results.df)
}