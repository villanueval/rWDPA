#' Get protected area
#'
#' This function gets the information available for the protected area specified with a WDPA ID. 
#'
#' @param wdpaid WDPA ID to return data for
#' @param api_key ProtectedPlanet.net API key 
#' @param with_geometry Boolean to return the geometry of the protected area
#' @param filename If \code{with_geometry} is \code{TRUE}, the filename to use when saving the geometry to a GeoJSON file. By default, the filename is the WDPA ID (\emph{e.g.} 10721.geojson)
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom geojsonio geojson_write
#' @export
#' @examples
#' \dontrun{
#' getWDPA(wdpaid = 10721, api_key='xxxxxxxxxxx', with_geometry = TRUE)
#' }

getWDPA <- function(wdpaid, api_key, with_geometry = TRUE, filename = NA) {
  #cat(paste("Searching for country:", country_iso))
  
  results.json <- fromJSON(paste('http://api.protectedplanet.net/v3/protected_areas/', wdpaid, '?with_geometry=', with_geometry, '&token=', api_key, sep=""))
  
  results.df <- results.json$protected_area
  cat(paste("\nFound data for the region:", results.df$name, "\n"))
  if (with_geometry == TRUE){
    if (is.na(filename)){
      filename = paste(wdpaid, ".geojson", sep="")
      geojson_write(toJSON(results.df), file = filename)
    }else{
      geojson_write(toJSON(results.df), file = filename)
    }
  }
  invisible(results.df)
}