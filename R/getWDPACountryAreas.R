#' Get the protected areas in a country from the ProtectedPlanet.net WDPA database
#'
#' This function searches the ProtectedPlanet database for protected areas in a country. The country is identified by its ISO code, which can be obtained using the function \code{getWDPACountries()}.
#'
#' @param country_iso The ISO code for the country to search for
#' @param filename Store the list of areas found in this file in CSV format
#' @param api_key ProtectedPlanet.net API key 
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' regions <- getWDPACountryAreas(country_iso = 'USA', api_key='xxxxxxxxxxx')
#' }

getWDPACountryAreas <- function(country_iso, filename = NA, api_key) {
  if (nchar(country_iso) != 3){
    stop('The country_iso has to be a 3-character code.')
  }
  
  cat(paste("Searching for country:", country_iso))
  cat('\n\n Working...\n This may take a while for countries with many protected areas...\n\n')
  
  page_size <- 50
  no_res <- page_size
  page_no <- 1
  
  while(no_res == page_size){
    results.json <- fromJSON(paste('http://api.protectedplanet.net/v3/protected_areas/search?per_page=', page_size, '&token=', api_key, '&country=', country_iso, '&page=', page_no, sep=""), flatten = TRUE)
    if (page_no == 1){
      if (length(results.json$protected_areas)==0){
        stop(' Nothing was found. Please check the ISO code and try again.')
      }
      results.df <- results.json$protected_areas
    }else{
      results.df1 <- results.json$protected_areas
      results.df <- rbind(results.df, results.df1)
    }
    no_res <- dim(results.json$protected_areas)[1]
    page_no <- page_no + 1
    cat(paste('\n Got data for', dim(results.df)[1], 'protected areas. Querying API for next page of results.'))
  }
  
  cat(paste("\nFound", dim(results.df)[1], "protected areas in:", country_iso, "\n"))
  if (is.na(filename) == FALSE){
    write.csv(x = results.df[,c("id", "name", "original_name", "wdpa_id", "marine", "iucn_category.id", "iucn_category.name", "designation.id", "designation.name", "designation.jurisdiction.id", "designation.jurisdiction.name", "links.protected_planet")], file = filename, quote = TRUE, row.names = FALSE, fileEncoding = 'UTF-8')
  }
  invisible(results.df)
}