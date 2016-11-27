#' Get the list of countries in the WDPA
#'
#' This function gets the countries available in the WDPA database, in which the regions are grouped in.
#'
#' @param filename Store the list of countries and their ISO code to this file in CSV format
#' @param api_key ProtectedPlanet.net API key 
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' countries <- getWDPACountries(api_key='xxxxxxxxxxx')
#' countries <- getWDPACountries(filename = 'countries.csv', api_key='xxxxxxxxxxx')
#' }

getWDPACountries <- function(filename = NA, api_key) {
  
  cat('working...')
  page_size <- 50
  no_res <- page_size
  page_no <- 1
  while(no_res == page_size){
    results.json <- fromJSON(paste('http://api.protectedplanet.net/v3/countries?per_page=', page_size, '&with_geometry=false&token=', api_key, '&page=', page_no, sep=""), flatten = TRUE)
    if (page_no == 1){
      results.df <- results.json$countries[,c("name", "iso_3")]
    }else{
      results.df1 <- results.json$countries[,c("name", "iso_3")]
      results.df <- rbind(results.df, results.df1)
      }
    no_res <- dim(results.json$countries)[1]
    page_no <- page_no + 1
    cat(paste('\n Got data for', dim(results.df)[1], 'countries. Querying API for next page of results.'))
  }
  
  cat(paste("\nFound", dim(results.df)[1], "countries\n"))
  if (is.na(filename) == FALSE){
    write.csv(x = results.df, file = filename, quote = TRUE, row.names = FALSE, fileEncoding = 'UTF-8')
  }
  
  invisible(results.df)
}
