#' Search protected areas
#'
#' Search protected areas in the WDPA database that match a set of parameters. 
#'
#' @param marine If \code{TRUE}, returns marine protected areas. If \code{FALSE}, returns terrestrial protected areas. Default is to return both.
#' @param country Returns areas matching the ISO3 code of the country.
#' @param designation Returns areas designated with the designation id (integer).
#' @param jurisdiction Returns areas designated with the jurisdiction code (integer).
#' @param governance Returns areas designated with the governance code (integer).
#' @param iucn_category Returns areas designated with the IUCN category code (integer).
#' @param filename Filename to store the results
#' @param api_key ProtectedPlanet.net API key 
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' searchWDPA(marine = TRUE, country = 'CUB', filename = 'CUB.csv', api_key='xxxxxxxxxxx')
#' }

searchWDPA <- function(marine = NA, country = NA, designation = NA, jurisdiction = NA, governance = NA, iucn_category = NA, filename = NA, api_key) {
  
  q <- paste('http://api.protectedplanet.net/v3/protected_areas/search?token=', api_key, sep="")
  
  if (is.na(marine) == FALSE){
    q = paste(q, '&marine=', marine, sep="")
  }
  
  if (is.na(country) == FALSE){
    if (nchar(country) != 3){
      stop('The country code has to be a 3-character ISO code.')
    }
    q = paste(q, '&country=', country, sep="")
  }
  
  if (is.na(designation) == FALSE){
    q = paste(q, '&designation=', designation, sep="")
  }
  
  if (is.na(jurisdiction) == FALSE){
    q = paste(q, '&jurisdiction=', jurisdiction, sep="")
  }
  
  if (is.na(governance) == FALSE){
    q = paste(q, '&governance=', governance, sep="")
  }
  
  if (is.na(iucn_category) == FALSE){
    q = paste(q, '&iucn_category=', iucn_category, sep="")
  }
  
  
  cat('working...')
  page_size <- 50
  no_res <- page_size
  page_no <- 1
  while(no_res == page_size){
    results.json <- fromJSON(paste(q, '&per_page=50&page=', page_no, sep=""), flatten = TRUE)
    if (page_no == 1){
      results.df <- results.json$protected_areas[,c("id", "name", "original_name", "wdpa_id", "marine", "iucn_category.id", "iucn_category.name", "designation.id", "designation.name", "designation.jurisdiction.id", "designation.jurisdiction.name", "links.protected_planet")]
    }else{
      results.df1 <- results.json$protected_areas[,c("id", "name", "original_name", "wdpa_id", "marine", "iucn_category.id", "iucn_category.name", "designation.id", "designation.name", "designation.jurisdiction.id", "designation.jurisdiction.name", "links.protected_planet")]
      results.df <- rbind(results.df, results.df1)
    }
    no_res <- dim(results.json$protected_areas)[1]
    page_no <- page_no + 1
    cat(paste('\n Got data for', dim(results.df)[1], 'areas. Querying API for next page of results.'))
  }
  
  cat(paste("\nFound", dim(results.df)[1], "areas\n"))
  if (is.na(filename) == FALSE){
    write.csv(x = results.df, file = filename, quote = TRUE, row.names = FALSE, fileEncoding = 'UTF-8')
  }
  
  invisible(results.df)
}