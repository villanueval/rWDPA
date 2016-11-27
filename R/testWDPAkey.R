#' Test an API key
#'
#' This function test if an API key is valid
#'
#' @param api_key The API key to test
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' testWDPAkey(api_key="xxxxxxxxxxx")
#' }

testWDPAkey <- function(api_key) {
  results.json <- fromJSON(paste('http://api.protectedplanet.net/test?token=', api_key, sep=""))
  if(results.json$status == 'Success!'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}