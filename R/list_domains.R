#' List available WRI domains
#'
#' @returns A character vector of domain names
#' @export
#'
#' @examples
#' # See all available domains
#' list_domains()
#'
#' # Check if a domain is valid
#' "water" %in% list_domains()
#'
#' # Loop through domains
#' for (domain in list_domains()) {
#'   print(domain)
#' }
list_domains <- function() {

  c("infrastructure", "communities", "livelihoods",
    "sense_of_place", "species", "habitats", "water", "air")
}
