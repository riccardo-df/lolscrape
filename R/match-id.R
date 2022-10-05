#' Scraping Match IDs
#'
#' Scrapes match ids of a given summoner.
#'
#' @param region Region where summoners are playing.
#' @param summoner_name The summoner's name.
#' @param api_key Your API key. It should be of the format \code{"RGAPI-xxxx"}.
#'
#' @details
#' The following are the valid values for the inputs:
#' \describe{
#'   \item{\code{region}}{\code{"br1"}, \code{"eun1"}, \code{"euw1"}, \code{"jp1"}, \code{"kr"}, \code{"la1"}, \code{"la2"}, \code{"na1"}, \code{"oc1"}, \code{"ru"}, \code{"tr1"}}
#' }
#'
#' To get an API key, it is sufficient to have a Riot account of at least level 5 and visit
#' \href{https://developer.riotgames.com/}{https://developer.riotgames.com/}. Once logged in,
#' you can generate a key from your dashboard.\cr
#'
#' @return
#' A vector of matches id recently played by \code{summoner_name}.
#'
#' For more details on these variables, please refer to the Riot's official documentation:
#' \href{https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID}{https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID}.
#'
#' @import stringr anytime
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{get_players_in_tier}}, \code{\link{get_match_info}}
#'
#' @export
get_match_ids <- function(region, summoner_name, api_key) {
  ## Checking inputs.
  if (!(region %in% c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "na1", "oc1", "ru", "tr1"))) stop("Invalid 'region'. Check the documentation for valid values.", call. = FALSE)
  if (!is.character(summoner_name)) stop("'summoner_name' must be a charachter.", call. = FALSE)
  if (!is.character(api_key)) stop("'api_key' must be a charachter.", call. = FALSE)

  summoner_name_adjusted <- stringr::str_replace_all(summoner_name, pattern = " ", replacement = "%20")

  ## Get summoner puuid. We use this to get the match ids.
  base_url_summoner <- paste("https://", ".api.riotgames.com/lol/summoner/v4/summoners/by-name", sep = region)
  url_summoner <- paste(base_url_summoner, summoner_name_adjusted, sep = "/")
  url_summoner_key <- paste(url_summoner, api_key, sep = "?api_key=")

  summoner <- httr::GET(url_summoner_key)
  summoner_data <- jsonlite::fromJSON(rawToChar(summoner$content))
  summoner_puuid <- summoner_data$puuid

  if (summoner$status_code == 403) stop("Your API key in invalid.", call. = FALSE)

  ## Get matches id.
  if (region == "euw1") region = "europe" ## RECODE ALSO OTHER REGIONS.

  # We do not know how many pages. Iterate across several start points and stop if the last page is empty or if
  # if last iteration scraped the last ids.
  start <- c(0, seq(101, 10000000, by = 100))
  matches_id <- list()
  counter <- 1

  for (i in start) {
    base_url_matches <- paste("https://", ".api.riotgames.com/lol/match/v5/matches/by-puuid", sep = region)
    url_matches <- paste(base_url_matches, summoner_puuid, sep = "/")
    url_matches_start <- paste(url_matches, "/ids?start=", i, "&count=100", sep = "")
    url_matches_key <- paste(url_matches_start, api_key, sep = "&api_key=")

    matches <- httr::GET(url_matches_key)
    new_matches_id <- jsonlite::fromJSON(rawToChar(matches$content))

    if (is.list(new_matches_id)) break

    matches_id[[counter]] <- new_matches_id
    counter <- counter + 1

    if (length(new_matches_id) < 100) break
  }

  ## Output.
  out <- unlist(matches_id)
  return(out)
}
