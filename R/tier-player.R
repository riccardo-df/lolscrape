#' Scraping Summoners in a Given Rank and Tier
#'
#' Scrapes all the summoners belonging to a given rank and tier.
#'
#' @param region Region where summoners are playing.
#' @param queue Queue type.
#' @param tier Tier.
#' @param rank Rank.
#' @param api_key Your API key. It should be of the format \code{"RGAPI-xxxx"}.
#'
#' @details
#' \code{get_players_in_tier} returns, among others, all the summoners' names in a given tier and rank. Then, one can
#' use \code{\link{get_match_ids}} and \code{\link{get_match_info}} to scrape all the matches of a given summoner,
#' ideally looping over all the names here obtained.\cr
#'
#' The following are the valid values for the inputs:
#' \describe{
#'   \item{\code{region}}{\code{"br1"}, \code{"eun1"}, \code{"euw1"}, \code{"jp1"}, \code{"kr"}, \code{"la1"}, \code{"la2"}, \code{"na1"}, \code{"oc1"}, \code{"ru"}, \code{"tr1"}}
#'   \item{\code{queue}}{\code{"RANKED_SOLO_5x5"}, \code{"RANKED_TFT"}, \code{"RANKED_FLEX_SR"}, \code{"RANKED_FLEX_TT"}}
#'   \item{\code{tier}}{\code{"CHALLENGER"}, \code{"GRANDMASTER"}, \code{"MASTER"}, \code{"DIAMOND"}, \code{"PLATINUM"}, \code{"GOLD"}, \code{"SILVER"}, \code{"BRONZE"}, \code{"IRON"}}
#'   \item{\code{rank}}{\code{"I"}, \code{"II"}, \code{"III"}, \code{"IV"}}
#' }
#'
#' To get an API key, it is sufficient to have a Riot account of at least level 5 and visit
#' \href{https://developer.riotgames.com/}{https://developer.riotgames.com/}. Once logged in,
#' you can generate a key from your dashboard.\cr
#'
#' Imposing a waiting time between each item scraped is useful to avoid hitting the rate limits imposed by Riot.
#' With a personal API key, it is not possible to get more than 20 requests every 1 second and 100 requests every 2 minutes.
#' To avoid clashes, set \code{min} and \code{max} so that the function waits a random amount of seconds before starting
#' scraping the next item. To use a deterministic amount of second, set \code{min} and \code{max} to the same number.
#'
#' @return
#' A data frame with the following variables:
#'   \item{\code{leagueID}}{The id of the league.}
#'   \item{\code{queueType}}{The queue type.}
#'   \item{\code{tier}}{The tier.}
#'   \item{\code{rank}}{The rank.}
#'   \item{\code{summoner_id}}{The summoner's id.}
#'   \item{\code{summoner_name}}{The summoner's name.}
#'   \item{\code{leaguePoints}}{The league points.}
#'   \item{\code{wins}}{Number of games won by the summoner.}
#'   \item{\code{losses}}{Number of games lost by the summoner.}
#'   \item{\code{veteran}}{Whether the summoner is a veteran.}
#'   \item{\code{inactive}}{Whether the summoner is inactive.}
#'
#' For more details on these variables, please refer to the Riot's official documentation:
#' \href{https://developer.riotgames.com/apis#league-exp-v4/GET_getLeagueEntries}{https://developer.riotgames.com/apis#league-exp-v4/GET_getLeagueEntries}.
#'
#' @import dplyr httr jsonlite
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{get_match_info}}
#'
#' @export
get_players_in_tier <- function(region, queue, tier, rank, api_key) {
  ## Checking inputs.
  if (!(region %in% c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "na1", "oc1", "ru", "tr1"))) stop("Invalid 'region'. Check the documentation for valid values.", call. = FALSE)
  if (!(queue %in% c("RANKED_SOLO_5x5", "RANKED_TFT", "RANKED_FLEX_SR", "RANKED_FLEX_TT"))) stop("Invalid 'queue'. Check the documentation for valid values.", call. = FALSE)
  if (!(tier %in% c("CHALLENGER", "GRANDMASTER", "MASTER", "DIAMOND", "PLATINUM", "GOLD", "SILVER", "BRONZE", "IRON"))) stop("Invaldi 'division'. Check the documentation for valid values.", call. = FALSE)
  if (!(rank %in% c("I", "II", "III", "IV"))) stop("Invalid 'tier'. Check the documentation for valid values.", call. = FALSE)
  if (!is.character(api_key)) stop("'api_key' must be a charachter.")

  ## Get all summoners for given rank and tier.
  # We do not know how many pages. Iterate across several pages and stop if the last page is empty.
  base_url <- paste("https://", ".api.riotgames.com/lol/league-exp/v4/entries", sep = region)
  url <- paste(base_url, queue, tier, rank, sep = "/")

  out <- list()
  counter <- 1

  for (i in seq_len(10000000L)) {
    page <- i
    url_page <- paste(url, page, sep = "?page=")
    url_page_key <- paste(url_page, api_key, sep = "&api_key=")

    summoners <- httr::GET(url_page_key)
    if (summoners$status_code == 403) stop("Your API key in invalid.", call. = FALSE)
    temp_dta <- jsonlite::fromJSON(rawToChar(summoners$content))

    if (is.null(dim(temp_dta)[1])) break

    out[[counter]] <- temp_dta[, -c(12:13)]
    counter <- counter + 1
  }

  ## Handling output.
  out <- dplyr::bind_rows(out)

  colnames(out)[5] <- "summoner_id"
  colnames(out)[6] <- "summoner_name"

  ## Output.
  return(out)
}
