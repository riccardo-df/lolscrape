#' Scraping Match Information
#'
#' Scrapes matches of a given summoner.
#'
#' @param region Region where summoners are playing One in \code{c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "na1", "oc1", "ru", "tr1")}.
#' @param summoner_name The name of the summoner.
#' @param api_key Your API key. It should be of the format \code{"RGAPI-xxxx"}.
#' @param min Wait for a random amount of seconds bounded from below by \code{min} before scraping next item.
#' @param max Wait for a random amount of seconds bounded from above by \code{max} before scraping next item.
#'
#' @details
#' \code{get_match_info} returns detailed information about the recent matches of \code{summoner_name}. It can be used
#' in a for loop to iterate over several summoners.
#'
#' To get an API key, it is sufficient to have a Riot account of at least level 5 and visit
#' https://developer.riotgames.com/. Once logged in, you can generate your key from your dashboard.\cr
#'
#' Imposing a waiting time between each item scraped is useful to avoid hitting the rate limits imposed by Riot.
#' With a personal API key, it is not possible to get more than 20 requests every 1 second and 100 requests every 2 minutes.
#' To avoid clashes, set \code{min} and \code{max} so that the function waits a random amount of seconds before starting
#' scraping the next item. To use a deterministic amount of second, set \code{min} and \code{max} to the same number.
#'
#' @return
#' A data frame with the following variables:
#'   \item{\code{match_id}}{The match id.}
#'   \item{\code{when}}{Time and date of the match.}
#'   \item{\code{duration}}{Duration of the match in minutes.}
#'   \item{\code{queue_id}}{The queue id.}
#'   \item{\code{game_mode}}{The game mode.}
#'   \item{\code{game_type}}{The game type.}
#'   \item{\code{participant_name}}{Name of each participant.}
#'   \item{\code{participant_id}}{The id of each participant.}
#'   \item{\code{participant_puuid}}{The puuid of each participant.}
#'   \item{\code{participant_level}}{The level of each participant.}
#'   \item{\code{champion}}{The champion picked by each participant.}
#'   \item{\code{position}}{The position played by each participant.}
#'   \item{\code{kills}}{Number of kills of each participant.}
#'   \item{\code{assists}}{Number of assists of each participant.}
#'   \item{\code{deaths}}{Number of deaths of each participant.}
#'   \item{\code{gold}}{Gold earned by each participant.}
#'   \item{\code{early_surrender}}{Whether the match ended by early surrender.}
#'   \item{\code{surrender}}{Whether the match ended by surrender.}
#'   \item{\code{win}}{Whether the participant belongs to the winning team.}
#'   \item{\code{bans}}{Champions banned for a given game.}
#'
#' For more details on these variables, please refer to the Riot's official documentation:
#' https://developer.riotgames.com/apis#summoner-v4/GET_getBySummonerName,
#' https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID, and
#' https://developer.riotgames.com/apis#match-v5/GET_getMatch.
#'
#' @import stringr anytime
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{get_players_in_tier}}
#'
#' @export
get_match_info <- function(region, summoner_name, api_key,
                           min = 0, max = 0) {
  ## Checking inputs.
  if (!(region %in% c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "na1", "oc1", "ru", "tr1"))) stop("Invalid 'region'. Check the documentation for valid values.", call. = FALSE)
  if (!is.character(summoner_name)) stop("'summoner_name' must be a charachter.", call. = FALSE)
  if (!is.character(api_key)) stop("'api_key' must be a charachter.", call. = FALSE)

  summoner_name2 <- stringr::str_replace_all(summoner_name, pattern = " ", replacement = "%20")

  ## Getting summoner puuid. We use this to get matches of a given summoner.
  base_url_summoner <- paste("https://", ".api.riotgames.com/lol/summoner/v4/summoners/by-name", sep = region)
  url_summoner <- paste(base_url_summoner, summoner_name2, sep = "/")
  url_summoner_key <- paste(url_summoner, api_key, sep = "?api_key=")

  summoner <- httr::GET(url_summoner_key)
  summoner_data <- jsonlite::fromJSON(rawToChar(summoner$content))

  summoner_puuid <- summoner_data$puuid

  ## Getting matches id for a given summoner puuid.
  if (region == "euw1") region = "europe" ## RECODE ALSO OTHER REGIONS.

  start <- c(0, seq(101, 10000000, by = 100)) # We iterate across several start points.
  matches_id <- list()
  counter <- 1

  for (i in start) {
    sleep(min = min, max = max)

    base_url_matches <- paste("https://", ".api.riotgames.com/lol/match/v5/matches/by-puuid", sep = region)
    url_matches <- paste(base_url_matches, summoner_puuid, sep = "/")
    url_matches_start <- paste(url_matches, "/ids?start=", i, "&count=100", sep = "")
    url_matches_key <- paste(url_matches_start, api_key, sep = "&api_key=")

    matches <- httr::GET(url_matches_key)
    new_matches_id <- jsonlite::fromJSON(rawToChar(matches$content))

    if (is.list(new_matches_id)) break # Stop if the last page is empty.

    matches_id[[counter]] <- new_matches_id
    counter <- counter + 1

    if (length(new_matches_id) < 100) break # True if last iteration scraped the last ids.
  }

  matches_id <- unlist(matches_id)

  ## Using match ids to scrape data.
  counter <- 1
  out <- list()

  for (match_id in matches_id) {
    sleep(min = min, max = max)

    cat(paste0("Fetching match ", counter, "/", length(matches_id), ", ", summoner_name, "\n"))

    ## Getting match data.
    base_url_match <- paste("https://", ".api.riotgames.com/lol/match/v5/matches", sep = region)
    url_match <- paste(base_url_match, match_id, sep = "/")
    url_match_key <- paste(url_match, api_key, sep = "?api_key=")

    match <- httr::GET(url_match_key)
    match_data <- jsonlite::fromJSON(rawToChar(match$content))

    # Skipping iteration if request is not successful.
    if (match$status_code != 200) {
      cat(paste0("Match ", counter, "/", length(matches_id), " has been skipped \n"))
      next
    }

    ## Storing relevant information.
    # Generics.
    when <- anytime::anytime(match_data$info$gameCreation / 1000)
    duration <- match_data$info$gameDuration / 60
    queue_id <- match_data$info$queueId
    game_mode <- match_data$info$gameMode
    game_type <- match_data$info$gameType
    tournament <- ifelse(match_data$info$tournamentCode == "", "NULL", match_data$info$tournamentCode)

    # Participants.
    participant_name <- match_data$info$participants$summonerName
    participant_id <- match_data$info$participants$summonerId
    participant_puiid <- match_data$info$participants$puuid
    participant_level <- match_data$info$participants$summonerLevel

    champion <- match_data$info$participants$championName
    position <- match_data$info$participants$teamPosition

    kills <- match_data$info$participants$kills
    assists <- match_data$info$participants$assists
    deaths <- match_data$info$participants$deaths
    gold <- match_data$info$participants$goldEarned

    early_surrender <- match_data$info$participants$gameEndedInEarlySurrender
    surrender <- match_data$info$participants$gameEndedInSurrender

    win <- match_data$info$participants$win

    # Bans.
    bans <- c(match_data$info$teams$bans[[1]]$championId, match_data$info$teams$bans[[2]]$championId)

    ## Binding everything and storing in list.
    out[[counter]] <- data.frame("match_id" = match_id, "when" = when, "duration" = duration,
                                 "queue_id" = queue_id, "game_mode" = game_mode, "game_type" = game_type,
                                 participant_name, participant_id, participant_puiid, participant_level,
                                 champion, position, kills, assists, deaths, gold,
                                 early_surrender, surrender, win, bans)
    counter <- counter + 1
  }

  ## Handling output.
  out <- dplyr::bind_rows(out)

  ## Output.
  return(out)
}
