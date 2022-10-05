#' Scraping Match Information
#'
#' Scrapes information for the match of interest.
#'
#' @param region Region where summoners are playing.
#' @param match_id The match ids of interest.
#' @param api_key Your API key. It should be of the format \code{"RGAPI-xxxx"}.
#' @param min Wait for a random amount of seconds bounded from below by \code{min} before scraping next item.
#' @param max Wait for a random amount of seconds bounded from above by \code{max} before scraping next item.
#'
#' @details
#' \code{get_match_info} returns detailed information about the match of interest. It can be used
#' in a for loop to iterate over several matches. Only classic games are returned (e.g., aram games and others are
#' ignored).\cr
#'
#' The following are the valid values for the inputs:
#' \describe{
#'   \item{\code{region}}{\code{"br1"}, \code{"eun1"}, \code{"euw1"}, \code{"jp1"}, \code{"kr"}, \code{"la1"}, \code{"la2"}, \code{"na1"}, \code{"oc1"}, \code{"ru"}, \code{"tr1"}}
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
#'   \item{\code{match_id}}{The match id.}
#'   \item{\code{when}}{Time and date of the match.}
#'   \item{\code{duration}}{Duration of the match in minutes.}
#'   \item{\code{queue_id}}{The queue id. See \href{https://static.developer.riotgames.com/docs/lol/queues.json}{here}. Currently, only 5v5 Ranked Solo games are returned.}
#'   \item{\code{game_mode}}{The game mode. See \href{https://static.developer.riotgames.com/docs/lol/gameModes.json}{here}. Currently, only \code{"CLASSIC"} matches are returned.}
#'   \item{\code{game_type}}{The game type. See \href{https://static.developer.riotgames.com/docs/lol/gameTypes.json}{here}. Currently, only \code{"MATCHED_GAME"} matches are returned.}
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
#' \href{https://developer.riotgames.com/apis#summoner-v4/GET_getBySummonerName}{https://developer.riotgames.com/apis#summoner-v4/GET_getBySummonerName},
#' \href{https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID}{https://developer.riotgames.com/apis#match-v5/GET_getMatchIdsByPUUID}, and
#' \href{https://developer.riotgames.com/apis#match-v5/GET_getMatch}{https://developer.riotgames.com/apis#match-v5/GET_getMatch}.
#'
#' @import stringr anytime
#'
#' @author Riccardo Di Francesco
#'
#' @seealso \code{\link{get_players_in_tier}}, \code{\link{get_match_ids}}
#'
#' @export
get_match_info <- function(region, match_id, api_key,
                           min = 0, max = 0) {
  ## Handling inputs and checks.
  if (!(region %in% c("br1", "eun1", "euw1", "jp1", "kr", "la1", "la2", "na1", "oc1", "ru", "tr1"))) stop("Invalid 'region'. Check the documentation for valid values.", call. = FALSE)
  if (!is.character(match_id)) stop("'match_ids' must be a charachter.", call. = FALSE)
  if (!is.character(api_key)) stop("'api_key' must be a charachter.", call. = FALSE)

  if (region == "euw1") region_adjusted = "europe" ## RECODE ALSO OTHER REGIONS.

  sleep(min, max)

  ## Get match data.
  base_url_match <- paste("https://", ".api.riotgames.com/lol/match/v5/matches", sep = region_adjusted)
  url_match <- paste(base_url_match, match_id, sep = "/")
  url_match_key <- paste(url_match, api_key, sep = "?api_key=")

  match <- httr::GET(url_match_key)
  match_data <- jsonlite::fromJSON(rawToChar(match$content))

  if (match$status_code != 200) return(NULL)

  ## Store relevant information.
  # Generics.
  when <- anytime::anytime(match_data$info$gameCreation / 1000)
  duration <- match_data$info$gameDuration / 60
  queue_id <- match_data$info$queueId
  game_mode <- match_data$info$gameMode
  game_type <- match_data$info$gameType

  if (queue_id != 420 | game_mode != "CLASSIC" | game_mode != "MATCHED_GAME") return(NULL)

  # Participants.
  participant_name <- match_data$info$participants$summonerName
  participant_id <- match_data$info$participants$summonerId
  participant_puiid <- match_data$info$participants$puuid
  participant_level <- match_data$info$participants$summonerLevel

  if (length(participant_name) != 10 | length(participant_id) != 10 |
      length(participant_puiid) != 10 | length(participant_level) != 10) return(NULL)

  champion <- match_data$info$participants$championName
  bans <- c(match_data$info$teams$bans[[1]]$championId, match_data$info$teams$bans[[2]]$championId)
  position <- match_data$info$participants$teamPosition

  if (length(champion) != 10 |length(bans) != 10 | length(position) != 10) return(NULL)

  kills <- match_data$info$participants$kills
  assists <- match_data$info$participants$assists
  deaths <- match_data$info$participants$deaths
  gold <- match_data$info$participants$goldEarned

  if (length(kills) != 10 | length(assists) != 10 | length(deaths) != 10 | length(gold) != 10) return(NULL)

  early_surrender <- match_data$info$participants$gameEndedInEarlySurrender
  surrender <- match_data$info$participants$gameEndedInSurrender
  win <- match_data$info$participants$win

  if (length(early_surrender) != 10 | length(surrender) != 10 |length(win) != 10) return(NULL)

  ## Output.
  out <- data.frame("match_id" = match_id, "when" = when, "duration" = duration,
                               "queue_id" = queue_id, "game_mode" = game_mode, "game_type" = game_type,
                               participant_name, participant_id, participant_puiid, participant_level,
                               champion, position, kills, assists, deaths, gold,
                               early_surrender, surrender, win, bans)
  return(out)
}
