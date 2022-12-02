# lolscrape

R package to pull League of Legends data from the Riot's API. Users need an API key to pull data from the servers. To get an API key, it is sufficient to have a Riot account of at least level 5 and visit [https://developer.riotgames.com/](https://developer.riotgames.com/). Once logged in, you can generate a key from your dashboard.

 ## Installation  
To install the package, run the following chunk of code in R:

```
# install.packages("devtools") # Uncomment this if you do not have yet installed the 'devtools' package.
devtools::install_github("riccardo-df/lolscrape")
```

## Usage Examples
A potential way to combine the functions in the package is as follows.

```
## Loading packages.
library(lolscrape)

# Scrape players ----------------------------------------------------------
## Settings.
api_key <- "your_key"
region <- "euw1"
queue <- "RANKED_SOLO_5x5"
tier <- "CHALLENGER"
rank <- "I"
min <- 2
max <- 2

## Scraping information on players.
players <- get_players_in_tier(region, queue, tier, rank, api_key)

# Scrape matches ----------------------------------------------------------
## Scraping matches of our players. Loop over nicknames.
summoner_names <- players$summoner_name

raw_results <- list()
counter_summoner <- 1
counter_storage <- 1

for (summoner in summoner_names) {
  match_ids <- get_match_ids(region, summoner, api_key)
  
  counter_match <- 1
  
  for (match_id in match_ids) {
    cat(paste0("Fetching match ", counter_match, "/", length(match_ids), " Player ", summoner, " ", counter_summoner, "/", length(summoner_names), "\n"))
    
    raw_results[[counter_storage]] <- get_match_info(region, match_id, api_key, min = min, max = max)
    
    counter_storage <- counter_storage + 1
    counter_match <- counter_match + 1
  }
  
  counter_summoner <- counter_summoner + 1
}

results <- dplyr::bind_rows(raw_results)
```

In the section `Scrape players`, I use `get_players_in_tier` to pull nicknames (and other information) about all the players that are currently in the specified
server (here, challenger solo/duo queue hosted on the european server). Then, in the section `Scrape matches`, I loop over all these nicknames to extract information about their most recent matches (approximately up to two years prior running the code). `get_match_ids` pulls the ids of these matches, and `get_match_info` uses 
these ids to pull match information. 
