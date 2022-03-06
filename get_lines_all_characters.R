library(rvest)
library(tidyverse)
library(stringr)
library(tidytext)
library(dplyr)

main_url <- "http://transcripts.foreverdreaming.org"
all_pages <- paste0("http://transcripts.foreverdreaming.org/viewforum.php?f=177&start=", seq(0, 200, 25))

episode_getter <- function(link) {
  title_reference <-
    link %>%
    read_html() %>%
    html_nodes(".topictitle") # Get the html node name with 'selector gadget'
  
  episode_links <- title_reference %>%
    html_attr("href") %>%
    gsub("^.", "", .) %>%
    paste0(main_url, .) %>%
    setNames(title_reference %>% html_text()) %>%
    enframe(name = "episode_name", value = "link")
  
  episode_links
}

all_episodes <- map_df(all_pages, episode_getter) # loop over all seasons and get all episode links
all_episodes$id <- 1:nrow(all_episodes)

#The remaining part is to actually scrape the text from each episode. We can work that out for a single episode and then turn that into a function and apply for all episodes.

episode_fun <- function(file) { file %>%
    read_html() %>%
    html_nodes(".postbody") %>%
    html_text() %>%
    str_split("\n|\t") %>%
    .[[1]] %>%
    data_frame(text = .) %>%
    filter(str_detect(text, ""), # Lots of empty spaces
           !str_detect(text, "^\\t"), # Lots of lines with \t to delete
           !str_detect(text, "^\\[.*\\]$"), # Text that start with brackets
           !str_detect(text, "^\\(.*\\)$"), # Text that starts with parenthesis
           str_detect(text, "^*.:"), # I want only lines with start with dialogue (:)
           !str_detect(text, "^ad")) # Remove lines that start with ad (for 'ads', the link of google ads)
}

#We now have a data frame with only dialogue for each character. We need to apply that function to each episode and bind everything together. We first apply the function to every episode.

all_episodes$text <- map(all_episodes$link, episode_fun)

# Some episodes (e.g. S08E09 or S04E06) don't have the characters with the dialoge or not the full script. we need to exclud them.

all_episodes$count <- map_dbl(all_episodes$text, nrow)

#Break the lines down per character                                                (ALL CHARACTERS)

lines_all_characters <- map(filter(all_episodes, count > 15) %>% pull(text), ~ {
  # only loop over episodes that have over 15 lines
  .x %>%
    mutate(episode_lines_id = 1:nrow(.))
}) %>%
  setNames(filter(all_episodes, count > 15) %>% # name according to episode
             unite(season_episode, episode_name, sep = "x") %>%
             pull(season_episode)) %>%
  enframe() %>%
  unnest() %>%
  mutate(all_lines_id = 1:nrow(.))


# Seperate Season from Episode

get_season_regex <- "^[0-9]*"
get_episode_regex <- "x[0-9]*"

lines_all_characters$episode <- lines_all_characters$name
lines_all_characters <- lines_all_characters[, c(1,5,2,3,4)]

get_season <- str_extract(lines_all_characters$name, get_season_regex)
lines_all_characters$name <- get_season
names(lines_all_characters)[1] <- 'season'

get_ep_number <- str_extract(lines_all_characters$episode, get_episode_regex)
get_ep_number <- str_remove(get_ep_number, "x")
lines_all_characters$episode <- get_ep_number

colnames(lines_all_characters)

#add column for the speaker
lines_all_characters$speaker <- lines_all_characters$text
lines_all_characters <- lines_all_characters[, c(1,2, 6, 3, 4, 5)]

#clean speaker
regex_speaker = "^[a-zA-Z]*:\\s|^[a-zA-Z]*\\s\\([a-zA-Z\\s]*\\):\\s|^[a-zA-Z'-]*?[\\s-][a-zA-Z0-9'-]*:|^[a-zA-Z']*?\\s[a-zA-Z']*?\\s[a-zA-Z']*:"
speakers <- str_extract(lines_all_characters$text, regex_speaker)
speakers <- str_remove(speakers, ":")
speakers <- gsub("\\s*\\([^\\)]+\\)","",as.character(speakers))

lines_all_characters$speaker <- tolower(speakers)

#clean text
clear_text <- str_remove(lines_all_characters$text, regex_speaker)
clear_text <- str_remove(clear_text, '^\\([a-zA-Z\\s]*\\)')
#clear_text <- str_remove(lines_all_characters$text, '^?.\\([a-zA-Z.\\s]*\\)')
lines_all_characters$text <- clear_text

#unification of character names based on regular expression patterns
all_characters <- lines_all_characters %>% group_by(speaker) %>% summarize(text_bound = paste(text, collapse = " "))

lines_all_characters %>% mutate(speaker = if_else(str_detect(speaker,"mar?h?s?h?ah?ll?|marshall's voice|marshall young"), "marshall", speaker))
lines_all_characters %>% mutate(speaker = if_else(str_detect(speaker,"ted\\s|narrtor"), "ted", speaker))
lines_all_characters %>% mutate(speaker = if_else(str_detect(speaker,"lill?y\\s"), "lily", speaker))
lines_all_characters %>% mutate(speaker = if_else(str_detect(speaker,"robii?b?n\\s"), "robin", speaker))
lines_all_characters %>% mutate(speaker = if_else(str_detect(speaker,"bare?n?en?y"), "barney", speaker))

#colappsing of text based on groups
new <- lines_all_characters %>% group_by(season, episode, speaker) %>% summarize(text_bound = paste(text, collapse = " "))
lines_per_characters <- lines_all_characters %>% group_by(speaker = "barney", season, episode) %>% summarize(text_bound = paste(text, collapse = " "))

write.csv(new,"/Users/marma/Documents/studium/Digital Humanities/lines_all_characters.csv", row.names = FALSE)
