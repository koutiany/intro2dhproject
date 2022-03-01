library(rvest)
library(tidyverse)
library(stringr)
library(tidytext)

main_url <- "http://transcripts.foreverdreaming.org"
all_pages <- paste0("http://transcripts.foreverdreaming.org/viewforum.php?f=177&start=", seq(0, 200, 25))
characters <- c("ted", "lily", "marshall", "barney", "robin")


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

#Organized data frame with all episodes and the links:  all_episodes
all_episodes

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

all_episodes$count <- map_dbl(all_episodes$text, nrow)     #? not sure about this step
#----------------------------------------------------------------

#We can extend the previous tibble to be a bit more organized by separating the episode-season column into separate season and episo numbers.

all_episodes <- all_episodes %>%
  separate(episode_name, c("season", "episode"), "-", extra = "merge") %>%
  separate(season, c("season", "episode_number"), sep = "x")
all_episodes

#Break the lines down per character                                                (ALL CHARACTERS)

lines_all_characters <- map(filter(all_episodes, count > 15) %>% pull(text), ~ { 
  # only loop over episodes that have over 15 lines
  .x %>%
    mutate(episode_lines_id = 1:nrow(.))
}) %>%
  setNames(filter(all_episodes, count > 15) %>% # name according to episode
             unite(season_episode, season, episode_number, sep = "x") %>%
             pull(season_episode)) %>%
  enframe() %>%
  unnest() %>%
  mutate(all_lines_id = 1:nrow(.))

#add column for the speaker 
lines_all_characters$speaker <- lines_all_characters$text
lines_all_characters <- lines_all_characters[, c(1,5,2, 3, 4)]


regTest = "^[a-zA-Z]*:\\s|^[a-zA-Z]*\\s\\([a-zA-Z\\s]*\\):\\s|^[a-zA-Z'-]*?[\\s-][a-zA-Z0-9'-]*:|^[a-zA-Z']*?\\s[a-zA-Z']*?\\s[a-zA-Z']*:"

clean_speaker <- function(lines_all_characters){
  for (text in lines_all_characters) {
    if (grepl(regTest, lines_all_characters$text)){
      lines_all_characters$speaker <- str_match(lines_all_characters$text, regTest)
      lines_all_characters$speaker <-- tolower(lines_all_characters$speaker)                   #that doesnt work yet
      lines_all_characters$speaker <- str_remove(lines_all_characters$speaker, ":")
      lines_all_characters$text <- str_remove(lines_all_characters$text, regTest)
    } else {
      lines_all_characters$speaker <- "n/a"
    }
  }
}
clean_speaker(lines_all_characters)


lines_all_characters

