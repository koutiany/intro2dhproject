setwd("~/studium/Digital Humanities")
options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)
library(stringr)
library(reshape2)
library(ggplot2)
library(dplyr)

textdata <- read.csv("final.csv", sep = ",", encoding = "UTF-8")
#textdata$text[50]
himym_corpus <- corpus(textdata$text_bound, docnames = textdata$doc_id)

# Build a dictionary of lemmas
lemma_data <- read.csv("resources/baseform_en.tsv", encoding = "UTF-8")
# extended stopword list
stopwords_extended <- readLines("resources/stopwords_himym.txt", encoding = "UTF-8") %>% str_trim()


# Create a DTM (may take a while)
corpus_tokens <- himym_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)
#corpus_tokens[[45]]

himym_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 5)

#himym_collocations <- himym_collocations[1:250, ] we didn't have to use this bc it doesn't apply

corpus_tokens <- tokens_compound(corpus_tokens, himym_collocations, join = TRUE)
#-----------------------------process text corpus as an input for topic modeling----------------------------------
#--------create the bag of words representations---------
DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 5)

dim(DTM)
#-----------------------------------------------------------------------
top10_terms <- c("ted", "lily", "marshall", "barney", "robin", "claudia", "zoey", "stella", "karen", "jenkins", "robin's", "natalie", "nora")
DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]

#So we remove empty rows in DTM

sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]
