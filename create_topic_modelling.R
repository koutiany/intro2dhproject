setwd("~/studium/Digital Humanities")
options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)

textdata <- read.csv("new_season1_6.csv", sep = ",", encoding = "UTF-8")
textdata$text[1]
himym_corpus <- corpus(textdata$text, docnames = textdata$doc_id)

# Build a dictionary of lemmas
lemma_data <- read.csv("resources/baseform_en.tsv", encoding = "UTF-8")
# extended stopword list
stopwords_extended <- readLines("resources/stopwords_himym.txt", encoding = "UTF-8")

# Create a DTM (may take a while)
corpus_tokens <- himym_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, 
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)

himym_collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 10)

#himym_collocations <- himym_collocations[1:250, ]

corpus_tokens <- tokens_compound(corpus_tokens, himym_collocations, join = TRUE)
#---------------------------------------------------------------
DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)

dim(DTM)
#most words I can think of already existed in the stopwords list

top10_terms <- c("ted", "lily", "marshall", "barney", "robin", "hey", "um", "ooh", "marshall's", "rn", "ll")
DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]

# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

# load package topicmodels
require(topicmodels)
# number of topics
K <- 13
# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 2500,
  seed = 1,
  verbose = 25,
  alpha = 0.005))

# have a look a some of the results (posterior distributions)

tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)

ncol(DTM) # lengthOfVocab

# topics are probability distribtions over the entire  vocabulary

beta <- tmResult$terms # get beta from results
dim(beta) # K distributions over ncol(DTM) terms

rowSums(beta) # rows in beta sum to 1

nrow(DTM) # size of collection

# for every document we have a probability distribution of
# its contained topics
theta <- tmResult$topics
dim(theta) # nDocs(DTM) distributions over K topics

rowSums(theta)[1:10] # rows in theta sum to 1

terms(topicModel, 25)

# Visualization
# LDAvis browser
library(LDAvis)
library("tsne")
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)


# get mean topic proportions per season
topic_proportion_per_season <- aggregate(theta, by = list(season = textdata$season), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_season)[2:(K+1)] <- topicNames

#reshape data frame
library(reshape2)
vizDataFrame <- melt(topic_proportion_per_season, id.vars = "season")
# plot topic proportions per deacde as bar plot
require(pals)
ggplot(vizDataFrame,
       aes(x=season, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

theta %>% as_tibble %>% mutate(speaker = new$speaker) %>% group_by(speaker) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
