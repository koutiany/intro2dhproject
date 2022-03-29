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


require(topicmodels)
K <- 7
# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 2000,
  seed = 1,
  verbose = 25,
  alpha = 0.02))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)

nrow(DTM) #length of vocab
ncol(DTM) 

# topics are probability distributions over the entire vocabulary
beta <- tmResult$terms # get beta from results
dim(beta) # K distributions over ncol(DTM) terms

rowSums(beta) # rows in beta sum to 1

nrow(DTM) # size of collection

# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics
dim(theta) # nDocs(DTM) distributions over K topics

rowSums(theta)[1:10] # rows in theta sum to 1

terms(topicModel, 25)
top8termsPerTopic <- terms(topicModel, 8)
topicNames <- apply(top8termsPerTopic, 2, paste, collapse=" " )
#----------------------------------------------------
# Visualization
# LDAvis browser
install.packages('servr')
library(LDAvis)
library("tsne")
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)


#-----------------------get bar-----------------
# get mean topic proportions per season
topic_proportion_per_season <- aggregate(theta, by = list(season= textdata$season), mean)
#set topic names to aggregated columns
colnames(topic_proportion_per_season)[2:(K+1)] <- topicNames

vizDataFrame <- melt(topic_proportion_per_season, id.vars="season")
#plot topic proportions per season as bar plot
require(pals)
ggplot(vizDataFrame,
       aes(x=season, y=value, fill=variable)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name= "topics")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('topics_over_seasons.jpeg', device = "jpeg", limitsize = FALSE)

#---------------theta comparison at the end------------------------

theta1 <- theta %>% as_tibble %>% mutate(speaker = textdata$speaker) %>% group_by(speaker) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
theta2 <- melt(theta1)
selectednames <- c("robin","lily","barney","ted","marshall")
theta3 <- filter(theta2, speaker %in% selectednames)

ggplot()+
  geom_point(data=theta3, mapping=aes(y=value,x=variable,color=speaker,shape=speaker))+
  labs(x="topic",y="theta")

# GET TOPIC DISTRIBUTION OVER DIFFERENT SEASONS 

#season1
theta1_seperated <- theta %>% as_tibble %>% mutate(speaker = textdata$speaker, season = textdata$season) %>% group_by(speaker, season) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
theta2_seperated <- filter(theta1_seperated, speaker %in% selectednames)

theta3_season <- filter(theta2_seperated, season == 1) 
theta3_season$season <- NULL
theta3_season <- melt(theta3_season)
ggplot()+
  geom_point(data=theta3_season, mapping=aes(y=value,x=variable,color=speaker,shape=speaker))+
  labs(x="topic",y="theta")
#save
ggsave('season1.jpeg', device = "jpeg", limitsize = FALSE)

#Season2
theta3_season <- filter(theta2_seperated, season == 2) 
theta3_season$season <- NULL
theta3_season <- melt(theta3_season)
ggplot()+
  geom_point(data=theta3_season, mapping=aes(y=value,x=variable,color=speaker,shape=speaker))+
  labs(x="topic",y="theta")
ggsave('season2.jpeg', device = "jpeg", limitsize = FALSE)

#Season3
theta3_season <- filter(theta2_seperated, season == 3) 
theta3_season$season <- NULL
theta3_season <- melt(theta3_season)
ggplot()+
  geom_point(data=theta3_season, mapping=aes(y=value,x=variable,color=speaker,shape=speaker))+
  labs(x="topic",y="theta")
ggsave('season3.jpeg', device = "jpeg", limitsize = FALSE)

#Season4
theta3_season <- filter(theta2_seperated, season == 4) 
theta3_season$season <- NULL
theta3_season <- melt(theta3_season)
ggplot()+
  geom_point(data=theta3_season, mapping=aes(y=value,x=variable,color=speaker,shape=speaker))+
  labs(x="topic",y="theta")
ggsave('season4.jpeg', device = "jpeg", limitsize = FALSE)

#Season5
theta3_season <- filter(theta2_seperated, season == 5) 
theta3_season$season <- NULL
theta3_season <- melt(theta3_season)
ggplot()+
  geom_point(data=theta3_season, mapping=aes(y=value,x=variable,color=speaker,shape=speaker))+
  labs(x="topic",y="theta")
ggsave('season5.jpeg', device = "jpeg", limitsize = FALSE)

#Season6
theta3_season <- filter(theta2_seperated, season == 6) 
theta3_season$season <- NULL
theta3_season <- melt(theta3_season)
ggplot()+
  geom_point(data=theta3_season, mapping=aes(y=value,x=variable,color=speaker,shape=speaker))+
  labs(x="topic",y="theta")
ggsave('season6.jpeg', device = "jpeg", limitsize = FALSE)