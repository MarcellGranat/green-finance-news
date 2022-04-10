library(tidyverse)
library(tidytext)
library(topicmodels)
library(stm)
library(quanteda)


news_df <- read_csv("data/us_equities_news_dataset.csv")

news_df %>% 
  count(category)

news_df <- news_df %>% 
  mutate(t = as.numeric(release_date - min(release_date)))

dat.proc <-  textProcessor(documents=news_df$content,
                           metadata = select(news_df, t), 
                           lowercase = TRUE, #*
                           removestopwords = TRUE, #*
                           removenumbers = TRUE, #*
                           removepunctuation = TRUE, #*
                           stem = TRUE, #*
                           wordLengths = c(4,Inf), #*
                           sparselevel = 1, #*
                           language = "en", #*
                           verbose = TRUE, #*
                           onlycharacter = TRUE, # not def
                           striphtml = FALSE, #*
                           customstopwords = NULL, #*
                           v1 = FALSE) #*

save(dat.proc, file = "data/dat_proc.RData")

prep <- prepDocuments(dat.proc$documents, dat.proc$vocab, dat.proc$meta)

save(prep, file = "data/prep.RData")