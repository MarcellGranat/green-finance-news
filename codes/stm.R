library(tidyverse)
library(tidytext)
library(topicmodels)
library(stm)
library(quanteda)

load("data/dat_proc.RData")

topic_numbers <- list.files("data/") %>% 
  keep(str_starts, "fit_stm") %>% 
  parse_number() %>% 
  setdiff(x = seq(2, 30, 2))

for (topic_number in topic_numbers) {
  print(topic_number)
  
  mod <- stm(documents = dat.proc$documents, 
             vocab = dat.proc$vocab,
             K = topic_number,
             prevalence = ~ s(t),
             data = dat.proc$meta,
             init.type = "Spectral")
  
  save(mod, file = str_c("data/fit_stm", topic_number, ".RData"))
}




