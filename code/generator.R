# Data wrangling
library(tidyverse)

# Text processing
library(tidytext)
library(textclean)
library(tokenizers)

# Markov Chain
library(markovchain)

library(gutenbergr)

nature <- readxl::read_excel("output/nature_all_paleo.xlsx")
nature <- data.frame(text=nature$title)

# text cleansing
nature_clean <- nature %>%  
  mutate(text  = tolower(text)) %>% 
  mutate(text = text %>%  
           str_replace(pattern = "--", " ") %>% 
           # remove punctuation selain tanda titik, koma sama seru
           str_remove_all(pattern = "(?![.,!])[[:punct:]]") %>% 
           str_remove_all(pattern = "[0-9]") %>% # remove numeric
           replace_contraction() %>% # I'll menjadi I will
           replace_white() %>% # remove double white space
           str_replace_all(pattern = "[.]", replacement = " .") %>% 
           str_replace_all(pattern = "[!]", replacement = " !") %>% 
           str_replace_all(pattern = "[,]", replacement = " ,"))

text_nature <- nature_clean %>% 
  pull(text) %>% 
  strsplit(" ") %>% 
  unlist() 

n_distinct(text_nature)


# Model fitting -----------------------------------------------------------

fit_markov <- markovchainFit(text_nature)

for (i in 1:10) {
  
  set.seed(i)
  
  markovchainSequence(n = 10, 
                      markovchain = fit_markov$estimate,
                      t0 = "the", include.t0 = T) %>% 
    
    # joint words
    paste(collapse = " ") %>% 
    
    # create proper sentence form
    str_replace_all(pattern = " ,", replacement = ",") %>% 
    str_replace_all(pattern = " [.]", replacement = ".") %>% 
    str_replace_all(pattern = " [!]", replacement = "!") %>% 
    
    str_to_sentence() %>% 
    
    print()
}


# bigram ------------------------------------------------------------------

bigram_nature <- nature_clean %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  pull(bigram)

bigram_nature %>% head(10)


markov_bigram <- markovchainFit(bigram_nature)