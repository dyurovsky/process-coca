library(here)
library(feather)
library(data.table)
library(tidytext)
library(tidyverse)

files <- list.files(here("wlp"), recursive = T, full.names = T)

read_coca_file <- function(file) {
  
  print(file)
  
  filename <- str_split(file, "/", simplify = T) %>% 
    last() %>% 
    sub(".txt", "", .) %>%
    str_split("_", simplify = F) %>%
    unlist()
  
  type <- filename[2]
  year <- filename[3]

  corpus <- fread(file, skip = 2, sep = "\t", 
                  col.names = c("word", "stem", "morph")) %>%
    filter(!is.na(stem)) %>%
    as_tibble() %>%
    mutate(sent_break = if_else(stem %in% c("<p>", ".", "?", "!"), 1, 0)) %>%
    mutate(sentence = cumsum(sent_break) + 1) %>%
    filter(morph != "y") %>%
    select(-word, -sent_break) 
  
  corpus %>%
    filter(morph %in% c("nn1", "nn2", "jj")) %>%
    mutate(type = type, year = year)
}

# coca <- map_dfr(files, read_coca_file)
# write_feather(coca, "coca.feather")
coca <- read_feather(here("coca.feather"))

adj_coca <- filter(coca, morph == "jj") %>%
  select(-morph) %>%
  rename(adj = stem) %>%
  filter(!adj %in% c("","-----", "--can", "--different", "--even",
                     "--just","-40-v"))

noun_coca <- filter(coca, morph != "jj") %>%
  select(-morph) %>%
  rename(noun = stem) %>%
  filter(!noun %in% c(""))

pairs <- inner_join(adj_coca, noun_coca, by = c("year", "type", "sentence"))

# write_feather(pairs, here("coca_pairs.feather"))

counts <- pairs %>%
  group_by(type, adj, noun) %>%
  summarise(n = n()) %>%
  filter(n >= 10)

write_feather(counts, here("coca_counts.feather"))




corpus <- read_lines(file) %>%
enframe(name = "number", value = "section") %>%
  filter(section != "") %>%
  unnest_tokens(sentence, section, token = "sentences") %>%
  unnest_tokens(sentence, sentence, token = "regex", pattern = "<p>") %>%
  unnest_tokens(sentence, sentence, token = "regex", pattern = ".\n") %>%
  mutate(sentence = gsub(" '", "'", sentence),
         sentence = gsub("[^'[:alpha:] ]", "", sentence),
         sentence = gsub("  ", " ", sentence),
         sentence = str_trim(sentence))

corpus[1, "sentence"] = sub("^section ", "", corpus[1, "sentence"])

