# Test time of selected code chunks from the apps


library(tidyverse)
library(tictoc)
library(microbenchmark)

library(quanteda) # 3.2.0
library(plotly) # 4.10.0
library(gmodels)
library(feather)

# Package loading ####
# Restarting the sesssion required here!

# Full tidyverse ?

# library(tictoc)
# tic("total")
# library(tidyverse)
# toc() # 1.56 sec
# 
# library(tictoc)
# tic("total")
# library(dplyr)
# library(magrittr)
# library(ggplot2)
# library(stringr)
# library(readr)
# toc() # 0.45


# Benchmark all (server) code elements ####

# mbm <- microbenchmark(
#   "v1" = { source("X-MockServerCodeV1.R") },  # Server routine of the initial words app
#   times = 5L
# 
#   )
# autoplot(mbm)


# # Inspect object loading times ####
# 
# # Tokens object as in V1 app
# bt <- read_rds("./PLS-words/Data/bt_tok.rds")
# 
# # write_rds(bt, "./PLS-words/Data/bt_tok2.rds", compress = "none")
# # file.size("./PLS-words/Data/bt_tok.rds")
# # file.size("./PLS-words/Data/bt_tok2.rds") # that'S the same
# 
# # DFM
# bt.dfm <- bt %>% dfm()
# write_rds(bt.dfm, "./PLS-words/Data/bt_dfm.rds")
# 
# # Matrix (doesn't work, too large)
# 
# # Raw text
# bt.corp <- read_rds("D:/Dropbox/ParlSpeechV2/Corpora/V2-Corpora/Corp_Bundestag_V2.rds") %>% 
#   filter(!chair) %>% # Drop speeches from the chair
#   select(-chair) %>% # Drop chair marker
#   select(-iso3country) %>% 
#   mutate(speechnumber = as.numeric(speechnumber)) %>%     # Attention!
#   mutate(id = row_number(),                              # ID to enable restoring of order later
#          month = str_remove(date, "-[0-9]{2}$")) %>%     # Month of speech, for aggregation
#   select(c(id, month, party, speaker, text)) # Same as docvars for the other objects
# 
# write_rds(bt.corp, "./PLS-words/Data/bt_corp.rds")
# 
# library(feather)
# write_feather(bt.corp, "./PLS-words/Data/bt_feather.feather")
# 
# 
# # Benchmark loading times
# mbm.load <- microbenchmark(
#   "tokens / readr" = { read_rds("./PLS-words/Data/bt_tok.rds") },
#   "tokens / base" = { readRDS("./PLS-words/Data/bt_tok.rds") },
#   "dfm / readr" = { read_rds("./PLS-words/Data/bt_dfm.rds") },
#   "dfm / base" = { readRDS("./PLS-words/Data/bt_dfm.rds") },
#   "text df / base" = { readRDS("./PLS-words/Data/bt_corp.rds") },
#   "text df / readr" = { read_rds("./PLS-words/Data/bt_corp.rds") },
#   "text df / feather" = { read_feather("./PLS-words/Data/bt_feather.feather") },
#   times = 50L
# )
# 
# # autoplot(mbm.load)
# 
# pl.load <- ggplot(mbm.load, aes(x = time/1000000, y= expr))+
#   geom_boxplot()+
#   stat_summary(geom = "point", fun = mean, color = "red")+
#   labs(x = "Time\n[Miliseconds]",
#        y = "Data type / reading method\n",
#        title = "File reading time ",
#        subtitle = "Each operation repeated 50 times")
# 
# # File sizes
# 
# fsizes <- data.frame(file = c("text df / feather", rep("text df / rds", 2), rep("dfm / rds", 2), rep("tokens / rds", 2)),
#                      size = c(file.size("./PLS-words/Data/bt_feather.feather"),
#                               rep(file.size("./PLS-words/Data/bt_corp.rds"), 2),
#                               rep(file.size("./PLS-words/Data/bt_dfm.rds"), 2),
#                               rep(file.size("./PLS-words/Data/bt_tok.rds"), 2)))
# # fsizes$file <- factor(fsizes$file, levels = c("text df feather", "text df rds", "dfm rds", "tokens rds"))
# fsizes$order <- nrow(fsizes):1
# fsizes$mb <- fsizes$size/1048576
# 
# pl.size <- ggplot(fsizes, aes(y=mb, x = order))+
#   geom_col(fill = "white", color = "black", width = .7)+
#   scale_x_continuous(breaks=fsizes$order, labels=fsizes$file)+
#   labs(title = "File size",
#        x = "\nObject type / file type",
#        y = "File size on disk\n[MB]")+
#   coord_flip()
# 
# library(patchwork)
# pl.load+pl.size +
#   plot_annotation(
#     title = 'Performance benchmarks for different text representations\n',
#     caption = 'Text data: ParlSpeech V2 Bundestag Corpus') &
#   theme(plot.title = element_text(hjust = .5, size = 12),
#         plot.subtitle = element_text(hjust = .5, size = 8),
#         plot.caption = element_text(hjust = .5))
# 
# ggsave("FileLoadBenchmarks.png", width = 24, height = 12, units = "cm")
# 
# 
# # Tokens / readr combination is already the quickest option ... sigh


# Benchmark keyword counting ####

user.words <- "migration*, flucht*" %>% 
  str_split(",") %>% 
  unlist() %>% 
  str_trim(side = "both")

user.dict <- list(hits = user.words) %>% 
  dictionary()

# Version 1 approach

out.tok <- bt %>% 
  # tokens_subset(date >= user.mindate & 
  #               date <= user.maxdate) %>% 
  tokens_lookup(user.dict,
                case_insensitive = TRUE,  # Not case sensitive
                exclusive = TRUE,         # Keep only tokens matching user word selection
                valuetype = "glob") %>%      # Allow for * wild cards
  dfm() %>% # Equals freq of 'hits' key from above lookup
  convert(to = "data.frame") %>% 
  cbind(docvars(out.tok))  %>% # row order should be consistent
  select(-c(id, doc_id)) %>% 
  mutate(user.words.pres = as.numeric(hits > 0)) # ar


# Raw text lookup
corp <- read_rds("D:/Dropbox/ParlSpeechV2/Corpora/V2-Corpora/Corp_Bundestag_V2.rds") %>% 
  filter(!chair) %>% # Drop speeches from the chair
  select(-chair) %>% # Drop chair marker
  select(-iso3country) %>% 
  mutate(speechnumber = as.numeric(speechnumber)) %>%     # Attention!
  mutate(id = row_number(),                              # ID to enable restoring of order later
         count = 1,                                      # Simple counter for aggregation purposes
         month = str_remove(date, "-[0-9]{2}$")) %>%     # Month of speech, for aggregation
  relocate(parliament, id, count, date, month) %>%
  select(-speechnumber)