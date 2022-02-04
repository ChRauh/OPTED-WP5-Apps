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



# # Benchmark keyword counting ####
#
#
# # Tokens data type
# bt <- read_rds("./PLS-words/Data/bt_tok.rds")
#
# # Corpus data type
# bt.corp <- read_rds("D:/Dropbox/ParlSpeechV2/Corpora/V2-Corpora/Corp_Bundestag_V2.rds") %>%
#   filter(!chair) %>% # Drop speeches from the chair
#   mutate(id = row_number(),                              # ID to enable restoring of order later
#          month = str_remove(date, "-[0-9]{2}$"),        # Month of speech, for aggregation
#          text = tolower(text),                        # for str detection !
#          text = paste0(" ", text, " "),               # full text must start and end with space for correct str detection
#          text = str_remove_all(text, "[:punct:]")) %>%  # as in tokenization !
#   select(c(id, month, party, speaker, terms, text)) # Same as docvars for the other objects
# 
#
#
# # User input words
# user.words <- "migration*, flucht*" %>%
#   str_split(",") %>%
#   unlist() %>%
#   str_trim(side = "both")
#
#
# # Version 1 approach
#
# user.dict <- list(hits = user.words) %>%
#   dictionary()
#
# out.tok <- bt %>%
#   tokens_lookup(user.dict,
#                 case_insensitive = TRUE,  # Not case sensitive
#                 exclusive = TRUE,         # Keep only tokens matching user word selection
#                 valuetype = "glob") %>%      # Allow for * wild cards
#   dfm() %>% # Equals freq of 'hits' key from above lookup
#   convert(to = "data.frame") %>%
#   cbind(docvars(bt))  %>% # row order should be consistent
#   select(-c(id, doc_id)) %>%
#   mutate(user.words.pres = as.numeric(hits > 0)) # ar
#
# # Alternative 1: raw text lookup (to be combined with feather loading)
#
# # User input words as regular expression - note: only lower text
# # Note also the white space in the beginning, brute force tokenization!
# user.regex <- paste0(" (", user.words, ")") %>%
#   paste(collapse = "|") %>%
#   str_replace_all(fixed("*"), "([a-z])*")
#
# bt.corp$user.words.pres <- str_detect(bt.corp$text, user.regex)
#
#
# # There are a few, but some very strange differences!
#
# # table(bt.corp$user.words.pres == out.tok$user.words.pres)
# #
# # sum(bt.corp$user.words.pres)
# # sum(out.tok$user.words.pres)
# #
# # bt.corp$tok <- as.logical(out.tok$user.words.pres)
# # bt.corp$diff <- bt.corp$user.words.pres != bt.corp$tok
# #
# # test <- bt.corp %>% filter(diff)
# #
# # edit(test$text[1])
# #
# # testext <- paste(types(tokens_subset(bt, id == 105548)), collapse = " ")
# #
# # types(tokens(test$text[1],
# #        remove_punct = T,
# #        remove_numbers = T,
# #        verbose = T))
#
# # Benchmark
#
# mbm.words <- microbenchmark(
#
#   "tokens > lookup > dfm > df" = {
#
#     user.dict <- list(hits = user.words) %>%
#       dictionary()
#
#     bt %>%
#       tokens_lookup(user.dict,
#                     case_insensitive = TRUE,  # Not case sensitive
#                     exclusive = TRUE,         # Keep only tokens matching user word selection
#                     valuetype = "glob") %>%      # Allow for * wild cards
#       dfm() %>% # Equals freq of 'hits' key from above lookup
#       convert(to = "data.frame") %>%
#       cbind(docvars(bt))  %>% # row order should be consistent
#       select(-c(id, doc_id)) %>%
#       mutate(user.words.pres = as.numeric(hits > 0))
#     },
#
#   "df$text > str_detect > df" = {
#     user.regex <- paste0(" (", user.words, ")") %>%
#       paste(collapse = "|") %>%
#       str_replace_all(fixed("*"), "([a-z])*")
#
#     str_detect(bt.corp$text, user.regex)
#   },
#
#   times = 50
# )
#
# autoplot(mbm.words)
#
# pl.lookup<- ggplot(mbm.words, aes(x = time/1000000000, y= expr))+
#   geom_boxplot()+
#   stat_summary(geom = "point", fun = mean, color = "red")+
#   labs(x = "Time\n[Seconds]",
#        y = "Keyword lookup method\n",
#        title = "Comparing different key word lookup methods ",
#        subtitle = "Each operation repeated 50 times")
#
# ggsave("BenchmarkLookup.png", pl.lookup, width = 20, height = 11, units = "cm")
#
#
# # Export corpus feather object for testing
# write_feather(bt.corp, "./PLS-words/Data/bt_corp.feather")



# # Benchmark all (server) code elements ####
# 
# mbm.server <- microbenchmark(
#   "V1: Based on tokens > loockup" = { source("X-MockServerCodeV1.R") },  # Server routine of the initial words app
#   "V2: Based on feather df > str_count" = { source("X-MockServerCodeV2.R") },  # Server routine based on featehr string count
#   times = 50L
# 
#   )
# 
# pl.server<- ggplot(mbm.server, aes(x = time/1000000000, y= expr))+
#   geom_boxplot()+
#   stat_summary(geom = "point", fun = mean, color = "red")+
#   labs(x = "Time\n[Seconds]",
#        y = "Server routine",
#        title = "Improvement of server routine?",
#        subtitle = "Each operation repeated 50 times")
# 
# ggsave("BenchmarkServer.png", pl.server, width = 20, height = 11, units = "cm")
# 
