###################################################################
# Project:  OPTED WP5 Apps
# Task:     Develop and design desired outputs for the "words" app
# Author:   Christian Rauh (26.01.2022)
###################################################################

# Packages ####
library(tidyverse) # 1.3.0
library(quanteda) # 3.2.0
library(plotly) # 4.10.0
library(gmodels)


# Example data from ParlSpeech V2 ####
# Include here all parliaments that shall be part of the app

bt <- read_rds("D:/Dropbox/ParlSpeechV2/Corpora/V2-Corpora/Corp_Bundestag_V2.rds") %>% 
  filter(!chair) %>% # Drop speeches from the chair
  select(-chair) %>% # Drop chair marker
  select(-iso3country) %>% 
  mutate(speechnumber = as.numeric(speechnumber)) %>%     # Attention!
  mutate(id = row_number(),                              # ID to enable restoring of order later
           count = 1,                                      # Simple counter for aggregation purposes
           month = str_remove(date, "-[0-9]{2}$")) %>%     # Month of speech, for aggregation
  relocate(parliament, id, count, date, month) %>%
  select(-speechnumber)

hc <- read_rds("D:/Dropbox/ParlSpeechV2/Corpora/V2-Corpora/Corp_HouseOfCommons_V2.rds") %>% 
  filter(!chair) %>% # Drop speeches from the chair
  select(-chair) %>% # Drop chair marker
  select(-iso3country) %>% 
  mutate(speechnumber = as.numeric(speechnumber)) %>%     # Attention!
  mutate(id = row_number(),                              # ID to enable restoring of order later
         count = 1,                                      # Simple counter for aggregation purposes
         month = str_remove(date, "-[0-9]{2}$")) %>%     # Month of speech, for aggregation
  relocate(parliament, id, count, date, month) %>%
  select(-speechnumber)

# Combined data set would be more convenient programming wise and could be the basis for the data selector app later
# but construction separate tokens or dfm objects might be advisable for memory reasons and app loading times
# pls <- rbind(bt, hc) %>% 
#   arrange(parliament, date, speechnumber) %>%            # Order!
#   mutate(id = row_number(),                              # ID to enable restoring of order later
#          count = 1,                                      # Simple counter for aggregation purposes
#          month = str_remove(date, "-[0-9]{2}$")) %>%     # Month of speech, for aggregation
#   relocate(parliament, id, count, date, month) %>% 
#   select(-speechnumber)
# 
# rm(list=setdiff(ls(), "pls"))
# gc()


# Tokens objects ####

# The data basis of the words app
# Compared to DFM, tokens objects contain a little less space on disk and preserve word order

# Indicate stopword removal in the app !
# https://quanteda.io/reference/stopwords.html

bt.tok <- corpus(bt$text,
                 docvars = bt[, 1:10],) %>% # Adapt if the above variable selection changes!
  tokens(remove_punct = T,
         remove_numbers = T,
         verbose = T) %>% 
  tokens_remove(stopwords("german"))
write_rds(bt.tok, "./PLS-words/Data/bt_tok.rds")


hc.tok <- corpus(hc$text,
                 docvars = hc[, 1:10],) %>% # Adapt if the above variable selection changes!
  tokens(remove_punct = T,
         remove_numbers = T,
         verbose = T) %>% 
  tokens_remove(stopwords("english"))
write_rds(hc.tok, "./PLS-words/Data/hc_tok.rds")

rm(list=ls())
gc()


# Object selection ####
# Basis to let the app sellect the right data based on user parliament selection
parl.select <- data.frame(parliament = c("UK-HouseOfCommons", "DE-Bundestag"),
                           dev.path = c("./PLS-words/Data/hc_tok.rds", "./PLS-words/Data/bt_tok.rds"), # relative path from project directory
                           app.path = c("./Data/hc_tok.rds", "./Data/bt_tok.rds")) # Relative path from app directory


# Input parameters ####
# This is what the app user may select/choose later

# Parliament
# Dropdown options: parliament ids in tok docvars, for now "UK-HouseOfCommons" or "DE-Bundestag"
user.parliament <- "DE-Bundestag"

# Date range 
# Default should be maximum available range
# Probably not even necessary as a parameter, plotly allows axis-based selection
user.mindate <- "1980-01-01"
user.maxdate <- "2022-12-31"

# The words!
# Allow multiple words by comma separation
user.words <- "migration*, flucht*" %>% 
  str_split(",") %>% 
  unlist() %>% 
  str_trim(side = "both")

user.dict <- list(hits = user.words) %>% 
  dictionary()

# think about compound tokens! "concatenator"


# Data selection ####
# For now, parliament, time period, and keywords of interest

out.tok <- read_rds(parl.select$dev.path[parl.select$parliament == user.parliament]) %>% 
  # tokens_subset(date >= user.mindate & 
  #               date <= user.maxdate) %>% 
  tokens_lookup(user.dict,
                case_insensitive = TRUE,  # Not case sensitive
                exclusive = TRUE,         # Keep only tokens matching user word selection
                valuetype = "glob")       # Allow for * wild cards

out.data <- out.tok %>% 
  dfm() %>% # Equals freq of 'hits' key from above lookup
  convert(to = "data.frame") %>% 
  cbind(docvars(out.tok))  %>% # row order should be consistent
  select(-c(id, doc_id)) %>% 
  mutate(user.words.pres = as.numeric(hits > 0)) # are user words present in speech?

# sum(out.data$user.words.pres)



# Over time ####

time.data <- out.data %>% 
  group_by(month) %>% 
  summarise(share = mean(user.words.pres) *100) %>% 
  mutate(share.ma = stats::filter(share, rep(1,5), sides = 2)/5) %>% 
  pivot_longer(2:3) %>% 
  mutate(series = ifelse(name == "share", "Monthly", "Moving average (5 months)")) %>% 
  select(-name) %>% 
  arrange(month, series)

time.breaks <- unique(time.data$month)
time.breaks <- time.breaks[which(str_detect(time.breaks, "-01"))] # Only January 
time.labels <- time.breaks %>% str_remove_all("-.*?$")
  

time.gg <- ggplot(time.data, aes(y = value, x = month, color = series, size = series, group = series))+
  geom_line()+
  scale_x_discrete(breaks = time.breaks, labels = time.labels)+
  scale_color_manual(values = c("grey60", "darkblue"), name = "Time series: ")+
  scale_size_manual(values = c(.5, 1.2), name = "Time series: ")+
  labs(title = "Keywords over time",
       subtitle = paste("Parliament: ", user.parliament, ". Keywords: ", paste(user.words, collapse = ", "), sep = ""),
       x = "Month",
       y = "Share of parliamentary speeches\nwith at least one keyword (%)\n")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = -1))

time.pl <- ggplotly(time.gg, tooltip = c("y", "x")) %>%
  layout(title = list(text = paste0('Keywords over time',
                                    '<br>',
                                    '<sup>',
                                    paste("Parliament: ", user.parliament, ". Keywords: ", paste(user.words, collapse = ", "), sep = ""),'</sup>')),
         legend = list(orientation = "h", x = 0.3, y = -0.2))


# Parties ####

party.data <- out.data %>% 
  group_by(party) %>% 
  summarise(share = gmodels::ci(user.words.pres)[1],
            lo = gmodels::ci(user.words.pres)[2],
            hi = gmodels::ci(user.words.pres)[3]) %>% 
  mutate(across(2:4, function(x){x*100})) %>% # Percentages
  filter(party != "independent" ) %>% 
  filter(!is.na(party)) %>% 
  arrange(share) %>% 
  mutate(party = factor(party)) # implicitly ordered by mean

party.data$party <- fct_reorder(party.data$party, party.data$share, mean)


parties.gg <- ggplot(party.data, aes(y = party))+
  geom_vline(xintercept = mean(party.data$share), linetype = "dashed")+
  geom_linerange(aes(xmin = lo, xmax = hi), color = "darkblue")+
  geom_point(aes(x= share), color = "darkblue")+
  labs(title = "Keywords by party of speaker",
       subtitle = paste("Parliament: ", user.parliament, ". Keywords: ", paste(user.words, collapse = ", "), sep = ""),
       x = "Share of parliamentary speeches\nwith at least one keyword (%)\n",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplotly(parties.gg, tooltip = c("y", "x")) %>%
  layout(title = list(text = paste0('Keywords by party of speaker',
                                    '<br>',
                                    '<sup>',
                                    paste("Parliament: ", user.parliament, ". Keywords: ", paste(user.words, collapse = ", "), sep = ""),'</sup>')))
  

# Speakers ####  

speaker.data <- out.data %>% 
  filter(party != "independent" ) %>% 
  filter(!is.na(party)) %>% 
  mutate(speaker = paste0(speaker, " (",party, ")")) %>% 
  select(-party) %>% 
  group_by(speaker) %>% 
  # summarise(share = gmodels::ci(user.words.pres)[1],
  #           lo = gmodels::ci(user.words.pres)[2],
  #           hi = gmodels::ci(user.words.pres)[3]) %>% 
  summarise(mentions = sum(hits),  
            totalwords = sum(terms)) %>% 
  mutate(share = (mentions/totalwords)*100) %>% 
  arrange(desc(share))

speaker.data$speaker <- fct_reorder(speaker.data$speaker, speaker.data$share, mean)

speaker.gg <- ggplot(head(speaker.data, 25), aes(y = speaker, x = share))+
  geom_col(fill = "darkblue", width = .7)+
  geom_vline(xintercept = mean(speaker.data$share), linetype = "solid", color = "red")+
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
  labs(title = "Keyword usage by individual speakers (Top 25, in relative terms)",
       subtitle = paste("Parliament: ", user.parliament, ". Keywords: ", paste(user.words, collapse = ", "), sep = ""),
       x = "Share of keywords\namong all words spoken in parliament (%)\n",
       y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplotly(speaker.gg, tooltip = c("y", "x")) %>%
  layout(title = list(text = paste0('Keyword usage by individual speakers (Top 25, in relative terms)',
                                    '<br>',
                                    '<sup>',
                                    paste("Parliament: ", user.parliament, ". Keywords: ", paste(user.words, collapse = ", "), sep = ""),'</sup>')))

