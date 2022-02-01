library(shiny)
library(shinythemes)

library(tidyverse) # 1.3.0
library(quanteda) # 3.2.0
library(plotly) # 4.10.0
library(gmodels)

# OPTED blue
# #0063a6


# Object selection ####
# Basis to let the app select the right data based on user parliament selection
parl.select <- data.frame(parliament = c("UK-HouseOfCommons", "DE-Bundestag"),
                          dev.path = c("./PLS-words/Data/hc_tok.rds", "./PLS-words/Data/bt_tok.rds"), # relative path from project directory
                          app.path = c("./Data/hc_tok.rds", "./Data/bt_tok.rds")) # Relative path from app directory



# Define UI
ui <- fluidPage(
                # theme = shinytheme("cerulean"),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "cerulean.OPTED.css") # Edited cerulean scheme with OPTED branding
                ),
                navbarPage(
                  title = "Words in Parliament",
                  # title = div(
                  #   "Words in Parliament",
                  #   div(
                  #     id = "img-id",
                  #     img(src = "opted_logo.png",
                  #         style = "display: block;
                  #                   width: 100px;
                  #                   height: 100px;
                  #                   margin-left: auto;")
                  #   )
                  # ),
                  id = "tabs",
                  tabPanel("Overview & Input",
                           sidebarLayout(position = "right",
                           sidebarPanel(
                             tags$h3("Your choices"),
                             selectInput("parl", label = h4("Parliament"),
                                         choices = list("DE: Bundestag" = "DE-Bundestag", 
                                                        "UK: House Of Commons" = "UK-HouseOfCommons"), 
                                         selected = 1),
                             helpText("Here you can choose one of the parliamentary chambers that we currently feature."),
                             textInput("words", label = h4("Words"), "migration*, flucht*"),
                             helpText("Enter the word(s) you want to track in parliamentary speeches here. You can combine multiple words in a comma-separated list. Use the * wildcard to match one or several characters."),
                             helpText("Once you submit your choices, we collect and analyze the data for you. Given the size of the text corpora, this may take a couple of seconds."),
                             actionButton("submit", label = "Submit!")
                             # submitButton(text = "Submit!")
                             
                           ), # sidebarPanel
                           mainPanel(
                             h3("Words in Parliament"),
                             p("This application allows you to quickly analyze whether and how
                               specific words have featured in the political debates of different national parliaments in Europe."),
                             p("Once you have chosen the parliament and the words of interest to you on the right-hand side,
                               we extract three sets of results for you:"),
                             
                             tags$ul(tags$li(HTML("Prominence of the words in parliament <b>over time</b>")),
                                     tags$li(HTML("Prominence of the words in parliaments <b>across different parties</b>")),
                                     tags$li(HTML("Prominence of the words in parliaments <b>across individual speakers</b>"))),
                             p(""),
                             p("Use the navigation on top of this page to jump to these results. The graphics can be customized and saved (just hoover over them). 
                               You may also download the aggregated data for each set of results."),
                             p(HTML("If you <b>use this material in your work</b>, 
                                    please refer to this application 
                                    and cite <a href=\"https://doi.org/10.7910/DVN/L4OAKN\" target=\"_blank\">Rauh and Schwalbach 2020</a> as the data source.")),
                             p("Authorship, OPTED reference, funding stuff HERE."),
                             p(""),
                             p("And now: Have fun!"),
                             p(""),
                             
                             h3("Your current selection"),
                             verbatimTextOutput("summary")
                             
                           ) # mainPanel
                           ) # sidebarLayout

                  ), # Navbar 1, tabPanel
                  tabPanel("Time", 
                           sidebarLayout(position = "left",
                                         sidebarPanel(width = 3, # Out of 12
                                                      helpText("This plot illustrates the prominence of your keywords in the respective parliament over time."),
                                                      helpText("It shows the monthly share of speeches in which at least one of the key words was used."),
                                                      helpText("Hoover over the plot to select your preferred time series, to zoom in or out, and to save the resulting picture."),
                                                      helpText("You can also download the underlying monthly time series, but note the usage and citation requirements on the main page."),
                                                      downloadButton("downloadTime", "Download time series")),
                                         mainPanel(plotlyOutput("timeplot"))
                           )
                  ),
                  tabPanel("Parties",
                           sidebarLayout(position = "left",
                                         sidebarPanel(width = 3, # Out of 12
                                                      helpText("This plot illustrates the prominence of your keywords across the parties in the respective parliament."),
                                                      helpText("It shows the share of partisan speeches in which at least one of the key words was used (including the 95% confidence interval around this average value)."),
                                                      helpText("Hoover over the plot to customize it or to save the resulting picture."),
                                                      helpText("You can also download the party shares below, but note the usage and citation requirements on the main page."),
                                                      downloadButton("downloadParty", "Download party shares")),
                                         mainPanel(plotlyOutput("partyplot"))
                           )
                  ),
                  tabPanel("Speakers",
                           sidebarLayout(position = "left",
                                         sidebarPanel(width = 3, # Out of 12
                                                      helpText("This plot illustrates the prominence of your keywords across the individual in the respective parliament."),
                                                      helpText("It shows the share of of keywords of all words a speaker has uttered in parliament (speaking time varies a lot!), listing only the top 25 speakers along that measure."),
                                                      helpText("Hoover over the plot to customize it or to save the resulting picture."),
                                                      helpText("You can also download the values for all speakers below, but note the usage and citation requirements on the main page."),
                                                      downloadButton("downloadSpeaker", "Download speaker shares")),
                                         mainPanel(plotlyOutput("speakerplot"))
                           )
                  )
                ) # navbarPage
) # fluidPage



# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Update user parliament choice upon submit button
  user.parliament <- eventReactive(input$submit, {
    input$parl
  })

  # Update user word(s) choice upon submit button
  user.words <- eventReactive(input$submit, {
      input$words %>%
      str_split(",") %>%
      unlist() %>%
      str_trim(side = "both")
  })
  
  # Quanteda dictionary of key words
  user.dict <- eventReactive(input$submit, {
    list(hits = user.words()) %>% 
    dictionary()
    })
  
  # Load tokens object of selected parliament, and select key words
  # The time killer
  out.tok <- eventReactive(input$submit, {
    
    showModal(modalDialog("Collecting data and searching your key words! Just a few seconds ...", footer=NULL))
    
    toks <- read_rds(parl.select$app.path[parl.select$parliament == user.parliament()]) %>%
    # tokens_subset(date >= user.mindate &
    #               date <= user.maxdate) %>%
    tokens_lookup(user.dict(),
                  case_insensitive = TRUE,  # Not case sensitive
                  exclusive = TRUE,         # Keep only tokens matching user word selection
                  valuetype = "glob")       # Allow for * wild cards
    
    removeModal()
    
    return(toks)
    })
   
  # Get data frame
  out.data <- eventReactive(input$submit, {
    
    showModal(modalDialog("Count your key words! Almost there ...", footer=NULL))
    
    df <- 
      out.tok() %>% 
      dfm() %>% # Equals freq of 'hits' key from above lookup
      convert(to = "data.frame") %>% 
      cbind(docvars(out.tok()))  %>% # row order should be consistent
      select(-c(id, doc_id)) %>% 
      mutate(user.words.pres = as.numeric(hits > 0)) # are user words present in speech?
    
    removeModal()
    
    return(df)
  })
  
  # Time series data
  time.data <- reactive({
    out.data() %>% 
    group_by(month) %>% 
    summarise(share = mean(user.words.pres) *100) %>% 
    mutate(share.ma = stats::filter(share, rep(1,5), sides = 2)/5) %>% 
    pivot_longer(2:3) %>% 
    mutate(series = ifelse(name == "share", "Monthly", "Moving average (5 months)")) %>% 
    select(-name) %>% 
    arrange(month, series)
    })
  
  # Time series plot
  output$timeplot <- renderPlotly({
    
    time.breaks <- unique(time.data()$month)
    time.breaks <- time.breaks[which(str_detect(time.breaks, "-01"))] # Only January 
    time.labels <- time.breaks %>% str_remove_all("-.*?$")
    
    
    time.gg <- ggplot(time.data(), aes(y = value, x = month, color = series, size = series, group = series))+
      geom_line()+
      scale_x_discrete(breaks = time.breaks, labels = time.labels)+
      scale_color_manual(values = c("grey60", "#0063a6"), name = "Time series: ")+
      scale_size_manual(values = c(.5, 1.2), name = "Time series: ")+
      labs(title = "Keywords over time",
           subtitle = paste("Parliament: ", user.parliament(), ". Keywords: ", paste(user.words(), collapse = ", "), sep = ""),
           x = "Month",
           y = "Share of parliamentary speeches\nwith at least one keyword (%)\n")+
      theme_bw()+
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90, vjust = .5, hjust = -1))
    
    time.pl <- ggplotly(time.gg, tooltip = c("y", "x")) %>%
      layout(title = list(text = paste0('Keywords over time',
                                        '<br>',
                                        '<sup>',
                                        paste("Parliament: ", user.parliament(), ". Keywords: ", paste(user.words(), collapse = ", "), sep = ""),'</sup>')),
             legend = list(orientation = "h", x = 0.3, y = -0.2))
    
    return(time.pl)
  })
  
  # Time series download
  output$downloadTime <- downloadHandler(
      filename = function() {
        paste('MyWordsInParliament-TimeSeries', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(time.data(), con, row.names = F)
      }
    )
  
  
  # Party data
  party.data <- reactive({
    df <- 
      out.data() %>% 
      group_by(party) %>% 
      summarise(share = gmodels::ci(user.words.pres)[1],
              lo = gmodels::ci(user.words.pres)[2],
              hi = gmodels::ci(user.words.pres)[3]) %>% 
      mutate(across(2:4, function(x){x*100})) %>% # Percentages
      filter(party != "independent" ) %>% 
      filter(!is.na(party)) %>% 
      arrange(share) %>% 
      mutate(party = factor(party)) # implicitly ordered by mean
  
  df$party <- fct_reorder(df$party, df$share, mean)
  
  return(df)
  })
  
  # Party plot
  output$partyplot <- renderPlotly({
    parties.gg <- ggplot(party.data(), aes(y = party))+
      geom_vline(xintercept = mean(party.data()$share), linetype = "dashed")+
      geom_linerange(aes(xmin = lo, xmax = hi), color = "#0063a6")+
      geom_point(aes(x=share), color = "#0063a6")+
      labs(title = "Keywords by party of speaker",
           subtitle = paste("Parliament: ", user.parliament(), ". Keywords: ", paste(user.words(), collapse = ", "), sep = ""),
           x = "Share of parliamentary speeches\nwith at least one keyword (%)\n",
           y = "")+
      theme_bw()+
      theme(legend.position = "none",
            axis.text = element_text(color = "black"))

    party.pl <- ggplotly(parties.gg, tooltip = c("y", "x")) %>%
      layout(title = list(text = paste0('Keywords by party of speaker',
                                        '<br>',
                                        '<sup>',
                                        paste("Parliament: ", user.parliament(), ". Keywords: ", paste(user.words(), collapse = ", "), sep = ""),'</sup>')))

    return(party.pl)
  })
  
  # Party download
  output$downloadParty <- downloadHandler(
    filename = function() {
      paste('MyWordsInParliament-PartyShares', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(party.data(), con, row.names = F)
    }
  )
  
  
  # Speaker data 
  speaker.data <- reactive({
    sdf <-
      out.data() %>% 
      filter(party != "independent" ) %>% 
      filter(!is.na(party)) %>% 
      mutate(speaker = paste0(speaker, " (",party, ")")) %>% 
      select(-party) %>% 
      group_by(speaker) %>% 
      summarise(mentions = sum(hits),  
                totalwords = sum(terms)) %>% 
      mutate(share = (mentions/totalwords)*100) %>% 
      arrange(desc(share))
    
    sdf$speaker <- fct_reorder(sdf$speaker, sdf$share, mean)
    
    return(sdf)
    
  })
  
  # Speaker plot
  output$speakerplot <- renderPlotly({
    speaker.gg <- ggplot(head(speaker.data(), 25), aes(y = speaker, x = share))+
      geom_col(fill = "#0063a6", width = .7)+
      geom_vline(xintercept = mean(speaker.data()$share), linetype = "solid", color = "red")+
      scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
      labs(title = "Keyword usage by individual speakers (Top 25, in relative terms)",
           subtitle = paste("Parliament: ", user.parliament(), ". Keywords: ", paste(user.words(), collapse = ", "), sep = ""),
           x = "Share of keywords\namong all words spoken in parliament (%)\n",
           y = "")+
      theme_bw()+
      theme(legend.position = "none",
            axis.text = element_text(color = "black"))
    
    ggplotly(speaker.gg, tooltip = c("y", "x")) %>%
      layout(title = list(text = paste0('Keyword usage by individual speakers (Top 25, in relative terms)',
                                        '<br>',
                                        '<sup>',
                                        paste("Parliament: ", user.parliament(), ". Keywords: ", paste(user.words(), collapse = ", "), sep = ""),'</sup>')))
    
  })
  
  # Speaker download
  output$downloadSpeaker <- downloadHandler(
    filename = function() {
      paste('MyWordsInParliament-Speakers-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(party.data(), con, row.names = F)
    }
  )
  

  # Summary output for start page
  output$summary <- renderText({
    paste0("Parliament:\t\t", user.parliament(), "\n",
          "Keywords:\t\t", paste(user.words(), collapse = ", "), "\n",
          "Total n of speeches:\t", nrow(out.data()), "\n",
          "Speeches with keywords:\t", sum(out.data()$user.words.pres), " (", round(sum(out.data()$user.words.pres)/nrow(out.data())*100, 2), "%)", "\n")
    })

  
}




shinyApp(ui, server)