library(shiny)
library(shinythemes)

library(tidyverse) # 1.3.0
library(quanteda) # 3.2.0
library(plotly) # 4.10.0
library(gmodels)

# OPTED blue
# #0063a6

# <a href=\"https://doi.org/10.7910/DVN/L4OAKN\" target=\"_blank\">Rauh and Schwalbach 2020</a>


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
                  id = "tabs",
                  tabPanel("Overview & Input",
                           sidebarLayout(position = "right",
                           sidebarPanel(
                             # tags$h3("Your choices"),
                             selectInput("parl", label = h4("Parliament"),
                                         choices = list("DE: Bundestag" = "DE-Bundestag", 
                                                        "UK: House Of Commons" = "UK-HouseOfCommons"), 
                                         selected = 1),
                             helpText("Choose one of the parliamentary chambers we currently feature."),
                             textInput("words", label = h4("Words"), "migration*, flucht*"),
                             helpText("The word(s) you want to track in parliamentary speeches. Combine multiple words in a comma-separated list. Use the * wildcard to match one or several characters."),
                             helpText("Once you submit your choices, we collect and analyze the data for you. Given the size of the text corpora, this may take a couple of seconds."),
                             actionButton("submit", label = "Submit!", style = "color: #0063a6;")
                             # submitButton(text = "Submit!")
                             
                           ), # sidebarPanel
                           mainPanel(
                             h4("Words in Parliament"),
                             p(HTML("<i style = \"color: #0063a6\">Quickly analyze how
                               specific words have featured in political debates of different national parliaments in Europe.</i>")),
                             p(HTML("")),
                             p(HTML("Once you have chosen the words and the parliament of interest to you on the right-hand side,
                               we visualize their prominence in all plenary speeches over <i>time</i>, across <i>different parties</i>, and across <i>individual speakers</i>.
                                    <br> The menu on top of this page leads you to these results. All graphics can be customized and saved by hoovering over them. 
                               You may also download the underlying data.")),
                             p(HTML("Before <i>using this material in your work</i>, please consult the 'About' page above.")),
                             p(HTML("This proto-type application has been developed in <a href=\"https://opted.eu/designing-an-infrastructure/wp5-parliamentary-government-and-legal-texts\" target=\"_blank\">Work Package 5</a> 
                                    of the <a href=\"https://opted.eu\" target=\"_blank\">OPTED initiative</a>. 
                                    This project has received funding from the European Union’s Horizon 2020 research & innovation programme under <a href=\"https://cordis.europa.eu/project/id/951832\" target=\"_blank\">grant agreement No 951832</a>.<br>")),
                             p(HTML("<br>")),

                             h4("Your current selection"),
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
                  ),
                  tabPanel("About", 
                           h3("Background"),
                           a(img(src='OPTED_logo_transparent.png', style = "float:right; width: 150px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
                           p(HTML("<p>This proto-type application has been developed in the context of the <a href=\"https://opted.eu\" target=\"_blank\">OPTED initiative</a>, aiming to facilitate access to systematic information from political texts.<p>
                           It is part of <a href=\"https://opted.eu/designing-an-infrastructure/wp5-parliamentary-government-and-legal-texts\" target=\"_blank\">Work Package 5</a>, focusssing on parliamentary, government and legal texts, in particular.<p>
                           The project has received funding from the European Union’s Horizon 2020 research & innovation programme under <a href=\"https://cordis.europa.eu/project/id/951832\" target=\"_blank\">grant agreement No 951832</a>.<p>
                           For feedback and questions on this particular application, please contact to <a href=\"http://christian-rauh.eu\" target=\"_blank\">Christian Rauh</a>. <br> ")),
                           p(HTML("<br>")),
                           h3("How to cite"),
                           p(HTML("When using any of the material generated here in your own work, please refer to this application and cite the underlying data sources.<br>
                           At the moment, all parliamentary text data are drawn from <a href=\"https://doi.org/10.7910/DVN/L4OAKN\" target=\"_blank\">Rauh and Schwalbach 2020</a>.")),
                           p(HTML("<br>")),
                           
                           h3("Project partners"),
                           p(HTML("<br>")),
                           tags$table(style = "border-collapse: separate; border-spacing: 50px 0; padding: 10px 0;",
                                      tags$tr(tags$th(a(img(src='WZB_Komb_portrait_Web_engl.png', style = "float:center; height: 100px; size: contain;", alt = "WZB"), href = "https://www.wzb.eu/en/persons/christian-rauh")),
                                              tags$th(a(img(src='pti_logo.png', style = "float:center; height: 100px; size: contain;", alt = "TKPTI"), href = "https://politikatudomany.tk.hu/en/researcher/sebok-miklos")),
                                              tags$th(a(img(src='CCCP_Logo.png', style = "float:center; height: 100px; size: contain;", alt = "CCCP"), href = "https://cccp.uni-koeln.de/en/team/core-faculty/prof-dr-sven-oliver-proksch"))),
                                      tags$tr(tags$td(HTML("<br>")),
                                              tags$td(HTML("<br>")),
                                              tags$td(HTML("<br>"))),
                                      tags$tr(tags$td("Christian Rauh"),
                                              tags$td(HTML("Miklós Sebők<br>Anna Székely<br>Péter Visnovitz")),
                                              tags$td(HTML("Sven-Oliver Proksch<br>Jan Schwalbach<br>Alexander Dalheimer"))))
                           # p(a(img(src='WZB_Komb_portrait_Web_engl.png', style = "float:left; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
                           #   a(img(src='pti_logo.png', style = "float:center; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"),
                           #   a(img(src='CCCP_Logo.png', style = "float:rigth; height: 100px; size: contain;", alt = "OPTED initiative"), href="https://opted.eu"))
                           
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