# example app for prepending/appending a navbarMenu
ui <- navbarPage("Navbar page", id = "tabs",
                 tabPanel("Home",
                          actionButton("prepend", "Prepend a navbarMenu"),
                          actionButton("append", "Append a navbarMenu")
                 )
)
server <- function(input, output, session) {
  observeEvent(input$prepend, {
    id <- paste0("Dropdown", input$prepend, "p")
    prependTab(inputId = "tabs",
               navbarMenu(id,
                          tabPanel("Drop1", paste("Drop1 page from", id)),
                          tabPanel("Drop2", paste("Drop2 page from", id)),
                          "------",
                          "Header",
                          tabPanel("Drop3", paste("Drop3 page from", id))
               )
    )
  })
  observeEvent(input$append, {
    id <- paste0("Dropdown", input$append, "a")
    appendTab(inputId = "tabs",
              navbarMenu(id,
                         tabPanel("Drop1", paste("Drop1 page from", id)),
                         tabPanel("Drop2", paste("Drop2 page from", id)),
                         "------",
                         "Header",
                         tabPanel("Drop3", paste("Drop3 page from", id))
              )
    )
  })
}

shinyApp(ui, server)