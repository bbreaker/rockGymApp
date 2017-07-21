pacman::p_load(shiny, markdown, ggplot2, dplyr, scales)

source("getInfo.R")

ui <- navbarPage("LRCC Boulder League",
  tabPanel("Participant Entry",
    sidebarLayout(
      sidebarPanel(
        textInput("name", "What's your handle?", "blank"),
        textInput("handi", "What's your handicap? (C1, C2,... C7)", ""),
        submitButton("Send it in!")
      ),
      mainPanel(
        textOutput("entryMessage")
      )
    )
  ),
  tabPanel("Enter Problems",
    sidebarLayout(
      sidebarPanel(
        selectInput("climber", "Who are you?", choices = listPeeps()),
        selectInput("problem", "What problem did you finish?", choices = listProblems()),
        numericInput("attempts", "How many attempts to finish it?", value = NA),
        submitButton("Send it in!")
    ),
      mainPanel(
        plotOutput("allPlot", height = "720px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  source("getInfo.R")
   
  output$entryMessage <- renderText({
    allInfo <- listAllInfo()
    if (input$name == "blank") {
      newMessage <- "Please enter your information."
    } else if (input$name %in% allInfo$Climber) {
      newMessage <- paste0("Sorry, ", input$name, " (if that's your real name) but that name already exists in the system.")
    } else {
      newEntry <- data.frame(Climber = input$name, Handicap = toupper(input$handi))
      allInfo <- dplyr::bind_rows(allInfo, newEntry)
      newMessage <- paste0("Thanks, ", input$name, "!. You've been entered. Go climb!")
      write.csv(allInfo, "data/people", row.names = FALSE)
    }
    updateTextInput(session, "climber")
    print(newMessage)
  })
  
  observe({
    x <- input$name
    if (x == "blank") {
      x <- NULL
    } else {
      updateSelectInput(session, "climber", "Who are you?", choices = listPeeps())
    }
  })

  output$allPlot <- renderPlot({
    allInfo <- listAllInfo()
    newName <- input$climber
    problem <- input$problem
    attempts <- as.numeric(input$attempts)
    allInfo[allInfo$Climber == newName, names(allInfo) == problem] <- as.numeric(attempts)
    write.csv(allInfo, "data/people", row.names = FALSE)
    allInfo <- listAllInfo()
    allProblems <- listAllProblems()
    probsVec <- t(names(allInfo))[4:length(names(allInfo))]
    plotAttempts <- dplyr::filter(allInfo, Climber == newName)
    plotAttempts <- data.frame(plotAttempts)
    plotAttempts <- as.numeric(plotAttempts[,4:length(plotAttempts)])
    plotDat <- data.frame(Problem = probsVec, Attempts = plotAttempts, stringsAsFactors = FALSE)
    gradeVec <- allProblems[allProblems$Problem == plotDat$Problem, 2]
    plotDat$Grade <- gradeVec$Grade
    plotDat$Handicap <- as.character(allInfo[allInfo$Climber == newName, 2])
    plotDat$Score <- NA
    for (i in seq(1, nrow(plotDat), 1)) {
      plotDat[i,5] <- compScore(plotDat[i, 4], plotDat[i, 3], plotDat[i, 2])
    }
    newScore <- sum(plotDat$Score)
    allInfo[allInfo$Climber == newName, 3] <- newScore
    write.csv(allInfo, "data/people", row.names = FALSE)
    p <- ggplot(data = plotDat, aes(x = Problem, y = Attempts, fill = Grade)) +
      geom_bar(stat = "identity") +
      labs(title = paste0("Hello, ", newName, "! Your score is ", newScore, ".")) +
      scale_y_continuous(breaks = pretty_breaks()) +
      scale_fill_gradient(low = "green", high = "red") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, size = 14, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 14),
            axis.title = element_text(size = 16),
            title = element_text(size = 16),
            legend.text = element_text(size = 12))
    p
  })
}

shinyApp(ui = ui, server = server)

