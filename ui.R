library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)

header <- dashboardHeader()

sidebar <- dashboardSidebar(
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  sidebarMenu(
    menuItem("Home", tabName = "h", icon = icon("home")),
    menuItem("Data Analysis", tabName = "e", icon = icon("poll")),
    menuItem("Text Analysis", tabName = "t", icon = icon("book")),
    menuItem("Check Tweets", tabName = "c", icon = icon("search")),
    menuItem("Data", tabName = "d", icon = icon("folder"))
  )
)

body <- dashboardBody(
  custom_theme,
  tabItems(
    tabItem(
      "h",
      HTML("<h3><b>Analyzing Tweets about Omnibus Law</b></h3><br><br>"),
      valueBoxOutput("val1"), valueBoxOutput("val2"), valueBoxOutput("val3"),
      br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("plotly1")
    ),
    
    tabItem(
      "e",
      box(width = 12, title = HTML("<b>Top Users</b>"),
        dropdownButton(inputId = "set", right = T,
                     radioButtons("by", "Metrics", choiceValues = c("twt", "w_c", "rel"), 
                                  choiceNames = c("Tweets", "Word Count", "Relevance Score"), inline = T),
                     sliderInput("top_n", "Number of Users", 5, 30, 20),
                     circle = TRUE, status = "danger", icon = icon("gear")
        ),
        bsTooltip("set", "Plot Settings"),
        plotlyOutput("top_user")), br(), br(),
      fluidRow(
        box(
          radioButtons("var", "Variable", inline = T, choiceNames = c("Gender", "Polarity", "Relevance Score"),
                       choiceValues = c("gender", "polarity", "relevance")),
          plotlyOutput("data_dist"), 
          title = HTML("<b>Data Distribution</b>"), solidHeader = F, height = "545px"),
        box(
          prettySwitch("outlier", "Remove Outliers", F, "info", F, T),
          div(style="display: inline-block", uiOutput("ui")),
          div(style="display: inline-block", uiOutput("ui2")),
          plotlyOutput("foll_post"), 
          title = HTML("<b>Followers vs Posts</b>"), solidHeader = F, height = "545px"))
    ),
    
    tabItem(
      "t",
      box(width = "900px", background = "olive",
        radioButtons("text", "Phrase Type", inline = T,
                       choices = c("Words", "Noun-Noun Phrase", "Noun-Adjective Phrase"))),
      plotlyOutput("word_count"), br(), br(),
      box(HTML("<h2><b>Word Cloud</b></h2>"), width = "900px", background = "olive"),
      wordcloud2Output("word_cloud"), br(), br()
    ),
    
    tabItem(
      "c", 
      box(HTML("<h2><b>Check Tweet Status</b></h2>"), width = "950px", background = "light-blue"),
      textAreaInput("tweets", NULL, height = "200px", width = "920px", placeholder = "Type something..."),
      actionButton("check", "Check"), br(), br(),
      valueBoxOutput("status1"), valueBoxOutput("status2"), valueBoxOutput("status3")
    ),
    
    tabItem(
      "d", DTOutput("dt")
    )
    
  )
)

dashboardPage(header, sidebar, body)