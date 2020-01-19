library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(stringr)

#If connection reaches limit
#dbDisconnect(con)

#First run
con = dbConnect(RMySQL::MySQL(), dbname = "data_base",username = "root", password = "f129968890"
                    ,host = "localhost", port = 3306)




# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: myuser  Password: mypass"),
                     br(),
                     tags$code("Username: myuser1  Password: mypass1")
                     ))
                     )

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = sapply(c("mypass", "mypass1"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Digital Music Platform", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItem(tabName ="dashboard", class = "active",
              fluidRow(
                box(width = 12, dataTableOutput('results'))
              ))
      fluidPage(
        title = "Examples of DataTables",
        sidebarLayout(
          sidebarPanel(
            conditionalPanel(
              'input.dataset === "dbReadTable(con, "song")"',
              checkboxGroupInput("show_vars", "Columns in song to show:",
                                 names(dbReadTable(con, "song")), selected = names(dbReadTable(con, "song")))
            ),
            conditionalPanel(
              'input.dataset === "datatable(dbReadTable(con, "albums")"',
              helpText("Click the column header to sort a column.")
            ),
            conditionalPanel(
              'input.dataset === "datatable(dbReadTable(con, "boards")"',
              helpText("Display 5 records by default.")
            )
          ),
          mainPanel(
            tabsetPanel(
              id = 'dataset',
              tabPanel("Find ID", DT::dataTableOutput("mytable2"),
                       textInput("sname", "Enter song name:", "song name"),
                       actionButton("find", "search")),
              tabPanel("Insert playlist", DT::dataTableOutput("mytable3"),
                       textInput("pnam", "Enter playlist name:", "list name"),
                       textInput("uid", "Enter uid:", "uid"),
                       actionButton("inl", "insert")),
              tabPanel("Insert Song", DT::dataTableOutput("mytable4"),
                       textInput("nam", "Enter song name:", "Song name"),
                       textInput("gen", "Enter song genre:", "Genre"),
                       numericInput("sid", "Enter Singer ID:", "SID"),
                       numericInput("aid", "Enter Album ID:", "AID"),
                       actionButton("ins", "insert")),
              tabPanel("Show Album", DT::dataTableOutput("mytable5"),
                       textInput("x", "Enter album name:", "album name"),
                       actionButton("go", "search")),
              tabPanel("Insert song to playlist", DT::dataTableOutput("mytable1"),
                       textInput("pi", "Enter playlist ID:", "ID"),
                       textInput("id", "Enter song ID:", "ID"),
                       actionButton("inse", "insert"))
            )

            
          )
        )
      )
      
      

      
      
      
    }
    else {
      loginpage
    }
    
    
    
    
    
  })
  

    
  # "select name from song where AID = (select AID from albums where Aname = 'Suck')"

  
  insert_s_p <- eventReactive(input$inse, {
    insert_song_to_playlist(input$pi, input$id)
  })
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(insert_s_p(), options = list(orderClasses = TRUE))
  })
  
  
  find_song <- eventReactive(input$find, {
    find_ID(input$sname)
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(find_song(), options = list(orderClasses = TRUE))
  })
  
  insertList <- eventReactive(input$inl, {
    insert_playlist(input$pnam, input$uid)
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(insertList(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  
  insertResult <- eventReactive(input$ins, {
    insert_song(input$nam, input$gen, input$sid, input$aid)
  })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(insertResult(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  searchResult <- eventReactive(input$go, {
    select_al(input$x)
  })
  
  output$mytable5 <- DT::renderDataTable({
    DT::datatable( searchResult() , options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  


  
}




runApp(list(ui = ui, server = server), launch.browser = TRUE)

