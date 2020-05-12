library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(tags$style(HTML(
        'h1 {
        font-family: "nimbus-sans",sans-serif;
        }
        .boxsection {
             border-radius: 10px;
             margin: 10px;
             background-color: #adcfeb;
             padding: 10px;
             overflow-wrap: break-word;
             font-size: 12pt;
        }
        ::selection {
             background: #FF00CC;
             color: #FFFFFF
        }
        #ref {
        }
        mark {
             background-color: #00FF00;
             color: #FFFFFF
        }'
    ))),
    
    column(1),
    column(11,
    titlePanel(h1(style = "margin: 10px 0px 10px 0px;", "Conflict statements"),
               windowTitle = "Conflicts statements"),
    
    column(7,
           fluidRow(class = "boxsection",
                    style = "min-height: 80vh",
                    h2(textOutput("ref")),
                    # h3(textOutput("doi")),
                    htmlOutput("doi"),
                    htmlOutput("con_text")
                    # textOutput("con_text")
                    )),
    
    column(4,
           fluidRow(class = "boxsection",
                    style = "min-height: 60vh",
                    uiOutput("select_cons"),
                    actionButton("submit", "Submit")),
           fluidRow(class = "boxsection",
                    stype = "min-height: 20vh",
                    textInput("comments", "Comments:"),
                    actionButton("skip", "Skip")
           )
           
    )
)
)
)