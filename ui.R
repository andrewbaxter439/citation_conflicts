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
        }
        ::selection {
        background: #FF00CC;
        color: #FFFFFF
        }'
    ))),
    
    column(1),
    column(11,
    titlePanel(h1(style = "margin: 10px 0px 10px 0px;", "Conflict statements"),
               windowTitle = "Conflicts statements"),
    
    column(7,
           fluidRow(class = "boxsection",
                    style = "min-height: 90vh",
                    h2(textOutput("ref")),
                    # h3(textOutput("doi")),
                    htmlOutput("doi"),
                    htmlOutput("con_text")
                    # textOutput("con_text")
                    )),
    
    column(4,
           fluidRow(class = "boxsection",
                    style = "min-height: 90vh",
                    uiOutput("select_cons"),
                    actionButton("submit", "Submit")
           )
    )
)
)
)