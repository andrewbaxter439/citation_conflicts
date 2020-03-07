library(shiny)
library(readr)
library(dplyr)
library(googlesheets)
library(httr)
library(rscopus)
library(stringr)
library(ggplot2)
library(tidyr)
library(SPHSUgraphs)


shinyServer(function(input, output, session) {

    sheet <- gs_title("new_conflicts")
    
    conflicts <- gs_read(sheet, ws = "new_conflicts")
    
    todo <- filter(conflicts, is.na(done))
    
    incr <- reactiveVal(1)
    
    rec <- reactive({todo[incr(), ]})
    
    output$ref <- renderText(rec()$Reference)
    output$doi <- renderUI(HTML(paste0("<a href='https://dx.doi.org/", rec()$doi, "', target='_new'>Full Text")))
    # output$doi <- renderText(rec()$doi)
    
    output$con_text <- renderUI(HTML(paste0("<li>",
                                            rec()$conflicts %>% 
                                      str_replace_all("\\n", "<li>"))))
    
output$select_cons <- renderUI({
    choices <- colnames(todo[9:22]) %>% 
        str_subset("other*.", negate = TRUE) %>% 
        c("Other:")
    
    div(id = incr(),
    checkboxGroupInput("conflicts",
                       "Conflicts declared",
                       choices),
    
    textInput("othertype", "", width = "150px"))
})

observeEvent(input$submit, {
    
    
    choices <- colnames(todo[9:22]) %>% 
        str_subset("other*.", negate = TRUE)
    
    # other1 <- colnames(todo[7:20]) %>% 
    #     str_subset("other*.", negate = FALSE)[1]
    
    newvals <- as.numeric(choices %in% input$conflicts)
    
    rowanchor <- paste0("I", nrow(conflicts) - nrow(todo) + incr() + 1)
    
    gs_edit_cells(sheet, "new_conflicts", input = newvals, anchor = rowanchor, byrow = TRUE)
    gs_edit_cells(sheet, "new_conflicts", input = 1, anchor = paste0("W",  nrow(conflicts) - nrow(todo) + incr() + 1))
    
    newval <- incr()+1
    incr(newval)
})

})
