library(shiny)
library(tidyverse)
library(googlesheets4)


shinyServer(function(input, output, session) {

    sheet <- stop("Google sheets source not set (substitute url in place of this error message in code)")
    
    # conflicts <- read_sheet(sheet, sheet = "conflicts_second_check")
    conflicts <- read_sheet(sheet, sheet = "conflicts_1203")
    
    todo <- filter(conflicts, done != 1|is.na(done)) %>% 
        filter(!is.na(Reference))
    
    incr <- reactiveVal(1)
    
    rec <- reactive({todo[incr(), ]})
    
    output$ref <- renderText(rec()$Reference)
    output$doi <- renderUI(HTML(paste0("<a href='https://dx.doi.org/", rec()$doi, "', target='_new'>Full Text")))
    # output$doi <- renderText(rec()$doi)
    
    output$con_text <- renderUI(
        
        # text <- rec()$conflicts%>% 
        #     str_replace_all("\\n", "<li>") %>% 
        #     gsub("([Cc]onflict.{,10}[Ii]nterest)", "<mark>\\1</mark>", .)
        
        HTML(rec()$conflicts%>% 
                 str_replace_all("\\n", "<li>") %>% 
                 gsub("(([Cc]onflict|[Dd]eclaration).{,10}[Ii]nterests?)", "<mark>\\1</mark>", .))
        # HTML(paste0("<li>",
        #                                     rec()$conflicts %>% 
        #                               str_replace_all("\\n", "<li>")))
        )
    
    choices <- reactiveVal({
        colnames(todo[9:22]) %>% 
            str_subset("other*.", negate = TRUE)
    })
    
output$select_cons <- renderUI({
    choices <-  c(choices(), "Other:")
    # choices <- colnames(todo[9:22]) %>% 
    #     str_subset("other*.", negate = TRUE) %>% 
    #     c("Other:")
    
    div(id = incr(),
    checkboxGroupInput("conflicts",
                       "Conflicts declared",
                       choices),
    
    textInput("othertype", "", width = "150px"))
})

observeEvent(input$submit, {
    
    
    choices <- colnames(todo[9:22]) %>% 
        str_subset("other*.", negate = TRUE)
    
    other1 <- which(colnames(todo) %>%
        str_detect("other*.", negate = FALSE))[1]
    
    if ("Other:" %in% input$conflicts) {
        gs_edit_cells(sheet, "conflicts_second_check", input = input$othertype, anchor = paste0("R1C",other1))
        gs_edit_cells(sheet, "conflicts_second_check", input = 1, anchor = paste0("R",nrow(conflicts) - nrow(todo) + incr() + 1, "C",other1))
        # gs_edit_cells(sheet, "new_conflicts", input = input$othertype, anchor = paste0("R1C",other1))
        # gs_edit_cells(sheet, "new_conflicts", input = 1, anchor = paste0("R",nrow(conflicts) - nrow(todo) + incr() + 1, "C",other1))
        choices(c(choices(), input$othertype))
    }
    
    newvals <- as.data.frame(t(as.numeric(choices %in% input$conflicts)))
    
    rownum <- nrow(conflicts) - nrow(todo) + incr() + 1
    
    range <- paste0("I", rownum, ":V", rownum)
    
    # gs_edit_cells(sheet, "conflicts_second_check", input = newvals, anchor = rowanchor, byrow = TRUE)
    # gs_edit_cells(sheet, "conflicts_second_check", input = 1, anchor = paste0("W",  nrow(conflicts) - nrow(todo) + incr() + 1))
    # gs_edit_cells(sheet, "conflicts_second_check", input = input$comments, anchor = paste0("X",  nrow(conflicts) - nrow(todo) + incr() + 1))
    
    comments <- input$comments
    
    range_write(sheet, sheet = "conflicts_1203", data = newvals, range = range, col_names = FALSE)
    range_write(sheet, sheet = "conflicts_1203", data = tibble(1), range = paste0("W", rownum, ":W", rownum), col_names = FALSE)
    range_write(sheet, sheet = "conflicts_1203", data = tibble(comments), range = paste0("X",  rownum), col_names = FALSE)

    newval <- incr()+1
    incr(newval)
})

observeEvent(input$skip, {
    
    rownum <- nrow(conflicts) - nrow(todo) + incr() + 1

    comments <- input$comments
    
    range_write(sheet, sheet = "conflicts_1203", data = tibble(0), range = paste0("W", rownum, ":W", rownum), col_names = FALSE)
    range_write(sheet, sheet = "conflicts_1203", data = tibble(comments), range = paste0("X",  rownum), col_names = FALSE)
    
    newval <- incr()+1
    incr(newval)
})

output$df <- DT::renderDataTable(head(todo %>% select(Reference)))
})
