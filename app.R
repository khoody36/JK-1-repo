library(shiny)
library(modules)
library(tidyverse)
library(plyr)
library(dplyr)
library(tools)
library(tibble)
library(formattable)
library(rsconnect)

mwrWebUI <- function(id,label = "MWR Web"){
  
  ns <- NS(id)
  
  
  
  tagList(
    
    fileInput(ns("file"), label),
    
    downloadButton(ns("mwrWebDownload"), "Download"),
    
    tableOutput(outputId = ns("mwrWeb_Table"))
    
  )
  
}

mwrWebServer <- function(id){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      mwrInput <- reactive({
        
        mwrWeb <- input$file
        
        ext <- tools::file_ext(mwrWeb$datapath)
        
        req(mwrWeb)
        
        mwrWebFile <- read_fwf(mwrWeb$datapath,
                               
                               fwf_cols(prgmcode = 2, rcrdtype = 1, stfips = 2, uiacct = 10, run = 5, formattype = 1,
                                        
                                        fein = 9, tradename = 35, streetaddress = 35, city = 30, stabbrev = 2, zipcode = 5,
                                        
                                        zipcodeexten = 4, cc1 = 2, cc2 = 2, cc3 = 2, refyear = 4,
                                        
                                        refqtr = 1, legalname = 35, worksite = 35, m1 = 6, m2 = 6,
                                        
                                        m3 = 6, wages = 10, narcomment = 57, datasourc = 1, countycode = 3, townshipcode = 3,
                                        
                                        ownershipcode = 1, filler = 1, naics = 6, agent = 4,
                                        
                                        intialliabyr = 4, initialliabmnth = 2, initialliabday = 2, eolyear= 4, eolmnth = 2,
                                        
                                        eolday = 2, wrksiteecoactdesc = 150, mwrcontactname = 35, mwrcontacttitle = 35,
                                        
                                        mailingstreetline1 = 35, mailingstreetline2 = 35, mailingcity = 30, mailingstateabbrev = 2,
                                        
                                        mailingzip = 5, mailingzipexten = 4, areacode = 3, phoneprefix = 3,
                                        
                                        phonesuffix = 4, phoneextension = 5, fax = 10, email = 60, btet1 = 1, btet2 = 1,
                                        
                                        btet3 = 1, btet4 = 1, btet5 = 1, bustranscompany = 35, collectstatus = 1))
        
        MWRweb_1 <- as.data.frame(mwrWebFile %>% select(uiacct, run, tradename, streetaddress, city, zipcode, worksite, m1, m2, m3, wages, cc1, cc2, cc3, narcomment))
        
        MWRweb_1[is.na(MWRweb_1)] <- ""
        
        MWRweb_1$m1 <- as.numeric(MWRweb_1$m1)
        
        MWRweb_1$m2 <- as.numeric(MWRweb_1$m2)
        
        MWRweb_1$m3 <- as.numeric(MWRweb_1$m3)
        
        MWRweb_1$wages <- as.numeric(MWRweb_1$wages)
        
        MWRweb_2 <- ddply(MWRweb_1, .(uiacct), numcolwise(sum, na.rm=T))
        
        MWRweb_2[is.na(MWRweb_2)] <- ""
        
        MWRweb_3 <- bind_rows(MWRweb_1,MWRweb_2)
        
        MWRweb_3[is.na(MWRweb_3)] <- ""
        
        MWRweb_4 <- MWRweb_3 %>% arrange(uiacct)
        
        MWRweb_4[is.na(MWRweb_4)] <- ""
        
        MWRweb_5 <- as.data.frame(lapply(MWRweb_4, as.character), stringsAsFactors = FALSE)
        
        MWRweb_6 <- do.call(rbind, by(MWRweb_5, MWRweb_4$uiacct, rbind, ""))
        
        formattable(MWRweb_6, align = c("l","l","l","r","l","l","c","c","c","r","r","r","r","r"))
        
      })
      
      
      
      output$mwrWeb_Table <- renderTable({
        
        mwrInput()
        
      })
      
      
      
      output$mwrWebDownload <- downloadHandler(
        
        filename = function(){
          
          paste(input$file, Sys.Date(),".csv", sep = "")
          
        },
        
        content = function(file){
          
          write.csv(mwrInput(), file)
          
        }
        
      )
      
    }         
    
  )
  
}

mwrEDIUI <- function(id,label = "MWR EDI"){
  
  ns <- NS(id)
  
  
  
  tagList(
    
    fileInput(ns("file"), label),
    
    downloadButton(ns("mwrEDIDownload"), "Download"),
    
    tableOutput(outputId = ns("mwrEDI_Table"))
    
  )
  
}

mwrEDIServer <- function(id){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      mwrInput <- reactive({
        
        mwrEDI <- input$file
        
        ext <- tools::file_ext(mwrEDI$datapath)
        
        req(mwrEDI)
        
        mwrEDIFile <- read_fwf(mwrEDI$datapath,
                               
                               fwf_cols(prgmcode = 2, rcrdtype = 1, stfips = 2, uiacct = 10, run = 5, formattype = 1,
                                        
                                        fein = 9, tradename = 35, streetaddress = 35, city = 30, stabbrev = 2, zipcode = 5,
                                        
                                        zipcodeexten = 4, cc1 = 2, cc2 = 2, cc3 = 2, refyear = 4,
                                        
                                        refqtr = 1, legalname = 35, worksite = 35, m1 = 6, m2 = 6,
                                        
                                        m3 = 6, wages = 10, narcomment = 57, datasourc = 1, countycode = 3, townshipcode = 3,
                                        
                                        ownershipcode = 1, filler = 1, naics = 6, agent = 4,
                                        
                                        intialliabyr = 4, initialliabmnth = 2, initialliabday = 2, eolyear= 4, eolmnth = 2,
                                        
                                        eolday = 2, reactyear = 4, reactmnth = 2, reactday = 2, formerui = 10, formerfein = 9,
                                        
                                        peophnenmbr = 10, peomnth = 2, peoyear = 4, peomnthend = 2, peoyearend = 4,
                                        
                                        ecoactivity = 33))
        
        MWRedi_1 <- as.data.frame(mwrEDIFile %>% select(uiacct, run, tradename, streetaddress, city, zipcode, worksite, m1, m2, m3, wages, cc1, cc2, cc3, narcomment))
        
        MWRedi_1[is.na(MWRedi_1)] <- ""
        
        MWRedi_1$m1 <- as.numeric(MWRedi_1$m1)
        
        MWRedi_1$m2 <- as.numeric(MWRedi_1$m2)
        
        MWRedi_1$m3 <- as.numeric(MWRedi_1$m3)
        
        MWRedi_1$wages <- as.numeric(MWRedi_1$wages)
        
        MWRedi_2 <- ddply(MWRedi_1, .(uiacct), numcolwise(sum, na.rm=T))
        
        MWRedi_2[is.na(MWRedi_2)] <- ""
        
        MWRedi_3 <- bind_rows(MWRedi_1,MWRedi_2)
        
        MWRedi_3[is.na(MWRedi_3)] <- ""
        
        MWRedi_4 <- MWRedi_3 %>% arrange(uiacct)
        
        MWRedi_4[is.na(MWRedi_4)] <- ""
        
        MWRedi_5 <- as.data.frame(lapply(MWRedi_4, as.character), stringsAsFactors = FALSE)
        
        MWRedi_6 <- do.call(rbind, by(MWRedi_5, MWRedi_4$uiacct, rbind, ""))
        
        formattable(MWRedi_6, align = c("l","l","l","r","l","l","c","c","c","r","r","r","r","r"))
        
      })
      
      
      
      output$mwrEDI_Table <- renderTable({
        
        mwrInput()
        
      })
      
      
      
      output$mwrEDIDownload <- downloadHandler(
        
        filename = function(){
          
          paste(input$file, Sys.Date(),".csv", sep = "")
          
        },
        
        content = function(file){
          
          write.csv(mwrInput(), file)
          
        }
        
      )
      
    }
    
  )
  
}

mwrPrintUI <- function(id,label = "MWR Print"){
  
  ns <- NS(id)
  
  
  
  tagList(
    
    fileInput(ns("file"), label),
    
    downloadButton(ns("mwrPrintDownload"), "Download"),
    
    tableOutput(outputId = ns("mwrPrint_Table"))
    
  )
  
}

mwrPrintServer <- function(id){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      mwrInput <- reactive({
        
        mwrPrint <- input$file
        
        ext <- tools::file_ext(mwrPrint$datapath)
        
        req(mwrPrint)
        
        mwrPrintFile <- read_fwf(mwrPrint$datapath,
                                 
                                 fwf_cols(prgmcode = 2, rcrdtype = 1, stfips = 2, uiacct = 10, run = 5, formattype = 1,
                                          
                                          fein = 9, tradename = 35, streetaddress = 35, city = 30, stabbrev = 2, zipcode = 5,
                                          
                                          zipcodeexten = 4, cc1 = 2, cc2 = 2, cc3 = 2, refyear = 4,
                                          
                                          refqtr = 1, legalname = 35, worksite = 35, m1 = 6, m2 = 6,
                                          
                                          m3 = 6, wages = 10, narcomment = 57, datasourc = 1, countycode = 3, townshipcode = 3,
                                          
                                          ownershipcode = 1, filler = 1, naics = 6, agent = 4,
                                          
                                          intialliabyr = 4, initialliabmnth = 2, initialliabday = 2, eolyear= 4, eolmnth = 2,
                                          
                                          eolday = 2, reactyear = 4, reactmnth = 2, reactday = 2, formerui = 10, formerfein = 9,
                                          
                                          peophnenmbr = 10, peomnth = 2, peoyear = 4, peomnthend = 2, peoyearend = 4,
                                          
                                          ecoactivity = 33, transnmbr = 3))
        
        MWRprint_1 <- as.data.frame(mwrPrintFile %>% select(transnmbr, uiacct, run, tradename, streetaddress, city, zipcode, worksite, m1, m2, m3, wages, cc1, cc2, cc3, narcomment))
        
        MWRprint_1[is.na(MWRprint_1)] <- ""
        
        MWRprint_1$m1 <- as.numeric(MWRprint_1$m1)
        
        MWRprint_1$m2 <- as.numeric(MWRprint_1$m2)
        
        MWRprint_1$m3 <- as.numeric(MWRprint_1$m3)
        
        MWRprint_1$wages <- as.numeric(MWRprint_1$wages)
        
        MWRprint_2 <- ddply(MWRprint_1, .(uiacct), numcolwise(sum, na.rm=T))
        
        MWRprint_2[is.na(MWRprint_2)] <- ""
        
        MWRprint_3 <- bind_rows(MWRprint_1,MWRprint_2)
        
        MWRprint_3[is.na(MWRprint_3)] <- ""
        
        MWRprint_4 <- MWRprint_3 %>% arrange(uiacct)
        
        MWRprint_4[is.na(MWRprint_4)] <- ""
        
        MWRprint_5 <- as.data.frame(lapply(MWRprint_4, as.character), stringsAsFactors = FALSE)
        
        MWRprint_6 <- do.call(rbind, by(MWRprint_5, MWRprint_4$uiacct, rbind, ""))
        
        formattable(MWRprint_6, align = c("l","l","l","r","l","l","c","c","c","r","r","r","r","r"))
        
      })
      
      
      
      output$mwrPrint_Table <- renderTable({
        
        mwrInput()
        
      })
      
      
      
      output$mwrPrintDownload <- downloadHandler(
        
        filename = function(){
          
          paste(input$file, Sys.Date(),".csv", sep = "")
          
        },
        
        content = function(file){
          
          write.csv(mwrInput(), file)
          
        }
        
      )
      
    }         
    
  )
  
}

ui <- fluidPage(
  selectInput("module", label = "Module Choices",
              choices = c("Web" = "mod1", "EDI" = "mod2", "Print" ="mod3")),
  uiOutput("ui")
)

server <- function(input, output, session) {
  
  output$ui <- renderUI({
    
    if (input$module == "mod1") mwrWebUI("mod1")
    
    else if (input$module == "mod2") mwrEDIUI("mod2")
    
    else if (input$module == "mod3") mwrPrintUI("mod3")
    
  })
  
  
  
  mod1 <- mwrWebServer("mod1")
  
  mod2 <- mwrEDIServer("mod2")
  
  mod3 <- mwrPrintServer("mod3")
  
}

shinyApp(ui, server)