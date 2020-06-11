#Australian Weather Analysis and prediction
  

if(FALSE){
  install.packages("gmodels")
  install.packages("C50")
  install.packages("rpart")
  install.packages("dplyr")
  install.packages("party")
  install.packages("rpart.plot")
  install.packages("shiny")
  install.packages("shinythemes")
  install.packages('rsconnect')
  install.packages("ggthemes")
  
}
#Environment Setup
{  
  rm(list=ls())
  setwd("/Users/neerajdeshpande/R Programs/DM Project")
  library(gmodels)
  library(C50)
  library(rpart)
  library(dplyr)
  library(party)
  library(shiny)
  library(ggplot2)
  library(shinythemes)
  #library(rsconnect)
  library(ggthemes)
  library(rpart.plot)
}
{
  weather_Data <- read.csv("weatherAUS.csv")
  #View(weather_Data)
  weather_Data <- na.omit(weather_Data)
  #myData[,c("")] <- data.frame(apply(myData[cols], 2, as.factor))
  set.seed(9997)
  sample_index <- sample(2,nrow(weather_Data),prob = c(0.9,0.1),replace = TRUE)
  weather_Training <- weather_Data[sample_index==1,]
  weather_Testing <- weather_Data[sample_index==2,]
  
}

#Creating Decisions Trees using various algorithms
{  
  myFormula <- RainTomorrow ~ RainToday + Evaporation + Sunshine + WindGustSpeed + MinTemp + MaxTemp + Rainfall  + Humidity9am + Humidity3pm + Pressure9am + Pressure3pm + Cloud9am + Cloud3pm + Temp9am + Temp3pm
  
  CTree1 <- ctree(formula = myFormula, data = weather_Training)
  C5Tree <- C5.0(weather_Data[,c(-24,-23)],weather_Data[,24])
  RpartTree <- rpart(formula = myFormula, data = weather_Training,method = "class")
  predictInput <- weather_Data[1,]
    
} 
  #plot(RpartTree)
  #part.plot(RpartTree)
  #table(predict(CTree1,weather_Data),weather_Data[,24])
  
  
  #plot(weather_Data$Rainfall,weather_Data$Evaporation)
  
  

  #Creating The UI Part of ShinyApp
  ui <- fluidPage(theme = shinytheme("darkly"),titlePanel("Australian Weather Data"),tabsetPanel(type = "tabs",
                                                                                                                                                                                                                                                                                                                                tabPanel("Visualizing Using a Plot",sidebarLayout(sidebarPanel("Input Placeholder",selectInput(inputId = "simpleX",choices = names(weather_Data)[c(-1,-2,-23,-24)],label = "Select X Variable"),selectInput(inputId = "simpleY",choices = names(weather_Data)[c(-1,-23,-24)],label = "Select Y Variable"),sliderInput("sampleSize","Sample Size",min=1,max=nrow(weather_Data),value = nrow(weather_Data)/2),radioButtons("plotType", "Plot Type :",c("ScatterPlot" = "sp","ScatterSmooth" = "sm","Histogram" = "ht"),selected = "sm")),mainPanel( plotOutput("plot")))),
                                                                                                                                                                                                                                                                                                                                tabPanel("Decision Trees",sidebarLayout(sidebarPanel(radioButtons("treeType", "Tree Algorithm :",c("CTREE" = "ct","C5.0" = "c5","RPART" = "rp"),selected = "rp"),width = 3),mainPanel(fluidRow(h3("Visualization"),plotOutput("treePlot")),fluidRow(h3("Summary"),verbatimTextOutput("summary"))))),

                                                                    tabPanel("Tree Results",sidebarLayout(sidebarPanel(radioButtons("treeType2", "Tree Algorithm :",c("CTREE" = "ct","C5.0" = "c5","RPART" = "rp"),selected = "rp")),mainPanel( fluidRow(h3("Chi-Squared Test Results"),tableOutput("table"),paste("P-Value : "),textOutput("pval")),uiOutput("prediction"),fluidRow(h3("Prediction is : --->"),h1(textOutput("predictionResult"))))))))
  
  #The Server Part of Shiny App
  server <- function(input, output){
   #Reactive Fuctions
    
    treeSelected <- reactive({
                                    switch(input$treeType,
                                     ct = plot(CTree1),
                                     c5 = plot(C5Tree),
                                     rp = rpart.plot(RpartTree)
    )})
    
    treeSelectedSummary <- reactive({
                                    switch(input$treeType,
                                     ct = CTree1,
                                     c5 = summary(C5Tree),
                                     rp = summary(RpartTree)
    )})
    
   
    treeSelectedTable <- reactive({switch(input$treeType2,
                    ct = CrossTable(predict(CTree1,weather_Data),weather_Data$RainTomorrow)$prop.row,
                    c5 = CrossTable(predict(C5Tree,weather_Data),weather_Data$RainTomorrow)$prop.row,
                    rp = CrossTable(predict(RpartTree,weather_Data,type = "class"),weather_Data$RainTomorrow)$prop.row
    )})
    
    set.seed(9997)
   
    #First Tab Plot Output
     output$plot <- renderPlot({
      ggplot(data = weather_Data[sample(nrow(weather_Data), input$sampleSize), ]
)+switch (input$plotType,
  sp ={ geom_point( shape = 16, size = 1, fill = NA,
                    alpha = NA, stroke = 0.5,aes_string(x = input$simpleX, y = input$simpleY,colour="RainTomorrow"))},
  sm={ geom_smooth(  size = 1, fill = NA,
                   alpha = NA, aes_string(x = input$simpleX, y = input$simpleY,colour="RainTomorrow"))},
  ht={ geom_histogram(  colour = "navy", size = 0.5,
                   alpha = NA, aes_string(x =input$simpleX,fill = "RainTomorrow"))}
)+theme_economist()  })
    
     #Second Tab Outputs
    output$treePlot <- renderPlot({ treeSelected() })
    
    
    output$summary <- renderPrint( treeSelectedSummary() )

  
    #3rd Tab Outputs(Dynamic UI)
    
    output$table <-renderTable({treeSelectedTable()
    })
    output$prediction <- renderUI({
      switch(input$treeType2,
             ct=fluidRow(checkboxInput("rainToday","Did it Rain Today?",value = FALSE),sliderInput("evaporation","Evaporation",min = min(weather_Data$Evaporation),max = max(weather_Data$Evaporation),step = 0.1,value = mean(weather_Data$Evaporation)),sliderInput("sunshine","Hrs of Sunshine",min = 1,max = 24,step = 0.1,value = 12),sliderInput("windspeed","Max Wind Speed",min = min(weather_Data$WindGustSpeed),max = max(weather_Data$WindGustSpeed),step = 1,value = mean(weather_Data$WindGustSpeed)),sliderInput("temp","Temperature Min and Max",min = min(weather_Data$MinTemp),max = max(weather_Data$MaxTemp),step = 0.1,value = c(16,32)),sliderInput("rainfall","RainFall(if any)",min = min(weather_Data$Rainfall),max = max(weather_Data$Rainfall),step = 0.1,value = mean(weather_Data$Rainfall)),sliderInput("humidity9","9am Humidity",min = min(weather_Data$Humidity9am),max = max(weather_Data$Humidity9am),step = 1,value = mean(weather_Data$Humidity9am)),sliderInput("humidity3","3pm Humidity",min = min(weather_Data$Humidity3pm),max = max(weather_Data$Humidity3pm),step = 1,value = mean(weather_Data$Humidity3pm)),sliderInput("pressure9","9am Pressure",min = min(weather_Data$Pressure9am),max = max(weather_Data$Pressure9am),step = 0.1,value = mean(weather_Data$Pressure9am)),sliderInput("pressure3","3pm Pressure",min = min(weather_Data$Pressure3pm),max = max(weather_Data$Pressure3pm),step = 0.1,value = mean(weather_Data$Pressure3pm)),sliderInput("cloud9","9am CloudCover(oktas)",min = min(weather_Data$Cloud9am),max = max(weather_Data$Cloud9am),step = 1,value = mean(weather_Data$Cloud9am)),sliderInput("cloud3","3pm CloudCover(oktas)",min = min(weather_Data$Cloud3pm),max = max(weather_Data$Cloud3pm),step = 1,value = mean(weather_Data$Cloud3pm)),sliderInput("temp9","9am Temparature",min = min(weather_Data$Temp9am),max = max(weather_Data$Temp9am),step = 0.1,value = mean(weather_Data$Temp9am)),sliderInput("temp3","3pm Temparature",min = min(weather_Data$Temp3pm),max = max(weather_Data$Temp3pm),step = 0.1,value = mean(weather_Data$Temp3pm))),
             c5=fluidRow(sliderInput("humidity3","3pm Humidity",min = min(weather_Data$Humidity3pm),max = max(weather_Data$Humidity3pm),step = 1,value = mean(weather_Data$Humidity3pm) ),sliderInput("sunshine","Hrs of Sunshine",min = 1,max = 24,step = 0.1,value = 12),sliderInput("windspeed","Max Wind Speed",min = min(weather_Data$WindGustSpeed),max = max(weather_Data$WindGustSpeed),step = 1,value = mean(weather_Data$WindGustSpeed))),
             rp=fluidRow(sliderInput("humidity3","3pm Humidity",min = min(weather_Data$Humidity3pm),max = max(weather_Data$Humidity3pm),step = 1,value = mean(weather_Data$Humidity3pm)),sliderInput("windspeed","Max Wind Speed",min = min(weather_Data$WindGustSpeed),max = max(weather_Data$WindGustSpeed),step = 1,value = mean(weather_Data$WindGustSpeed)))
             )
    }
    )
    output$pval <- renderText({switch(input$treeType2,ct=">2.2e-16",
                                      c5=chisq.test(predict(C5Tree,weather_Data),weather_Data$RainTomorrow)$p.value,
                                      rp=chisq.test(predict(RpartTree,weather_Data,type = "class"),weather_Data$RainTomorrow)$p.value)})
    output$predictionResult <- renderText({ 
        
        switch(input$treeType2,
               ct = {
                 print(input$rainToday)
              if(input$rainToday){predictInput$RainToday[1]<-factor("Yes")}else{predictInput$RainToday[1]<-factor("No")}
               predictInput$Evaporation[1]=input$evaporation
               predictInput$Sunshine[1]=input$sunshine
               predictInput$WindGustSpeed[1]=as.integer(input$windspeed)
               predictInput$MinTemp[1]=input$temp[1]
               predictInput$MaxTemp[1]=input$temp[2]
               predictInput$Rainfall[1]=input$rainfall
               predictInput$Humidity9am[1]=as.integer(input$humidity9)
               predictInput$Humidity3pm[1]=as.integer(input$humidity3)
               predictInput$Pressure9am[1]=input$pressure9
               predictInput$Pressure3pm[1]=input$pressure9
               predictInput$Cloud9am[1]=as.integer(input$cloud9)
               predictInput$Cloud3pm[1]=as.integer(input$cloud3)
               predictInput$Temp9am[1]=input$temp9
               predictInput$Temp3pm[1]=input$temp3
               as.character(predict(CTree1,predictInput))
               },
               
               c5 = {             
                 predictInput$WindGustSpeed[1]=as.integer(input$windspeed)
                 predictInput$Humidity3pm[1]=as.integer(input$humidity3)
                 predictInput$Sunshine[1]=input$sunshine
                 as.character(predict(C5Tree,predictInput))},
               rp = {            
                 predictInput$WindGustSpeed[1]=as.integer(input$windspeed)
             		 predictInput$Humidity3pm[1]=as.integer(input$humidity3)
                 as.character(predict(RpartTree,predictInput,type = "class"  
            ))}
        )})
  }
  
  
  #Final Deployment
  shinyApp(ui = ui, server = server)
 
  
  
