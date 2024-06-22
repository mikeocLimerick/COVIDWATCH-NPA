# covid trends/'Data' page

# Already inside server
output$pageStub <- renderUI(fluidPage(
  #tags$head(includeHTML("google-analytics.html")),
  
  # Application title
  # titlePanel("Countries"),
  # Sidebar with a daterangeinput & country selector
  
  
  ### bit to white out the sidebar
  tags$head(tags$style(
    HTML('
             #sidebar {
             background-color: #FFFFFF;
             color:#000066;font-size:15px;font-family:"Arial";
                      }')
      #body, label, input, button, select { 
      #font-family: "Arial";
      #}')
  )),
  ### white bit
  
  
  sidebarLayout(
    sidebarPanel(id="sidebar",
      dateRangeInput("pickdates", "Select Dates:",
                     start = min(dataset1$cDate)+7,
                     end   = max(dataset1$cDate)),
      
      checkboxGroupInput('ctries', "Pick countries:",
                         c("Northern Ireland"= "NorthernIreland",
                           "Scotland" = "Scotland",
                           "Faroe Islands" = "FaroeIslands",
                           "Greenland" = "Greenland",
                           "Finland"="Finland",
                           "Iceland"="Iceland",
                           "Ireland"="Ireland",
                           "Norway"="Norway",
                           "Sweden"="Sweden"),selected = c("Ireland","Norway","Sweden")),
      radioButtons('divider', "See data by:",
                         c("Day"= "dAy",
                           "Week" = "wEek",
                           "Month" = "mOnth"),selected = "wEek")
      , 
      plotOutput("plotLEG"),
      width=2),
    
    # Show a plot of the generated distribution
    mainPanel(
      HTML("<H3>New Cases + Deaths : Total Cases + Deaths</H3>"),
      fluidRow(
        column(5, plotOutput("plotflexiCs")),
        column(5, plotOutput("plotT")),
        column(5, plotOutput("plotflexiDs")),
        column(5, plotOutput("plotB"))),
      width=10)
  )
))

output$plotT <- renderPlot({
  
  dataset1b <- dataset1 %>%
    filter(
      as.Date(cDate) >= as.Date(input$pickdates[1]),
      as.Date(cDate) <= as.Date(input$pickdates[2])
    ) 
  
  p1 <- ggplot(data=dataset1b)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
    ggtitle(paste("Total Cases\n /100,000"))
  
  for (i in 1:length(input$ctries)) {
    p1 <- p1 + geom_line(aes_string(x='cDate',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p1)
  }

  
}, height=300)


output$plotB <- renderPlot({
  
  dataset2b <- dataset2 %>%
    filter(
      as.Date(cDate) >= as.Date(input$pickdates[1]),
      as.Date(cDate) <= as.Date(input$pickdates[2])
    ) 
  
  p2 <- ggplot(data=dataset2b)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
    ggtitle(paste("Total Deaths\n /100,000"))
  
  
  for (i in 1:length(input$ctries)) {
    p2 <- p2 + geom_line(aes_string(x='cDate',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p2)
  }
  
}, height=300)

output$plotNT <- renderPlot({
  
  dataset3b <- dataset3 %>%
    filter(
      cDateCmth >= (input$pickdates[1]),
      cDateCmth < (input$pickdates[2])
    ) 
  
  p3 <- ggplot(data=dataset3b)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
   ggtitle(paste("Monthly New Cases\n /100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    p3 <- p3 + geom_line(aes_string(x='cDateCmth',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=2.0)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p3)
  }
}, height=300)

output$plotNB <- renderPlot({
  
  dataset4b <- dataset4 %>%
    filter(
      cDateDmth >= (input$pickdates[1]),
      cDateDmth < (input$pickdates[2])
    ) 
  
  
  p4 <- ggplot(data=dataset4b)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
   ggtitle(paste("Monthly New Deaths\n /100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    p4 <- p4 + geom_line(aes_string(x='cDateDmth',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=2.0)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p4)
  }
}, height=300)

output$plotLEG <- renderPlot({

  dataledge <- as.data.frame(input$ctries)
p5 <- ggplot(data=dataledge)+
  theme_classic()+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())+ 
  theme(axis.ticks.x = element_blank())+
  theme(axis.ticks.y = element_blank())+
  theme(legend.title = element_blank())+
  theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
  theme(axis.line.x = element_blank())+
  theme(axis.line.y = element_blank())+
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_blank())+
  theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
  ggtitle(paste("Legend"))

for (i in 1:length(input$ctries)) {
  p5 <- p5 + geom_point(aes_string(x=0,y= i),colour=colourlisted[input$ctries[i]],shape=15, size=5)+
    geom_text(aes_string(x=0.2,y= i), face="plain", color="darkgrey",size=6, label=input$ctries[i], hjust = 0)
}

#just to left-justify legend points
p5 <- p5 + geom_point(aes_string(x=1,y= 0),colour="white",shape=15, size=5)
  
if(is.null(input$ctries)){}
else{
  print(p5)
}
}, width=200, height= 250)


output$plotNEWc <- renderPlot({
  
  dataset6b <- dataset6 %>%
    filter(
      as.Date(cDate) >= as.Date(input$pickdates[1]),
      as.Date(cDate) <= as.Date(input$pickdates[2])
    ) 
  
  p6 <- ggplot(data=dataset6b)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
    ggtitle(paste("New Daily Cases\n /100,000"))
  
  for (i in 1:length(input$ctries)) {
    p6 <- p6 + geom_line(aes_string(x='cDate',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p6)
  }
  
}, height=300)


output$plotNEWd <- renderPlot({
  
  dataset7b <- dataset7 %>%
    filter(
      as.Date(cDate) >= as.Date(input$pickdates[1]),
      as.Date(cDate) <= as.Date(input$pickdates[2])
    ) 
  
  p7 <- ggplot(data=dataset7b)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
    ggtitle(paste("New Daily Deaths\n /100,000"))
  
  for (i in 1:length(input$ctries)) {
    p7 <- p7 + geom_line(aes_string(x='cDate',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p7)
  }
  
}, height=300)


### !!! add in graphs for Cases & Deaths by week (like the Dataset 3 & 4 (cases/deaths by Mth) graphs above)

output$plotweeklyCs <- renderPlot({

dataset8b <- dataset8 %>%  
  filter(
    as.Date(cDateCWK) >= as.Date(input$pickdates[1]),
    as.Date(cDateCWK) <= as.Date(input$pickdates[2])
  ) 
  
  p8 <- ggplot(data=dataset8b)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
 ggtitle(paste("Weekly New Cases\n /100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    p8 <- p8 + geom_line(aes_string(x='cDateCWK',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p8)
  }
}, height=300)

output$plotweeklyDs <- renderPlot({
  

  dataset9b <- dataset9 %>%  
    filter(
      as.Date(cDateDWK) >= as.Date(input$pickdates[1]),
      as.Date(cDateDWK) <= as.Date(input$pickdates[2])
    ) 
  
  p9 <- ggplot(data=dataset9b)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
   ggtitle(paste("Weekly New Deaths\n /100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    p9 <- p9 + geom_line(aes_string(x='cDateDWK',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p9)
  }
}, height=300)

### !!! end

#~~~10/1/21 sort day/week/month divider

output$plotflexiCs <- renderPlot({
  
  datasetflexi <- flexiCs %>%     #first adapt to dates selection
    filter(
      as.Date(cDate) >= as.Date(input$pickdates[1]),
      as.Date(cDate) <= as.Date(input$pickdates[2])
    ) 
#then modify as per divider selected
if(input$divider == "wEek"){
  datasetflexi <- datasetflexi %>%    
    mutate(week = format(cDate, "%U"), year = format(cDate, "%Y")) %>%
    group_by(week,year) %>% 
    summarise(cDate=max(cDate),FaroeIslands=sum(FaroeIslands),Greenland=sum(Greenland),
              Finland=sum(Finland),Iceland=sum(Iceland),Ireland=sum(Ireland),
              Norway=sum(Norway),Sweden=sum(Sweden),NorthernIreland=sum(NorthernIreland),Scotland=sum(Scotland))
  datasetflexi<-datasetflexi[ with(datasetflexi, order(year,week)),]
  descripty<-"Weekly"
} else if (input$divider == "mOnth") {
  datasetflexi <- datasetflexi %>%    #then modify as per divider selected
    mutate(month = format(cDate, "%m"), year = format(cDate, "%Y")) %>%
    group_by(month,year) %>% 
    summarise(cDate=max(cDate),FaroeIslands=sum(FaroeIslands),Greenland=sum(Greenland),
              Finland=sum(Finland),Iceland=sum(Iceland),Ireland=sum(Ireland),
              Norway=sum(Norway),Sweden=sum(Sweden),NorthernIreland=sum(NorthernIreland),Scotland=sum(Scotland))
  datasetflexi<-datasetflexi[ with(datasetflexi, order(year,month)),]
  descripty<-"Monthly"
} else if (input$divider == "dAy") {  #do nothing (datasetflexi in Days by default)
  descripty<-"Daily"
  datasetflexi <- datasetflexi[ with(datasetflexi, order(cDate)),]
  }

  # now put it into a rate
  datasetflexi$FaroeIslands <- 100000*datasetflexi$FaroeIslands/popns[1]
  datasetflexi$Greenland <- 100000*datasetflexi$Greenland/popns[2]
  datasetflexi$Finland <- 100000*datasetflexi$Finland/popns[3]
  datasetflexi$Iceland <- 100000*datasetflexi$Iceland/popns[4]
  datasetflexi$Ireland <- 100000*datasetflexi$Ireland/popns[5]
  datasetflexi$Norway <- 100000*datasetflexi$Norway/popns[6]
  datasetflexi$Sweden <- 100000*datasetflexi$Sweden/popns[7]
  datasetflexi$NorthernIreland <- 100000*datasetflexi$NorthernIreland/popns[8]
  datasetflexi$Scotland <- 100000*datasetflexi$Scotland/popns[9]

  p10 <- ggplot(data=datasetflexi)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
    ggtitle(paste("New ",descripty," Cases\n /100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    p10 <- p10 + geom_line(aes_string(x='cDate',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=2.0)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p10)
  }
}, height=300)



output$plotflexiDs <- renderPlot({
  
  datasetflexiD <- flexiDs %>%     #first adapt to dates selection
    filter(
      as.Date(cDate) >= as.Date(input$pickdates[1]),
      as.Date(cDate) <= as.Date(input$pickdates[2])
    ) 
  #then modify as per divider selected
  if(input$divider == "wEek"){
    datasetflexiD <- datasetflexiD %>%    
      mutate(week = format(cDate, "%U"), year = format(cDate, "%Y")) %>%
      group_by(week,year) %>% 
      summarise(cDate=max(cDate),FaroeIslands=sum(FaroeIslands),Greenland=sum(Greenland),
                Finland=sum(Finland),Iceland=sum(Iceland),Ireland=sum(Ireland),
                Norway=sum(Norway),Sweden=sum(Sweden),NorthernIreland=sum(NorthernIreland),Scotland=sum(Scotland))
    datasetflexiD<-datasetflexiD[ with(datasetflexiD, order(year,week)),]
    descripty<-"Weekly"
  } else if (input$divider == "mOnth") {
    datasetflexiD <- datasetflexiD %>%    #then modify as per divider selected
      mutate(month = format(cDate, "%m"), year = format(cDate, "%Y")) %>%
      group_by(month,year) %>% 
      summarise(cDate=max(cDate),FaroeIslands=sum(FaroeIslands),Greenland=sum(Greenland),
                Finland=sum(Finland),Iceland=sum(Iceland),Ireland=sum(Ireland),
                Norway=sum(Norway),Sweden=sum(Sweden),NorthernIreland=sum(NorthernIreland),Scotland=sum(Scotland))
    datasetflexiD<-datasetflexiD[ with(datasetflexiD, order(year,month)),]
    descripty<-"Monthly"
  } else if (input$divider == "dAy") {  #do nothing (datasetflexi in Days by default)
    descripty<-"Daily"
    datasetflexiD <- datasetflexiD[ with(datasetflexiD, order(cDate)),]
  }
  
  # now convert to per 100k popn
  datasetflexiD$FaroeIslands <- 100000*datasetflexiD$FaroeIslands/popns[1]
  datasetflexiD$Greenland <- 100000*datasetflexiD$Greenland/popns[2]
  datasetflexiD$Finland <- 100000*datasetflexiD$Finland/popns[3]
  datasetflexiD$Iceland <- 100000*datasetflexiD$Iceland/popns[4]
  datasetflexiD$Ireland <- 100000*datasetflexiD$Ireland/popns[5]
  datasetflexiD$Norway <- 100000*datasetflexiD$Norway/popns[6]
  datasetflexiD$Sweden <- 100000*datasetflexiD$Sweden/popns[7]
  datasetflexiD$NorthernIreland <- 100000*datasetflexiD$NorthernIreland/popns[8]
  datasetflexiD$Scotland <- 100000*datasetflexiD$Scotland/popns[9]
  
  p11 <- ggplot(data=datasetflexiD)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
    ggtitle(paste("New ",descripty," Deaths\n /100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    p11 <- p11 + geom_line(aes_string(x='cDate',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=2.0)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p11)
  }
}, height=300)

### ~~~ end
