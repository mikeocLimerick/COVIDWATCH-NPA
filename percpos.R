# PERCENTAGE POSITIVE page


# Already inside server
output$pageStub <- renderUI(fluidPage(
  #tags$head(includeHTML("google-analytics.html")),  
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
                                start = min(dataset5$tDate),
                                end   = max(dataset5$tDate),
                                min = min(dataset5$tDate),
                                max   = max(dataset5$tDate)),
                 
                 checkboxGroupInput('ctries', "Pick countries:",
                                    c("Northern Ireland"= "NorthernIreland",
                                      "Scotland" = "Scotland",
                                      "FaroeIslands"="FaroeIslands",
                                      "Finland"="Finland",
                                      "Iceland"="Iceland",
                                      "Ireland"="Ireland",
                                      "Norway"="Norway",
                                      "Sweden"="Sweden"),selected = c("Iceland","Ireland","Sweden"))
                 
                 , 
                 plotOutput("plotLEG"),
                 width=2),
    
    # Show a plot of the generated distribution
    mainPanel(
      HTML("<BR><H3>Percentage Positivity = a Measure of Control</H3><BR>",
           "<p>The Graph below ('Positive tests per 100 tests') shows how many tests for COVID-19 were positive in each country each month as the pandemic progressed.<BR>",
           "This can be a useful measure to consider ",
           " as it tells us something about how well a country is staying on top of outbreaks/virus activity in its communities<BR><BR></p>",
           "<p>If a country has a fit-for-purpose testing (& contact tracing) system for COVID-19, they should be testing lots of people to try and find out where the virus is bubbling up. ",
           "<p>Anyone who has respiratory symptoms and anyone who has been in close contact with someone who has known SARS-CoV-2 infection need to be isolated and tested quickly.<BR>",
           "Different countries took different lengths of time to get their Testing systems 'ramped up' and many were only testing people who were ending up in hospital (i.e. people unwell with COVID-19 symptoms) early on in the pandemic.<BR>",
           "This means that their positivity rate early on in the pandemic was high, as they were not testing those with few symptoms or asymptomatic infections and indeed the 'Net' was not being thrown wide enough for each cluster of COVID-19 in communities.</p>",
           "<p>It seems we should be aiming for as low a percentage as possible when it comes to positive tests, as countries who achieve this are remaining on top of the virus.<BR></p>"),
      fluidRow(
        column(10, style="padding:10px;", plotOutput("plotPercPos"))),
      HTML("<p>In order for you to be able to compare the countries' positivity rates, Total Cases and Deaths are included below. <BR><BR><BR></p>"),
      fluidRow(
        column(10, style="padding:10px;", plotOutput("plotTC"))),
      fluidRow(
        column(10, style="padding:10px;", plotOutput("plotTD"))),
      width=10)
  )
  )
  )

#div(style = "padding: 0px 0px; margin-top:-2em",

##########################


output$plotPercPos <- renderPlot({
  
  datasetcurr <- dataset5 %>%
#      filter(
#        Mth >= month(input$pickdates[1]),
#        Mth <= month(input$pickdates[2])
#     ) 
    filter(
      as.Date(tDate) >= as.Date(input$pickdates[1]),
      as.Date(tDate) <= as.Date(input$pickdates[2])
    ) 
    
        
  pPerc <- ggplot(data=datasetcurr)+
    #scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    #theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.015))+
    #scale_x_continuous(labels = function(Mth) month.abb[Mth], breaks=seq(1,nrow(datasetcurr)+2,1))+
    ggtitle(paste("  Positive tests \nper 100 tests (%)"))
  
  for (i in 1:length(input$ctries)) {
    pPerc <- pPerc + geom_line(aes_string(x='tDate',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=2.5)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(pPerc)
  }
  
}, height=350)


##########################

output$plotTC <- renderPlot({
  
  dataset1b <- dataset1 %>%
    filter(
      as.Date(cDate) >= as.Date(input$pickdates[1]),
      as.Date(cDate) <= as.Date(input$pickdates[2])
    ) 
  
  p1 <- ggplot(data=dataset1b)+
    #scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b")+
    theme_classic()+
    #theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    #theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
    ggtitle(paste("  Total Cases\n /100,000"))
  
  for (i in 1:length(input$ctries)) {
    p1 <- p1 + geom_line(aes_string(x='cDate',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p1)
  }
}, height=220)


output$plotTD <- renderPlot({
  
  dataset2b <- dataset2 %>%
    filter(
      as.Date(cDate) >= as.Date(input$pickdates[1]),
      as.Date(cDate) <= as.Date(input$pickdates[2])
    ) 
  
  p2 <- ggplot(data=dataset2b)+
    #scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b")+
    theme_classic()+
    #theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    #theme(legend.title = element_blank())+
    theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.02))+
    ggtitle(paste("  Total Deaths\n /100,000"))
  
  
  for (i in 1:length(input$ctries)) {
    p2 <- p2 + geom_line(aes_string(x='cDate',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p2)
  }
  
}, height=220)

output$plotNT <- renderPlot({
  
  dataset3b <- dataset3 %>%
    filter(
      Mth >= month(input$pickdates[1]),
      Mth <= month(input$pickdates[2])
    ) 
  
  p3 <- ggplot(data=dataset3b)+
    #scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
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
    scale_x_continuous(labels = function(Mth) month.abb[Mth], breaks=seq(1,nrow(dataset3b),1))+
    ggtitle(paste("Monthly New Cases\n /100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    p3 <- p3 + geom_line(aes_string(x='Mth',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p3)
  }
}, height=300)

output$plotNB <- renderPlot({
  
  dataset4b <- dataset4 %>%
    filter(
      Mth >= month(input$pickdates[1]),
      Mth <= month(input$pickdates[2])
    ) 
  
  
  p4 <- ggplot(data=dataset4b)+
    #scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
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
    scale_x_continuous(labels = function(Mth) month.abb[Mth], breaks=seq(1,nrow(dataset4b),1))+
    ggtitle(paste("Monthly New Deaths\n /100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    p4 <- p4 + geom_line(aes_string(x='Mth',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(p4)
  }
}, height=300)

output$plotLEG <- renderPlot({
  
  #  dataledge<-input$ctries[1]
  #  for (i in 2:length(input$ctries)) {
  #    dataledge <- rbind(dataledge,input$ctries[i])
  #  }
  #  dataledge<-as.data.frame(dataledge)
  
  dataledge <- as.data.frame(input$ctries)
  p5 <- ggplot(data=dataledge)+
    #scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
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
    #theme(plot.margin = margin(0, 4, 0, 0, "cm"))+
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
}, width=200, height= 300)
