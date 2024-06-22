# TESTING page


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
                                      "Sweden"="Sweden"),selected = c("Iceland","FaroeIslands","Sweden"))
                 
                 , 
                 plotOutput("plotLEG"),
                 width=2),
    
    # Show a plot of the generated distribution
    mainPanel(
      HTML("<BR><H3>The Tip of the Iceberg</H3><BR><p>Testing for SARS-CoV-2 virus by swabbing people who might have been infected forms a very important part of the response to COVID-19. ",
           "Different countries took different lengths of time to get their Testing systems 'ramped up'.<BR></p>",
           "<p>The Graph below ('Total Tests/100,000') shows when in the pandemic the various countries (which you can select on the Left-hand menu) started testing a lot of people. We will add countries in due course...<BR></p>",
           "<p>Of course different countries might have good reason not to be testing lots early in the pandemic (as perhaps the virus was not yet very active in their country). Nevertheless, in recent times all countries have needed to test lots of people ",
"and we can see that having a responsive Testing system is a key piece in the puzzle of both understanding and managing COVID-19. However, we have also seen that Testing and Results Reporting needs to be well integrated with other parts of the public health response (e.g. Contact Tracing) ",
"as otherwise it cannot fulfil its potential. <BR><BR>",
"It may be useful to illustrate with an example - you will see Iceland started testing large numbers of people very early on, which has allowed them to more accurately record the true burden of COVID-19 AND get outbreaks under control quickly.",
" In other countries, who started more slowly, mostly sick people ending up at hospitals were being tested early on in the pandemic. Therefore rates of severe COVID-19 disease seemed to be higher in these countries, while in truth these countries were just seeing the 'tip of the iceberg' i.e. there was a large amount of 'wild virus' out in their communities ",
"that they were not detecting.</p>",
"<p>Finally, it is worth noting here that small countries, who have small populations (and hence smaller numbers acting as the denominator in calculations), will change their trajectory quicker on the graphs on this site. <BR>It is also true that for big countries that due to their large denominator (populations) they appear to have a slower start when it came to viral spread early in the pandemic.",
"As it is in the nature of the virus to spread among people in clusters or outbreaks, these will 'appear' in the data more quickly and seem more significant for a smaller country.",
"However, unfortunately for big countries, when the amount of COVID-19 being detected shows as a ripple in their large populations, it means there are far more pockets of COVID-19 to shut down than for the same amount of activity per capita in a small country. Hence the big ships take longer to turn, both to show virus activity and to get outbreaks under control. </p>"),
      fluidRow(
        column(10, style="padding:10px;", plotOutput("plotTTests"))),
      HTML("<p>In order for you to be able to compare the countries' testing strategies, Total Cases and Deaths are included below. <BR><BR><BR></p>"),
      fluidRow(
        column(10, style="padding:10px;", plotOutput("plotTC"))),
      fluidRow(
        column(10, style="padding:10px;", plotOutput("plotTD"))),
      width=10)
  )
  ))

#div(style = "padding: 0px 0px; margin-top:-2em",

##########################


output$plotTTests <- renderPlot({
  
  datasetTests <- dataset5 %>%
    filter(
      as.Date(endweekdate) >= as.Date(input$pickdates[1]),
      as.Date(endweekdate) <= as.Date(input$pickdates[2])
    )
  #pick out country4 (the cumulative tests per 100,000)
  #datasetTests<-subset(datasetTests,select=c('tDate','NorthernIreland4','Scotland4','FaroeIslands4','Sweden4','Ireland4','Iceland4','Finland4','Norway4'))
  #names(datasetTests)[names(datasetTests) == "NorthernIreland4"] <- "NorthernIreland"
  #names(datasetTests)[names(datasetTests) == "Scotland4"] <- "Scotland"
  #names(datasetTests)[names(datasetTests) == "FaroeIslands4"] <- "FaroeIslands"
  #names(datasetTests)[names(datasetTests) == "Sweden4"] <- "Sweden"
  #names(datasetTests)[names(datasetTests) == "Ireland4"] <- "Ireland"
  #names(datasetTests)[names(datasetTests) == "Iceland4"] <- "Iceland"
  #names(datasetTests)[names(datasetTests) == "Finland4"] <- "Finland"
  #names(datasetTests)[names(datasetTests) == "Norway4"] <- "Norway"
  
  #pick out country2 (the testing rate (per week) per 100,000)
  datasetTests<-subset(datasetTests,select=c('tDate','NorthernIreland2','Scotland2','FaroeIslands2','Sweden2','Ireland2','Iceland2','Finland2','Norway2'))
  names(datasetTests)[names(datasetTests) == "NorthernIreland2"] <- "NorthernIreland"
  names(datasetTests)[names(datasetTests) == "Scotland2"] <- "Scotland"
  names(datasetTests)[names(datasetTests) == "FaroeIslands2"] <- "FaroeIslands"
  names(datasetTests)[names(datasetTests) == "Sweden2"] <- "Sweden"
  names(datasetTests)[names(datasetTests) == "Ireland2"] <- "Ireland"
  names(datasetTests)[names(datasetTests) == "Iceland2"] <- "Iceland"
  names(datasetTests)[names(datasetTests) == "Finland2"] <- "Finland"
  names(datasetTests)[names(datasetTests) == "Norway2"] <- "Norway"
  
  pTest <- ggplot(data=datasetTests)+
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
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=16))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16, hjust = -0.045))+
    ggtitle(paste("  Weekly Tests\n /100,000"))
  
 pTest <- pTest + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  
  for (i in 1:length(input$ctries)) {
    pTest <- pTest + geom_line(aes_string(x='tDate',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=2.5)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(pTest)
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
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
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
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=16))+
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
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
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
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=16))+
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
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=16))+
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
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=16))+
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




#23/5/24

#write.csv(UKNIcases, "C:/Users/Mike/Downloads/UKNICases.csv")
#write.csv(UKNIdeaths, "C:/Users/Mike/Downloads/UKNIDeaths.csv")
#write.csv(UKNIdata, "C:/Users/Mike/Downloads/UKNIData.csv")
