# case fatality proportion page


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
                                start = min(dataset1$cDate),
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
                                      "Sweden"="Sweden"),selected = c("Iceland","Ireland","Sweden"))
                 
                 , 
                 plotOutput("plotLEG"),
                 width=2),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(class = "myRow0", 
        column(12,HTML("<H3>How is the same virus more damaging in some countries?</H3><BR><BR>",
           "<p style='margin-left: 40px;margin-right: 60px;font-size:16px;'>The answer mainly lies in Testing and how well positive Cases are picked up.<BR> Case fatality proportion (CFP) can help us explore this.<BR>",
           "<p style='margin-left: 40px;margin-right: 60px;font-size:16px;'>CFP is calculated by dividing the number of deaths by the numbers of cases in a country.<BR>",
           "<p style='margin-left: 40px;margin-right: 60px;font-size:16px;'>CFP can be calculated on a cumulative basis (Total Deaths divided by Total Cases) or per month (Monthly Deaths divided by Monthly Cases).",
           "<BR>You can see these graphs below, and toggle countries on/off using the country selector on the Left...</p><BR><BR>",
           "<p style='margin-left: 40px;margin-right: 60px;font-size:16px;'>Early on in the pandemic, Community Testing was less frequent than Hospital-based testing and thus early Case Fatality Proportion values were a lot higher ",
           " than recent estimates.</p>",
           "<p style='margin-left: 40px;margin-right: 60px;font-size:16px;'>As countries test more people in communities (many of whom are younger and do not experience severe illness), CFP will continue to fall to better reflect the true population-wide effect of COVID-19.</p><BR>",
           "<p style='margin-left: 40px;margin-right: 16px;font-size:16px;'>This leads us to the conclusion that it will be useful to examine COVID-19 trends by Age Group, where available...</p><BR><BR>"))),
     # fluidRow(class = "myRow2", 
    #    column(11, div(style = "height:150px",plotOutput("plotmon")))),
      fluidRow(class = "myRow1", 
        column(11, div(style = "height:300px",plotOutput("plotC1")))),
      fluidRow(class = "myRow1", 
        column(11, div(style = "height:300px",plotOutput("plotC2")))),
      tags$head(tags$style("
      .myRow1{height:350px;}
      .myRow2{height:150px;}"
      ))
      ,width=10)
  )
  ))




output$plotC1 <- renderPlot({
  
  datasetC1 <- cumcfr.df %>%
  #  filter(
  #    Mth >= month(input$pickdates[1]),
  #    Mth <= month(input$pickdates[2])
  #  ) 
  
    filter(
      as.Date(cDateCmth) >= as.Date(input$pickdates[1]),
      as.Date(cDateCmth) <= as.Date(input$pickdates[2])
    ) 
  
  pc1 <- ggplot(data=datasetC1)+
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
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16))+
    #scale_x_continuous(labels = function(Mth) month.abb[Mth], breaks=seq(1,nrow(datasetC1),1))+
    ggtitle(paste("Total Case\nFatality Proportion (%)"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    pc1 <- pc1 + geom_line(aes_string(x='cDateCmth',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(pc1)
  }
}, height=350)


output$plotmon <- renderPlot({
  
  dataset3b <- dataset3 %>%
#   filter(
#      Mth >= month(input$pickdates[1]),
#      Mth <= month(input$pickdates[2])
#    ) 
  filter(
    as.Date(cDateCmth) >= as.Date(input$pickdates[1]),
    as.Date(cDateCmth) <= as.Date(input$pickdates[2])
  ) 
  
  
  pmonc <- ggplot(data=dataset3b)+
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
    theme(axis.text.x=element_blank())+
    theme(axis.text.y=element_blank())+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16))+
    #scale_x_continuous(labels = function(Mth) month.abb[Mth], breaks=seq(1,nrow(dataset3b),1))+
    ggtitle(paste("Monthly Cases/100,000"))
  
  # loop for countries selected
  for (i in 1:length(input$ctries)) {
    pmonc <- pmonc + geom_line(aes_string(x='cDateCmth',y= input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1)
  }
  
  if(is.null(input$ctries)){}
  else{
    print(pmonc)
  }
}, height=150)


output$plotC2 <- renderPlot({
  
  datasetC2 <- cfr.df %>%
   # filter(
  #    Mth >= month(input$pickdates[1]),
  #    Mth <= month(input$pickdates[2])
  #  ) 
  filter(
    as.Date(cDateCmth) >= as.Date(input$pickdates[1]),
    as.Date(cDateCmth) <= as.Date(input$pickdates[2])
  ) 
  
  pc2 <- ggplot(data=datasetC2)+
    scale_x_date(expand = c(0, 0), date_breaks = "month" , date_labels = "%b-%y")+
    theme_classic()+
    theme(axis.title.y = element_blank())+
    theme(axis.title.x = element_blank())+ 
    theme(axis.ticks.x = element_blank())+
    theme(axis.ticks.y = element_blank())+
    #theme(legend.title = element_blank())+
    #theme(legend.text =  element_text(face="plain", color="darkgrey",size=22))+
    theme(axis.line.x = element_blank())+
    theme(axis.line.y = element_blank())+
    theme(axis.text.x=element_text(face="plain", color="darkgrey",angle=60,size=18, hjust=1,vjust = 1))+
    theme(axis.text.y=element_text(face="plain", color="darkgrey",size=20))+
    theme(plot.title = element_text(family = "sans", face="bold", colour="darkgrey", size=16))+
    #scale_x_continuous(labels = function(Mth) month.abb[Mth], breaks=seq(1,nrow(datasetC2),1))+
    ggtitle(paste("Monthly Case\nFatality Proportion (%)"))
  
  for (i in 1:length(input$ctries)) {
    pc2 <- pc2 + geom_line(aes_string(x='cDateCmth',y=input$ctries[i]),colour=colourlisted[input$ctries[i]],size=1.75)
  }

  
  if(is.null(input$ctries)){}
  else{
    print(pc2)
  }
  
}, height=250)



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
  
  
  #put in a blank/white point just to left-justify legend points
  p5 <- p5 + geom_point(aes_string(x=1,y= 0),colour="white",shape=15, size=5)
  
  if(is.null(input$ctries)){}
  else{
    print(p5)
  }
}, width=200, height= 300)
