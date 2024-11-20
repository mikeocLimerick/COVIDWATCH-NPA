# excess mortality page

output$pageStub <- renderUI(tagList(
  fluidRow(
    #column(10,
    HTML("<p style='margin-left: 40px;margin-right: 30px; text-align:right; font-size:11px'><BR>Updated 01/04/23</p>",
         "<H3 style='margin-left: 40px;margin-right: 30px'>Excess Mortality</H3><BR>",
         "<p style='margin-left: 40px;margin-right: 30px'><BR><b>What is excess mortality, and why is it important when we talk about COVID-19? </b> </p><BR>",
         "<p style='margin-left: 40px;margin-right: 30px'><BR>Excess mortality is a critical tool for public health analysis, providing a useful measure of the true impact of COVID-19.",
         "<BR>It is calculated over a given period of interest, by dividing the number of deaths observed in that time period",
         " by the average number of deaths seen in the same timeframe during a baseline period. In our project, we took the period from 2015-2019 inclusive",
         " as our baseline.</p>",
         "<p style='margin-left: 40px;margin-right: 30px'><BR>Unlike case or death counts tied solely to COVID-19 diagnoses, excess mortality accounts for underreported cases, healthcare disruptions, and indirect effects like delayed treatments for other conditions.</p>",
         "<p style='margin-left: 40px;margin-right: 30px'><BR>Note that for excess mortality to be a useful measure, overall deaths reporting in the country must be accurate.</p>",
         "<p style='margin-left: 40px;margin-right: 30px'><BR>If this is the case, excess mortality calculations allow us to appraise the overall effects from COVID-19 being experienced in different countries, helping contextualise pandemic outcomes within the broader range of healthcare issues being faced across diverse regions.",
         "<BR><BR><p style='margin-left: 40px;margin-right: 30px'><BR>The following Table is a summary of excess mortality across partner countries for 2020 and 2021<BR><BR>",
         "<p align='center'><img src='excesstable.png', width='80%', height='80%''></p><BR><BR><BR>")
    
    #)
  )
))
