# NPA regional analysis page

output$pageStub <- renderUI(tagList(
  fluidRow(
    #column(10,
    HTML("<H3 style='margin-left: 40px;margin-right: 30px'>NPA Regional Data</H3> <BR>",
         "<p style='margin-left: 40px;margin-right: 30px'><BR>Examining COVID-19 case and death data from across a ",
         "range of countries participating in the NPA programme can offer insights and help us learn lessons from our responses to COVID-19.",
         "<BR>This page details results from analyses of case and death data by Region in each participating country. <BR><BR><BR> </p>",
         "<p style='margin-left: 40px;margin-right: 30px'>These maps details case and death counts per 100,000 population across the NPA and non-NPA region Ireland, Northern Ireland, Scotland and Sweden (for whome regional deaths data are available). <BR><BR></p>",
         "<p align='center'><img src='thebigone.png', width='90%', height='90%''></p>",
         "<p style='margin-left: 40px;margin-right: 30px'>These maps show us that for the first year of the pandemic, where COVID-19 activity is high, COVID-19 mortality is correspondingly high. ", 
"However, case levels depend on Testing systems, whereas COVID-19 deaths can be a more robust measure of the impact of the pandemic. Interestingly areas of high case density on the island of Ireland, i.e. darker blue areas on the left hand maps, do not generate very high death rates deaths when compared to the Scottish maps.",
"The Scottish maps show comparatively high death rates in the non-NPA regions of Scotland, whereas these same areas are not associated with high density on the blue/case graph on the left hand side of the image. This again likely reflects under-testing or more unchecked spread in Scotland, possibly in conjunction with more infections of more vulnerable citizens and/or signs of a more vulnerable population.",
"<BR><BR></p>",
         "<BR>",
         "<p style='margin-left: 40px;margin-right: 30px'>The charts below shows analyses of case data by population density for regions in each of the partner countries listed below, divided into two groups: NPA or non-NPA regions. The chart on the left ",
         "covers the period from March 2020 to February 2021, while the chart on the right covers the period from March 2021 to February 2022. ",
         "<BR> </p> ",
         "<p align='center'><img src='denscases.png', width='80%', height='80%''></p>",
         "<p style='margin-left: 40px;margin-right: 30px'><BR> These charts suggest a weak to moderate association between rising population density and increased case rate, which was stronger for the (on average) more populated non-NPA regions. While there is some crossover of confidence intervals as indicated by the shaded grey areas, on average the lower population densities seen in the NPA regions experienced lower case counts than the non-NPA regions. Examination of the y-axis scales shows the greater volume of case reporting in the second year of the pandemic.",
"<BR><BR><BR> </p>",
"<BR><p style='margin-left: 40px;margin-right: 30px'> The panel below shows analyses of deaths data (where available) by population density for regions in each of the partner countries listed below, divided into two groups: NPA or non-NPA regions. Analyses are carried out over the same time periods as the cases charts above. </p>",
         "<p align='center'><img src='densdeaths.png', width='80%', height='80%''></p>",
"<p style='margin-left: 40px;margin-right: 30px'><BR> For the four countries with available regional deaths data, these charts show that deaths were weakly associated with increasing population density and that non-NPA regions fared worse in terms of COVID-19 mortality in the first year of the pandemic. ", 
" However, in the second year of the pandemic, NPA regions, on average, experienced more COVID-19 deaths, although there is again considerable overlap of confidence intervals.", 
" It is clear from examination of the y-axis scales that the volume of COVID-19 death reporting decreased considerably during the second year of the pandemic, which is the opposite of trends seen for the case data above.</p><BR><BR><BR>")
    #)

  )
))





# don't want to publish full tables (as looking for a journal article to be accepted)
# so code for the lot (Tables, Maps, Charts) is as follows:
#column(10,
#HTML("<H3 style='margin-left: 40px;margin-right: 30px'>NPA Regional Data</H3> <BR>",
#     "<p style='margin-left: 40px;margin-right: 30px'><BR>Examining COVID-19 case and death data from across a ",
#     "range of countries participating in the NPA programme can offer insights and help us learn lessons from our responses to COVID-19.",
#     "<BR>This page details results from analyses of case and death data by Region in each participating country. <BR><BR><BR> ",
#     "This first table details case counts per 100,000 population for the NPA regions and non-NPA regions (where applicable) within partner countries <BR><BR></p>",
#     "<p align='center'><img src='npacases.png', width='80%', height='80%''></p>",
#     "<p style='margin-left: 40px;margin-right: 30px'>Over the first year of the pandemic, 4 countries examined exceeded 3,000 cases per 100,000 population (Ireland, Northern Ireland, Scotland, Sweden). Meanwhile, the Faroes, Finland, Iceland, Greenland and Norway confirmed COVID-19 infection rates were much lower. This is contrast to the second year of the pandemic, where totals exceed 10,000 cases per 100,000 (i.e. 10%) for all countries examined. Some countries, particularly those with small populations see very large rates due to small population size effects. In any case, much COVID-19 was confirmed in countries examined from March 2021 to February 2022, due to a combination of widespread testing and viral spread as societal restrictions were relaxed in the wake of large vaccination campaigns. Significant differences in COVID-19 case rates were found between NPA regions and non-NPA regions all applicable countries in the first and second year of the pandemic. <BR><BR></p>",
#     "<BR>",
#     "<p style='margin-left: 40px;margin-right: 30px'>This table details death counts per 100,000 population for the NPA regions and non-NPA regions (where applicable) within partner countries <BR><BR></p>",
#     "<p align='center'><img src='npadeaths.png', width='80%', height='80%''></p>",
#     "<p style='margin-left: 40px;margin-right: 30px'>Significant differences in COVID-19 death rates were found between NPA regions and non-NPA regions of three of the four of the countries in the first 12 months of the pandemic.  Despite the persistence of significantly higher case counts across all non-NPA regions, a similar difference in death rate across NPA versus non-NPA regions only persisted in the second year of the pandemic for Scotland. ",
#     "These data demonstrate how relatively high COVID-19 deaths were experienced in Ireland, Northern Ireland, Scotland and Sweden in the first year of the pandemic, around April-May 2020 and Winter 2020/21, which understandably tallies with the case data of Table 1. There was more even spread of COVID-19 deaths across the nine countries in the second year of the pandemic, concentrated within the Nov 21-Feb 22 period. Earlier deaths seem to have done little to temper effects from disease spread in the second year of the pandemic. Nevertheless, despite comparatively huge increases in cases recorded (at least 10x) seen in the second year, deaths in the second winter of the pandemic did not follow this pattern.",
#     "<BR><BR></p>",
#     "<BR>",
#     "<p style='margin-left: 40px;margin-right: 30px'>These maps details case and death counts per 100,000 population across the NPA and non-NPA region Ireland, Northern Ireland, Scotland and Sweden (for whome regional deaths data are available). <BR><BR></p>",
#     "<p align='center'><img src='thebigone.png', width='90%', height='90%''></p>",
#     "<p style='margin-left: 40px;margin-right: 30px'>These maps show us that for the first year of the pandemic, where COVID-19 activity is high, COVID-19 mortality is correspondingly high. ", 
#     "However, case levels depend on Testing systems, whereas COVID-19 deaths can be a more robust measure of the impact of the pandemic. Interestingly areas of high case density on the island of Ireland, i.e. darker blue areas on the left hand maps, do not generate very high death rates deaths when compared to the Scottish maps.",
#     "The Scottish maps show comparatively high death rates in the non-NPA regions of Scotland, whereas these same areas are not associated with high density on the blue/case graph on the left hand side of the image. This again likely reflects under-testing or more unchecked spread in Scotland, possibly in conjunction with more infections of more vulnerable citizens and/or signs of a more vulnerable population.",
#     "<BR><BR></p>",
#     "<BR>",
#     "<p style='margin-left: 40px;margin-right: 30px'>The charts below shows analyses of case data by population density for regions in each of the partner countries listed below, divided into two groups: NPA or non-NPA regions. The chart on the left ",
#     "covers the period from March 2020 to February 2021, while the chart on the right covers the period from March 2021 to February 2022. ",
#     "<BR> </p> ",
#     "<p align='center'><img src='denscases.png', width='80%', height='80%''></p>",
#     "<p style='margin-left: 40px;margin-right: 30px'><BR> These charts suggest a weak to moderate association between rising population density and increased case rate, which was stronger for the (on average) more populated non-NPA regions. While there is some crossover of confidence intervals as indicated by the shaded grey areas, on average the lower population densities seen in the NPA regions experienced lower case counts than the non-NPA regions. Examination of the y-axis scales shows the greater volume of case reporting in the second year of the pandemic.",
#     "<BR><BR><BR> </p>",
#     "<BR><p style='margin-left: 40px;margin-right: 30px'> The panel below shows analyses of deaths data (where available) by population density for regions in each of the partner countries listed below, divided into two groups: NPA or non-NPA regions. Analyses are carried out over the same time periods as the cases charts above. </p>",
#     "<p align='center'><img src='densdeaths.png', width='80%', height='80%''></p>",
#     "<p style='margin-left: 40px;margin-right: 30px'><BR> For the four countries with available regional deaths data, these charts show that deaths were weakly associated with increasing population density and that non-NPA regions fared worse in terms of COVID-19 mortality in the first year of the pandemic. ", 
#     " However, in the second year of the pandemic, NPA regions, on average, experienced more COVID-19 deaths, although there is again considerable overlap of confidence intervals.", 
#     " It is clear from examination of the y-axis scales that the volume of COVID-19 death reporting decreased considerably during the second year of the pandemic, which is the opposite of trends seen for the case data above.</p><BR><BR><BR>")
#)
