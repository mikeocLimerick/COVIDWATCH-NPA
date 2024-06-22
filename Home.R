# home page

output$pageStub <- renderUI(tagList(
  fluidRow(
    #column(10,
           HTML("<p style='margin-left: 40px;margin-right: 30px; text-align:right; font-size:11px'><BR>Updated 10/06/24</p>",
                "<H3 style='margin-left: 40px;margin-right: 30px'>Welcome</H3> <BR>",
                "<p style='margin-left: 40px;margin-right: 30px'><BR>COVIDWATCH EU-NPA is an international project summarising lessons learnt from the COVID-19 ",
                "response of countries at the North-Western edge of Europe. We use 'Open Data' in addition to local context provided by clinicians in participating countries.",
                "<BR>This website allows the general public to pick through the various elements of each country's response, ",
                " thereby allowing people to learn more about the pandemic and what the options are when it comes to dealing with a virus like SARS-CoV-2. ",
                "<BR><BR>You can see which countries are involved in this project (and short summaries of each country's response) using the 'Countries' tab above. ",
                "Up to date data from all countries can be graphed using the 'Cases+Deaths' tab. Of note Northern Ireland and Scotland stopped publishing data in Mid-2022.",
                "<BR><BR>While we are all tired of hearing about COVID-19, it is clear that this health challenge is not going away just yet.",
                "Therefore the more we can understand about this new disease and how our responses have affected health outcomes, directly and indirectly, the better we can learn from the various national responses of our countries.",
                "<BR><BR>At this point we should remind you that direct comparisons of countries are generally not straightforward.",
                "<BR>For example, COVID-19 Case detection depends on each country's Testing strategy and COVID-19 Deaths ",
                " data depend on the system in place to record Deaths in each country and how a probable or confirmed diagnosis of COVID-19 affects this recording.",
                "These limitations, and the fact that COVID-19 Testing and Public Health strategies matured at different rates across the world, ",
                "mean we need to be cautious when trying to form conclusions about COVID-19. ",
                "However, by graphing a country's data over time, we can at least say with confidence how it is currently doing versus its previous performance with respect to major outcome measures.</p><BR>",
                "<p align='center'><img src='npamap.png', width='40%', height='40%''></p>",
                
                "<p style='margin-left: 40px;margin-right: 30px'><BR>The following Table is a summary of our collaboration's findings, highlighting initial responses deemed important to controlling an airborne pathogen in the context of a pandemic. Of note, these are our findings before the roll-out of effective COVID-19 vaccines. <BR><BR>",
                "<p align='center'><img src='collab.png', width='80%', height='80%''></p><BR><BR><BR>",
                "<p style='margin-left: 40px;margin-right: 30px';color:#000066;font-size:15px;'><BR>Feedback welcomed. Contact us at <u>covidwatchnpa@gmail.com</u>. Built in R Shiny.</p>",
                "<p style='margin-left: 40px;font-size:11px;'><b>Open data sources:</b>",
                "https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases AND",
                "https://api.coronavirus.data.gov.uk/</p>",
                "<BR><p style='margin-left: 40px;margin-right: 30px'>Supported by the <a href='http://www.interreg-npa.eu/covid-19/npa-covid-19-response-group/'>NPA COVID-19 Response Group</a></p><BR><BR>")
           
    #)
  )
))


#"<p align='center'><img src='npamap.png' style='width:450px;height:262px;'></p>",
