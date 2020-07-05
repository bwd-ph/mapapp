library(shiny) ; library(dplyr) ; library(readr); library(sf) ; library(leaflet) ; library(htmltools); library(htmlwidgets); library(sp); library(RColorBrewer) ; library(rgeos) ; library(plotly) ; library(shinyWidgets); library(xml2); library(shinyBS); library(shinyLP)

# Load data ---------------------------


# See https://www.labnol.org/internet/free-file-hosting-github/29092/ for '?raw=true' syntax

PCNdefs <- read.csv("https://github.com/bwd-ph/mapdata/blob/master/PCNdefs.csv?raw=true", stringsAsFactors = FALSE)
PCNdefs$PCN <- trimws(PCNdefs$PCN) # found trailing right spaces
PCNdefs$Practice.Name <- trimws(PCNdefs$Practice.Name) # found trailing right spaces
lsoapops <- read.csv("https://github.com/bwd-ph/mapdata/blob/master/lsoapops.csv?raw=true")
practicepops <- read.csv("https://github.com/bwd-ph/mapdata/blob/master/practicepops.csv?raw=true")
# turn age numeric, including turning '95+' to '95'
practicepops$AGE <- as.numeric(gsub("[+]", "", as.character(practicepops$AGE))) 
practicepops$AGE_grp <- 
ifelse(practicepops$AGE<= 4,'0-4',
ifelse(practicepops$AGE<= 9,'5-9',
ifelse(practicepops$AGE<= 14,'10-14',
ifelse(practicepops$AGE<= 19,'15-19',
ifelse(practicepops$AGE<= 24,'20-24',
ifelse(practicepops$AGE<= 29,'25-29',
ifelse(practicepops$AGE<= 34,'30-34',
ifelse(practicepops$AGE<= 39,'35-39',
ifelse(practicepops$AGE<= 44,'40-44',
ifelse(practicepops$AGE<= 49,'45-49',
ifelse(practicepops$AGE<= 54,'50-54',
ifelse(practicepops$AGE<= 59,'55-59',
ifelse(practicepops$AGE<= 64,'60-64',
ifelse(practicepops$AGE<= 69,'65-69',
ifelse(practicepops$AGE<= 74,'70-74',
ifelse(practicepops$AGE<= 79,'75-79',
ifelse(practicepops$AGE<= 84,'80-84',
ifelse(practicepops$AGE<= 89,'85-89',
ifelse(practicepops$AGE<= 94,'90-94','95+')))))))))))))))))))
practicepops$AGE_grp <- factor(practicepops$AGE_grp, levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                                                "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95+"))

imd <- read_csv("https://github.com/bwd-ph/mapdata/blob/master/IMD_2019_LSOA.csv?raw=true",
                col_types = cols(
                  IndID = col_factor(NULL),
                  polycode = col_factor(NULL),
                  value = col_double(),
                  label = col_character()))
lh <- read_csv("https://github.com/bwd-ph/mapdata/blob/master/PennineLH_MSOA.csv?raw=true",
               col_types = cols(
                 IndID = col_factor(NULL),
                 polycode = col_factor(NULL),
                 value = col_double(),
                 label = col_character()))
OtherMSOA <- read_csv("https://github.com/bwd-ph/mapdata/blob/master/PennineOther_MSOA.csv?raw=true",
               col_types = cols(
                 IndID = col_factor(NULL),
                 polycode = col_factor(NULL),
                 value = col_double(),
                 label = col_character()))
OtherLSOA <- read_csv("https://github.com/bwd-ph/mapdata/blob/master/PennineOther_LSOA.csv?raw=true",
                      col_types = cols(
                        IndID = col_factor(NULL),
                        polycode = col_factor(NULL),
                        value = col_double(),
                        label = col_character()))
Nomis <- read_csv("https://github.com/bwd-ph/mapdata/blob/master/PennineNomis_LSOA.csv?raw=true",
              col_types = cols(
                IndID = col_factor(NULL),
                polycode = col_factor(NULL),
                value = col_double(),
                label = col_character()))
ESA <- read_csv("https://github.com/bwd-ph/mapdata/blob/master/PennineESA_LSOA.csv?raw=true",
                  col_types = cols(
                    IndID = col_factor(NULL),
                    polycode = col_factor(NULL),
                    value = col_double(),
                    label = col_character()))
df <- rbind(imd,lh,OtherMSOA,OtherLSOA,Nomis,ESA)
metadata <- read_csv("https://github.com/bwd-ph/mapdata/blob/master/Metadata.csv?raw=true")
metaCategories <- read_csv("https://github.com/bwd-ph/mapdata/blob/master/MetaCategories.csv?raw=true")

PatientDate <- metadata %>% mutate(PatientDate = paste(Unit,Years," ")) %>% filter(IndID == "PatientData") %>% select(PatientDate)
PatientDate <- as.character(PatientDate) # This is the date of the patient registration data from NHSD - e.g. 'April 2020 '

# Super Output Area boundaries (source: ONS Open Geography Portal)
lsoa <- st_read("https://github.com/bwd-ph/boundaries/blob/master/lsoa.geojson?raw=true")
colnames(lsoa)[2] = "polycode"
colnames(lsoa)[3] = "polyname"
colnames(lsoa)[4] = "polynamew"
msoa <- st_read("https://github.com/bwd-ph/boundaries/blob/master/msoa.geojson?raw=true")
colnames(msoa)[2] = "polycode"
colnames(msoa)[3] = "polyname"
colnames(msoa)[4] = "polynamew"
# combine into one layer
subdistricts <- rbind(lsoa, msoa)

# work out which CCG each LSOA is in
lsoa$CCG <- ifelse(startsWith(as.character(lsoa$polyname),"Blackburn"),"BwD",
                   ifelse(startsWith(as.character(lsoa$polyname),"Burnley"),"E Lancs",
                          ifelse(startsWith(as.character(lsoa$polyname),"Hyndburn"),"E Lancs",
                                 ifelse(startsWith(as.character(lsoa$polyname),"Pendle"),"E Lancs",
                                        ifelse(startsWith(as.character(lsoa$polyname),"Rossendale"),"E Lancs",
                                               ifelse(lsoa$polycode %in% c("E01025316","E01025317","E01025322","E01025325","E01025326","E01025327","E01025328","E01025342"),"Gtr P","E Lancs"))))))

# work out centroids for LSOA population labels or circles
lsoalayer <- as(lsoa,"Spatial")
proj4string(lsoalayer) <- CRS("+init=epsg:4326")
centers <- data.frame(gCentroid(lsoalayer, byid = TRUE))
centers$lsoacode <- lsoalayer@data$polycode

# Local Authority boundaries (source: ONS Open Geography Portal)
la <- st_read("https://github.com/bwd-ph/boundaries/blob/master/local_authorities.geojson?raw=true")
laLayer <- as(la,"Spatial")
proj4string(laLayer) <- CRS("+init=epsg:4326")


# CCG boundaries (source: ONS Open Geography Portal)
ccg <- st_read("https://github.com/bwd-ph/boundaries/blob/master/CCGs.geojson?raw=true")
ccgLayer <- as(ccg,"Spatial")
proj4string(ccgLayer) <- CRS("+init=epsg:4326")

# Neighbourhood boundaries
nhood <- st_read("https://github.com/bwd-ph/boundaries/blob/master/Pennine_Nhoods.geojson?raw=true")
nhoodLayer <- as(nhood,"Spatial")
proj4string(nhoodLayer) <- CRS("+init=epsg:4326")

# Ward boundaries
ward <- st_read("https://github.com/bwd-ph/boundaries/blob/master/Wards2018.geojson?raw=true")
wardLayer <- as(ward,"Spatial")
proj4string(wardLayer) <- CRS("+init=epsg:4326")

# BwD mask
BwDmask <- st_read("https://github.com/bwd-ph/boundaries/blob/master/BwD_mask.geojson?raw=true")

# map title - see https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map
# tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title { 
#                                 transform: translate(-50%,10%);
#                                 position: fixed !important;
#                                 left: 50%;
#                                 text-align: center;
#                                 padding-left: 10px; 
#                                 padding-right: 10px; 
#                                 background: rgba(255,255,255,0.85);
#                                /* font-weight: bold; */
#                                 font-size: 14px;
#                                 }
#                                 "))

# function to unescape an HTML character coding such s "&pound;" - see https://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

# function to find the 'Intro' tabl title link and rewrite its attributes. See https://stackoverflow.com/questions/36530613/r-shiny-use-navbarpage-with-bsmodal-by-shinybs

jsStr <- '$(document).ready(function(){
  $("a[data-value=\'Intro\']").attr({
    "href":"#", 
    "data-toggle":"modal",
    "data-target":"#landingPage"
  });
})
' 


###### UI ########

ui <- bootstrapPage(
  tags$head(includeCSS("styles_base.css"), includeCSS("styles_shiny.css"), includeCSS("styles_map.css"),tags$script(HTML(jsStr)),
            tags$style(type = "text/css", "html, body {width:100%;height:100%}",".selectize-dropdown-content {max-height: 600px; }")),
  
  bsModal(id = 'landingPage',title = "Welcome to the Pennine Lancs Mapping App",trigger = 'intro',size = 'large',fluidRow(
    column(6, panel_div(class_type = "primary", panel_title = "'Shaded Map' tab",
                        tags$div(
                          tags$p("The map is shaded according to the characteristics of the ",span("resident",style = "font-weight:bold"),"population (i.e. those who ",span("live",style = "font-weight:bold"),"in each area.)"),              
                          tags$h4("Mapping an indicator"),
                          tags$ul(
                            tags$li("Use 'Select Category' to explore available topics."),
                            tags$li("Click against an indicator to map it."),
                            tags$li("Hover over a shaded area to read the exact values.")
                          ),
                          tags$h4("Choosing an overlay"),
                          tags$ul(
                            tags$li("Click against an overlay to map its boundaries."),
                            tags$li("The box at the bottom right tells you which boundary your mouse is in.")
                          ), # end of ul
                          tags$p("N.B. - The 'Neighbourhoods' are just one way of carving the area up. If you wish to suggest an alternative or additional set of boundaries, please get in touch."),
                          tags$p("P.S. - East Lancs neighbourhoods have been temporarily removed as they are under review.")
                        ) # end of tags$div
    ) # end of panel_div
    ), # end of column
    column(6, panel_div(class_type = "primary", "'Registered Pop' tab",
                        tags$div(
                          tags$p("The term 'Registered Population' refers to the ",span("patients",style = "font-weight:bold"),"of a particular CCG, PCN or practice, regardless of where they live."),
                          tags$h4("Counting the patients"),
                          tags$ul(
                            tags$li("Initially, a population pyramid is shown for all patients of both CCGs combined."),
                            tags$li("You can drill down to a single CCG, PCN or practice."),
                            tags$li("Hover over the pyramid to read the exact numbers."),
                            tags$li("Tick 'Show on map?' to see blue circles on the map, showing where the patients live."),
                            tags$li("Click on a blue circle to read the exact count.")
                          ), # end of ul
                          tags$p("N.B. - In this app, the term 'PCN' is used to mean 'Primary Care ",span("Network",style = "font-weight:bold"),"' - i.e. a group of GP practices. This is ",span("not",style = "font-style:italic"),"the same thing as a geographical neighbourhood."),
                          tags$p("Also, please note that the Fairmore Medical Practice has been wholly assigned to Pendle West PCN (as stipulated by NHS Digital).")
                        ) # end of tags$div
    ) # end of panel_div
    ) # end of column
  ),  # end of fluidRow
  fluidRow(
    column(12, panel_div("success", "Feedback",
                         HTML("If you have any comments, queries or suggestions about the Pennine Lancs Mapping App, please contact: <a href='mailto:anne.cunningham@blackburn.gov.uk?Subject=Pennine%20Lancs%20Mapping%20App%20Feedback' target='_top'>Anne Cunningham</a>")))
  ) # end of fluidRow
  
  ),
  
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, height = "auto",width = "350px",
  tabsetPanel(
    tabPanel("Shaded Map",
      selectInput(inputId="category",label = "Select Category", choices = sort(unique(metaCategories$Category))),
      fluidRow(
        tabsetPanel(
           tabPanel("Indicators",
             wellPanel("",style = "overflow-y:scroll; max-height: 250px",          
              uiOutput("radio")),
              
             tabsetPanel(
               tabPanel("Overlay",
               radioButtons("mainBoundaries", label = h4("Overlay (black)"),
                 choices = list("Districts (shown also in white)" = 1, "CCGs" = 2, "Neighbourhoods (BwD only)" = 3,
                                "Wards (2018)" = 4), selected = 1)),
               tabPanel("Transparency",
                        sliderInput("sliderTrans",label = h5("(0=transparent, 1=solid shading)"), min=0,max=1,value = 0.6))),
             
               verbatimTextOutput("out")),
             
           tabPanel("About",
              tags$br(),
              htmlOutput("about"),
              tags$br(),
              tags$h4("Additional sources"),
                 "Boundary layers: ", 
                 tags$a(href="http://geoportal.statistics.gov.uk/", "ONS Open Geography Portal", target="_blank"),
                 tags$br(),
                 tags$br(),
                "MSOA names: ", 
                 tags$a(href="https://visual.parliament.uk/msoanames/", "House of Commons Library", target="_blank"),
                tags$br(),
                tags$br(),
              tags$a(href="https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice", "Patient registration", target="_blank"),
                 " and ",
              tags$a(href="https://digital.nhs.uk/services/organisation-data-service/data-downloads/gp-and-gp-practice-related-data", "PCN affiliation", target="_blank"),
                 " data: ",
                 tags$a(href="https://digital.nhs.uk/", "NHS Digital", target="_blank"),
                 tags$br(),
                 tags$br(),
                 tags$h4("Credits"),
                 "Original code by ", 
                 tags$a(href="https://github.com/traffordDataLab/apps/tree/master/IMD_2015", 
                 "Trafford Data Lab", target="_blank")," gratefully acknowledged",
                 tags$br(),
                 tags$br(),
                 "This",
                 tags$a(href="https://cran.r-project.org/web/packages/shiny/index.html", "Shiny", target="_blank"), 
                 "app uses the following R packages:",
                     tags$a(href="https://cran.r-project.org/web/packages/tidyverse/index.html", "tidyverse,", target="_blank"),
                     tags$a(href="https://cran.r-project.org/web/packages/sf/index.html", "sf,", target="_blank"),
                     tags$a(href="https://cran.r-project.org/web/packages/leaflet/index.html", "leaflet,", target="_blank"),
                     tags$a(href="https://cran.r-project.org/web/packages/htmltools/index.html", "htmltools.", target="_blank")
                )))),
    tabPanel("Registered Pop",
#        tags$h4("Click to show/hide CCG/PCN/Practice menu"),
#        br(),
#        dropdownButton(
      tabsetPanel(id = "selections",
        tabPanel("CCGs",
          selectInput(inputId = "CCG",
                               label = "Choose a CCG:",
                               choices = c("BwD and E Lancs combined" = "Both","Blackburn with Darwen" = "BwD",
                                           "East Lancashire" = "E Lancs", "Other" = "Other"),
                               selected = "Both")
        ),
        tabPanel("PCNs",
          uiOutput("PCN_input")
        ),
        tabPanel("Practices",
          uiOutput("Practice_input")
        )
      ),
#        circle = TRUE, 
#        status = "primary",
#        icon = icon("bars"),
#        width = "300px",
#        label = "Select"),
#        br(),
        conditionalPanel("input.CCG != 'Other'",#style = "overflow-y:scroll; max-height: 400px", 
                         #textOutput("result"),
                         # uiOutput("result"),
                         htmlOutput("result"),
                         plotlyOutput("pyramid", height = "300px")
        ),
        htmlOutput("inarea"),
        checkboxInput("showblobs",label = "Show on map?",value = FALSE),
        htmlOutput("outofarea")
    ), # end of tabPanel
   tabPanel("Intro") # end of tabPanel
   
  ) # end of tabsetPanel

)
             
             )   # end of UI
   

######### SERVER ########

server <- function(input, output, session) {
  values <- reactiveValues(highlight = c())
  
  output$cat <- renderUI({
    input$category
  })
  
  varsInCat <- reactive({
    filter(metaCategories,Category == input$category) %>%
    inner_join(metadata,by = "IndID") %>%
    select(IndID,IndName)
  })
  
  sliderValue <- reactive({
    return(input$sliderTrans)
  })
  
  output$radio <- reactiveUI(function() {
    radioButtons(inputId = "IndID",label = NULL, 
                 choiceNames = varsInCat()$IndName,choiceValues = varsInCat()$IndID)
  })
  
  filteredData <- reactive({
        subdistricts <- inner_join(subdistricts, subset(df, IndID == input$IndID), by = "polycode")
  })
  
  output$about <- renderUI({
    aboutText <- filter(metadata, IndID %in% filteredData()$IndID)$About
    HTML(aboutText)
  })
  
  output$PCN_input <- renderUI({
    selectInput("PCN_output",ifelse(input$CCG == "BwD","Select PCN in Blackburn with Darwen",
                                    ifelse(input$CCG == "E Lancs","Select PCN in East Lancashire","Select PCN")),choices = c("All",sort(unique(filter(PCNdefs,CCG == input$CCG)$PCN))),selected = "All")
  })
  
  output$Practice_input <- renderUI({
    if(input$CCG == "Both") {
      selectInput("Practice_output","Select Practice",choices = c("All")) # avoids an error message when initialising
    } else {
      selectInput("Practice_output",ifelse(input$PCN_output != "All",paste0("Select Practice in ",input$PCN_output),"Select Practice"),choices = c("All",sort(unique(filter(PCNdefs,PCN == input$PCN_output)$Practice.Name))),selected = "All")
    }
  })
  
  output$result <- renderUI({
    print("now in output$result")
    print(input$CCG)
    print(input$PCN_output)
    print(input$Practice_output)
    HTML(paste("In ",PatientDate,ifelse(input$CCG == "Both","<b>BwD and E Lancs CCGs combined</b> had ",
          ifelse(is.null(input$PCN_output) | input$PCN_output == "All",
                  ifelse(input$CCG == "BwD","<b>Blackburn with Darwen CCG</b> had ","<b>East Lancashire CCG</b> had "),
          ifelse(is.null(input$Practice_output) | input$Practice_output == "All",
                  paste0("<b>",input$PCN_output,"</b>"," had "),paste0("<b>",input$Practice_output,"</b> had ")))),
          sum(semi_join(practicepops,filteredPracs(), by = c("ORG_CODE" = "PCode"))$abs_pop), " registered patients:"))
  })
  
  output$inarea <- renderUI({
    if(input$CCG == "Other") {
      HTML(paste("In ",PatientDate," CCGs <em>other</em> than BwD and E Lancs had millions of registered patients across England (population pyramid not shown).<br/><br/>",sum(filteredlsoapops()$patients)," of them live in the shaded area."))
    } else
    {
      HTML(paste("<br/>",sum(filteredlsoapops()$patients)," of them live in the shaded area."))
    }
  })
  
  output$outofarea <- renderUI({
    if(input$CCG == "Other") {
      HTML(paste("The rest of them live outside it."))
    } else
    {
      ifelse(sum(semi_join(practicepops,filteredPracs(), by = c("ORG_CODE" = "PCode"))$abs_pop) == sum(filteredlsoapops()$patients),HTML("There are no patients living outside it."),HTML(paste("The other ",sum(semi_join(practicepops,filteredPracs(), by = c("ORG_CODE" = "PCode"))$abs_pop)-sum(filteredlsoapops()$patients)," registered patients live outside it.")))
    }
  })
  
  observeEvent(input$CCG,{
    print("now doing observeevent input$CCG")
    print(input$CCG)
    print(input$PCN_output)
    print(input$Practice_output)
    if(input$CCG == "Both" | input$CCG == "Other") {
      hideTab(inputId = "selections",target = "PCNs")
      hideTab(inputId = "selections",target = "Practices")
    }
    else {
      showTab(inputId = "selections",target = "PCNs", select = TRUE)
    }
  })
  
  observeEvent(input$PCN_output,{
    print("now doing observeevent input$PCN_output")
    print(input$CCG)
    print(input$PCN_output)
    print(input$Practice_output)
    if(input$PCN_output == "All") {
      hideTab(inputId = "selections",target = "Practices")
    }
    else {
      showTab(inputId = "selections",target = "Practices", select = TRUE)
    }
  })
  
  output$pyramid <- renderPlotly({
    pyramidpops <- practicepops %>% semi_join(filteredPracs(), by = c("ORG_CODE" = "PCode")) %>%
      group_by(AGE_grp,SEX) %>% summarise(POP = sum(POP), abs_pop = sum(abs_pop))
    plot_ly(pyramidpops,x = ~POP, y = ~AGE_grp, color = ~SEX) %>%
      add_bars(orientation = "h",
               hoverinfo = "y+text+name", text = ~abs_pop, 
               colors = c("mediumvioletred", "mediumslateblue"), showlegend = FALSE) %>%
      add_annotations(
        x= 0.1,
        y= 0.9,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        text=sprintf("<b>%s</b>", "M"),
        showarrow = F,
        font = list(
          color = "mediumslateblue",
          size = 18
        )
      ) %>%
      add_annotations(
        x= 0.9,
        y= 0.9,
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        text=sprintf("<b>%s</b>", "F"),
        showarrow = F,
        font = list(
          color = "mediumvioletred",
          size = 18
        )
      ) %>%
      config(displayModeBar = F) %>% 
      layout(bargap = 0, barmode = "overlay", 
             hovermode = "compare",
             xaxis = list(tickmode = "array", 
                          showticklabels = FALSE,
                          showlegend = FALSE,
                          title = "Registered patients\n(hover to see counts)"), 
             yaxis = list(title = "Age Group", showspikes = TRUE,spikemode = "across",
                          spikecolor = '#80ffffff',spikethickness = 0.01)) # transparency seems to be ignored, hence use of a thin line 
  })
  
  filteredPracs <- reactive({
    print("now in filteredPracs")
    if(input$CCG == "Both") 
    {
      print("doing CCG both")
      select(PCNdefs,PCode)
    }else
    if(input$CCG == "Other")
    {
      print("doing CCG Other")
      filter(PCNdefs,CCG=="Other") %>% select(PCode)
    }else
      {
        req(input$PCN_output)
        if(is.null(input$PCN_output) | input$PCN_output == "All")
        {
          print("doing no specific PCN")
          print(length(input$PCN_output))
          print(is.null(input$PCN_output))
          filter(PCNdefs,CCG == input$CCG) %>% select(PCode)
        }else
          {
            req(input$Practice_output)
            if(is.null(input$Practice_output) | input$Practice_output == "All")
            {
              print("doing no specific practice")
              filter(PCNdefs,PCN == input$PCN_output) %>% select(PCode)
            }else
            {
              print("doing specific practice")
              filter(PCNdefs,Practice.Name == input$Practice_output) %>% select(PCode)
            }
        }
      }
})
  
  filteredlsoapops <- reactive({
    print("now in filteredlsoapops")
    print(nrow(filteredPracs()))
    if(nrow(filteredPracs()) == 0) {
      anti_join(lsoapops,PCNdefs,by=c('PRACTICE_CODE' = 'PCode')) %>% right_join(centers,by=c('LSOA_CODE' = 'lsoacode')) %>% group_by(LSOA_CODE) %>% summarise(patients = sum(Number.of.Patients)) %>% filter(!is.na(patients)) %>% left_join(centers, by = c('LSOA_CODE' = 'lsoacode')) # count patients in each of our lsoas whose GP is NOT in PCNdefs (i.e. NOT in BwD or ELancs CCGs)
    }
    else {
      semi_join(lsoapops,filteredPracs(),by=c('PRACTICE_CODE' = 'PCode')) %>% right_join(centers,by=c('LSOA_CODE' = 'lsoacode')) %>% group_by(LSOA_CODE) %>% summarise(patients = sum(Number.of.Patients)) %>% filter(!is.na(patients)) %>% left_join(centers, by = c("LSOA_CODE" = "lsoacode")) # count patients in our lsoas who belong to the chosen practices
    }
  })
  
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
               attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, 
               <a href="http://cartodb.com/attributions">CartoDB</a> | 
               <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2018)</a>',
               group = "CartoDB",
               options = providerTileOptions(minZoom = 10, maxZoom = 15)) %>% 
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2018)</a>',
               group = "OpenStreetMap",
               options = providerTileOptions(minZoom = 10, maxZoom = 15)) %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", 
               attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, 
                    Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community | 
                    <a href="https://www.ons.gov.uk/methodology/geography/licences"> Contains OS data © Crown copyright and database right (2018)</a>', 
               group = "Satellite",
               options = providerTileOptions(minZoom = 10, maxZoom = 15)) %>%
      addTiles(urlTemplate = "", 
               attribution = '<a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2018)</a>',
               group = "No background") %>% 
      setView(-2.22405, 53.7893, zoom = 10) %>% # coordinates of Burnley
      addLayersControl(position = 'topleft',
                       baseGroups = c("CartoDB", "OpenStreetMap", "Satellite", "No background"),
#                       overlayGroups = c("Blackburn with Darwen only","Registered Pops by LSOA"),
                       overlayGroups = c("Blackburn with Darwen only"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("Blackburn with Darwen only") %>%
#      hideGroup("Registered Pops by LSOA") %>%
      onRender( # from https://stackoverflow.com/questions/46132742/coordinates-of-current-mouse-position-on-leaflet-map-with-shiny
        "function(el,x){
        this.on('mousemove', function(e) {
        var lat = e.latlng.lat;
        var lng = e.latlng.lng;
        var coord = [lat, lng];
        Shiny.onInputChange('hover_coordinates', coord)
        });
        this.on('mouseout', function(e) {
        Shiny.onInputChange('hover_coordinates', null)
        })
  }"
            )
  })
  
  output$out <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
       pt <- SpatialPoints(cbind(input$hover_coordinates[2],input$hover_coordinates[1]))
       proj4string(pt) <- CRS("+init=epsg:4326")
       
       # report District if cursor is in one, plus other chosen overlay layer if cursor is in one of those
       districtLine <- ""
       ccgLine <- ""
       nhoodLine <- ""
       wardLine <- ""
       reportDistrict <- !is.na(over(pt,laLayer)$lad16nm) 
       reportCCG <- input$mainBoundaries == 2 & !is.na(over(pt,ccgLayer)$ccg18nm) 
       reportNhood <- input$mainBoundaries == 3 & !is.na(over(pt,nhoodLayer)$NAME)
       reportWard <- input$mainBoundaries == 4 & !is.na(over(pt,wardLayer)$wardName)
       if(reportDistrict) {districtLine <- paste0("District: ",over(pt,laLayer)$lad16nm,"\n")}
       if(reportCCG) {ccgLine <- paste0("CCG: ",over(pt,ccgLayer)$ccg18nm,"\n")}
       if(reportNhood) {nhoodLine <- paste0("Neighbourhood: ",over(pt,nhoodLayer)$NAME,"\n")}
       if(reportWard) {wardLine <- paste0("Ward: ",over(pt,wardLayer)$wardName,"\n")}
       paste0(districtLine,ccgLine,nhoodLine,wardLine)
    }
  }) 
  
  observe({
    palDeciles <- colorFactor(c("#40004b", "#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#d9f0d3", 
          "#a6dba0", "#5aae61", "#1b7837", "#00441b"), domain = 1:10, ordered = TRUE)
    thisMeta <- filter(metadata, IndID %in% filteredData()$IndID)
    ShadeType <- thisMeta$ShadeType
    print(ShadeType)
    if(length(ShadeType) == 0) ShadeType <- "deciles" # avoids errors when initialising
    
    blobtext <- ifelse(input$showblobs,paste0("<h6 style='color:blue;text-align:center'>","Blue circles show registered population of<br/>",
                  ifelse(input$CCG == "Both","<strong>BwD and E Lancs CCGs combined</strong> ",
                  ifelse(input$CCG == "Other", "<strong> CCGs <em>other</em> than BwD and E Lancs</strong> ", 
                  ifelse(is.null(input$PCN_output) | input$PCN_output == "All",
                  ifelse(input$CCG == "BwD","<strong>Blackburn with Darwen CCG</strong> ","<strong>East Lancashire CCG</strong> "),
                  ifelse(is.null(input$Practice_output) | input$Practice_output == "All",
                  paste0("<strong>",input$PCN_output," PCN</strong> "),paste0("<strong>",input$Practice_output,"</strong> "))))),         
                  "<br/>living in shaded LSOAs.<br/><em>Click on circle for number of patients.</em></h6>"),
                  "")
    
    if(ShadeType == "deciles"){ ############################################# DECILES
  
      leafletProxy("map", data = subdistricts) %>% 
        clearShapes() %>% clearControls() %>% clearMarkers() %>% 
        addMapPane("circles_on_top", zIndex = 420) %>% 
        addPolygons(data = filteredData(), 
                    fillColor = ~palDeciles(value), 
                    fillOpacity = sliderValue(), weight = 0.7, opacity = 1, color = "#757575", layerId = ~polycode,
                    label = paste0(filteredData()$label,"<br/>") %>% lapply(HTML),
                    labelOptions = labelOptions(style=list('line-height' = 'normal','font-size' = '12px')),
                    
                    highlight = highlightOptions(color = "#FFFF00", weight = 3, bringToFront = TRUE,sendToBack = TRUE)) %>%
        
        # Changed to use addPolygons rather than addPolylines, so that I can subsequently detect what polygon the cursor is in. 
        # Need 'fill=FALSE' to make it still possible to click or hover on the LSOAs beneath 
        # (see https://stackoverflow.com/questions/41105342/clickable-overlying-layers-in-leaflet).
        
        # Always show the LA boundary in white:
        addPolygons(data = la, stroke = TRUE, weight = 8,color = "#ffffff",
                    opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
        
        # Also, show black boundary (i.e. stroke = TRUE) for the overlay that the user has chosen using the radio buttons.
        addPolygons(data = la, stroke = input$mainBoundaries == 1, weight = 2.5,color = "#212121",
                    opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
        addPolygons(data = ccg, stroke = input$mainBoundaries == 2, weight = 2.5,color = "#212121",
                    opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
        addPolygons(data = nhood, stroke = input$mainBoundaries == 3, weight = 2.5,color = "#212121",
                    opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
        addPolygons(data = ward, stroke = input$mainBoundaries == 4, weight = 2.5,color = "#212121",
                    opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
        
        # Show registered pop per LSOA if user has chosen to do so
#        addPolygons(data = lsoa,stroke = TRUE, weight = 1,opacity = 1,color = "red",dashArray = "5,5",fillOpacity = 0, fillColor = "transparent",fill = FALSE, group = "Registered Pops by LSOA") %>%
        # nb - now removing
        # , group = "Registered Pops by LSOA" 
        # from the next two statements
        addPolygons(data = lsoalayer,stroke = input$showblobs,weight = 0.3,opacity = 1,color = "blue",fillOpacity = 0, fillColor = "transparent",fill = FALSE) %>%
        addCircleMarkers(data = filteredlsoapops(),stroke = input$showblobs, fill = input$showblobs, lat = filteredlsoapops()$y,lng = filteredlsoapops()$x,weight = 2, radius = sqrt(filteredlsoapops()$patients), popup = paste0(as.character(filteredlsoapops()$patients)," patients <br/> (all ages)") %>% lapply(HTML), options = pathOptions(pane = "circles_on_top")) %>%
        
        # Crop to BwD if user has chosen to do so
        addPolygons(data = BwDmask,stroke = TRUE, weight = 2.5, color = "black",
                    opacity = 1,fillOpacity = 1, fillColor = "white",fill = TRUE,group = "Blackburn with Darwen only") %>%
        
#        addControl(tags$div(tag.map.title,ifelse(input$showblobs,paste0("Blue circles show registered population of ",
#                                                 
#                    ifelse(input$CCG == "Both","<b>BwD and E Lancs CCGs combined</b> ",
#                           
#                    ifelse(input$CCG == "Other", "<b> CCGs <em>other</em> than BwD and E Lancs</b> ", 
#                           
#                    ifelse(is.null(input$PCN_output) | input$PCN_output == "All",
#                          ifelse(input$CCG == "BwD","<b>Blackburn with Darwen CCG</b> ","<b>East Lancashire CCG</b> "),
#                    ifelse(is.null(input$Practice_output) | input$Practice_output == "All",
#                          paste0("<b>",input$PCN_output," PCN</b> "),paste0("<b>",input$Practice_output,"</b> "))))),         
#              "living in each LSOA of shaded area. <br/><em>Click on circle to see number of patients.</em>") %>% lapply(HTML),
#               "" %>% lapply(HTML))),position = "topleft",className = "map-title") %>% 
# see https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map
        
        addLegend(position = "bottomleft", 
                  colors = c("#40004b", "#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837", "#00441b"),
                  title = paste0("<a href='http://togetherahealthierfuture.org.uk/' target='_blank'><img src='HealthierPL.png' style='width: 200px; border: 0; display: block; margin: 0px auto;' alt='Together a Healthier Future' border='0'></a>","<br/>",
                  blobtext %>% lapply(HTML),"<br/>",
                  thisMeta$Legtitle) %>% lapply(HTML),
 #                 title = paste0(thisMeta$Legtitle,"<br/>") %>% lapply(HTML),
                  labels = c("1 (10% most deprived)", "2","3","4","5","6","7","8","9", "10 (10% least deprived)"), opacity = sliderValue()) 
      
 
    }else if(ShadeType == "diverge"){ ############################################# DIVERGING PALETTE

    rc1 <- colorRampPalette(colors = c(ifelse(thisMeta$HighBad == 1,"#00441b","#40004b"), "white"), 
                            space = "Lab")(thisMeta$DivergePoint-min(filteredData()$value,na.rm = TRUE))
    rc2 <- colorRampPalette(colors = c("white", ifelse(thisMeta$HighBad == 1,"#40004b","#00441b")), 
                            space = "Lab")(max(filteredData()$value,na.rm = TRUE)-thisMeta$DivergePoint)
    ## Combine the two color palettes
    rampcols <- c(rc1, rc2)
    palDiverge <- colorNumeric(palette = rampcols, domain = NULL, na.color = "#8B3E2F")
    
    leafletProxy("map", data = subdistricts) %>% 
      clearShapes() %>% clearControls() %>% clearMarkers() %>% 
      addPolygons(data = filteredData(), 
         fillColor = ~palDiverge(value), 
         fillOpacity = sliderValue(), weight = 0.7, opacity = 1, color = "#757575", layerId = ~polycode,
         label = paste0(filteredData()$label,"<br/>") %>% lapply(HTML),
         labelOptions = labelOptions(style=list('line-height' = 'normal','font-size' = '12px')),

    highlight = highlightOptions(color = "#FFFF00", weight = 3, bringToFront = TRUE,sendToBack = TRUE)) %>%

# Changed to use addPolygons rather than addPolylines, so that I can subsequently detect what polygon the cursor is in. 
# Need 'fill=FALSE' to make it still possible to click or hover on the LSOAs beneath 
# (see https://stackoverflow.com/questions/41105342/clickable-overlying-layers-in-leaflet).
      
# Always show the LA boundary in white:
    addPolygons(data = la, stroke = TRUE, weight = 8,color = "#ffffff",
                  opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
      
# Also, show black boundary (i.e. stroke = TRUE) for the overlay that the user has chosen using the radio buttons.
    addPolygons(data = la, stroke = input$mainBoundaries == 1, weight = 2.5,color = "#212121",
            opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
    addPolygons(data = ccg, stroke = input$mainBoundaries == 2, weight = 2.5,color = "#212121",
            opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
    addPolygons(data = nhood, stroke = input$mainBoundaries == 3, weight = 2.5,color = "#212121",
                  opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
    addPolygons(data = ward, stroke = input$mainBoundaries == 4, weight = 2.5,color = "#212121",
                  opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
      
      # Show registered pop per LSOA if user has chosen to do so
#      addPolygons(data = lsoa,stroke = TRUE, weight = 1,opacity = 1,color = "red",dashArray = "5,5",fillOpacity = 0, fillColor = "transparent",                      fill = FALSE, group = "Registered Pops by LSOA") %>%
      addPolygons(data = lsoalayer,stroke = input$showblobs,weight = 0.3,opacity = 1,color = "blue",fillOpacity = 0, fillColor = "transparent",fill = FALSE ) %>%
      addCircleMarkers(data = filteredlsoapops(),stroke = input$showblobs, fill = input$showblobs, lat = filteredlsoapops()$y,lng = filteredlsoapops()$x,weight = 2, radius = sqrt(filteredlsoapops()$patients), popup = paste0(as.character(filteredlsoapops()$patients)," patients <br/> (all ages)") %>% lapply(HTML), options = pathOptions(pane = "circles_on_top")) %>%
      # Crop to BwD if user has chosen to do so
    addPolygons(data = BwDmask,stroke = TRUE, weight = 2.5, color = "black",
                  opacity = 1,fillOpacity = 1, fillColor = "white",fill = TRUE,group = "Blackburn with Darwen only") %>%
      
    addLegend(position = "bottomleft", 
          pal = palDiverge,values = ~filteredData()$value, 
          title = paste0("<a href='http://togetherahealthierfuture.org.uk/' target='_blank'><img src='HealthierPL.png' style='width: 200px; border: 0; display: block; margin: 0px auto;' alt='Together a Healthier Future' border='0'></a>","<br/>",
                         blobtext %>% lapply(HTML),"<br/>",
                         thisMeta$Legtitle) %>% lapply(HTML),labFormat = labelFormat(prefix = ifelse(is.na(thisMeta$Prefix),"",unescape_html(thisMeta$Prefix)) ,suffix = ifelse(is.na(thisMeta$Suffix),"",thisMeta$Suffix)),
          opacity = sliderValue()) 
    
  }else if(ShadeType == "0toX"){ ############################################# PALETTE goes from 0 to some number X
    
      pal0to100 <- colorNumeric(
      palette = c("white", "#003366"),
      domain = c(0,thisMeta$TopRange))
    
    leafletProxy("map", data = subdistricts) %>% 
      clearShapes() %>% clearControls() %>% clearMarkers() %>% 
      addPolygons(data = filteredData(), 
                  fillColor = ~pal0to100(value), 
                  fillOpacity = sliderValue(), weight = 0.7, opacity = 1, color = "#757575", layerId = ~polycode,
                  label = paste0(filteredData()$label,"<br/>") %>% lapply(HTML),
                  labelOptions = labelOptions(style=list('line-height' = 'normal','font-size' = '12px')),
                  
                  highlight = highlightOptions(color = "#FFFF00", weight = 3, bringToFront = TRUE,sendToBack = TRUE)) %>%
      
      # Changed to use addPolygons rather than addPolylines, so that I can subsequently detect what polygon the cursor is in. 
      # Need 'fill=FALSE' to make it still possible to click or hover on the LSOAs beneath 
      # (see https://stackoverflow.com/questions/41105342/clickable-overlying-layers-in-leaflet).
      
      # Always show the LA boundary in white:
      addPolygons(data = la, stroke = TRUE, weight = 8,color = "#ffffff",
                  opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
      
      # Also, show black boundary (i.e. stroke = TRUE) for the overlay that the user has chosen using the radio buttons.
      addPolygons(data = la, stroke = input$mainBoundaries == 1, weight = 2.5,color = "#212121",
                  opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
      addPolygons(data = ccg, stroke = input$mainBoundaries == 2, weight = 2.5,color = "#212121",
                  opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
      addPolygons(data = nhood, stroke = input$mainBoundaries == 3, weight = 2.5,color = "#212121",
                  opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
      addPolygons(data = ward, stroke = input$mainBoundaries == 4, weight = 2.5,color = "#212121",
                  opacity = 1,fillOpacity = 0, fillColor = "transparent", fill = FALSE) %>%
      
      # Show registered pop per LSOA if user has chosen to do so
#      addPolygons(data = lsoa,stroke = TRUE, weight = 1,opacity = 1,color = "red",dashArray = "5,5",fillOpacity = 0, fillColor = "transparent",                      fill = FALSE, group = "Registered Pops by LSOA") %>%
      addPolygons(data = lsoalayer,stroke = input$showblobs,weight = 0.3,opacity = 1,color = "blue",fillOpacity = 0, fillColor = "transparent",fill = FALSE ) %>%
      addCircleMarkers(data = filteredlsoapops(),stroke = input$showblobs, fill = input$showblobs, lat = filteredlsoapops()$y,lng = filteredlsoapops()$x,weight = 2, radius = sqrt(filteredlsoapops()$patients), popup = paste0(as.character(filteredlsoapops()$patients)," patients <br/> (all ages)") %>% lapply(HTML), options = pathOptions(pane = "circles_on_top")) %>%
      # Crop to BwD if user has chosen to do so
      addPolygons(data = BwDmask,stroke = TRUE, weight = 2.5, color = "black",
                  opacity = 1,fillOpacity = 1, fillColor = "white",fill = TRUE,group = "Blackburn with Darwen only") %>%
      
      addLegend(position = "bottomleft", 
                pal = pal0to100,values = ~filteredData()$value,
                title = paste0("<a href='http://togetherahealthierfuture.org.uk/' target='_blank'><img src='HealthierPL.png' style='width: 200px; border: 0; display: block; margin: 0px auto;' alt='Together a Healthier Future' border='0'></a>","<br/>",
                               blobtext %>% lapply(HTML),"<br/>",
                               thisMeta$Legtitle) %>% lapply(HTML),labFormat = labelFormat(prefix = ifelse(is.na(thisMeta$Prefix),"",unescape_html(thisMeta$Prefix)),suffix = ifelse(is.na(thisMeta$Suffix),"",thisMeta$Suffix)),
                opacity = sliderValue()) 
  }
  })

toggleModal(session,"landingPage",toggle = "open")
  
} # end of SERVER

shinyApp(ui, server)