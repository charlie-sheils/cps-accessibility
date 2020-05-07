library(readxl)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(rgdal)
library(colorspace)
library(openxlsx)
library(shiny)
library(data.table)
library(shinydashboard)
options(stringsAsFactors = FALSE)

# Set working directory and folder organization
root <- "~/Documents/HCA"
# setwd(root)

# Import school network shapefile
elem_networks <- readOGR(dsn = ".", layer = "geo_export_1b8baa06-7cd3-4db5-9678-6f7da9dc4d5b")
pal2 <- rainbow_hcl(13)


# Import CPS school location data with school names, addresses, and coordinates
all_CPS <- read.csv("Chicago_Public_Schools_-_School_Locations_SY1920.csv")
# Revise certain high school names to match accessibility data
all_CPS[all_CPS$`School_Nm` == "CAMELOT - EXCEL SOUTH SHORE HS", "School_Nm"] = "CAMELOT - EXCEL SOUTHSHORE HS"
all_CPS[all_CPS$`School_Nm` == "CHIARTS HS", "School_Nm"] = "CHICAGO ARTS HS"
all_CPS[all_CPS$`School_Nm` == "HOPE LEARNING ACADEMY", "School_Nm"] = "HOPE INSTITUTE"
all_CPS[all_CPS$`School_Nm` == "KIPP - ASCEND", "School_Nm"] = "KIPP CHICAGO - ASCEND PRIMARY"
all_CPS[all_CPS$`School_Nm` == "AVONDALE-LOGANDALE", "School_Nm"] = "LOGANDALE"
all_CPS[all_CPS$`School_Nm` == "ENGLEWOOD STEM HS", "School_Nm"] = "ROBESON HS"
all_CPS[all_CPS$`School_Nm` == "SADLOWSKI", "School_Nm"] = "SOUTHEAST"
all_CPS[all_CPS$`School_Nm` == "URBAN PREP - ENGLEWOOD HS", "School_Nm"] = "TEAM HS"
all_CPS[all_CPS$`School_Nm` == "AUSTIN CCA HS", "School_Nm"] = "VOISE HS"
CPS_elem_schools = all_CPS[all_CPS$Grade_Cat == "ES",]

# Import CPS school accessibility data
accessibility_by_school <- read_excel("20190604_CPS_Accessibility_List_(1).xlsx", range = "A8:E534")

# Import CPS ratings data, 2019-2020
elem_ratings <- read_excel("Accountability_SQRPratings_2019-2020_SchoolLevel (1).xls",
                           sheet ="Elem Schools (grds PreK-8 only)",
                           skip=1,
                           col_names=TRUE)
elem_ratings <- elem_ratings[complete.cases(elem_ratings$`School ID`), ]
keep_cols = c("School ID", "School Name", "Network", "SQRP Total Points Earned",
              "SY 2019-2020 SQRP Rating", "SY 2019-2020 Accountability Status")
elem_ratings = elem_ratings[keep_cols]

# Append combo schools and option schools to ratings dataframe
combo_ratings <- read_excel("Accountability_SQRPratings_2019-2020_SchoolLevel (1).xls",
                            sheet ="Combo Schools (grds PreK-12)",
                            skip=1,
                            col_names=TRUE)
combo_ratings <- combo_ratings[complete.cases(combo_ratings$`School ID`), ]
combo_ratings = combo_ratings[keep_cols]
elem_ratings <- rbind(elem_ratings, combo_ratings)

option_ratings <- read_excel("Accountability_SQRPratings_2019-2020_SchoolLevel (1).xls",
                             sheet ="Option Schools",
                             skip=1,
                             col_names=TRUE)
option_ratings <- option_ratings[complete.cases(option_ratings$`School ID`), ]
option_ratings = option_ratings[keep_cols]
elem_ratings <- rbind(elem_ratings, option_ratings)

# Merge dataframes together
all_schools_to_map = merge(all_CPS, accessibility_by_school, by.x = "School_Nm", by.y = "School Name",
                           all.x = TRUE, all.y = TRUE)
all_schools_to_map = merge(all_schools_to_map, elem_ratings, by.x = "School_ID", by.y = "School ID",
                           all.x = TRUE, all.y = TRUE)

# Divide schools by elementary school/high school (excluding one middle school for now)
elem_schools = all_schools_to_map %>% filter(Grade_Cat == "ES")

# Pull in old network (2014-2015)
ratings_old = read_excel("SY14_SQRP_Report_CPSEDU_FINAL_20151026.xlsx",
                         sheet ="Elem Schools (grds PreK-8 only)",
                         # skip=1,
                         col_names=TRUE)
ratings_old <- ratings_old[complete.cases(ratings_old$`School ID`), ]
keep_cols = c("School ID", "School Name", "Network")
ratings_old = ratings_old[keep_cols]

# Append combo schools and option schools to ratings dataframe
combo_ratings_old <- read_excel("SY14_SQRP_Report_CPSEDU_FINAL_20151026.xlsx",
                                sheet ="Combo Schools (grds PreK-12)",
                                # skip=1,
                                col_names=TRUE)
combo_ratings_old <- combo_ratings_old[complete.cases(combo_ratings_old$`School ID`), ]
combo_ratings_old = combo_ratings_old[keep_cols]
ratings_old <- rbind(ratings_old, combo_ratings_old)

option_ratings_old <- read_excel("SY14_SQRP_Report_CPSEDU_FINAL_20151026.xlsx",
                                 sheet ="Option Schools",
                                 # skip=1,
                                 col_names=TRUE)
option_ratings_old <- option_ratings_old[complete.cases(option_ratings_old$`School ID`), ]
option_ratings_old = option_ratings_old[keep_cols]
ratings_old <- rbind(ratings_old, option_ratings_old)
ratings_old <- rename(ratings_old, `Network_old` = `Network`)

# Merge in old ratings
elem_schools = merge(elem_schools, ratings_old, by.x = "School_ID", by.y = "School ID",
                     all.x=TRUE, all.y=FALSE)

# Re-categorize schools' quality level based on internet research
elem_schools[elem_schools$`School_Nm` == "ALCOTT ES", "SY 2019-2020 SQRP Rating"] = "Level 1"
# https://schoolinfo.cps.edu/schoolprofile/SchoolDetails.aspx?SchoolId=609774
elem_schools[elem_schools$`School_Nm` == "OGDEN ES", "SY 2019-2020 SQRP Rating"] = "Level 1"
# https://schoolinfo.cps.edu/schoolprofile/schooldetails.aspx?SchoolId=610101
elem_schools[elem_schools$`School_Nm` == "DISNEY II ES", "SY 2019-2020 SQRP Rating"] = "Level 1"
# https://schoolinfo.cps.edu/schoolprofile/SchoolDetails.aspx?SchoolId=610515
elem_schools[elem_schools$`School_Nm` == "CAMELOT - SAFE ES", "SY 2019-2020 SQRP Rating"] = "Level 1+"
# https://schoolinfo.cps.edu/schoolprofile/SchoolDetails.aspx?SchoolId=610572

# Re-categorize schools' accessibility level based on internet research
elem_schools[elem_schools$`School_Nm` == "ALBANY PARK", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/6290ADA.pdf
elem_schools[elem_schools$`School_Nm` == "FRAZIER CHARTER", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/6650ADA.pdf
elem_schools[elem_schools$`School_Nm` == "KINZIE", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/4330ADA.pdf
elem_schools[elem_schools$`School_Nm` == "KIPP - ACADEMY", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/9605ADA.pdf
elem_schools[elem_schools$`School_Nm` == "KIPP - BLOOM", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/9627ADA.pdf
elem_schools[elem_schools$`School_Nm` == "LEARN - SOUTH CHICAGO", "Summary Conclusion (as of today)"] = "Not Accessible"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/8029ADA.pdf
elem_schools[elem_schools$`School_Nm` == "LEGACY", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/5870ADA.pdf
elem_schools[elem_schools$`School_Nm` == "NORTHWEST", "Summary Conclusion (as of today)"] = "First Floor Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/4600ADA.pdf
elem_schools[elem_schools$`School_Nm` == "TELPOCHCALLI", "Summary Conclusion (as of today)"] = "Not Accessible"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/3380ADA.pdf
elem_schools[elem_schools$`School_Nm` == "U OF C - NKO", "Summary Conclusion (as of today)"] = "Not Accessible"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/3060ADA.pdf


# Revise network variable for maps
elem_schools$Network_raw = elem_schools$Network
elem_schools$Network <- with(elem_schools, ifelse(Network_raw == "ISP", as.character(Network_old),
                                                  as.character(Network)))
elem_schools$Network <- with(elem_schools, ifelse(School_Nm == "CARNEGIE", "Network 9",
                                                  as.character(Network)))

# For now, keep only CPS non-charter schools
elem_schools$network_nu = as.numeric(gsub("[^0-9]", "", x=elem_schools$Network))
elem_schools <- elem_schools %>% filter(!is.na(network_nu))
elem_schools <- elem_schools[elem_schools$`SY 2019-2020 SQRP Rating` != "Inability to Rate",]
elem_schools$short_quality_rating = gsub("^Level ", "", elem_schools$`SY 2019-2020 SQRP Rating`)

# NEW divide schools by accessibility
usable_schools = elem_schools[elem_schools$`Summary Conclusion (as of today)` == "Usable",]
ff_usable_schools = elem_schools[elem_schools$`Summary Conclusion (as of today)` == "First Floor Usable",]
not_accessible_schools = elem_schools[elem_schools$`Summary Conclusion (as of today)` == "Not Accessible",]

# Calculate # of schools by accessibility level in each network
# Aggregate by rating and accessibility level (and network)
elem_schools$`Count` = 1
elem_schools$`SY 2019-2020 SQRP Rating` <- addNA(elem_schools$`SY 2019-2020 SQRP Rating`)
elem_schools$`Summary Conclusion (as of today)` <- addNA(elem_schools$`Summary Conclusion (as of today)`)
elem_accessibility_by_network = aggregate(`Count` ~ `Summary Conclusion (as of today)` +
                                            network_nu,
                                          data = elem_schools,
                                          FUN=sum, na.rm=FALSE, na.action=na.pass)
elem_accessibility_by_network_wide <- reshape(elem_accessibility_by_network, idvar = "network_nu",
                                              timevar = "Summary Conclusion (as of today)",
                                              direction = "wide")
elem_accessibility_by_network_wide[is.na(elem_accessibility_by_network_wide)] <- 0
elem_ratings_by_network = aggregate(`Count` ~ `SY 2019-2020 SQRP Rating` +
                                      network_nu,
                                    data = elem_schools,
                                    FUN=sum, na.rm=FALSE, na.action=na.pass)
elem_ratings_by_network_wide <- reshape(elem_ratings_by_network, idvar = "network_nu",
                                        timevar = "SY 2019-2020 SQRP Rating",
                                        direction = "wide")
elem_ratings_by_network_wide[is.na(elem_ratings_by_network_wide)] <- 0

# Merge quality and accessility ratings into network shapfile
elem_networks <- merge(elem_networks, elem_accessibility_by_network_wide, by='network_nu')
elem_networks <- merge(elem_networks, elem_ratings_by_network_wide, by='network_nu')

pal2_df = data.frame(pal2)
pal2_df$ord = elem_networks@data$network_nu
networks_id_df = data.frame(elem_networks@data[["network_nu"]])
networks_id_df$polygon_id = row.names(networks_id_df)
names(networks_id_df) <- c("network_nu", "polygon_id")

# Export ISP schools to Excel
# ISP_schools = elem_schools[elem_schools$Network_raw == "ISP", ]
# ISP_df = elem_schools
# ISP_df$ISP = ifelse(elem_schools$Network_raw == "ISP", "ISP", "Not ISP")
# 
# ISP_summary_accessibility_by_network = aggregate(`Count` ~ `Summary Conclusion (as of today)` +
#                                                    network_nu + ISP,
#                                                  data = ISP_df,
#                                                  FUN=sum, na.rm=FALSE, na.action=na.pass)
# ISP_summary_accessibility_by_network_wide = reshape(ISP_summary_accessibility_by_network,
#                                                  idvar = c("network_nu", "ISP"),
#                                                  timevar = "Summary Conclusion (as of today)",
#                                                  direction = "wide")
# ISP_summary_accessibility_by_network_wide[is.na(
#   ISP_summary_accessibility_by_network_wide$Count.Usable), "Count.Usable"] <- 0
# ISP_summary_accessibility_by_network_wide[is.na(
#   ISP_summary_accessibility_by_network_wide$`Count.First Floor Usable`), "Count.First Floor Usable"] <- 0
# ISP_summary_accessibility_by_network_wide[is.na(
#   ISP_summary_accessibility_by_network_wide$`Count.Not Accessible`), "Count.Not Accessible"] <- 0
# ISP_summary_accessibility_by_network_wide <- ISP_summary_accessibility_by_network_wide[with(ISP_summary_accessibility_by_network_wide,
#   order(network_nu, rev(ISP))),]
# wb <- createWorkbook()
# addWorksheet(wb, "ES ISP by Network_cs")
# writeDataTable(wb, sheet="ES ISP by Network_cs",
#                x=ISP_summary_accessibility_by_network_wide,
#                startRow=5, colNames=TRUE, keepNA=TRUE, na.string="--")
# saveWorkbook(wb, file="ISP Comparison Tables 4.7.2020 Elementary Schools.xlsx", overwrite=TRUE)


# Create map
rm(map_v1)
map_v1 <- leaflet() %>%
  # Base map
  addTiles() %>%
  
  # Set zoom to center of Chicago
  setView(-87.629710, 41.878289, zoom = 11) %>%
  
  # addPolygons(data = elem_networks,
  addPolygons(data = elem_networks,
              fillColor = ~pal2,
              fillOpacity = 0.6, 
              weight = 1,
              # color = "#BDBDC3",
              label = paste0("Network ", elem_networks$network_nu),
              popup = paste0("Planning Zone: ", elem_networks$planning_z, " (Network ", elem_networks$network_nu, ") ", "<br>",
                             "# Level 1+ Schools: ", elem_networks$`Count.Level 1+`, "<br>",
                             "# Level 1 Schools: ", elem_networks$`Count.Level 1`, "<br>",
                             "# Level 2+ Schools: ", elem_networks$`Count.Level 2+`, "<br>",
                             "# Level 2 Schools: ", elem_networks$`Count.Level 2`, "<br>",
                             "# Level 3 Schools: ", elem_networks$`Count.Level 3`),
                             # "# Schools Inability to Rate: ", elem_networks$`Count.Inability to Rate`),
              labelOptions = labelOptions(noHide = T, style=list("color" = "blue",
                                                                 "font-family" = "serif",
                                                                 "font-style" = "italic",
                                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                 "font-size" = "15px",
                                                                 "border-color" = "rgba(0,0,0,0.5)"),
                                          opacity = 0.7),
              group = "Elementary School Planning Zones") %>%
  
  # Add scale bar
  addScaleBar(position = "bottomleft", scaleBarOptions(metric = FALSE)) %>%
  addLegend(position = "bottomright",
            colors = pal2_df[order(pal2_df$ord), "pal2"],
            labels = paste(elem_networks@data[order(elem_networks@data$network_nu), "network_nu"], '-',
                           elem_networks@data[order(elem_networks@data$network_nu), "planning_z"]),
            title = "Network Legend",
            layerId = "network_legend") %>%

  addLegend(position = "bottomleft",
            colors = c("green", "orange", "red"),
            labels = c("Usable", "First Floor Usable", "Not Accessible"),
            title = "Accessibility Legend")

map_v1


ui <- navbarPage(
  "CPS Accessibility Maps",
  tabPanel("Elementary Schools",
           div(class="outer",
               tags$head(
                 # Custom CSS used from R Shiny Superzip example online: https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R
                 includeCSS("styles.css")
               ),
               leafletOutput("mymap", width="100%", height="100%"),
                             
          title = "CPS Accessibility and Ratings Map: Elementary Schools, No Charters",
          absolutePanel(top = 10, right = 0, left = "auto", bottom = "auto", height = "auto",
                        selectInput(inputId = "network_zoom",
                                    label = strong("Zoom to Network"),
                                    choices = c("Choose a Network", 1:13)),
                        checkboxGroupInput(inputId = "quality_level",
                                           label = strong("Quality Level"),
                                           choices = list("Level 1+", "Level 1", "Level 2+", "Level 2", "Level 3"),
                                           selected = list("Level 1+", "Level 1", "Level 2+", "Level 2", "Level 3")),
                        checkboxGroupInput(inputId = "access_level",
                                           label = strong("Accessibility Level"),
                                           choices = list("Usable", "First Floor Usable", "Not Accessible"),
                                           selected = list("Usable", "First Floor Usable", "Not Accessible")),
                        checkboxGroupInput(inputId = "ISP_nonISP",
                                           label = strong("Schools to Include"),
                                           choices = list("ISP (Independent School Principals) Schools", "Non-ISP Schools"),
                                           selected = list("ISP (Independent School Principals) Schools", "Non-ISP Schools"))
                       )
            )
  )
)


server <- function(input, output, session) {
  interactive_filter <- . %>% filter(`SY 2019-2020 SQRP Rating` %in% input$quality_level,
                                     `Summary Conclusion (as of today)` %in% input$access_level,
                                     # {if (!input$ISP_include) {Network_raw != "ISP"} else TRUE},
                                     {if (!("ISP (Independent School Principals) Schools" %in% input$ISP_nonISP))
                                       {Network_raw != "ISP"} else TRUE},
                                     {if (!("Non-ISP Schools" %in% input$ISP_nonISP)) {Network_raw == "ISP"} else TRUE})
  
  filtered_usable_schools <- reactive({usable_schools %>% interactive_filter})
  filtered_ff_usable_schools <- reactive({ff_usable_schools %>% interactive_filter})
  filtered_not_accessible_schools <- reactive({not_accessible_schools %>% interactive_filter})
  
  center <- reactive({
    if (input$network_zoom == "Choose a Network") {
      c(-87.629710, 41.878289, 11)
    }
    else {
        c(elem_networks@polygons[[as.numeric(
          networks_id_df[networks_id_df$network_nu == as.numeric(input$network_zoom), "polygon_id"])
          ]]@labpt, 13)
    }
  })

  output$mymap <- renderLeaflet({ map_v1 %>% 
      
      
      setView(lng =  center()[1], lat = center()[2], zoom = center()[3]) %>%
      addAwesomeMarkers(data = filtered_usable_schools(),
                        lng = filtered_usable_schools()$X, lat = filtered_usable_schools()$Y,
                        popup = paste0(filtered_usable_schools()$`School_Nm`, "<br>",
                                       filtered_usable_schools()$`Sch_Addr`, "<br>",
                                       filtered_usable_schools()$Network),
                                       # filtered_usable_schools()$Network_raw),
                        icon = makeAwesomeIcon(text=filtered_usable_schools()$short_quality_rating,
                                               markerColor = 'green', library='fa', iconColor = 'black')) %>%
      addAwesomeMarkers(data = filtered_ff_usable_schools(),
                        lng = filtered_ff_usable_schools()$X, lat = filtered_ff_usable_schools()$Y,
                        popup = paste0(filtered_ff_usable_schools()$`School_Nm`, "<br>",
                                       filtered_ff_usable_schools()$`Sch_Addr`, "<br>",
                                       filtered_ff_usable_schools()$Network),
                                       # filtered_ff_usable_schools()$Network_raw),
                        icon = makeAwesomeIcon(text=filtered_ff_usable_schools()$short_quality_rating,
                                               markerColor = 'orange', library='fa', iconColor = 'black')) %>%
      addAwesomeMarkers(data = filtered_not_accessible_schools(),
                        lng = filtered_not_accessible_schools()$X, lat = filtered_not_accessible_schools()$Y,
                        popup = paste0(filtered_not_accessible_schools()$`School_Nm`, "<br>",
                                       filtered_not_accessible_schools()$`Sch_Addr`, "<br>",
                                       filtered_not_accessible_schools()$Network),
                                       # filtered_not_accessible_schools()$Network_raw),
                        icon = makeAwesomeIcon(text=filtered_not_accessible_schools()$short_quality_rating,
                                               markerColor = 'red', library='fa', iconColor = 'black'))
    })
}

shinyApp(ui, server)
