library(readxl)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(rgdal)
library(colorspace)
library(openxlsx)
library(data.table)
options(stringsAsFactors = FALSE)

# Set working directory and folder organization
root <- "~/Documents/HCA"
# setwd(root)
# # Import school network shapefile
high_networks <- readOGR(dsn = ".", layer = "geo_export_699d5441-6a61-43a9-8ccb-abada8b78f27")
pal <- rainbow_hcl(4)

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
CPS_high_schools = all_CPS[all_CPS$Grade_Cat == "HS", ]

# Import CPS school accessibility data
accessibility_by_school <- read_excel("20190604_CPS_Accessibility_List_(1).xlsx", range = "A8:E534")

# Import CPS ratings data
high_ratings <- read_excel("Accountability_SQRPratings_2019-2020_SchoolLevel (1).xls",
                           sheet ="High Schools (grds 9-12 only)",
                           skip=1,
                           col_names=TRUE)
high_ratings <- high_ratings[complete.cases(high_ratings$`School ID`), ]
keep_cols = c("School ID", "School Name", "Network", "SQRP Total Points Earned",
              "SY 2019-2020 SQRP Rating", "SY 2019-2020 Accountability Status")
high_ratings = high_ratings[keep_cols]

# Append combo schools and option schools to ratings dataframe
combo_ratings <- read_excel("Accountability_SQRPratings_2019-2020_SchoolLevel (1).xls",
                            sheet ="Combo Schools (grds PreK-12)",
                            skip=1,
                            col_names=TRUE)
combo_ratings <- combo_ratings[complete.cases(combo_ratings$`School ID`), ]
combo_ratings = combo_ratings[keep_cols]
high_ratings <- rbind(high_ratings, combo_ratings)

option_ratings <- read_excel("Accountability_SQRPratings_2019-2020_SchoolLevel (1).xls",
                             sheet ="Option Schools",
                             skip=1,
                             col_names=TRUE)
option_ratings <- option_ratings[complete.cases(option_ratings$`School ID`), ]
option_ratings = option_ratings[keep_cols]
high_ratings <- rbind(high_ratings, option_ratings)

# Merge dataframes together
all_schools_to_map = merge(all_CPS, accessibility_by_school, by.x = "School_Nm", by.y = "School Name",
                           all.x = TRUE, all.y = TRUE)
all_schools_to_map = merge(all_schools_to_map, high_ratings, by.x = "School_ID", by.y = "School ID",
                           all.x = TRUE, all.y = TRUE)
all_schools_to_map = all_schools_to_map[!is.na(all_schools_to_map$the_geom),]

# Divide schools by elementary school/high school (excluding one middle school for now)
high_schools = all_schools_to_map[all_schools_to_map$Grade_Cat == "HS",]

# Pull in old network (2014-2015)
ratings_old = read_excel("SY14_SQRP_Report_CPSEDU_FINAL_20151026.xlsx",
                         sheet ="High Schools (grds 9-12 only)",
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
high_schools = merge(high_schools, ratings_old, by.x = "School_ID", by.y = "School ID",
                     all.x=TRUE, all.y=FALSE)

# Re-categorize schools' quality level based on internet research
# https://cps.edu/Schools/Pages/school.aspx?SchoolID=400142
high_schools[high_schools$`School_Nm` == "YCCS - VIRTUAL HS", "SY 2019-2020 SQRP Rating"] = "Inability to Rate"
# https://schoolinfo.cps.edu/schoolprofile/SchoolDetails.aspx?SchoolId=400181
high_schools[high_schools$`School_Nm` == "ART IN MOTION", "SY 2019-2020 SQRP Rating"] = "Inability to Rate"
# https://en.wikipedia.org/wiki/Paul_Robeson_High_School_(Illinois)
high_schools <- high_schools[!(high_schools$`School_Nm` == "ROBESON HS"),]


# Re-categorize schools' accessibility level based on internet research
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/7270ADA.pdf
high_schools[high_schools$`School_Nm` == "BRONZEVILLE HS", "Summary Conclusion (as of today)"] = "First Floor Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/7770ADA.pdf
high_schools[high_schools$`School_Nm` == "CHICAGO ACADEMY HS", "Summary Conclusion (as of today)"] = "First Floor Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/7680ADA.pdf
high_schools[high_schools$`School_Nm` == "INFINITY HS", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/7630ADA.pdf
high_schools[high_schools$`School_Nm` == "MULTICULTURAL HS", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/9645ADA.pdf
high_schools[high_schools$`School_Nm` == "NOBLE - ACADEMY HS", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/9614ADA.pdf
high_schools[high_schools$`School_Nm` == "NOBLE - BAKER HS", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/9613ADA.pdf
high_schools[high_schools$`School_Nm` == "NOBLE - BUTLER HS", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/1106ADA.pdf
high_schools[high_schools$`School_Nm` == "NORTH LAWNDALE - COLLINS HS", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/1962ADA.pdf
high_schools[high_schools$`School_Nm` == "PERSPECTIVES - TECH HS", "Summary Conclusion (as of today)"] = "Not Accessible"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/7140ADA.pdf
high_schools[high_schools$`School_Nm` == "RICKOVER MILITARY HS", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/7600ADA.pdf
high_schools[high_schools$`School_Nm` == "SOCIAL JUSTICE HS", "Summary Conclusion (as of today)"] = "Usable"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/7930ADA.pdf
high_schools[high_schools$`School_Nm` == "SPRY HS", "Summary Conclusion (as of today)"] = "Not Accessible"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/3061ADA.pdf
high_schools[high_schools$`School_Nm` == "U OF C - WOODLAWN HS", "Summary Conclusion (as of today)"] = "Not Accessible"
# https://schoolreports.cps.edu/Accessibility_PDF_Summary_2015/8027ADA.pdf
high_schools[high_schools$`School_Nm` == "URBAN PREP - BRONZEVILLE HS", "Summary Conclusion (as of today)"] = "Not Accessible"

# Revise network variable for maps
high_schools$Network_raw = high_schools$Network
high_schools$Network <- with(high_schools, ifelse(Network_raw == "ISP", as.character(Network_old),
                                                  as.character(Network)))

# For now, keep only CPS non-charter schools
high_schools$networknum = as.numeric(gsub("[^0-9]", "", x=high_schools$Network))
high_schools <- high_schools %>% filter(!is.na(networknum) | Network == "ISP")

# For now, keep only CPS non-charter schools
high_schools <- high_schools[high_schools$`SY 2019-2020 SQRP Rating` != "Inability to Rate",]
high_schools$short_quality_rating = gsub("^Level ", "", high_schools$`SY 2019-2020 SQRP Rating`)

# Divide schools by accessibility
usable_schools = high_schools[high_schools$`Summary Conclusion (as of today)` == "Usable",]
ff_usable_schools = high_schools[high_schools$`Summary Conclusion (as of today)` == "First Floor Usable",]
not_accessible_schools = high_schools[high_schools$`Summary Conclusion (as of today)` == "Not Accessible",]

# Calculate # of schools by accessibility level in each network
# Aggregate by rating and accessibility level (and network)
high_schools$`Count of Schools` = 1
high_schools$`SY 2019-2020 SQRP Rating` <- addNA(high_schools$`SY 2019-2020 SQRP Rating`)
high_schools$`Summary Conclusion (as of today)` <- addNA(high_schools$`Summary Conclusion (as of today)`)
high_schools$Network <- addNA(high_schools$Network)
# Revise network variable for maps
high_schools$Network_raw = high_schools$Network

high_schools$`Count` = 1
high_schools$`SY 2019-2020 SQRP Rating` <- addNA(high_schools$`SY 2019-2020 SQRP Rating`)
high_schools$`Summary Conclusion (as of today)` <- addNA(high_schools$`Summary Conclusion (as of today)`)
high_accessibility_by_network = aggregate(`Count` ~ `Summary Conclusion (as of today)` +
                                            # Network,
                                            networknum,
                                          data = high_schools,
                                          FUN=sum, na.rm=FALSE, na.action=na.pass)
high_accessibility_by_network_wide <- reshape(high_accessibility_by_network, idvar = "networknum",
                                              timevar = "Summary Conclusion (as of today)",
                                              direction = "wide")
high_accessibility_by_network_wide[is.na(high_accessibility_by_network_wide)] <- 0
high_ratings_by_network = aggregate(`Count` ~ `SY 2019-2020 SQRP Rating` +
                                      # Network,
                                      networknum,
                                    data = high_schools,
                                    FUN=sum, na.rm=FALSE, na.action=na.pass)
high_ratings_by_network_wide <- reshape(high_ratings_by_network, idvar = "networknum",
                                        timevar = "SY 2019-2020 SQRP Rating",
                                        direction = "wide")
high_ratings_by_network_wide[is.na(high_ratings_by_network_wide)] <- 0

# Merge quality and accessility ratings into network shapfile
high_networks <- merge(high_networks, high_accessibility_by_network_wide, by='networknum')
high_networks <- merge(high_networks, high_ratings_by_network_wide, by='networknum')


pal_df = data.frame(pal)
pal_df$ord = high_networks@data$networknum
networks_id_df = data.frame(high_networks@data[["networknum"]])
networks_id_df$polygon_id = row.names(networks_id_df)
names(networks_id_df) <- c("networknum", "polygon_id")

# Create map
rm(map_v1)
map_v1 <- leaflet() %>%
  # Base map
  addTiles() %>%
  
  # Set zoom to center of Chicago
  setView(-87.629710, 41.878289, zoom = 11) %>%
  
  addPolygons(data = high_networks,
              fillColor = ~pal,
              fillOpacity = 0.6, 
              weight = 1,
              label = high_networks$networknam,
              popup = paste0(high_networks$networknam, "<br>",
                             "# Level 1+ Schools: ", high_networks$`Count.Level 1+`, "<br>",
                             "# Level 1 Schools: ", high_networks$`Count.Level 1`, "<br>",
                             "# Level 2+ Schools: ", high_networks$`Count.Level 2+`, "<br>",
                             "# Level 2 Schools: ", high_networks$`Count.Level 2`, "<br>",
                             "# Level 3 Schools: ", high_networks$`Count.Level 3`, "<br>"),
              labelOptions = labelOptions(noHide = T, style=list("color" = "blue",
                                                                 "font-family" = "serif",
                                                                 "font-style" = "italic",
                                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                 "font-size" = "15px",
                                                                 "border-color" = "rgba(0,0,0,0.5)"),
              opacity = 0.7),
              group = "High School Networks") %>%
  
  # Add footnote
  addControl(paste0("CPS’s specialty high schools exclusively serving for students with disabilities do not have school ratings.",
                    "<br>", "Still, Graham HS, one of the four specialty high schools, is inaccessible."),
             position = "bottomleft") %>%
  
  # Add scale bar
  addScaleBar(position = "bottomleft", scaleBarOptions(metric = FALSE)) %>%
  addLegend(position = "bottomright",
            colors = pal_df[order(pal_df$ord), "pal"],
            labels = high_networks@data[order(high_networks@data$networknum), "networknam"],
            title = "Network Legend",
            layerId = "network_legend") %>%
  addLegend(position = "bottomleft",
            colors = c("green", "orange", "red"),
            labels = c("Usable", "First Floor Usable", "Not Accessible"),
            title = "Accessibility Legend")

map_v1

ui <- navbarPage(
  "CPS Accessibility Maps",
  tabPanel("High Schools",
           div(class="outer",
               tags$head(
                 # Custom CSS used from R Shiny Superzip example online: https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R
                 includeCSS("styles.css")
               ),
               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leafletOutput("mymap", width="100%", height="100%"),
                             
          title = "CPS Accessibility and Ratings Map: Elementary Schools, No Charters",
          absolutePanel(top = 10, right = 0, left = "auto", bottom = "auto", height = "auto",
                        selectInput(inputId = "network_zoom",
                                    label = strong("Zoom to Network"),
                                    choices = c("Choose a Network", 14:17)),
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
        c(high_networks@polygons[[as.numeric(
          networks_id_df[networks_id_df$networknum == as.numeric(input$network_zoom), "polygon_id"])
          ]]@labpt, 12)
    }
  })
  
  output$mymap <- renderLeaflet({ map_v1 %>% 
      
      setView(lng =  center()[1], lat = center()[2], zoom = center()[3]) %>%
      addAwesomeMarkers(data = filtered_usable_schools(),
                        lng = filtered_usable_schools()$X, lat = filtered_usable_schools()$Y,
                        popup = paste0(filtered_usable_schools()$`School_Nm`, "<br>",
                                       filtered_usable_schools()$`Sch_Addr`, "<br>",
                                       filtered_usable_schools()$Network_raw),
                        icon = makeAwesomeIcon(text=filtered_usable_schools()$short_quality_rating,
                                               markerColor = 'green', library='fa', iconColor = 'black')) %>%
      addAwesomeMarkers(data = filtered_ff_usable_schools(),
                        lng = filtered_ff_usable_schools()$X, lat = filtered_ff_usable_schools()$Y,
                        popup = paste0(filtered_ff_usable_schools()$`School_Nm`, "<br>",
                                       filtered_ff_usable_schools()$`Sch_Addr`, "<br>",
                                       filtered_ff_usable_schools()$Network_raw),
                        icon = makeAwesomeIcon(text=filtered_ff_usable_schools()$short_quality_rating,
                                               markerColor = 'orange', library='fa', iconColor = 'black')) %>%
      addAwesomeMarkers(data = filtered_not_accessible_schools(),
                        lng = filtered_not_accessible_schools()$X, lat = filtered_not_accessible_schools()$Y,
                        popup = paste0(filtered_not_accessible_schools()$`School_Nm`, "<br>",
                                       filtered_not_accessible_schools()$`Sch_Addr`, "<br>",
                                       filtered_not_accessible_schools()$Network_raw),
                        icon = makeAwesomeIcon(text=filtered_not_accessible_schools()$short_quality_rating,
                                               markerColor = 'red', library='fa', iconColor = 'black'))
  })
}

shinyApp(ui, server)
