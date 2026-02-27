################################################################################
# Program Name: RShiny_Dashboard.R
# Program Purpose: Create R Shiny Dashboard with results from RTP CVD small area analysis

# Author: Katherine Burley Farr
# Contact: kburley@ad.unc.edu
# Affiliation: UNC Department of Public Policy, Data-Driven EnviroLab
################################################################################

rm(list = ls())

library(sf)
library(leaflet)
library(ggplot2)
library(ggpubr)
library(fresh)
library(shiny)
library(shinydashboard)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 1. BRING IN DATA ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

map_df <- st_read("Data/map_df.shp")
waterfall_df <- read.csv("Data/waterfall_df.csv")
demobar_df <- read.csv("Data/demobar_df.csv")
demobar_df$area[demobar_df$area=="All Research Triangle"] <- "All Research\n Triangle" # fix legend overlaps
demobar_allrtp <- read.csv("Data/demobar_allrtp.csv")

# Color Palettes
pal <- c("#3B9AB2","#78B7C5","#EBCC2A","#E1AF00","#F21A00")
pal2 <- c("#FF0000","#00A08A","#F2AD00","#F98400","#5BBCD6")

# green_cont_pal <- colorRampPalette(c('#84EFD8', '#034036'),100)

green_cont_pal <- colorRampPalette(c('#d2f9f1', "#00A08A", "#006E5F", '#003930'))(4) # colorRampPalette(c('#d2f9f1', "#00A08A", "#0B7E6D", "#006E5F", '#024F43', '#003930'))(6)

cont_pal <- colorNumeric(
  palette = green_cont_pal, # "inferno"
  domain = range(map_df$tot_an_rt)
)

factpal <- colorFactor(c(pal[5],pal2[2], "#fdae61", "#8AE3FE"), map_df$cluster)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2. CREATE FUNCTIONS ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# factor(group, levels=c(1,2,3,4), 
# labels=c("RT Summer Avg. Rate","Health Contribution","Heat Contribution", "CBG Attributable Rate"))
#### WATERFALL PLOT ####

# Decomposition Waterfall Selection Function
plot_waterfall <- function(data, cbg_geoid) {
  if(nrow(data)==0) {
    return(NULL)
  }
  
  ggplot() + 
    geom_rect(data=data[data$GEOID==cbg_geoid,], mapping=aes(x=id, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start,
                                                                                  colour=color_assignment, fill=color_assignment), alpha=0.75) +
    geom_text(data=data[data$GEOID==cbg_geoid,], aes(x=id,y=label_loc+1.5, label=round(amount,2)),size=6) + 
    geom_segment(data=data[data$GEOID==cbg_geoid & data$id!=4,], 
                 aes(x=id+.45, xend=id+.55, y=end, yend=end),linetype="twodash",linewidth=0.75) +
    scale_colour_manual(values=c("Average"="gray50",
                                 "CBG" = pal2[2],
                                 "Large Above" = pal[5],
                                 "Small Above" = "#FFA1A0",
                                 "Small Below" = "#81D1E9",
                                 "Large Below" = pal[1]),
                        aesthetics=c("colour","fill")) +
    ylim(c(0,24)) +
    theme_minimal() +
    # Note: must be set manually as "group" will plot in reverse if included as x in geom_text above
    scale_x_continuous(
      breaks = c(1,2,3,4),
      labels = c("RT Summer Avg. Rate","Heat Contribution","Health Contribution","CBG Attributable Rate")
    ) +
    theme(legend.position = "none",
          axis.text.x = element_text(size=12)) +
    labs(title = "Decomposition of CBG Attributable Burden Rate",
         # subtitle = paste("CBG ",cbg_geoid),
         y="",
         x="") 
}


# Default Waterfall Plot 
ggplot() +
  geom_rect(data=waterfall_df[waterfall_df$id==1,], mapping=aes(x=group, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start,
                                                           colour=color_assignment, fill=color_assignment), alpha=0.75) +
  geom_text(data=waterfall_df[waterfall_df$id==1,], aes(x=group,y=label_loc+1.5, label=round(amount,2)), size=6) +
  scale_colour_manual(values=c("Average"="gray50",
                               "CBG" = pal2[2],
                               "Large Above" = pal[5],
                               "Small Above" = "#FFA1A0", # lighten(pal[5],0.5)
                               "Small Below" = "#81D1E9", # lighten(pal[1],0.5)
                               "Large Below" = pal[1]),
                      aesthetics=c("colour","fill")) +
  ylim(c(0,24)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=12)) +
  labs(title = "RTP Average Heat-Attributable Hospitalizations per 10,000",
       subtitle = "Summer 2018",
       y="",
       x="")

#### RISK GROUP MAP ####

# Map Plot Function
plot_map <- function(data) {
  
  leaflet() %>%
    addTiles() %>%
    # addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    setView(lng = -78.8, lat = 35.84, zoom = 8) %>%
    addPolygons(data=data,

                # state border stroke color
                color = 'white',

                # soften the weight of the state borders
                weight = 1,

                # values >1 simplify the polygons' lines for less detail but faster loading
                smoothFactor = .3,

                # set opacity of polygons
                fillOpacity = .65,

                # specify that the each state should be colored per paletteNum()
                fillColor = ~factpal(cluster),
                
                # Popup
                label = ~content) %>% # issue is here
    addLegend("topleft", pal=factpal, values = data$cluster,
              title = "CBG Cluster",
              # labFormat = labelFormat(prefix = "$"),
              opacity = 1)
  
}

#### DEMOGRAPHICS BAR PLOT ####

plot_demobar <- function(data, cbg_geoid){
  if(nrow(data)==0) {
    return(NULL)
  }
  

  # SEX
  sex_comp <- ggplot(data[data$subgroup_type=="Sex" & data$GEOID %in% c("All Research Triangle",cbg_geoid),],
                     aes(x=var_label, y=pct_round, fill=area)) + #
    geom_bar(position="dodge",stat="identity",color="black") +
    geom_text(aes(label=format(pct_round, nsmall=1)), vjust=-0.25, position=position_dodge(width=0.9), size=4.5) +
    coord_cartesian(clip="off") +
    labs(y= "Percent", x = "Sex", fill="Geography") + # , fill = "CBG Attr. Rate Group"
    scale_y_continuous(limits=c(0,100), n.breaks=5) +
    scale_fill_manual(values = c("All Research\n Triangle"="gray50", "CBG"=pal2[2])) + # "Top 10%"=pal[5], "Bottom 90%"=lighten(pal2[5],0.5)
    theme_minimal() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 15),
          axis.text.x = element_text(size=12),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          legend.spacing.x = grid::unit(30, "pt"))

  # Race
  race_comp <- ggplot(data[data$subgroup_type=="Race" & data$var!="pct_native" & data$GEOID %in% c("All Research Triangle",cbg_geoid),],
                      aes(x=var_label, y=pct_round, fill=area)) + # exclude native bc not in model
    geom_bar(position="dodge",stat="identity",color="black") +
    geom_text(aes(label=format(pct_round, nsmall=1)), vjust=-0.25, position=position_dodge(width=0.9), size=4.5) +
    coord_cartesian(clip="off") +
    labs(y= "Percent", x = "Race", fill="Geography") +
    scale_y_continuous(limits=c(0,100), n.breaks=5) +
    scale_fill_manual(values = c("All Research\n Triangle"="gray50", "CBG"=pal2[2])) + # "Top 10%"=pal[5], "Bottom 90%"=lighten(pal2[5],0.5)
    theme_minimal() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 15),
          axis.text.x = element_text(size=12),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          legend.spacing.x = grid::unit(30, "pt"))

  # Age
  age_comp <- ggplot(data[data$subgroup_type=="Age" & data$GEOID %in% c("All Research Triangle",cbg_geoid),],
                     aes(x=var_label, y=pct_round, fill=area)) + 
    geom_bar(position="dodge",stat="identity",color="black") +
    geom_text(aes(label=format(pct_round, nsmall=1)), vjust=-0.25, position=position_dodge(width=0.9), size=4.5) +
    coord_cartesian(clip="off") +
    labs(y= "Percent", x = "Age", fill="Geography") +
    scale_y_continuous(limits=c(0,100), n.breaks=5) +
    scale_fill_manual(values = c("All Research\n Triangle"="gray50", "CBG"=pal2[2])) + # "Top 10%"=pal[5], "Bottom 90%"=lighten(pal2[5],0.5)
    theme_minimal() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 15),
          axis.text.x = element_text(size=12),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          legend.spacing.x = grid::unit(30, "pt"))
  
  # Poverty + Education
  other_comp <- ggplot(data[data$subgroup_type=="Poverty + Education" & data$GEOID %in% c("All Research Triangle",cbg_geoid),],
                       aes(x=var_label, y=pct_round, fill=area)) + #
    geom_bar(position="dodge",stat="identity",color="black") +
    geom_text(aes(label=format(pct_round, nsmall=1)), vjust=-0.25, position=position_dodge(width=0.9), size=4.5) +
    coord_cartesian(clip="off") +
    labs(y= "Percent", x = "Poverty + Education", fill="Geography") +
    scale_y_continuous(limits=c(0,100), n.breaks=5) +
    scale_fill_manual(values = c("All Research\n Triangle"="gray50", "CBG"=pal2[2])) + # "Top 10%"=pal[5], "Bottom 90%"=lighten(pal2[5],0.5)
    theme_minimal() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 15),
          axis.text.x = element_text(size=12),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          legend.spacing.x = grid::unit(30, "pt"))

  bar_comp <- ggarrange(sex_comp, race_comp, age_comp, other_comp,
                        nrow=2, ncol=2, common.legend = T) # sooo similar
  bar_comp
}


# Default Demographics Bar Plot
# SEX
sex_comp_default <- ggplot(demobar_allrtp[demobar_allrtp$subgroup_type=="Sex",],
                   aes(x=var_label, y=pct_round, fill=area)) + #
  geom_bar(position="dodge",stat="identity",color="black") +
  geom_text(aes(label=format(pct_round, nsmall=1)), vjust=-0.25, position=position_dodge(width=0.9), size=4.5) +
  coord_cartesian(clip="off") +
  labs(y= "Percent", x = "Sex", fill = "Geography") + # , fill = "CBG Attr. Rate Group"
  scale_y_continuous(limits=c(0,100), n.breaks=5) +
  scale_fill_manual(values = c("All Research Triangle"="gray50")) + # "Top 10%"=pal[5], "Bottom 90%"=lighten(pal2[5],0.5)
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11))

# Race
race_comp_default <- ggplot(demobar_allrtp[demobar_allrtp$subgroup_type=="Race" & demobar_allrtp$var!="pct_native",],
                    aes(x=var_label, y=pct_round, fill=area)) + # exclude native bc not in model
  geom_bar(position="dodge",stat="identity",color="black") +
  geom_text(aes(label=format(pct_round, nsmall=1)), vjust=-0.25, position=position_dodge(width=0.9), size=4.5) +
  coord_cartesian(clip="off") +
  labs(y= "Percent", x = "Race", fill = "Geography") +
  scale_y_continuous(limits=c(0,100), n.breaks=5) +
  scale_fill_manual(values = c("All Research Triangle"="gray50")) + # "Top 10%"=pal[5], "Bottom 90%"=lighten(pal2[5],0.5)
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=11))

# Age
age_comp_default <- ggplot(demobar_allrtp[demobar_allrtp$subgroup_type=="Age",],
                   aes(x=var_label, y=pct_round, fill=area)) + #
  geom_bar(position="dodge",stat="identity",color="black") +
  geom_text(aes(label=format(pct_round, nsmall=1)), vjust=-0.25, position=position_dodge(width=0.9), size=4.5) +
  coord_cartesian(clip="off") +
  labs(y= "Percent", x = "Age", fill = "Geography") +
  scale_y_continuous(limits=c(0,100), n.breaks=5) +
  scale_fill_manual(values = c("All Research Triangle"="gray50")) + # "Top 10%"=pal[5], "Bottom 90%"=lighten(pal2[5],0.5)
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11))

# Poverty + Education
other_comp_default <- ggplot(demobar_allrtp[demobar_allrtp$subgroup_type=="Poverty + Education",],
                     aes(x=var_label, y=pct_round, fill=area)) + #
  geom_bar(position="dodge",stat="identity",color="black") +
  geom_text(aes(label=format(pct_round, nsmall=1)), vjust=-0.25, position=position_dodge(width=0.9), size=4.5) +
  coord_cartesian(clip="off") +
  labs(y= "Percent", x = "Poverty + Education", fill = "Geography") +
  scale_y_continuous(limits=c(0,100), n.breaks=5) +
  scale_fill_manual(values = c("All Research Triangle"="gray50")) + # "Top 10%"=pal[5], "Bottom 90%"=lighten(pal2[5],0.5)
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(size=12),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11))

demobar_default <- ggarrange(sex_comp_default, race_comp_default, age_comp_default, other_comp_default,
                      nrow=2, ncol=2, common.legend = T) # sooo similar
rm(sex_comp_default, age_comp_default, race_comp_default, other_comp_default)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3. Set Up Dashboard ----
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### UI ####

my_theme = create_theme(
  adminlte_color(
    light_blue = "#78B7C5"
  )
)

# LAYOUT
ui<- dashboardPage(
  # Header
  dashboardHeader(disable=TRUE), # width doesn't work

  # # Sidebar
  dashboardSidebar(
    disable = TRUE
  ),
  # Body
  dashboardBody(

    use_theme(my_theme), # custom skin color defined in my_theme
    # tabItems(
    ######################### Map #
    # tabItem(tabName = "map",
    h2("CBG Heat-Related CVD Burdens in North Carolina's Research Triangle Area During Summer 2018"),
    # fluidPage(
    fluidRow(
      column(width = 6,
             # TESTING - input dropdown
             selectInput(
               inputId = "select_map",
               label = "Select Map Value",
               choices = c(
                 "Risk Groups",
                 "Heat-Attributable CVD Rates"
               ),
               selected = "Counts",
               multiple = FALSE
             ),
             # Map on Upper Left
             box(width=NULL, height=830,
                 leafletOutput("mymap",height=800),
                 status='primary',
                 headerBorder =FALSE)


      ),
      column(width=6,

             # Waterfall Plot Middle Right
             box(width=NULL, height=480, # height=290
                 htmlOutput('ui_waterfall'),
                 plotOutput('waterfall',height=260),
                 status='primary',
                 headerBorder =FALSE),
             
             # Demographics Bar Plot Bottom Right
             box(width=NULL, height=410,
                 title = "Demographic Characteristics of Selected Census Block Group",
                 plotOutput('demobar',height=340),
                 status='primary',
                 headerBorder =FALSE)
      )
    )
  )
)

# fluidRow within fluidPage - adds up to 12 so pick width to add up to 12

#### SERVER ####

server <- shinyServer(function(input, output) {
  
  #### initialize reactive values ####
  rvs <- reactiveValues(map=NULL, poly_cbg = map_df)
  
  rv_shape <- reactiveVal(FALSE)
  rv_location <- reactiveValues(id=NULL,lat=NULL,lng=NULL)
  rv_location_move_old <- reactiveValues(lat=NULL,lng=NULL)
  rv_text <- reactiveValues(click='Click on the map to see census block group information.')
  
  #### output ####
  
  ## output: leaflet map ## 
  output$mymap <- renderLeaflet({
    if (input$select_map == "Heat-Attributable CVD Rates") {
      # Rate Map
      leaflet() %>%
        addTiles() %>%
        # addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
        setView(lng = -78.8, lat = 35.84, zoom = 9) %>%
        addPolygons(data=map_df,
                    
                    # state border stroke color
                    color = 'white',
                    
                    # soften the weight of the state borders
                    weight = 1,
                    
                    # values >1 simplify the polygons' lines for less detail but faster loading
                    smoothFactor = .3,
                    
                    # set opacity of polygons
                    fillOpacity = .75,
                    
                    # specify that the each state should be colored per paletteNum()
                    fillColor = ~cont_pal(tot_an_rt),
                    
                    # Specify ID for reactive values to use
                    layerId = rvs$poly_cbg$GEOID,
                    
                    # popup - would like to add county and city here.
                    label = ~content) %>%
        addLegend("topleft", pal=cont_pal, values = map_df$tot_an_rt,
                  title = "CBG Heat-Attributable Hosp. per 10k",
                  opacity = 1)
    }
    else{
    # if (input$select_map == "Risk Groups") {
      # Risk Group Map
      leaflet() %>% 
        addTiles() %>% # color background map
        # addProviderTiles(providers$CartoDB.Positron) %>% # black and white background map
        setView(lng = -78.8, lat = 35.84, zoom = 8.5) %>%
        addPolygons(data=map_df,
                    
                    # state border stroke color
                    color = 'white',
                    
                    # soften the weight of the state borders
                    weight = 1,
                    
                    # values >1 simplify the polygons' lines for less detail but faster loading
                    smoothFactor = .3,
                    
                    # set opacity of polygons
                    fillOpacity = .7,
                    
                    # specify that the each state should be colored per paletteNum()
                    fillColor = ~factpal(cluster),
                    
                    # Specify ID for reactive values to use
                    layerId = rvs$poly_cbg$GEOID,
                    
                    # Popup
                    label = ~content) %>% 
        addLegend("topleft", pal=factpal, values = map_df$cluster,
                  title = "CBG Cluster",
                  opacity = 1)
      
    }
  })
  
  ## output: Print Information on CBG ## 
  
  # TEXT ABOVE WATERFALL PLOT
  output$ui_waterfall <- renderUI({
    location_info <- reactiveValuesToList(rv_location)
    if (!all(is.null(unlist(location_info)))){ # if any entry in rv_location is not NULL
      HTML(paste(h4(strong('Results for Census Block Group:',rv_location$id)),
                 
                 h4('Overall, the attributable burden rate for this CBG is',
                    rvs$poly_cbg$anrpct_txt[rvs$poly_cbg$GEOID==rv_location$id],
                    'which is ',
                    strong(rvs$poly_cbg$anr_text[rvs$poly_cbg$GEOID==rv_location$id]), 
                    ' than the Research Triangle average.'),
                 
                 h4('This census block group is in the ', 
                    strong(rvs$poly_cbg$cluster[rvs$poly_cbg$GEOID==rv_location$id]),
                    'Group, meaning that the Health Contribution is ', 
                    rvs$poly_cbg$hl_text[rvs$poly_cbg$GEOID==rv_location$id],
                    '(',
                    rvs$poly_cbg$hl_sign[rvs$poly_cbg$GEOID==rv_location$id],
                    '0) and the Heat-Contribution is ',
                    rvs$poly_cbg$ht_txt[rvs$poly_cbg$GEOID==rv_location$id],
                    '(',
                    rvs$poly_cbg$ht_sign[rvs$poly_cbg$GEOID==rv_location$id],
                    '0).',
                    'This indicates that',
                    strong(rvs$poly_cbg$rg_txt1[rvs$poly_cbg$GEOID==rv_location$id]),
                    rvs$poly_cbg$rg_txt2[rvs$poly_cbg$GEOID==rv_location$id],
                    'in this CBG.')
      ))

    }
    else{
      HTML(paste(h4('Click on the map to see census block group information.')))
    }
  })

  ## output: Waterfall Plot ##
  output$waterfall <- renderPlot ({
    # Before CBG is selected - just show area average
    if (!rv_shape()){
      ggplot() + 
        geom_rect(data=waterfall_df[waterfall_df$id==1,], mapping=aes(x=group, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start,
                                                                      colour=color_assignment, fill=color_assignment), alpha=0.75) +
        geom_text(data=waterfall_df[waterfall_df$id==1,], aes(x=group,y=label_loc+1.5, label=round(amount,2)),vjust=.5, size=6) +
        scale_colour_manual(values=c("Average"="gray50",
                                     "CBG" = pal2[2],
                                     "Large Above" = pal[5],
                                     "Small Above" = "#FFA1A0",
                                     "Small Below" = "#81D1E9",
                                     "Large Below" = pal[1]),
                            aesthetics=c("colour","fill")) +
        ylim(c(0,24)) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(size=12)) +
        labs(title = "Research Triangle Average Heat-Attributable Hospitalizations per 10,000",
             subtitle = "Summer 2018",
             y="",
             x="")
      
    # Once CBG is selected on map
    }else{
      print(rv_location$id)
      plot_waterfall(data=waterfall_df, cbg_geoid = rv_location$id)
    }
  })
  
  ## output: Demographic Bar Plot ##
  # Before CBG is selected - just show area average
  output$demobar <- renderPlot ({
    if (!rv_shape()){
      demobar_default

    # Once CBG is selected on map
    }else{
      print(rv_location$id)
      plot_demobar(data=demobar_df, cbg_geoid = rv_location$id)
    }
  })

  #### observe mouse events ####
  
  ## when any click happens
  observeEvent(input$mymap_click,{
    mymap_shape_click_info <- input$mymap_shape_click
    mymap_click_info <- input$mymap_click
    print(mymap_shape_click_info)
    rv_shape(TRUE)
    
    rv_location$id <- mymap_shape_click_info$id
    rv_location$lat <- round(mymap_click_info$lat, 4)
    rv_location$lng <- round(mymap_click_info$lng, 4)
  })

  
}
)

#### VIEW APP ####
shinyApp(ui = ui, server = server)

