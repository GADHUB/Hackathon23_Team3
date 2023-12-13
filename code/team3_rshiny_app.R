## Read Me =====================================================================

## Title:         GAD Hackaton - Team 3 [Gender gap data]

## Author:        Michele Castiglioni
## Last edited:   30-November-2023
## Version:       1.0.1
## Standard:      {tidyverse}


## 0. Setup ====================================================================

# Install and load packages
required_packages <- c("tidyverse","purrr","stringr","ggplot2", #tidyverse libraries
                       "data.table","gridExtra", #additional basic libraries
                       "shiny", #app
                       "afcolours", #Analysis Function colour palettes
                       "sf", "rmapshaper", #shapefiles
                       "foreach", "doParallel", #parallel coding
                       "janitor", #datacleaning
                       "gender","genderdata") #gender data

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)

# Set options
rm(list = ls())
options(scipen=999)

# Set theme
theme_set(theme_bw() +
            theme(
              legend.position = "none",
              legend.title = element_blank(), 
              legend.key = element_blank(),
              panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              rect = element_blank(),
              plot.title = element_text(size = 15, face = "bold", hjust = 0)
            ))


## 1. Load =========================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### GENDER GAP DATA

file_list <- 
  list.files(path = "../data/gender_gap/raw", pattern = "\\.csv$", full.names = T)

data <- 
  do.call(rbind, lapply(file_list, read_csv)) %>% 
  clean_names()

### SIC (Sector Industry Codes)

sic <- read.csv("../data/sic_complete.csv") %>% 
  dplyr::select(code,section_description) %>% 
  rename(sic_codes=code,
         sector=section_description) %>% 
  mutate(sic_codes=as.character(sic_codes)) %>% 
  mutate(sector=stringr::str_to_title(as.character(sector)))

### POSTCODES

uk_region <- read.csv("../data/UK Postcode Areas.csv") 


### SHAPEFILES

#Load and simplify the UK map. 
#Granularity at Postcode areas (i.e. first letters of the postcode), for readability

#shp_data <- st_read("../data/Distribution/Sectors.shp")
#shp_data <- st_read("../data/Distribution/Districts.shp")
#shp_data <- st_read("../data/Distribution/Areas.shp")

shp_data_postcode <- 
  st_read("../data/Distribution/Areas.shp") %>% 
  ms_simplify(keep = 0.01, keep_shapes = FALSE) %>% 
  st_make_valid() %>% 
  dplyr::rename(postcode_area=name) 

shp_data_region <- 
  st_read("../data/Distribution/Areas.shp") %>% 
  ms_simplify(keep = 0.01, keep_shapes = FALSE) %>% 
  st_make_valid() %>% 
  dplyr::rename(postcode_area=name) %>% 
  left_join(uk_region) %>% 
  group_by(postcode_region) %>%
  summarize(geometry = st_union(geometry))

#Unite polygons to obtain UK silhouette (for missing areas later on)
shp_union <- 
  shp_data_postcode %>% 
  summarize(geometry = st_union(geometry), do_union = FALSE) %>%
  ungroup()



## 2. Clean data ===============================================================

### GENDER GAP DATA

summary(data)
head(data)

# Drop the following information 
# - "employer_name" - Irrelevant (possibly extract incorporation status)
# - "current_name" - Irrelevant (possibly extract incorporation status)
# - "address" - Too specific, use post_code
# - "company_link_to_gpg_info" - Irrelevant
# - "date_submitted",due_date - Irrelevant
# - "responsible_person" - Irrelevant (check whether whether the gender of reporter has an impact)

# Manipulate the following information
# - "sic_codes" - split data on a given, consistent character, possibly split over multiple cols? Are there macro-codes?
# - "due_date" - Extract year 

gender_gap_data <- 
  data %>% 
  dplyr::filter(!is.na(post_code)) %>% 
  dplyr::mutate(year = as.numeric(str_sub(start = 1, end = 4, due_date))) %>% 
  dplyr::mutate(sic_codes = str_replace_all(string = sic_codes, "\n", ""),
                sic_codes = str_remove_all(sic_codes, pattern = "^1,"),
                sic_codes = str_remove_all(sic_codes, pattern = "^1,"),
                sic_codes = str_remove_all(sic_codes, pattern = "^1,"),
                sic_codes = str_remove_all(sic_codes, ",.*")) %>% 
  dplyr::mutate(postcode_area=str_remove(pattern = "\\s.*$",post_code),
                postcode_area=str_replace_all(pattern = "[0-9]", replacement = " ", postcode_area),
                postcode_area=str_remove(pattern = "\\s.*$", postcode_area)) %>% 
  dplyr::mutate(employer_size = str_remove(pattern = ",", employer_size),
                min_size = as.numeric(str_extract(employer_size, "\\b\\w+\\b")),
                min_size = if_else(employer_size=="Less than 250",150,min_size)) %>% #,
  left_join(sic) %>% 
  left_join(uk_region) %>% 
  mutate(sector = str_remove(sector, pattern = ";.*"),
         sector = str_to_sentence(sector),
         sector = factor(sector)) %>% 
  dplyr::select(-employer_name,-address,-company_link_to_gpg_info,-date_submitted,
                -due_date,-responsible_person, -submitted_after_the_deadline, -current_name,
                -sic_codes) %>%
  dplyr::filter(!is.na(sector)) %>% 
  dplyr::filter(!is.na(postcode_area)) %>% 
  dplyr::filter(!is.na(min_size)) %>% 
  dplyr::filter(year<=2023) %>%
  group_by(year, company_number) %>% 
  slice(1) %>% 
  ungroup()


### VISUALIZATION; EXPLORE AREAS' MODAL SECTOR (NO. EMPLOYEES)
### WHAT DOES THE UK INDUSTRIAL LANDSCAPE LOOK LIKE?

# data_modalsector <-
#   gender_gap_data %>% 
#   group_by(postcode_area, year, sector) %>%
#   summarise(size=sum(min_size)) %>% 
#   group_by(postcode_area, year) %>%
#   arrange(desc(size)) %>% 
#   slice(1) %>%
#   ungroup() %>% 
#   left_join(shp_data)
# 
# 
# ggplot() +
#   geom_sf(data = data_modalsector, aes(geometry = geometry, fill = sector), color = NA) +  
#   geom_sf(data = shp_union, fill = NA, color = "black") + 
#   facet_wrap(~ year, nrow = 1) + 
#   theme_void() +
#   guides(fill = FALSE)


 
### DEFINE AN INTRA-SECTOR COMPETITION METRIC
### DOES COMPETITION DRIVE GENDER GAP?

# data_competition <-
#   gender_gap_data %>% 
#   group_by(postcode_area, year, sector) %>%
#   summarise(competiton_firms=length(unique(employer_id)),
#             competition_people=sum(min_size),
#             competition_avg=competition_people/competiton_firms)


### AGGREGATE DATA BY REGION 
### DEVELOP A METRIC OF INTEREST


data_aggregated <-
  gender_gap_data %>% 
  filter(!is.na(sector)) %>% 
  mutate("total_diff_mean_hourly_percent" = min_size*diff_mean_hourly_percent) %>% 
  group_by(postcode_region, sector, year) %>% 
  summarise(across(c("min_size",
                     "total_diff_mean_hourly_percent"), sum, .names = "sum_{.col}")) %>% 
  ungroup() %>% 
  left_join(shp_data_region) %>%
  mutate("weight_diff_mean_hourly_percent"= sum_total_diff_mean_hourly_percent/sum_min_size)



## 3. Build a dashboard ========================================================




# Define UI
ui <- fluidPage(
  titlePanel("Gender Gap Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sectorInput", "Select a Sector:", choices = unique(gender_gap_data$sector))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Geomapping", plotOutput("mapOutput")),
        tabPanel("Representation", plotOutput("mapOutput2")),
        tabPanel("Trend", plotOutput("lineGraphOutput"))
      )
    )
  )
)




# Server Logic
server <- function(input, output) {
  
  # Summarized Data
  summarized_data <- reactive({
    gender_gap_data %>%
      filter(!postcode_region %in% c("Scotland","Wales","Northern Ireland")) %>% 
      group_by(sector, year, postcode_area) %>%
      summarize(average_female_top_quartile = mean(female_top_quartile, na.rm = TRUE))
  })
  # Summarized Data (region)
  summarized_data_region <- reactive({
    data_aggregated %>%
      filter(!postcode_region %in% c("Scotland","Wales","Northern Ireland")) #%>% 
      #group_by(sector, year, postcode_region) %>%
      #summarize(weight_diff_mean_hourly_percent)
  })
  # Summarized Data (region)
  
  names(gender_gap_data)
  
  summarized_data_quartiles <- reactive({
    gender_gap_data %>%
      filter(!postcode_region %in% c("Scotland","Wales","Northern Ireland")) %>% 
      group_by(sector, year, postcode_region) %>%
      summarize(female_lower_quartile = mean(female_lower_quartile, na.rm = TRUE),
                female_lower_middle_quartile = mean(female_lower_middle_quartile, na.rm = TRUE),
                female_upper_middle_quartile = mean(female_upper_middle_quartile, na.rm = TRUE),
                female_top_quartile = mean(female_top_quartile, na.rm = TRUE))
  })
  
  # Map Output
  output$mapOutput <- renderPlot({
    # Prepare data for the first map
    map_data <- summarized_data() %>%
      filter(year == 2022, sector == input$sectorInput)
    merged_data <- left_join(shp_data_postcode, map_data, by = "postcode_area")
    
    # First map plot
    p1 <- ggplot(merged_data) +
      geom_sf(aes(fill = average_female_top_quartile)) +
      labs(title = "Map for Postcode Areas")
    
    # Remove legend from the first plot
    
    # Prepare data for the second map
    map_data_region <- 
      summarized_data_region() %>%
      filter(year == 2022, 
             sector == input$sectorInput)
    merged_data_region <- left_join(shp_data_region, map_data_region, by = "postcode_region")
    
    # Second map plot
    p2 <- ggplot(merged_data_region) +
      geom_sf(aes(fill = weight_diff_mean_hourly_percent)) +
      labs(title = "Map for Regions", fill = "weight_diff_mean_hourly_percent") 
    
    
    # Define layout matrix for equal-sized plots
    layout_matrix <- matrix(c(1, 2), nrow = 1)
    
    # Arrange two plots with the same size
    grid.arrange(p2, p1, layout_matrix = layout_matrix)
  })

  
  # Map Output quantiles
  output$mapOutput2 <- renderPlot({
    # Prepare data for the first map
    map_data_region_quartiles_pre <- 
      summarized_data_quartiles() %>%
      filter(year == 2018, 
           sector == input$sectorInput)
    merged_data_region_quartiles_pre <- left_join(shp_data_region, map_data_region_quartiles_pre, by = "postcode_region")
    
    map_data_region_quartiles_post <- 
      summarized_data_quartiles() %>%
      filter(year == 2022, 
             sector == input$sectorInput)
    merged_data_region_quartiles_post <- left_join(shp_data_region, map_data_region_quartiles_post, by = "postcode_region")
    
    
    
    # First map plot
    p1 <- ggplot(merged_data_region_quartiles_pre) +
      geom_sf(aes(fill = female_lower_quartile)) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +  
      labs(title = "Pre COVID-19 - Bottom qt")
    
    p2 <- ggplot(merged_data_region_quartiles_post) +
      geom_sf(aes(fill = female_lower_quartile)) +
      scale_fill_gradient(low = "lightpink", high = "darkred")  +
      labs(title = "Post COVID-19 - Bottom qt")

    p3 <- ggplot(merged_data_region_quartiles_pre) +
      geom_sf(aes(fill = female_top_quartile)) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Pre COVID-19 - Top qt")

    p4 <- ggplot(merged_data_region_quartiles_post) +
      geom_sf(aes(fill = female_top_quartile)) +
      scale_fill_gradient(low = "lightpink", high = "darkred")+
      labs(title = "Post COVID-19 - Top qt")

    # Define layout matrix for equal-sized plots
    layout_matrix <- matrix(c(1, 2, 3, 4), ncol = 2)
    
    # Arrange four plots
    grid.arrange(p1, p2, p3, p4, layout_matrix = layout_matrix)
    
    
  })
  
  
  
  # Line Graph Output
  output$lineGraphOutput <- renderPlot({
    filtered_data_region <- 
      summarized_data_region() %>%
      filter(sector == input$sectorInput) %>% 
      filter(!postcode_region %in% c("Scotland","Wales","Northern Ireland"))
    
    ggplot(filtered_data_region, aes(x = year, y = weight_diff_mean_hourly_percent, 
                                     group = postcode_region, color = postcode_region)) +
      geom_line() +
      labs(title = "Trend of Average Difference in Hourly Pay", 
           x = "Year", y = "Average Difference in Hourly Pay") +
      theme_minimal()
    
    
  })
}


# Run the application
shinyApp(ui = ui, server = server)
``
