prepare_admin_boundaries = function(data) {
    
    data |>
        select(-`...1`, 
               -state_lgd_code, 
               -state_no, 
               -state_name, 
               -SUCountry, 
               -`2011_code`,
               -district_iso, 
               -state_iso) |>
        rename(distname = district_name)
}

prepare_poverty = function(data) {
    
    data |>
        rename(distname = District,
               headcount_ratio = `Headcount ratio (%)`,
               intensity = `Intensity (%)`) %>%
        mutate(distname = case_match(distname, "Aurangabad" ~ "Chhatrapati Sambhajinagar",
                                     "Bid (Beed)" ~ "Beed",
                                     "Osmanabad" ~ "Dharashiv",
                                     "Ratnagirli" ~ "Ratnagiri", 
                                     .default = distname))
}

prepare_crop_locations = function(data) {
    
    colnames(data) = data[1,]
    
    projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    data %>%
        slice(-1) %>%
        # st_as_sf(., coords = c("X", "Y")) %>%
        dplyr::filter(`Adm1 Name` == "Maharashtra") %>%
        rename(distname = `Adm2 Name`) %>%
        mutate(Production = as.numeric(paste(`Production (t)`, sep = ""))) %>%
        group_by(distname) %>%
        summarise(Production = mean(Production, na.rm = T),
                  number_farms = n()) %>%
        ungroup() %>%
        mutate(distname = case_match(distname, "Ahmadnagar" ~ "Ahmednagar",
                                     "Aurangabad" ~ "Chhatrapati Sambhajinagar",
                                     "Bid" ~ "Beed", "Buldhana" ~ "Buldana",
                                     "Gondiya" ~ "Gondia", "Osmanabad" ~ "Dharashiv",
                                     "Raigarh" ~ "Raigad",
                                     .default = distname)) |>
        as_tibble()
    
}

# crop_locations = readxl::read_excel("data/raw/Crops_located_2020/India_SPAM2020_full_data.xlsx")
# crop_locations |>
#     prepare_crop_locations()

prepare_crops_district = function(data) {
    
    data |>
        select(`Dist Name`, 
               `SUGARCANE PRODUCTION (1000 tons)`,
               `SUGARCANE AREA (1000 ha)`, 
               `SUGARCANE YIELD (Kg per ha)`) |>
        rename(sugarcane_production = `SUGARCANE PRODUCTION (1000 tons)`,
               sugarcane_area = `SUGARCANE AREA (1000 ha)`, 
               sugarcane_yield = `SUGARCANE YIELD (Kg per ha)`,
               distname = `Dist Name`) |>
        mutate(distname = case_match(distname, "Amarawati" ~ "Amravati",
                                     "Aurangabad" ~ "Chhatrapati Sambhajinagar", 
                                     "Nasik" ~ "Nashik",
                                     "Osmanabad" ~ "Dharashiv", 
                                     "Yeotmal" ~ "Yavatmal",
                                     .default = distname)) |>
        as_tibble()
    
}

# crops_district_df <- read.csv("data/raw/Crops_2017/original_data/ICRISAT-District Level Data.csv")
# crops_district_df |>
#     prepare_crops_district()

prepare_crime = function(data) {
    
    data |>
        dplyr::filter(`States/UTs` == "Maharashtra") %>%
        dplyr::select(District, 
                      Year, 
                      POA_Murder, 
                      POA_Rape,
                      POA_Robbery, 
                      IPC_Murder, 
                      IPC_Rape, 
                      IPC_Robbery,
                      `Total crimes against STs`) %>%
        rename(total_crimes = `Total crimes against STs`,
               distname = District) %>%
        mutate(distname = case_match(distname, "Amravati Commr." ~ "Amravati",
                                     "Amravati Rural" ~ "Amravati",
                                     "Aurangabad Commr." ~ "Chhatrapati Sambhajinagar",
                                     "Aurangabad Rural" ~ "Chhatrapati Sambhajinagar",
                                     "Mumbai Commr." ~ "Mumbai", 
                                     "Mumbai Railway" ~ "Mumbai Suburban",
                                     "Navi Mumbai" ~ "Mumbai Suburban",
                                     "Nagpur Commr." ~ "Nagpur", "Nagpur Railway" ~ "Nagpur",
                                     "Nagpur Rural" ~ "Nagpur",
                                     "Nasik Commr." ~ "Nashik", "Nasik Rural" ~ "Nashik",
                                     "Osmanabad" ~ "Dharashiv",
                                     "Pune Commr." ~ "Pune", "Pune Railway" ~ "Pune",
                                     "Pune Rural" ~ "Pune",
                                     "Solapur Commr." ~ "Solapur", 
                                     "Solapur Rural" ~ "Solapur",
                                     "Thane Commr." ~ "Thane", "Thane Rural" ~ "Thane",
                                     .default = distname)) %>%
        group_by(distname) %>%
        summarise(POA_Murder = sum(POA_Murder, na.rm = T),
                  POA_Rape = sum(POA_Rape, na.rm = T),
                  POA_Robbery = sum(POA_Robbery, na.rm = T),
                  IPC_Murder = sum(IPC_Murder, na.rm = T),
                  IPC_Rape = sum(IPC_Rape, na.rm = T),
                  IPC_Robbery = sum(IPC_Robbery, na.rm = T),
                  total_crimes = sum(total_crimes, na.rm = T)) %>%
        dplyr::select(distname, total_crimes) |>
        as_tibble()
    
}

# crime_df <- read.csv("data/raw/Crime_2014_MH/original_data/02_District_wise_crimes_committed_against_ST_2014.csv")
# crime_df |>
#     prepare_crime()

prepare_education = function(data) {
    
    # Clean education dataframe
    
    colnames(data) <- data[18,]
    
    data <- data %>%
        dplyr::select(STATNAME, 
                      DISTNAME,
                      OVERALL_LI, FEMALE_LIT, MALE_LIT) %>%
        slice(-1:-18) %>%
        dplyr::filter(STATNAME == "MAHARASHTRA") %>%
        rename(distname = DISTNAME) %>%
        mutate(OVERALL_LI = as.numeric(paste(OVERALL_LI, sep = "")),
               FEMALE_LIT = as.numeric(paste(FEMALE_LIT, sep = "")),
               MALE_LIT = as.numeric(paste(MALE_LIT)),
               fem_male_lit = FEMALE_LIT/MALE_LIT,
               distname = str_to_title(distname)) %>%
        dplyr::select(-STATNAME, -FEMALE_LIT, -MALE_LIT) %>%
        mutate(distname = case_match(distname, "Ahmadnagar" ~ "Ahmednagar",
                                     "Aurangabad (Maharashtra)" ~ "Chhatrapati Sambhajinagar",
                                     "Bid" ~ "Beed", "Buldana" ~ "Buldhana",
                                     "Gondiya" ~ "Gondia", "Mumbai Ii" ~ "Mumbai",
                                     "Raigarh (Maharashtra)" ~ "Raigarh",
                                     "Mumbai Ii" ~ "Mumbai",
                                     "Mumbai (Suburban)" ~ "Mumbai Suburban",
                                     "Osmanabad" ~ "Dharashiv",
                                     "Raigarh (Maharashtra)" ~ "Raigad",
                                     .default = distname))
}

# education_df <- readxl::read_excel("data/raw/Education/original_data/Distt Report Card 2015-16_new.xlsx")
# education_df |>
#     prepare_education()

prepare_industries = function(data) {
    
    data |> 
        dplyr::select(-...1, -...4, -...5) %>%
        rename(STATE_UT = `As on 8-08-2023`,
               industry_variable = `289482645`) %>%
        slice(-1:-3) %>%
        dplyr::filter(STATE_UT == "Mahrash") %>%
        mutate(STATE_UT = case_match(STATE_UT, 
                                     "Mahrash" ~ "Maharashtra"),
               industry_variable = industry_variable/1000000)
}

# industries_df <- readxl::read_excel("data/raw/Industries/original_data/eShram Data as on 08-08-2023.xlsx")
# industries_df |>
#     prepare_industries()

# function to load in csvs
load_csv = function(path = 'data/raw',
                    file) {
    
    filepath = paste0(path, "/", file)
    readr::read_csv(filepath)
}

# function to load in csvs
load_excel = function(path = 'data/raw',
                      file) {
    
    filepath = paste0(path, "/", file)
    readxl::read_excel(filepath)
}

# general function for loading in csvs
load_files = function(folder) {
    
    csvs <- list.files(folder, pattern = "\\.csv")
    paths = paste0(folder, "/", csvs)
    
    map_df(paths,
           ~ read_csv(.x, show_col_types = F)
    )
    
}

# load specific datasets
# admin boundaries
load_admin_boundaries = function() {
    
    load_csv(file = "Geospatial District and State Boundaries/LGD_Districts_All.csv")
    
}

# poverty
load_poverty = function() {
    
    load_csv(file = "Poverty_Original_data/Maharashtra Districts Poverty Data.csv")
    
}

# load crop_locations
load_crop_locations = function() {
    
    load_excel(file = "Crops_located_2020/India_SPAM2020_full_data.xlsx")
}

# crops district
load_crops_district = function() {
    
    load_csv(file = "Crops_2017/original_data/ICRISAT-District Level Data.csv")
}

# crime
load_crime = function() {
    
    load_csv(file = "Crime_2014_MH/original_data/02_District_wise_crimes_committed_against_ST_2014.csv")
    
}

# education
load_education = function() {
    
    load_excel(file = "Education/original_data/Distt Report Card 2015-16_new.xlsx")
    
}

# industries
load_industries = function() {
    
    load_excel(file = "Industries/original_data/eShram Data as on 08-08-2023.xlsx")
    
}

# tidy files with lulc
prepare_lulc = function(data) {
    
    data |>
        select(d_cell_id, starts_with("lulc_")) |>
        distinct()
}

# use function to load in geospatial
load_geospatial = function(path) {
    
    load_files(folder = 'data/raw/Geospatial Data')
}

# landcover
load_landcover = function() {
    
    load_files(folder = 'data/raw/mh_lulc_bare')
}

# builtup
load_builtup = function() {
    
    load_files(folder = 'data/raw/mh_lulc_builtup')
}

# crop area
load_crop_area = function() {
    
    load_files(folder = 'data/raw/mh_lulc_crop')
}

# forest
load_forest = function() {
    
    load_files(folder = 'data/raw/mh_lulc_forest')
}

# hydro veg
load_hyroveg = function() {
    
    load_files(folder = 'data/raw/mh_lulc_hydroveg')
}

# Rangeland area
load_rangeland = function() {
    
    load_files(folder = 'data/raw/mh_lulc_rangeland')
}

# water cover
load_watercover = function() {
    
    load_files(folder = 'data/raw/mh_wb')
}

# railroads
load_railroads= function() {
    
    load_files(folder = 'data/raw/mh_rails')
}

# road types
load_roads = function() {
    
    load_files(folder = 'data/raw/mh_roads')
}

# bus stations
load_bus = function() {
    
    load_files(folder = 'data/raw/mh_bus_stations')
}

# buildings
load_buildings = function() {
    
    load_files(folder = 'data/raw/mh_buildings_v1')
}

# climate
load_climate = function() {
    
    load_files(folder = 'data/raw/mh_climate')
}

# geospatial
prepare_geospatial = function(data) {
    
    data |>
        distinct() |>
        mutate(outcome = case_when(bk == 1 | sf == 1 ~ 1, .default = 0)) |>
        select(-bk, -sf) |>
        mutate(district_lgd_code = substr(d_cell_id, 2, 4),
               district_lgd_code = as.numeric(paste0(district_lgd_code)))
}

# prepare roads
prepare_roads = function(data) {
    
    data |>
        select(d_cell_id, road1, road2, road3, road4, road5, road6) |>
        distinct()
}

# prepare bus stations
prepare_bus = function(data) {
    
    data |>
        select(d_cell_id, bus) |>
        distinct()
}

# prepare buildings
prepare_buildings = function(data) {
    
    data |>
        select(d_cell_id, b_count, b_density_class) |>
        distinct()
}

# prepare climate
prepare_climate = function(data) {
    
    data |>
        select(d_cell_id, vuln_index, exp, sens, adapt) |>
        distinct() #
}

# prepare watercover
prepare_watercover = function(data) {
    
    data |>
        select(d_cell_id, wb_pc) |>
        distinct()
}

# prepare railroads
prepare_railroads = function(data) {
    
    data |>
        select(d_cell_id, rail_st) |>
        distinct()
}

# geospatial = load_geospatial()
# geospatial |>
#     prepare_geospatial()

# prepare_admin_boundaries(
#     
# )
# 
#   tidytable::select(d_cell_id, lulc_bare_pc) %>%
#   distinct()

