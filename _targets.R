# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
    packages = c("tidyverse",
                 "qs"), # Packages that your targets need for their tasks.
    format = "qs", # Optionally set the default storage format. qs is fast.
    memory = "transient"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
    tar_target(
        name = admin_boundaries,
        command = 
            load_admin_boundaries() |>
            prepare_admin_boundaries()
    ),
    tar_target(
        name = poverty,
        command =  
            load_poverty() |>
            prepare_poverty()
    ),
    tar_target(
        name = crop_locations,
        command = 
            load_crop_locations() |>
            prepare_crop_locations()
    ),
    tar_target(
        name = crops_district,
        command = 
            load_crops_district() |>
            prepare_crops_district()
    ),
    tar_target(
        name = crime,
        command = 
            load_crime() |>
            prepare_crime()
    ),
    tar_target(
        name = education,
        command = 
            load_education() |>
            prepare_education()
    ),
    tar_target(
        name = industries,
        command = 
            load_industries() |>
            prepare_industries()
    ),
    tar_target(
        name = geospatial,
        command = 
            load_geospatial() |>
            prepare_geospatial()
    ),
    tar_target(
        name = landcover,
        command = 
            load_landcover() |>
            prepare_lulc()
    ),
    tar_target(
        name = builtup,
        command = 
            load_builtup() |>
            prepare_lulc()
    ),
    tar_target(
        name = crop_area,
        command = 
            load_crop_area() |>
            prepare_lulc()
    ),
    tar_target(
        name = forest,
        command = 
            load_forest() |>
            prepare_lulc()
    ),
    tar_target(
        name = hydroveg,
        command = 
            load_hyroveg() |>
            prepare_lulc()
    ),
    tar_target(
        name = rangeland,
        command = 
            load_rangeland() |>
            prepare_lulc()
    ),
    tar_target(
        name = watercover,
        command = 
            load_watercover() |>
            prepare_watercover()
    ),
    tar_target(
        name = railroads,
        command = 
            load_railroads() |>
            prepare_railroads()
    ),
    tar_target(
        name = roads,
        command = 
            load_roads() |>
            prepare_roads()
    ),
    tar_target(
        name = bus,
        command = 
            load_bus() |>
            prepare_bus()
    ),
    tar_target(
        name = buildings,
        command = 
            load_buildings() |>
            prepare_buildings()
    ),
    tar_target(
        name = climate,
        command = 
            load_climate() |>
            prepare_climate()
    ),
    tar_target(
        districts,
        command = 
            admin_boundaries |>
            left_join(poverty, by = c("distname")) |>
            left_join(crime, by = c("distname")) |>
            left_join(education, by = c("distname")) |>
            left_join(crops_district, by = c("distname")) |>
            left_join(crop_locations, by = c("distname")) |>
            rename_with(.fn = tolower)
    ),
    tar_target(
        outcomes,
        command = 
            geospatial |>
            left_join(landcover, by = c("d_cell_id")) |>
            left_join(builtup, by = c("d_cell_id")) |>
            left_join(crop_area, by = c("d_cell_id")) |>
            left_join(forest, by = c("d_cell_id")) |>
            left_join(hydroveg, by = c("d_cell_id")) |>
            left_join(rangeland, by = c("d_cell_id")) |> 
            left_join(watercover, by = c("d_cell_id")) |>
            left_join(railroads, by = c("d_cell_id"))  |>
            left_join(roads, by = c("d_cell_id")) |>
            left_join(bus, by = c("d_cell_id")) |>
            left_join(buildings, by = c("d_cell_id")) |>
            left_join(climate, by = c("d_cell_id")) |>
            left_join(districts, by = c("district_lgd_code")) |>
            distinct() |>
            rename_with(.fn = tolower)
    ),
    tar_target(
        full_data,
        command = 
            outcomes |>
            mutate(outcome = case_when(outcome == 1 ~ 'yes',
                                       TRUE ~ 'no'),
                   outcome = factor(outcome, levels = c('no', 'yes')))
    )
)
