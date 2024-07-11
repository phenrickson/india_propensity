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
                 "tidymodels",
                 "themis",
                 "qs"), # Packages that your targets need for their tasks.
    format = "qs", # Optionally set the default storage format. qs is fast.
    memory = "transient",
    # specify seed out of paranoia
    seed = 1999
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
            select(-col_index, -row_index) |>
            mutate(outcome = case_when(outcome == 1 ~ 'yes',
                                       TRUE ~ 'no'),
                   outcome = factor(outcome, levels = c('no', 'yes')))
    ),
    tar_target(
        split,
        command = 
            full_data |>
            initial_validation_split(strata = outcome,
                                     prop = c(0.8, 0.1))
    ),
    tar_target(
        train_data,
        command = 
            split |>
            training()
    ),
    tar_target(
        validation_split,
        command = 
            split |>
            validation_set() |>
            pluck("splits", 1)
    ),
    tar_target(
        train_folds,
        command = 
            train_data |>
            vfold_cv(strata = outcome,
                     v = 5)
    ),
    tar_target(
        tune_control,
        command = 
            control_resamples(verbose = T,
                              save_pred = T,
                              event_level = 'second')
    ),
    tar_target(
        ctrl_grid,
        command = 
            control_grid(verbose = T,
                         save_pred = T,
                         event_level = 'second')
    ),
    tar_target(
        prob_metrics,
        command = metric_set(mn_log_loss,
                             roc_auc)
    ),
    tar_target(
        base_recipe,
        command = 
            recipe(outcome ~., data = train_data) |>
            update_role(d_cell_id, 
                        s_cell_id, 
                        distname,
                        district_lgd_code,
                        new_role = "ID")
    ),
    tar_target(
        downsample_recipe,
        command = 
            base_recipe |>
            themis::step_downsample(outcome, under_ratio = tune::tune()) |>
            step_impute_mean(
                total_crimes,
                sugarcane_production,
                sugarcane_area,
                sugarcane_yield,
                production,
                number_farms,
                overall_li,
                fem_male_lit
            ) |>
            step_zv(all_predictors()) |>
            step_normalize(all_predictors())
    ),
    tar_target(
        downsample_grid,
        command = 
            grid_regular(
                under_ratio(
                    range = c(1, 4), 
                    trans = scales::transform_log10()
                ),
                levels = c(under_ratio = 4)
            )
    ),
    tar_target(
        null_model,
        command = 
            workflow() |> 
            add_model(
                logistic_reg()
            ) |>
            add_recipe(
                recipe(
                    outcome ~ 1,
                    data = train_data
                )
            )
    ),
    tar_target(
        null_tuning_results,
        command = 
            null_model |>
            fit_resamples(resamples = train_folds,
                          metrics = prob_metrics,
                          control = tune_control)
    ),
    tar_target(
        glmnet_model,
        command = 
            workflow() |>
            add_model(
                logistic_reg(engine = 'glmnet',
                             penalty = 0.001,
                             mixture = 0)
            ) |>
            add_recipe(
                downsample_recipe
            )
    ),
    tar_target(
        glmnet_tuning_results,
        command = 
            glmnet_model |>
            tune_grid(resamples = train_folds,
                      grid = downsample_grid,
                      metrics = prob_metrics,
                      control = ctrl_grid)
    ),
    tar_target(
        glmnet_best_tune,
        command = 
            glmnet_tuning_results |>
            collect_metrics() |>
            filter(under_ratio == 1000) |>
            head(1)
    ),
    tar_target(
        oos_preds,
        command = 
            glmnet_tuning_results |>
            collect_predictions(parameters = glmnet_best_tune)
    ),
    tar_target(
        glmnet_train_fit,
        command = 
            glmnet_model |>
            finalize_workflow(parameters = glmnet_best_tune) |>
            last_fit(
                split = validation_split,
                metrics = prob_metrics
            )
    ),
    tar_target(
        valid_metrics,
        command = 
            glmnet_train_fit |>
            collect_metrics()
    ),
    tar_target(
        valid_preds,
        command = 
            glmnet_train_fit |>
            collect_predictions()
    )
    # tar_target(
    #     null_training_results,
    #     
    # )
    # tar_target(
    #     base_recipe,
    #     command = 
    #         
    # )
)
