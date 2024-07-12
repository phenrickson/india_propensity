# propensity

modeling probability of forced labor for survey sampling in india. 

## requirements

this project uses the `targets` package to create a pipeline for reproducible results. running the pipeline will catch
the `data` used in this project is stored externally and needs to be manually moved into the project folder.

## steps

1. in project folder, create `data` folder with `raw` and `processed` subfolders
2. copy external project data from dropbox into `data/raw`
3. run `renv::restore()` to install project dependencies
4. run `targets::tar_make()` to run pipeline
5. propensity scores will be written to `data/processed` as `propensity.csv`
