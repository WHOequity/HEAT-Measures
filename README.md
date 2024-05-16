# HEAT-Measures package

This package contains the code for computing the summary measures. There is an incomplete vignette in the vignettes folder but it gives a lot of detail on how to error check, how to run code etc.

To test you can use the data from the HEAT-Data repository and create a subset that is just one strata and then run the summary measures you want to test. To compute the final inequality dataset that is being used in the HEAT application, though, you would use the HEAT-Data package.

```r
library(heatmeasures)
library(dplyr)
library(future)
library(heatdata)
heat_data <- readxl::read_excel("~/git-repos/HEAT-Data/data-raw/raw-nonspatial-data/20190702-HEAT-database.xlsx")
heat_data <- heatmeasures::add_strata_id(heat_data)
heat_data <- heatdata:::HEAT_data_fixes(heat_data, table_type = "heat_data") #remove missing rows
heat_data <- heatdata:::HEAT_force_variable_types(heat_data)
heat_data <- heatdata:::HEAT_drop_defective_strata(heat_data)
heat_data <- heatdata:::HEAT_rename_variables(heat_data, table_type = "heat_data")
which_measures <- HEAT_choose_measures_dataset(heat_data)
data_tests <- heatdata::HEAT_table_validation_tests(
  heat_data,
  grep("^test_", names(heatdata::HEAT_variable_descriptions), value = TRUE)
)


one_strata <- dplyr::filter(heat_data,
       setting == "Indonesia",
       year == "2007",
       source == "DHS",
       dimension == "Subnational region",
       indicator_abbr == "itnwm")
       
       
calc_r(
  ordered = vals$ordered,
  favourable = vals$favourable,
  max_est = vals$max_est,
  min_est = vals$min_est,
  max_se = vals$max_se,
  min_se = vals$min_se
  
)
```