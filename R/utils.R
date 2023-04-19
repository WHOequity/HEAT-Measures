# © Copyright World Health Organization (WHO) 2016-2021.
# This file is part of the WHO Health Equity Assessment Toolkit 
# (HEAT and HEAT Plus), a software application for assessing 
# health inequalities in countries.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>. 

#' Returns the cumulative mid point proportion of each group
#'
#' @param w a vector of numbers of the population in each group
#'
#' @return
#' 
#' A vector representing the cumulative mid-point proportion
#' 
#' @export
midPointProp <- function(w) {

  if(!is.numeric(w)){
    stop('This function operates on vector of numbers')
  }
  if(all(w==0)){
    stop('The population is of size 0 in all cells')
  }
  p <- w/sum(w)  # Calculate the pop. proportion in each group 
  p.mid <- p/2   # Calculate the mid-point proportion in each group
  p.cumsum <- cumsum(p) # Calculate the cumulative proprtion
  p.mid.cumsum <- p.mid + c(0, p.cumsum)[1:length(w)]  # Calculate the cumulative mid point proportion of each group
  return(p.mid.cumsum)  # Return the answer
}

valid_se <- function(strata) {

  anyNA <- any(is.na(strata$se))
  
  "se"%in%names(strata) && !anyNA
}

is_dimension_sex <- function(strata) {
  all(strata$subgroup%in%c("Female", "Male"))
}

reference_group_exists <- function(strata) {
  ref_subgrp <- strata$reference_subgroup
  sum(ref_subgrp==1 & !is.na(ref_subgrp)) == 1
}

is_ordered_dimension <- function(strata) all(strata$ordered_dimension == 1)

is_favourable_indicator <- function(strata) unique(strata$favourable_indicator) == 1

which_is_refgroup <- function(strata){
  if(!reference_group_exists(strata)) stop("A reference group is required for this function")
  which(strata$reference_subgroup==1)
}

#******************************************************************************
# Get the estimate for the low and high ordered level. Note that the "min" and
# "max" can be based on the estimates themselves (userank=FALSE) or they can
# be based on the subgroup order.
#******************************************************************************
get_est_se_min_max <- function(strata, 
                               userank = FALSE, 
                               ascending = TRUE, 
                               userefgroup = FALSE) {

  if(!userank & !userefgroup){
    
    if(ascending) strata <- arrange(strata, estimate)
    if(!ascending) strata <- arrange(strata, desc(estimate))
    min.indx <- 1
    max.indx <- nrow(strata)
  }
  
  if(userank & !userefgroup){
    
    if(ascending) strata <- arrange(strata, subgroup_order)
    if(!ascending) strata <- arrange(strata, desc(subgroup_order))
    min.indx <- 1
    max.indx <- nrow(strata)
  }
  
  if(userefgroup){
    favourable <- is_favourable_indicator(strata)
    
    if(favourable){
      max.indx <- which(strata$reference_subgroup == 1)
      # a hack because we can't choose reference for both
      vals <- strata$estimate
      vals[max.indx] <- Inf
      min.indx <- which.min(vals)
    }
    
    if(!favourable){
      min.indx <- which(strata$reference_subgroup == 1)
      # a hack because we can't choose reference for both
      vals <- strata$estimate
      vals[min.indx] <- -Inf
      max.indx <- which.max(vals)
      
    }
    
  }
  
  if("se"%in%names(strata)){
    vals <- strata[c(min.indx, max.indx), c("estimate", "se")]
  } else {
    vals <- strata[c(min.indx, max.indx), c("estimate"), drop=FALSE]
    vals$se <- NA
  }
  vals$type <- c("min", "max")
  vals
  
}


#' Add a new column called `strata_id` that will be unique for each strata. The default fields
#' used to determine unique strata are created in the data-raw/strata-variables.R file and are a 
#' variable called HEAT_strata_variables
#'
#' @param .data 
#' @param strata_vars as string, a vector of field names to use to determine unique strata.
#'
#' @return
#' @export
add_strata_id <- function(.data, strata_vars = heatmeasures::HEAT_strata_variables) {
  
  if(!"strata_id"%in%names(.data)){
  strata_vars <- syms(strata_vars)
  .data <- .data %>% 
    dplyr::group_by(!!!strata_vars) %>% 
    dplyr::mutate(strata_id = dplyr::cur_group_id()) %>% 
    dplyr::ungroup()
  }
  
  .data
}

#******************************************************************************
# Determine if the SE can be used to compute formula/boot SE
#******************************************************************************

usefulSE <- function(strata) {
  
  # Not useful if any are NA
  anyNA <- any(is.na(strata$se))
  
  "se"%in%names(strata) && !anyNA
}

#' Map the validation tests for inequality measures to a strata
#' 
#' Makes use of a built in table called summary_measure_types to determine the
#' appropriate test.
#'
#' @param strata a dataset representing data for a single strata
#' 
#' @param measures a vector of inequality measure abbreviations
#' 
#' @param only_return_passing if TRUE it only returns a vector of summary
#'   measures that pass all tests otherwise returns a list (see below Value)
#'
#' @details
#' 
#' ```
#' Observations: 31
#' Variables: 5
#' $ measure         <chr> "aci", "bgsd", "bgv", "cov", "d", "d", "d", "d", "idisu", "idisw", "mdbu", ... \cr
#' $ just2_subgroups <lgl> FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, ... \cr
#' $ ordered         <lgl> TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, F... \cr
#' $ simulation_se   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ... \cr
#' $ test_name       <list> [<"test_estimates_all_zero", "test_two_subgroups", "test_less_than2_subgro... \cr
#' ```
#' 
#' @return 
#' 
#' If only_return_passing = TRUE it only returns a vector of summary measures
#' that pass all tests otherwise returns a list with three items. One is a
#' vector of those that pass `Measures that pass`, those that fail `Measures
#' that fail` and a table of test results describing which tests fail.
#' 
#' @export
HEAT_measures_validation_tests <- function(strata, measures, only_return_passing = FALSE) {

  measures <- heatmeasures::HEAT_summary_measures_types %>% 
    filter(measure%in%measures) %>% 
    tidyr::unnest(test_name)
  
  tests <- unique(measures$test_name)
  
  test_results <- purrr::map_dfr(tests, function(x){
    func <- get(x)
    tibble(test_name = x, bad_test_result = func(strata))
  })

  
  test_results <- measures %>% 
    left_join(., test_results, by = "test_name")
  
  test_results_ok <- group_by(test_results, measure) %>% 
    summarize(res = sum(bad_test_result) == 0) %>% 
    filter(res == TRUE) %>% 
    pull(measure)
  
  res <- list(`Measures that pass` = test_results_ok)
  
  if(!only_return_passing){
    
    test_results_bad <- group_by(test_results, measure) %>% 
      summarize(res = sum(bad_test_result) == 0) %>% 
      filter(res == FALSE) %>% 
      pull(measure)
    
    res[["Measures that fail"]] <- test_results_bad
    res[["Test Results"]] <- test_results
  }
  
  
  res
}

should_simulation_run <- function(strata, test_results) {
  
  sim_measures <- filter(test_results$`Test Results`, 
         simulation_se == 1,
         !bad_test_result) %>% 
    nrow() > 0
  
  if(!sim_measures) return(FALSE)
  
  usefulSE(strata)
}

do_simulation <- function(strata, simulation_iterations) {

  scaleval <- unique(strata$indicator_scale)
  estrand <- purrr::map(1:simulation_iterations, 
                        ~rnorm(nrow(strata), 
                               strata$estimate/scaleval, 
                               strata$se/scaleval))
  
  estrand
}

conf.int.norm <- function(val, se) {
  l <- val - se * qnorm(0.975)
  u <- val + se * qnorm(0.975)
  
  return(list(l = l, u = u))
}



#' Create list of inputs
#'
#' @param .data A data frame.
#' 
#' @param force_simulations for testing if you want to get simulations even if
#'   they wouldn't be calculated otherwise
#'
#' @return
#' 
#' A list.
#' 
#' @export
HEAT_create_input_list <- function(.data, 
                                   validation_results, 
                                   simulation_n = 100, 
                                   force_simulations = FALSE) {
  
  dat <- arrange(.data, subgroup_order)
  
  pop <- .data$population
  est <- .data$estimate
  se <- .data$se
  SEuseful <- usefulSE(.data)
  ordered <- is_ordered_dimension(.data)
  favourable <- is_favourable_indicator(.data)
  ascending <- ifelse(ordered, favourable, TRUE)
  hasrefgroup <- reference_group_exists(.data)
  reference_subgroup <- .data$reference_subgroup
  ranked_vals <- get_est_se_min_max(.data, 
                                    userank = ordered, 
                                    ascending = ascending, 
                                    userefgroup = hasrefgroup)
  scaleval <- unique(.data$indicator_scale)
  min_est <- ranked_vals$estimate[ranked_vals$type == "min"]
  max_est <- ranked_vals$estimate[ranked_vals$type == "max"]
  min_se <- ranked_vals$se[ranked_vals$type == "min"]
  max_se <- ranked_vals$se[ranked_vals$type == "max"]
  
  subgroup_order <- .data$subgroup_order
  
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  strata_info <- select(dat, !!!syms(heatmeasures::HEAT_strata_variables)) %>% 
    distinct()
  
  run_simulation <- should_simulation_run(.data, validation_results)
  simulations <- NULL
  if(run_simulation | force_simulations) simulations <- do_simulation(.data, simulation_n)
  
  
  list(
    strata_info = strata_info,
    pop = pop,
    popsh = popsh,
    est = est,
    est_natl = est_natl,
    se = se,
    SEuseful = SEuseful,
    ordered = ordered,
    favourable = favourable,
    ascending = ascending,
    hasrefgroup = hasrefgroup,
    reference_subgroup = reference_subgroup,
    max_est = max_est,
    min_est = min_est,
    max_se = max_se,
    min_se = min_se,
    scaleval = scaleval,
    subgroup_order = subgroup_order,
    simulations = simulations
  )
  
}

#' Applies validation tests to a set of strata
#'
#' @param .data_split a list created by breaking up a dataset by strata
#' 
#' @param measures the set of measures
#'
#' @return
#' 
#' A list.
#' 
#' @export
HEAT_measures_validation_map <- function(.data_split, measures) {
  
  plan(multisession)
  
  #pb <- dplyr::progress_estimated(length(.data_split))
  
  validation_test_results <- furrr::future_map(.data_split, function(x) {
    
    #pb$tick()$print()
    
    dat_measures <- filter(measures, dimension == x$dimension[1]) %>% 
      pull(measure) %>% unlist()
    
    heatmeasures::HEAT_measures_validation_tests(x, dat_measures)
  }, .progress = TRUE)
  
  validation_test_results
}


#' Title
#'
#' @param .data_split 
#' @param validation_results 
#'
#' @return
#' @export
HEAT_measures_map <- function(.data_split, validation_results){
  #browser()
  # Changing from multiprocess because it's deprecated
  plan(multisession) #https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
  final_results <-furrr::future_map2_dfr(.data_split, 
                                  validation_results, 
                                  function(x, y){

                                    
                                    #pb$tick()$print()
                                    
                                    inequal_functions <- as.list(paste0("calc_", tolower(y$`Measures that pass`)))
                                    
                                    vals <- heatmeasures::HEAT_create_input_list(x, y)
                                    
                                    res <- suppressWarnings(purrr::map_dfr(inequal_functions, function(z) {
                                      
                                      func <- get(z)
                                      func(
                                        pop = vals$pop,
                                        popsh = vals$popsh,
                                        est = vals$est,
                                        est_natl = vals$est_natl,
                                        se = vals$se,
                                        SEuseful = vals$SEuseful,
                                        ordered = vals$ordered,
                                        favourable = vals$favourable,
                                        ascending = vals$ascending,
                                        hasrefgroup = vals$hasrefgroup,
                                        reference_subgroup = vals$reference_subgroup,
                                        max_est = vals$max_est,
                                        min_est = vals$min_est,
                                        max_se = vals$max_se,
                                        min_se = vals$min_se,
                                        scaleval = vals$scaleval,
                                        subgroup_order = vals$subgroup_order,
                                        simulations = vals$simulations
                                      )
                                    }))
                                    
        
                                    res <- cbind(vals$strata_info[rep(1, nrow(res)),], res) %>% 
                                      #rename(ccode = iso3) %>% 
                                      mutate(strata_id = x$strata_id[1],
                                             r_national = x$setting_average[1])
                                    
                                    
                                  }, .progress = TRUE)
  
  final_results <- mutate(final_results,
                          measure = toupper(measure))
  
}



#' Apply validation tests and compute inequality measures in one step
#'
#' @param .data main data as a data.frame or tibble
#'
#' @return
#' @export
HEAT_measures_full_process <- function(.data){
  

  library(future)
  if(!"strata_id"%in%names(.data)) stop("You need a 'strata_id' field")
  which_measures <- heatmeasures::HEAT_choose_measures_dataset(.data)
  
  .data_split <- .data %>% 
    split(.$strata_id) 
  
  message("Starting data validation process")
  validate_results <- heatmeasures::HEAT_measures_validation_map(.data_split, which_measures)
  
  some_passing_measures <- purrr::map_lgl(validate_results, function(x){
    length(x$`Measures that pass`) != 0
  })
  
  
  message("Starting measure calculations")
  .res <- heatmeasures::HEAT_measures_map(.data_split[some_passing_measures],
                    validate_results[some_passing_measures])
  
  message("Setting outliers to missing")
  
  heatmeasures::HEAT_measures_outlier2NA(.res)
  
  
}



#' Set outlying inequality measures to NA
#' 
#' @description Outliers are defined in the summary measures Excel file in data-raw. They are defined
#' solely on the value of inequal -- there is an inequal_min and inequal_max field. This function
#' identifies if inequal is in range between those and if not it sets inequal, se, se.lowerci
#' and se.upperci to NA.
#'
#' @param .data 
#'
#' @return
#' @export
HEAT_measures_outlier2NA <- function(.data){
  

  .data <- select(heatmeasures::HEAT_summary_measures, measure = measure_abbr, inequal_min, inequal_max) %>% 
    left_join(.data, ., by = "measure")
  
  
  out_of_range <- .data$inequal > .data$inequal_max | .data$inequal < .data$inequal_min | is.na(.data$inequal)
  
  .data[out_of_range, c("inequal", "se", "se.lowerci", "se.upperci")] <- NA
  
  # x <- mutate_at(.data, 
  #                  vars("inequal", "se", "se.lowerci", "se.upperci"), 
  #                  funs(ifelse(inequal>inequal_max | inequal < inequal_min, NA, .)))
  
  .data %>% select(-inequal_min, -inequal_max)
}




HEAT_subset_strata <- function(.data, setting1, year1, source1, dimension1, indicator_abbr1){
  filter(.data, 
         setting == setting1,
         year == year1,
         source == source1,
         dimension == dimension1,
         indicator_abbr == indicator_abbr1)
}
