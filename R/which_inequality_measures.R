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

#' A function to identify which inequality measures to use for a strata
#'
#' @param strata one strata worth of data as a data.frame or tibble
#' @return
#' @export
#' @examples
#' filter(sample_strata, strata_id == sample_strata$strata_id[1]) %>% 
#' which_inequality_measures()
HEAT_choose_measures_strata <- function(strata){
  # Originally which_inequal()

  has_two_subgroups <- test_two_subgroups(strata)
  ordered_data <- test_is_ordered(strata)
  
  measures <- heatmeasures::HEAT_summary_measures_types %>% 
    filter(just2_subgroups == has_two_subgroups,
         ordered == ordered_data) %>% 
    pull(measure)

  res <- strata %>%
    select(!!!strata_variables) %>% 
    distinct() %>% 
    mutate(inequal_measures = list(measures))
    
  res
  
}


#' A function to identify which inequality measures to use for a full dataset based on dimensions
#'
#' @param .data a full dataset, potentially with many dimensions
#'
#' @return
#' @export
HEAT_choose_measures_dataset <- function(.data){

  tmp <- select(.data, dimension, subgroup, ordered_dimension) %>% 
    distinct() %>% 
    group_by(dimension, ordered_dimension) %>% 
    summarize(count_subgroup = n()) %>% 
    mutate(just2_subgroups = count_subgroup == 2,
           ordered = ordered_dimension == 1)
  
  measures <- heatmeasures::HEAT_summary_measures_types[,c("measure", "just2_subgroups", "ordered")]
  tmp <- inner_join(tmp, measures, by = c("just2_subgroups", "ordered"))
  

  tmp %>% 
    select(dimension, measure) %>% 
    distinct() %>% 
    group_by(dimension) %>% 
    summarize(measure = list(measure))
  
  
}