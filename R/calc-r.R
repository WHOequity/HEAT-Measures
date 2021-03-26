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

######### Rate Ratio (RR)
# The rate Ratio is used to measure the extent to which one group is relatively better (or worse) off than another group.  It is 
# implemented here in a way that ignores an a priori ordering and selects the two groups (from 2 plus groups) that have
# the best and worst outcomes.
#
# Reference: Ontario Agency for Health Protection and Promotion (Public Health Ontario). Summary measures of
#            socioeconomic inequalities in health. Toronto, ON: Queen’s Printer for Ontario; 2013. (p.17)
#########

# Absolute, simple, non ordered, unweighted, two groups, not logscale
# wealth, educ, area, sex, region

#' Title
#'
#' @param ordered 
#' @param favourable 
#' @param max_est 
#' @param min_est 
#' @param max_se 
#' @param min_se 
#' @param ... 
#'
#' @return
#' @export
calc_r <- function(ordered,
                   favourable,
                   max_est,
                   min_est,
                   max_se,
                   min_se,
                   ...){

  if(!ordered) ascending <- TRUE
  if(ordered) ascending <- favourable

  inequal.r <-  max_est/min_est

  
  se.formula <- NA

  # note there is no usefulSE test see git 873
  se.formula <- sqrt((1/(min_est^2)) * ((max_se^2) + (inequal.r^2) * ((min_se^2))))
  
  # New git 203
  ci <- conf.int.norm(log(inequal.r), se.formula/inequal.r)
  
  # New git 203
  ci$l <- exp(ci$l)
  ci$u <- exp(ci$u)
  
  
  
  if(is.infinite(inequal.r)){
    inequal.r <- NA
    se.formula <- NA
    ci$l <- NA
    ci$u <- NA
  }
  
  return(tibble(measure = "r",
                inequal = inequal.r, 
                se = se.formula, 
                se.lowerci = ci$l, 
                se.upperci = ci$u))  
  
}
