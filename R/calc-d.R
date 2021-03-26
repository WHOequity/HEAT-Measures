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

######### Rate Difference (RD)
# The rate difference is used to measure the extent to which one group is better (or worse) off than another group.  It is 
# implemented here in a way that ignores an a priori ordering and selects the two groups (from 2 plus groups) that have
# the best and worst outcomes.
#
# Reference: Ontario Agency for Health Protection and Promotion (Public Health Ontario). Summary measures of
#            socioeconomic inequalities in health. Toronto, ON: Queen’s Printer for Ontario; 2013. (p.17)
#########

# Absolute, simple, non ordered, unweighted, two groups, not logscale
# wealth, educ, area, sex, region

#' Caculate D
#'
#' @param max_est Max.
#' 
#' @param min_est Min.
#' 
#' @param max_se Max.
#' 
#' @param min_se Min.
#' 
#' @param ... 
#'
#' @return
#' 
#' A tibble.
#' 
#' @export
calc_d <- function(max_est,
                   min_est,
                   max_se,
                   min_se,
                   ...){

  inequal.d <-  max_est - min_est
  
  se.formula <- NA

  # note there is no usefulSE test see git 873
  se.formula <- sqrt(max_se^2 + min_se^2 ) 
  
  ci <- conf.int.norm(inequal.d, se.formula)
  
  # Return the results as a list
  return(tibble(measure = "d", 
                inequal = inequal.d, 
                se = se.formula, 
                se.lowerci = ci$l, 
                se.upperci = ci$u))

  }



