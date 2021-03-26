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

######### Absolute Concentration Index (ACI)
# Is a measures of the covariance between social rank and health. It measures the extent to which health/illness 
# is concentrated among groups on the absolute scale.  It may only be used with groups that have a natural ordering (p.9);
# e.g., income quintiles, levels of education, etc.
#
# Reference: Handbook on Health Inequality Monitoring, WHO (2013)
#########

# absolute, complex, ordered, weighted, greater than two, not logscale
# wealth, educ

#' Calculate ACI measure
#'
#' @param pop Pop.
#' 
#' @param popsh Popsh.
#' 
#' @param est Estimate.
#' 
#' @param se Se.
#' 
#' @param SEuseful A boolean?
#' 
#' @param ... Other arguments
#'
#' @return A tibble.
#' 
#' @export
calc_aci <- function(pop, popsh, est, se, SEuseful, subgroup_order, ...) { 

  #git 401
  if(!all(diff(subgroup_order) == 1)){
    reorder <- order(subgroup_order)
    pop <- pop[reorder]
    popsh <- popsh[reorder]
    se <- se[reorder]
    est <- est[reorder]
  }
  
  mid.point <- midPointProp(pop) 
  inequal.aci <- sum(popsh * (2*mid.point - 1) * est)
  
  se.formula <- NA
  ci <- list(l = NA, u = NA)
  
  
  if(SEuseful){
    # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
    se.formula <- sqrt(sum((popsh^2)*(2*midPointProp(popsh)-1)^2*se^2))
    ci <- conf.int.norm(inequal.aci, se.formula)
  }
  
  
  return(tibble(measure = "aci", 
                inequal = inequal.aci, 
                se = se.formula, 
                se.lowerci = ci$l, 
                se.upperci = ci$u))
}
