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

######### Relative Concentration Index (RCI)
# Is a measures of the covariance between social rank and health and measures the extent to which health/illness 
# is concentrated among groups on the absolute scale.  It may only be used with groups that have a natural ordering (p.9);
# e.g., income quintiles, levels of education, etc.
#
# Reference:
#########

# relative, complex, ordered, weighted, greater than two
# wealth, educ

#' Title
#'
#' @param pop 
#' @param popsh 
#' @param est 
#' @param est_natl 
#' @param se 
#' @param SEuseful 
#' @param ... 
#'
#' @return
#' @export
calc_rci <- function(pop,
                     popsh,
                     est,
                     est_natl,
                     se,
                     SEuseful,
                     subgroup_order,
                     ...){ 

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
  
  # added 100 * per git 374
  inequal.rci <- 100 * inequal.aci / est_natl
  
  se.formula <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){

    mid.point <- midPointProp(popsh) 
    sumprodsqrd <- sum(popsh * est)^2
    
    s2_s6 <- (popsh^2) * ((2 * mid.point - 1) - inequal.rci)^2 * se^2
    se.formula <- sqrt( sum(s2_s6) / sumprodsqrd )
    
    ci <- conf.int.norm(inequal.rci, se.formula)
  }
  

  
  return(tibble(measure = "rci",
                inequal = inequal.rci, 
                se = se.formula, 
                se.lowerci = ci$l, 
                se.upperci = ci$u))
}


