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

######### Between-Groups Variance (BGSD)

# absolute, complex, non ordered, weighted, greater than 2, no logscale
# region

#' Caculate BGSD
#'
#' @param popsh Popsh
#' 
#' @param est Est
#' 
#' @param est_natl Est national
#' 
#' @param se Standard
#' 
#' @param SEuseful A boolean
#' 
#' @param ... Other arguments.
#'
#' @return
#' 
#' A tibble.
#' 
#' @export
calc_bgsd <- function(popsh, est, est_natl, se, SEuseful, ...) {

  # TODO: do we need this?
  #if(any(badData) || is.na(est_natl)) return(na_return)
  
  inequal.bgsd <- sqrt(sum(popsh * (est - est_natl)^2))
  
  se.formula <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){
    
    # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
    # This formula is not going to be corrected if the bgsd estimate is based on
    # a national_est for the weighted.mean
    weighted.mean <- sum(popsh * est)
    p2__1_p2__se4 <- (popsh^2)*((1-popsh)^2)*(se^4)
    s4 <- (popsh^4)*(se^4)
    s2 <- (popsh^2)*(se^2)
    p2se2__y_mu__2 <- (popsh^2)*(se^2)*((est-weighted.mean)^2)  ################
    
    se.formula <- sqrt(4*(sum(p2se2__y_mu__2))+2*(((sum(s2))^2)-sum(s4)+sum(p2__1_p2__se4)))
    ci <- conf.int.norm(inequal.bgsd, se.formula)
    
  }
  
  
  
  return(list(measure = "bgsd", 
              inequal = inequal.bgsd, 
              se = se.formula, 
              se.lowerci = ci$l, 
              se.upperci = ci$u))  
}



