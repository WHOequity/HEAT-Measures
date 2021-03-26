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

######### Mean Difference from the Mean (mdmu)

# absolute, complex, non ordered, weighted, greater than two
# region

#' Title
#'
#' @param pop 
#' @param est 
#' @param se 
#' @param SEuseful 
#' @param scaleval 
#' @param simulations 
#' @param ... 
#'
#' @return
#' @export
calc_mdmu <- function(pop, 
                      est, 
                      se, 
                      SEuseful,
                      scaleval,
                      simulations, ...){


  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  n <- length(est)
  
  # TODO: required?
  #if(any(badData) || is.na(est_natl)) return(na_return)
  
  est1 <- est / scaleval
  se1 <- se / scaleval
 
  inequal.mdmu <- sum(abs(est1 - sum(popsh * est1)))/n
  #inequal.mdmu <- sum(popsh * abs(est1 - sum(popsh * est1)))
  
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  if(SEuseful){

      inequal.boot.vals <- purrr::map_dbl(simulations, function(x){
        if ((scaleval == 100 & all(x >= 0) & all(x <= 1)) | (scaleval != 100 & all(x >= 0))) {
          # The weighted mean calculated for each simulation was used instead of the national average.
          return(scaleval* sum(abs(x - sum(popsh * x)))/n)
        }else{
          return(NA)
        }
      })
      
      
      boot.lcl2 <- quantile(inequal.boot.vals, probs = c(0.025), na.rm = T)
      boot.ucl2 <- quantile(inequal.boot.vals, probs = c(0.975), na.rm = T)
    
  }

  inequal.mdmu <- inequal.mdmu * scaleval

  
  # Return the results as a list
  return(tibble(measure = "mdmu",
                inequal = inequal.mdmu, 
                se = NA, 
                se.lowerci = boot.lcl2, 
                se.upperci = boot.ucl2))  # return a list of the inequality measure and the standard error 
  
}
