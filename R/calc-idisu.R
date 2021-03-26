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

######### Index of Disparity (IDis)
# The Index of Disparity summarizes the difference between several group rates
# and a reference rate and expresses the average differences as a proportion of 
# the reference rate (Keppel & Pearcy, 2002: http://www.ncbi.nlm.nih.gov/pubmed/12432138).
# 
# 
#
#
#########

# relative, complex, not ordered, weighted, greater than two
# region

#' Title
#'
#' @param popsh 
#' @param est 
#' @param est_natl 
#' @param se 
#' @param SEuseful 
#' @param scaleval 
#' @param simulations 
#' @param ... 
#'
#' @return
#' @export
calc_idisu <- function(popsh, est, est_natl, se, SEuseful, scaleval, simulations, ...){

  
  # TO DO, do I need this?
  #if(any(badData) || is.na(est_natl)) return(na_return)
  
  est1 <- est / scaleval
  se1 <- se / scaleval
  est_natl1 <- est_natl/scaleval
  
  inequal.idis <- (sum(abs(est1 - est_natl1)) /(length(est1)))/est_natl1 * 100

  
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  if(SEuseful){
    
      inequal.boot.vals <- purrr::map_dbl(simulations, function(x){
        (sum(abs(x - sum(popsh * x))) /(length(x)))/sum(popsh * x)*100
      })
      
      
      # Method : instead of providing the SE, the 95% CIs were constructed by the quantiles
      # of the valid simulated values (the 2.5 and 97.5 percentiles)
      boot.lcl2 <- quantile(inequal.boot.vals, probs = c(0.025), na.rm = T)
      boot.ucl2 <- quantile(inequal.boot.vals, probs = c(0.975), na.rm = T)


  }
  
  return(tibble(measure = "idisu",
                inequal = inequal.idis, 
                se = NA, 
                se.lowerci = boot.lcl2, 
                se.upperci = boot.ucl2))
  
}
