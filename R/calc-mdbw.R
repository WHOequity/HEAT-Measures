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

######### Mean Difference from the Best performing Subgroup (mdb)

# absolute, complex, non ordered, weighted, greater than two
# region

#' Title
#'
#' @param popsh 
#' @param est 
#' @param se 
#' @param SEuseful 
#' @param favourable 
#' @param hasrefgroup 
#' @param scaleval 
#' @param simulations 
#' @param ... 
#'
#' @return
#' @export
calc_mdbw <- function(popsh, 
                      est, 
                      se, 
                      SEuseful,
                      favourable,
                      hasrefgroup,
                      scaleval,
                      simulations,
                      reference_subgroup, ...){
  
  if(!hasrefgroup) ref_est <- ifelse(favourable, max(est), min(est))
  if(hasrefgroup) ref_est <- est[reference_subgroup == 1] #dat$estimate[dat$reference_subgroup == 1], #git440
  
  est1 <- est / scaleval
  se1 <- se / scaleval
  ref_est1 <- ref_est / scaleval
  
  inequal.mdb <- sum(popsh * abs(ref_est1 - est1)) * scaleval
  
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  
  
  if(SEuseful){
    
    inequal.boot.vals <- purrr::map_dbl(simulations, function(x){
      if ((scaleval == 100 & all(x >= 0) & all(x <= 1)) | (scaleval != 100 & all(x >= 0))) {
        # The weighted mean calculated for each simulation was used instead of the national average.
        return(scaleval * sum(popsh * abs(ref_est1 - x)))
        
      }else{
        return(NA)
      }
      
    })
    
    boot.lcl2 <- quantile(inequal.boot.vals, probs = c(0.025), na.rm = T)
    boot.ucl2 <- quantile(inequal.boot.vals, probs = c(0.975), na.rm = T)
    
  }
  
  # Return the results as a list
  return(tibble(measure = "mdbw",
                inequal = inequal.mdb, 
                se = NA, 
                se.lowerci = boot.lcl2, 
                se.upperci = boot.ucl2))  
  
}
