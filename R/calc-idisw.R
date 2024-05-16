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

######### Mean Difference from the Mean (mdm)

# absolute, complex, non ordered, weighted, greater than two


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
calc_idisw <- function(popsh, est, est_natl, se, SEuseful, scaleval, simulations, ...){
  

  est1 <- est / scaleval
  se1 <- se / scaleval
  est_natl1 <- est_natl/scaleval
  
  inequal.idisw <- 100 * sum(popsh * abs(est1 - sum(popsh * est1)))/est_natl1
  
  # Calculate 95% confidence intervals
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  if(SEuseful){

    idisw_sim <- sapply(simulations, function(one_sim){ 
      100 * (sum(popsh * abs(one_sim - sum(popsh * one_sim)))/sum(popsh * 
                                                                    one_sim))
    })
    
    boot.lcl2 <- quantile(idisw_sim, probs = c(0.025), na.rm = T)
    boot.ucl2 <- quantile(idisw_sim, probs = c(0.975), na.rm = T)
  }
  
  return(tibble(measure = "idisw",
                inequal=inequal.idisw,
                se = NA,
                se.lowerci = boot.lcl2,
                se.upperci = boot.ucl2))  # return a list of the inequality measure and the standard error
  
}
