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

######### Mean Difference from the Reference Subgroup (mdrw)

# absolute, complex, non ordered, weighted, greater than two
# region

#' Title
#'
#' @param est
#' @param se
#' @param SEuseful
#' @param favourable
#' @param scaleval
#' @param simulations
#' @param reference_subgroup
#' @param ...
#'
#' @return
#' @export
calc_mdrw <- function(est,
                      se,
                      popsh,
                      SEuseful,
                      favourable,
                      scaleval,
                      simulations,
                      reference_subgroup, ...){
  
  ref_est <- est[reference_subgroup == 1] #dat$estimate[dat$reference_subgroup == 1], #git440
  
  est1 <- est / scaleval
  se1 <- se / scaleval
  ref_est1 <- ref_est / scaleval
  
  inequal.mdr <- sum(popsh * abs(ref_est1 - est1)) * scaleval
  
  # Calculate 95% confidence intervals
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  if(SEuseful){
    
    mdrw_sim <- sapply(simulations, function(one_sim){
      ref_estimate <- ifelse(favourable == 1, max(one_sim), min(one_sim))
      (sum(popsh * abs(ref_estimate - one_sim)))
    })
    
    boot.lcl2 <- quantile(mdrw_sim, probs = c(0.025), na.rm = T)
    boot.ucl2 <- quantile(mdrw_sim, probs = c(0.975), na.rm = T)
  }
  
  
  # Return the results as a list
  return(tibble(measure = "mdrw",
                inequal = inequal.mdr,
                se = NA,
                se.lowerci = boot.lcl2,
                se.upperci = boot.ucl2))
  
}