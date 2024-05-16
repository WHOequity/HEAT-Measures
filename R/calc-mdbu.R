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

######### Mean Difference from the Best performing Subgroup (mdbu)

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
#' @param ...
#'
#' @return
#' @export
calc_mdbu <- function(est,
                      se,
                      SEuseful,
                      favourable,
                      scaleval,
                      simulations,...){
  
  ref_est <- ifelse(favourable, max(est), min(est))
  
  est1 <- est / scaleval
  se1 <- se / scaleval
  ref_est1 <- ref_est / scaleval
  n <- length(est)
  
  inequal.mdbu <- sum(abs(ref_est1-est1))/n
  inequal.mdbu <- inequal.mdbu * scaleval
  #inequal.mdbu <- sum(popsh * abs(ref_est1-est1))
  
  # Calculate 95% confidence intervals
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  if(SEuseful){
    
    mdbu_sim <- sapply(simulations, function(one_sim){
      
      ref_estimate <- ifelse(favourable == 1, max(one_sim), min(one_sim))
      (sum(abs(ref_estimate-one_sim))/n)
    })
    
    boot.lcl2 <- quantile(mdbu_sim, probs = c(0.025), na.rm = T)
    boot.ucl2 <- quantile(mdbu_sim, probs = c(0.975), na.rm = T)
  }
  
  return(tibble(measure = "mdbu",
                inequal = inequal.mdbu,
                se = NA,
                se.lowerci = boot.lcl2,
                se.upperci = boot.ucl2))
  
}