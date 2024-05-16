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

######### Between-Groups Variance (cov)
# absolute, complex, non ordered, weighted, greater than 2, no logscale
# region

#' Title
#'
#' @param popsh
#' @param est
#' @param est_natl
#' @param se
#' @param SEuseful
#' @param scaleval
#' @param ...
#'
#' @return
#' @export
calc_cov <- function(popsh, est, est_natl, se, SEuseful, scaleval, simulations,...){
  
  # TODO is this needed
  #if(any(badData) || is.na(est_natl)) return(na_return)
  
  inequal.cov <- 100 * sqrt(sum(popsh * (est - est_natl)^2))/est_natl
  
  # Calculate 95% confidence intervals
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  if(SEuseful){

    cov_sim <- sapply(simulations, function(one_sim){
      (100 * sqrt(sum(popsh * (one_sim - est_natl)^2))/est_natl)
    })
    
    boot.lcl2 <- quantile(cov_sim, probs = c(0.025), na.rm = T)
    boot.ucl2 <- quantile(cov_sim, probs = c(0.975), na.rm = T)
  }
  
  return(tibble(measure = "cov",
                inequal = inequal.cov,
                se = NA,
                se.lowerci = boot.lcl2,
                se.upperci = boot.ucl2))
}


