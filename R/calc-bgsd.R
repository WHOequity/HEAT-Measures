# © Copyright World Health Organization (WHO) 2016-2024.
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
#' @param se Standard error
#'
#' @param SEuseful A boolean
#'
#' @param scaleval Indicator scale
#'
#' @param ... Other arguments.
#'
#' @return
#'
#' A tibble.
#'
#' @export
calc_bgsd <- function(popsh, est, est_natl, se, SEuseful,scaleval, favourable, simulations,...) {
  
  inequal.bgsd <- sqrt(sum(popsh * (est - est_natl)^2))
  
  # Calculate 95% confidence intervals
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
  if(SEuseful){
    
    bgsd_sim <- sapply(simulations, function(one_sim){
      (sqrt(sum(popsh * (one_sim - est_natl)^2)))
    })
    
    boot.lcl2 <- quantile(bgsd_sim, probs = c(0.025), na.rm = T)
    boot.ucl2 <- quantile(bgsd_sim, probs = c(0.975), na.rm = T)
  }
  
  return(tibble(measure = "bgsd",
              inequal = inequal.bgsd,
              se = NA,
              se.lowerci = boot.lcl2,
              se.upperci = boot.ucl2))
}


