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

######### Theil Index
# http://en.wikipedia.org/wiki/Theil_index
#########

#' Title
#'
#' @param popsh
#' @param est
#' @param est_natl
#' @param se
#' @param SEuseful
#' @param ...
#'
#' @return
#' @export
calc_ti <- function(popsh,
                    est,
                    est_natl,
                    se,
                    SEuseful,
                    ...){
  
  # TODO: required?
  #if(any(badData) || is.na(est_natl)) return(na_return)
  est_nonzero <- ifelse(est==0, 0.000001, est)
  rj <- est_nonzero / est_natl
  # added 1000 * per git 374
  inequal.ti <- 1000 * sum(popsh * rj * log(rj))
  
  se.formula <- NA
  
  if(SEuseful){
    mu <- sum(popsh * est_nonzero)
    rj <- est_nonzero / mu
    s_u <- popsh * rj * (1 + log(rj))
    ti.se.indiv <- ((1 + log(rj) - sum(s_u))^2)*((popsh^2)*(se^2)/(mu^2))
    se.formula <- sqrt(sum(ti.se.indiv))
  }
  
  ci <- conf.int.norm(inequal.ti, se.formula)
  
  return(tibble(measure = "ti",
                inequal = inequal.ti,
                se = se.formula,
                se.lowerci = ci$l,
                se.upperci = ci$u))
}
