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

######### Mean Log Deviation (MLD)
# Mean Log Deviation is a measures of general disproportionality, developed
# by the economist Henri Theil (10)
#
#########

#relative, complex, non ordered, weighted, greater than two
# region

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
calc_mld <- function(popsh,
                     est,
                     est_natl,
                     se,
                     SEuseful, ...){
  
  #TODO: required?
  #if(any(badData) || is.na(est_natl)) return(na_return)
  
  est_nonzero <- ifelse(est==0, 0.000001, est)
  # added 1000 * per git 374
  inequal.mld <- 1000 * sum(popsh * -log(est_nonzero/est_natl))
  
  se.formula <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){
    
    # The SE of MLD
    
    # The xls formula for the se component of each group's estimate:
    # =(((H2^2)*(Y2^2))/(SUMPRODUCT(G2:G6,Y2:Y6)^2))*((1-(1/CF2))^2)
    wgt.mean <- weighted.mean(est_nonzero, popsh) # Calculate the weighted mean, because it is re-used
    
    # Element 1: ((H2^2)*(Y2^2))
    #pop.prop <- w/sum(w) # Y is the proportion weight of each group
    el1 <- se^2 * popsh^2 # H is the vector of se's
    
    # Element 2: (SUMPRODUCT(G2:G6,Y2:Y6)^2))  # The sumproduct is the weighted mean
    el2 <- (wgt.mean)^2  # square of the weighted mean
    # Element 3: ((1-(1/CF2))^2)  # CF2 is the ratio of each x/weighted.mean(x)
    rj <- est_nonzero/wgt.mean
    el3 <- (1-(1/rj))^2
    # Element 4: combine elements 1..3 according to the xls formula
    el4 <- (el1/el2)*el3
    # Return the combined se elements
    se.formula <- sqrt(sum(el4))
    
    
    if(is.nan(se.formula)){
      se.formula <- NA
    }
    ci <- conf.int.norm(inequal.mld, se.formula)
  }
  
  
  
  # Return the results as a list
  return(tibble(measure = "mld",
                inequal = inequal.mld,
                se = se.formula,
                se.lowerci = ci$l,
                se.upperci = ci$u))
  
}
