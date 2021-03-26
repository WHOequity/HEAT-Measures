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

######### Population Attributable Risk (PAR)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########
     
#' Title
#'
#' @param pop 
#' @param est 
#' @param est_natl 
#' @param se 
#' @param SEuseful 
#' @param ordered 
#' @param favourable 
#' @param hasrefgroup 
#' @param reference_subgroup 
#' @param max_est 
#' @param min_est 
#' @param subgroup_order 
#' @param ... 
#'
#' @return
#' @export
calc_par <- function(pop,
                     est,
                     est_natl,
                     se,
                     SEuseful,
                     ordered,
                     favourable,
                     hasrefgroup,
                     reference_subgroup,
                     max_est,
                     min_est,
                     subgroup_order,
                     ...){

  refgroup_index <- which(reference_subgroup == 1)
  most_advantaged_indx <- which.max(subgroup_order)
  min_est_indx <- which(est == min_est)
  
  # git 322
  most_advantaged_indx <- most_advantaged_indx[1]
  min_est_indx <- min_est_indx[1]

  if(ordered & !hasrefgroup) ref_est <- est[most_advantaged_indx]
  if(!ordered & !hasrefgroup) ref_est <- ifelse(favourable, max_est, min_est)
  if(hasrefgroup) ref_est <- est[refgroup_index]
  
  inequal.par <- ref_est - est_natl
  
  if(favourable & inequal.par < 0) inequal.par  <- 0
  if(!favourable & inequal.par > 0) inequal.par <- 0

  se.formula <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){

    mu<-weighted.mean(est, pop) #af
    
    co6<-pop[min_est_indx] - (min_est/100)*pop[min_est_indx]
    cp6<-(min_est/100)*pop[min_est_indx]
    cq6 <- (mu/100)*sum(pop)-cp6
    cr6 <- sum(pop)-cq6-cp6-co6
    cv6 <- mu-min_est
    cx6 <- cv6/mu
    cs6<-sqrt((cr6+cx6*(cq6+co6))/((sum(pop)*cp6)))
    
    ct6<-(log(1-cx6))-qnorm(0.975)*cs6
    cu6<-(log(1-cx6))+qnorm(0.975)*cs6
    cy6<-abs((1-exp(ct6))-(1-exp(cu6)))/(2*(qnorm(0.975)))
    
    se.formula <- abs(mu*((cx6+qnorm(0.975)*cy6)-(cx6-qnorm(0.975)*cy6))) / (2*qnorm(0.975))
    ci <- conf.int.norm(inequal.par, se.formula)
    
  }
  
  
  return(tibble(measure = "par",
                inequal = inequal.par, 
                se = se.formula, 
                se.lowerci = ci$l, 
                se.upperci = ci$u)) 
  
}
