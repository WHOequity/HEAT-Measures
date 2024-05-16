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

######### Population Attributable Fraction (PAF)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########

# relative complex, non ordered, weighted, two groups
# wealth, educ, area, sex, region



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
calc_paf <- function(pop,
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
                     scaleval,
                     ...){
  
  popsh <- pop / sum(pop)
  #est_natl <- get_weighted_mean(est, popsh, est_natl)
  
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
  # END wrap par
  
  inequal.paf <- (inequal.par/est_natl) * 100
  
  if(favourable & inequal.paf < 0) inequal.paf  <- 0
  if(!favourable & inequal.paf > 0) inequal.paf <- 0
  
  se.formula <- NA
  ci <- list(l = NA, u = NA)
  
  mu <- est_natl
  ref_population = pop[est==ref_est]
  
  c <- (ref_est / scaleval) * ref_population
  d <- ref_population - c
  a <- (mu / scaleval) * sum(pop) - c
  b <- sum(pop) - a - ref_population
  N <- a + b + c + d
  se.formula <- sqrt((c * N * (a * d * (N-c) + b * c^2)) / ((a+c)^3 * (c+d)^3))
  ci <- conf.int.norm(inequal.paf, se.formula)
  
  
  # Return the results as a list
  return(tibble(measure = "paf",
                inequal = inequal.paf,
                se = se.formula,
                se.lowerci = ci$l,
                se.upperci = ci$u))
  
}

