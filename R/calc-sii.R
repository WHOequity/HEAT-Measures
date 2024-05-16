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


#' Slope Index of Inequality (SII)
#' relative, complex, ordered, weighted, greater than two (wealth, educ)
#'
#' @param pop
#' @param est
#' @param se
#' @param SEuseful
#' @param favourable
#' @param scaleval
#' @param subgroup_order
#' @param ...
#'
#' @return
#' @export
calc_sii <- function(pop,
                     est,
                     se,
                     SEuseful,
                     favourable,
                     scaleval,
                     subgroup_order,
                     ...){
  
  
  #subgroup_new_order <- (favourable-0.5) * subgroup_order
  new_order <- order(subgroup_order)
  est <- est[new_order]
  pop <- pop[new_order]
  
  pop <- ceiling(pop) # to avoid population less than 1 become 0
  est <- round((est/scaleval) * pop)
  cest <- pop-est
  
  if(any(cest < 0 | est > pop | est < 0))
    return(tibble(measure = "sii",
                  inequal = NA,
                  se = NA,
                  se.lowerci = NA,
                  se.upperci = NA))
  
  # Compute the ranks
  rank <- midPointProp(pop)
  
  # Set up new data with the y/pop as y
  newdat <- data.frame(rank=rank, est=est, cest=cest, pop = pop)
  
  modelsii <- glm(formula = cbind(est, cest) ~ rank,
                  weights = pop, data = newdat,
                  family = binomial("logit"))
  
  siie <- marginaleffects::avg_comparisons(modelsii, comparison="difference",
                          variables = list(rank = c(0,1)),  vcov = "HC1")
  
  inequal.sii <- siie$estimate
  se.formula <- siie$std.error
  ci <- list(l = siie$conf.low, u = siie$conf.high)
  
  # Return the results as a list
  return(tibble(measure = "sii",
                inequal = inequal.sii*scaleval,
                se = se.formula,
                se.lowerci = ci$l*scaleval,
                se.upperci = ci$u*scaleval))  # return a list of the inequality measure and the standard error
}
