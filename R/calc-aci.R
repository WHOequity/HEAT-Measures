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

######### Absolute Concentration Index (ACI)
# Is a measures of the covariance between social rank and health. It measures the extent to which health/illness 
# is concentrated among groups on the absolute scale.  It may only be used with groups that have a natural ordering (p.9);
# e.g., income quintiles, levels of education, etc.
#
# Reference: Handbook on Health Inequality Monitoring, WHO (2013)
#########

# absolute, complex, ordered, weighted, greater than two, not logscale
# wealth, educ

#' Calculate ACI measure
#'
#' @param pop Pop.
#'  
#' @param est Estimate.
#' 
#' @param ... Other arguments
#'
#' @return A tibble.
#' 
#' @export
calc_aci <- function(pop, 
                     est, 
                     subgroup_order, ...) { 

  #git 401
  if(!all(diff(subgroup_order) == 1)){
    reorder <- order(subgroup_order)
    pop <- pop[reorder]
    est <- est[reorder]
  }
  
  #git 1004 Update ACI measure
  sumw <- sum(pop, na.rm = TRUE)
  cumw <- cumsum(pop)
  intercept <-sqrt(pop)
  cumw1 <- dplyr::lag(cumw)
  cumw1[is.na(cumw1)] <- 0
  newdat_aci <- as.data.frame(cbind(est,
                                    pop,
                                    subgroup_order,
                                    sumw,
                                    cumw,
                                    cumw1,
                                    intercept))
  newdat_aci <- newdat_aci %>%
    group_by(subgroup_order) %>%
    mutate(cumwr = max(.data$cumw, na.rm = TRUE),
           cumwr1 = min(.data$cumw1, na.rm = TRUE)) %>%
    ungroup()
  rank <- (newdat_aci$cumwr1 + 0.5 *
             (newdat_aci$cumwr-newdat_aci$cumwr1)) / newdat_aci$sumw
  tmp <- (newdat_aci$pop / newdat_aci$sumw) * ((rank - 0.5)^2)
  sigma1 <- sum(tmp)
  tmp1 <- newdat_aci$pop*newdat_aci$est
  meanlhs <- sum(tmp1)
  meanlhs1 <- meanlhs/newdat_aci$sumw
  lhs <- (sigma1 * 2 * (newdat_aci$est / meanlhs1) * newdat_aci$intercept)
  lhs1 <- lhs * meanlhs1
  rhs <- rank * newdat_aci$intercept
  newdat_aci <- as.data.frame(cbind(newdat_aci, lhs, lhs1, rhs))
  
  # Calculate ACI
  mod <- glm(lhs1 ~ 0 + rhs + intercept,
             family = gaussian,
             data = newdat_aci)
  
  inequal.aci <- mod$coefficients[[1]]
  
  # Calculate 95% confidence intervals
  se.formula <- sqrt(diag(vcov(mod)))[[1]] 
  lowerci <- inequal.aci - se.formula * qnorm(0.975)
  upperci <- inequal.aci + se.formula * qnorm(0.975)
  
  return(tibble(measure = "aci", 
                inequal = inequal.aci, 
                se = se.formula, 
                se.lowerci = lowerci, 
                se.upperci = upperci))
}
