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
  
  
  subgroup_new_order <- (favourable-0.5) * subgroup_order
  new_order <- order(subgroup_new_order)
  est <- est[new_order]
  pop <- pop[new_order]
  
  # TODO: required?
  #if(any(badData) || !SEuseful) return(na_return)
  
  est <- round((est/scaleval) * pop)
  pop <- round(pop)
  
  # Compute the ranks
  rank <- midPointProp(pop)
  
  # Set up new data with the y/pop as y
  newdat <- data.frame(rank=rank, est=est, pop = pop)
  
  # there must be a better way than this. Looks like it computes the
  # model twice, but we want to know if there's a warning
  
  res <- suppressWarnings(try(metafor::rma.glmm(measure="PLO", mods = ~rank, xi=est, ni=pop, method="FE",data=newdat), TRUE))
  
  model_error <- any(class(res) == "try-error")
  
  inequal.sii <- NA
  se.formula <- NA
  ci <- list(l = NA, u = NA)
  
  if(!model_error){
    
    model2 <- res
    summary.info <- coef(summary(model2))
    coefs <- summary.info[,"estimate"]
    
    alpha <- summary.info["intrcpt","estimate"]
    se.alpha <- summary.info["intrcpt","se"]
    
    beta <- summary.info["rank","estimate"]
    se.beta <- summary.info["rank","se"]
    
    vcov <- metafor::vcov.rma(model2)
    q.val <- qnorm(0.975)
    
    # predicted values at bottom and top of rank
    p1 <- exp(alpha + beta) / (1 + exp(alpha + beta))
    p0 <- exp(alpha) / (1 + exp(alpha))
    
    
    if(favourable){
      
      SII <- scaleval * (p1 - p0)  
      
      SIIse <- scaleval * msm::deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                                      coefs, vcov)
    }
    
    if(!favourable){
      SII <- scaleval * (p0 - p1) 
      SIIse <- scaleval * msm::deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                                      coefs, vcov)
    }
    
    
    inequal.sii <- SII
    se.formula <- SIIse
    
    if(SEuseful){
      ci <- conf.int.norm(inequal.sii, se.formula)
    }
  }
  
  
  
  
  
  
  
  
  # Return the results as a list
  return(tibble(measure = "sii",
                inequal = inequal.sii, 
                se = se.formula, 
                se.lowerci = ci$l, 
                se.upperci = ci$u))  # return a list of the inequality measure and the standard error 
}
