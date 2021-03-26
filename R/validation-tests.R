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

# NOTE, if you add a test you need to also add it to




#******************************************************************************
# Estimates all zero
#******************************************************************************

test_estimates_all_zero <- function(strata){
  all(strata$estimate[!is.na(strata$estimate)] == 0)
}


#******************************************************************************
# Test if dimension has 2 subgroups
#******************************************************************************
test_two_subgroups <- function(strata) {
  length(unique(strata$subgroup)) == 2
}


#******************************************************************************
# Test if dimension has 2 subgroups
#******************************************************************************
test_less_than2_subgroups <- function(strata) nrow(strata) < 2


#******************************************************************************
# Test if dimension has more than 2 subgroups
#******************************************************************************
test_more_than2_subgroups <- function(strata) nrow(strata) > 2


#******************************************************************************
# test if data is not ordered
#******************************************************************************
test_not_ordered <- function(strata) !is_ordered_dimension(strata)
test_is_ordered <- function(strata) is_ordered_dimension(strata)

#******************************************************************************
# tests if any estimates are missing
#******************************************************************************
test_missing_estimates <- function(strata){
  # strata <- dat
  est <- strata$estimate
  sum(!is.na(est)) != nrow(strata)
}

#******************************************************************************
# Tests if it's two subgroups and one is missing
#******************************************************************************
test_two_subgroups_and_missing <- function(strata){
  is2 <- test_two_subgroups(strata)
  miss <- test_missing_estimates(strata)
  is2 & miss
}

#******************************************************************************
# Test if a popshare value is missing
#******************************************************************************
test_missing_population <- function(strata){
  pop <- strata$population
  sum(!is.na(pop)) != nrow(strata) || sum(pop, na.rm=T) == 0
}

#******************************************************************************
# Test if the national average is missing
#******************************************************************************
test_missing_natl_avg <- function(strata){
  unique_natl <- unique(strata$national)
  length(unique_natl)!=1 | is.na(unique_natl)
}


#******************************************************************************
# National all zero
#******************************************************************************

test_natl_all_zero <- function(strata){
  all(strata$national == 0 | is.na(strata$national))
}



#******************************************************************************
# Has zero se
#******************************************************************************

test_has_zero_se <- function(strata){
  
  if(test_se_all_missing(strata)) return(FALSE)
  any(strata$se[!is.na(strata$se)] == 0)
}




#******************************************************************************
# SE all missing
#******************************************************************************

test_se_all_missing <- function(strata){
  all(is.na(strata$se))
}


#******************************************************************************
# d and r share a set of rules for pass/fail so rather than use
# a series of rules we'll use one combination rule. TRUE means fail
#******************************************************************************

# For dimensions with 2 subgroups: Return NA if any subgroup estimate is missing. 
# For ordered dimensions with >2 subgroups: Return NA if the estimate for the most-disadvantaged subgroup (min subgroup_order) or most-advantaged subgroup (max subgroup_order) are missing.
# For non-ordered dimensions with >2 subgroups: Return NA if any subgroup estimate is missing. 


test_d_r_fail <- function(strata){
  
  est <- strata$estimate
  subgroup_order <- strata$subgroup_order
  
  
  less2 <- test_less_than2_subgroups(strata)
  ord <- is_ordered_dimension(strata)
  two_groups <- test_two_subgroups(strata)
  more_groups <- test_more_than2_subgroups(strata)
  estimates.all.zero <- test_estimates_all_zero(strata)
  #natl.all.zero <- natl_all_zero(strata)
  if(estimates.all.zero) return(TRUE)
  if(less2) return(TRUE)
  #if(estimates.all.zero | natl.all.zero) return(TRUE)
  # if we have two subgroups and either is missing
  # an estimate return TRUE (fail)
  if(two_groups){
    if(any(is.na(est))) return(TRUE)
  }
  
  # if we have an ordered dimension and more than
  # two dimensions
  if(ord  & more_groups){
    eitherNA <- any(is.na(get_est_se_min_max(strata, userank = TRUE)$estimate))
    if(eitherNA) return(TRUE)
  }
  
  if(!ord & more_groups){
    anyNA <- any(is.na(est))
    if(anyNA) return(TRUE)
  }
  
  return(FALSE)
  
}


#******************************************************************************
# par, paf share a set of rules for pass/fail so rather than use
# a series of rules we'll use one combination rule. TRUE means fail
#******************************************************************************


test_par_paf_fail <- function(strata){
  #strata <- dat
  less2 <- test_less_than2_subgroups(strata)
  ord <- is_ordered_dimension(strata)
  est <- strata$estimate
  ref_group_exists <- reference_group_exists(strata)
  if(ref_group_exists) ref_group_indx <- which_is_refgroup(strata)
  est_se <- get_est_se_min_max(strata)
  est_max <- est_se$estimate[est_se$type=="max"]
  estimates.all.zero <- test_estimates_all_zero(strata)
  #natl.all.zero <- natl_all_zero(strata)
  
  # git845 manual computation
  popsh <- strata$population / sum(strata$population)
  est_natl <- sum(popsh * strata$estimate)
  
  if(estimates.all.zero | is.na(est_natl) | less2) return(TRUE)
  
  # If reference_subgroup is specified: 
  # Return NA if the estimate for the reference_subgroup is missing. 
  
  if(ref_group_exists && is.na(est[ref_group_indx])) return(TRUE)
  
  # If reference_subgroup is not specified: For ordered dimensions: 
  # Return NA if the estimate for the most-advantaged subgroup 
  # (max subgroup_order) is missing. 
  
  if(!ref_group_exists & ord){
    if(is.na(est_max)) return(TRUE)
  }
  
  
  # If reference_subgroup is not specified: 
  # For non-ordered dimensions: 
  # Return NA if any subgroup estimate is missing. 
  
  if(!ref_group_exists & !ord){
    if(any(is.na(est_se$estimate))) return(TRUE)
  }
  
  return(FALSE)
  
}










