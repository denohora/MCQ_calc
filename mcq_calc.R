# What does this function do?

# Calculates k value for the Monetary Choice Questionnaire
# https://link.springer.com/article/10.1007/s40614-016-0070-9
# An estimate of the respondent’s discounting rate can be calculated as 
# the geometric mean (to avoid underweighting) of the k at indifference 
# between the two questions that reflect when the respondent changes between 
# choosing the delayed reward versus the immediate reward. In cases where 
# the respondent’s change between preferring the delayed versus the immediate 
# reward is not consistent, the two questions that are most proportional to 
# their responses are chosen. If the participant always chooses the immediate 
# reward or the delayed reward, the estimation of k is equal to one of the 
# endpoints (0.25 or 0.00016).

# data 
# a vector containing 
# 0s (i.e., selection of the SIR)
# 1s (i.e., selection of the LDR) 
# in the original MCQ item order

mcq_calc = function(data, Output = "Geomean_k"){

# First, the 27 items are ordered based on their associated k values, starting with 
# the smallest k values (Table 1). 

  item = c(13,  1,  9, 20,  6, 17, 26, 24, 12, 22, 16, 15,  3, 10,  2, 18, 21, 25,  5, 14, 
           23,  7,  8, 19, 11, 27,  4)
  magnitude = rep(c("Small",  "Medium", "Large"), 9) 
  k_val = c(0.000158128, 0.000158278, 0.000158278, 0.000399042, 0.000398936, 0.000398089, 0.001002674, 
            0.001001001, 0.001003386, 0.002500000, 0.002522357, 0.002548176, 0.005958292, 0.006048387,
            0.005961252, 0.015804598, 0.015686275, 0.016049383, 0.041353383, 0.040564374, 0.041463415,
            0.102564103, 0.100000000, 0.101731602, 0.246753247, 0.250000000, 0.248847926)
  
  mcq.vars = data.frame(item, magnitude, k_val)
  
  # geo mean function (https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in)
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  
  ## Consistency
  # For each item, the degree to which the respondents’ selections are consistent 
  # with response patterns preceding, as well as following, the switch(es) from SIR 
  # choices to LDR choices is calculated. 
  
  # A consistency score is determined by counting 
  # the instances of 0s (i.e., selection of the SIR) prior to the given k value and 
  # instances of 1s (i.e., selection of the LDR) at and following the given k value. 
  # This number is then divided by the number of items possible (27 in the case of overall; 
  # 9 in the case of each of the three magnitudes). The larger the number, the more consistent
  # the response pattern.
  
  # sort the data from the mcq responses
  sorted = data[mcq.vars$item] 
  # 
  sorted.mat = cbind(sorted[magnitude=="Small"], 
                     sorted[magnitude=="Medium"], 
                     sorted[magnitude=="Large"])
  
  # calculate consistency
  consistency = matrix(0, nrow = 9, ncol = 3)
  
  for(mag.ix in 1:3){
    consistency[1, mag.ix] = sum(sorted.mat[1:9,mag.ix]==1)/9
    for(item.ix in 2:9) consistency[item.ix, mag.ix] = (sum(sorted.mat[1:(item.ix-1),mag.ix]==0) + sum(sorted.mat[item.ix:9,mag.ix]==1))/9
    #consistency[9, mag.ix] = sum(sorted.mat[item.ix:9,mag.ix]==1))/9
  }# mag
  
  # identify most consistent k vals
  k.mat = cbind(mcq.vars$k_val[mcq.vars$magnitude=="Small"],
                mcq.vars$k_val[mcq.vars$magnitude=="Medium"],
                mcq.vars$k_val[mcq.vars$magnitude=="Large"])
  
  k.vals.mag = rep(0,3)
  
  
  for(mag.ix in 1:3){
    # If the participant always chooses the immediate 
    # reward or the delayed reward, the estimation of k is equal to one of the 
    # endpoints (0.25 or 0.00016).
    if(sum(sorted.mat[,mag.ix]==9)) k.vals.mag[mag.ix] = 0.00016
    if(sum(sorted.mat[,mag.ix]==0)) k.vals.mag[mag.ix] = 0.25
          
    if(sum(sorted.mat[,mag.ix] %in% c(0,9) ==F)){
      max.con = which(consistency[, mag.ix] == max(consistency[, mag.ix]))
      # If the highest consistency score occurs only once, then the geometric mean between that k value and the k 
      # value immediately preceding it is returned to estimate that value as the respondent’s k value.
      if(length(max.con)==1) k.vals.mag[mag.ix] = gm_mean( c(k.mat[max.con, mag.ix], k.mat[(max.con-1), mag.ix]))    
      # In cases where the highest consistency score occurs more than once, the geometric mean of k values at
      # each of the items associated with those consistency scores determines the final k value 
      # (similar to the process described above). 
      if(length(max.con)>1){
        k.vals = rep(0, length(max.con))
        for(max.ix in 1:length(max.con)){ # get geo mean for each max.con
          k.vals[max.ix] = gm_mean( c(k.mat[max.con[max.ix], mag.ix], k.mat[(max.con[max.ix]-1), mag.ix]))
          } # for max.ix 
          k.vals.mag[mag.ix] = gm_mean( k.vals)  #   
        } # if length      
      } # if sum 
    
    }# mag.ix
  
  if(Output == "Geomean_k") return(gm_mean(k.vals.mag))
  if(Output == "SML") return(k.vals.mag)

}


## test cases

# provided by https://link.springer.com/article/10.1007/s40614-016-0070-9

# these vectors are in the original item order (they are sorted in the above ms)
# steep = c(0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1)
# shallow = c(0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1)
# 
# mcq_calc(steep, Output = "SML")
# mcq_calc(steep)
# mcq_calc(shallow, Output = "SML")
# mcq_calc(shallow)

# values from Kaplan et al Excel file (https://kuscholarworks.ku.edu/handle/1808/15424)
#               steep   shallow
# Ov k        0.06521	  0.00040
# small       0.02557	  0.00063
# med         0.06369	  0.00159
# large       0.06495	  0.00025
# geo_mean    0.04729	  0.00063

