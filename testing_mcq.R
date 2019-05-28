## MCQ test cases
source("mcq_calc.R")
# provided by https://link.springer.com/article/10.1007/s40614-016-0070-9

# these vectors are in the original item order (they are sorted in the above ms)
steep = c(0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1)
shallow = c(0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1)

mcq_calc(steep, Output = "SML")
mcq_calc(steep)
mcq_calc(shallow, Output = "SML")
mcq_calc(shallow)

# values from Kaplan et al Excel file (https://kuscholarworks.ku.edu/handle/1808/15424)
#               steep   shallow
# Ov k        0.06521	  0.00040
# small       0.02557	  0.00063
# med         0.06369	  0.00159
# large       0.06495	  0.00025
# geo_mean    0.04729	  0.00063

mcq = read.csv("mcq_testdata.csv") # check against Kaplan et al excel file

mcq_calc(mcq$X1, Output = "SML")
mcq_calc(mcq$X1, Output = "Consistency")


