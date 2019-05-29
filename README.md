# MCQ_calc
# Calculates k value for the Monetary Choice Questionnaire
Based on 
Kaplan, B.A., Amlung, M., Reed, D.D. et al. BEHAV ANALYST (2016) 39: 293. 
https://doi.org/10.1007/s40614-016-0070-9

"An estimate of the respondent’s discounting rate can be calculated as 
the geometric mean (to avoid underweighting) of the k at indifference 
between the two questions that reflect when the respondent changes between 
choosing the delayed reward versus the immediate reward. In cases where 
the respondent’s change between preferring the delayed versus the immediate 
reward is not consistent, the two questions that are most proportional to 
their responses are chosen. If the participant always chooses the immediate 
reward or the delayed reward, the estimation of k is equal to one of the 
endpoints (0.25 or 0.00016)."

mcq_calc is the function and I've provided some examples for testing. The 
default Output of mcq_calc is the geometric mean of the 3 k values, one from
magnitude level. Two other Outputs are available. You can ask for "SML" which 
will give return the small, medium and large k values. You can also ask for 
"Consistency" to return the maximum consistency in each of the magnitudes. 
Consistency above .75 is preferred (see Kaplan et al).

