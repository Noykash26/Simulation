# ------------------------------------- read vectors --------------------------------------------
## Current state
current_state <- read.csv(choose.files(), header=TRUE)

## Alternative 1
Alternative_1 <- read.csv(choose.files(),header=TRUE)

## Alternative 2
Alternative_2 <- read.csv(choose.files(),header=TRUE)

# --------------------------------------- functions -----------------------------------------
find_difference_vec <- function(vec1,vec2){
  difVec <- c()
  for (i in 1:30){
    difVec[i] <- vec1[i]-vec2[i]
  }
  return (difVec)
} # a function that returns the differece between two vectors

# ----------------------------------- השוואה בין חלופות ---------------------------------------
# assmptions
# 1. יש תלות בין ריצות מקבילות בשתי הסדרות - set.seed זהה
# 2. אין תלות בין ריצות בכל חלופה בפני עצמה - set.seed(const+i) 
# 3. ניתן להניח שוויון שונויות בין הסדרות.
# 4. התוצאות מתפלגות נורמלית. 

# step 1 - calculate the difference between each to parallel runs:
# מספר ההשוואות הכולל = מספר ההשוואות * מספר המדדים = 3 * 2 = 6

dif_abandon_p_a1 <- find_difference_vec(current_state[,2], Alternative_1[,2])
dif_votes_p_a1 <- find_difference_vec(current_state[,4], Alternative_1[,4])

dif_abandon_p_a2 <- find_difference_vec(current_state[,2], Alternative_2[,2])
dif_votes_p_a2 <- find_difference_vec(current_state[,4], Alternative_2[,4])

dif_abandon_a1_a2 <- find_difference_vec(Alternative_1[,2], Alternative_2[,2])
dif_votes_a1_a2 <- find_difference_vec(Alternative_1[,4], Alternative_2[,4])

# step 2 - calculations of average and sd to the difference from step 1:

mean_abandon_p_a1 <- mean(dif_abandon_p_a1)
sd_abandon_p_a1 <- sd(dif_abandon_p_a1)

mean_votes_p_a1 <- mean(dif_votes_p_a1)
sd_votes_p_a1 <- sd(dif_votes_p_a1)
  
mean_abandon_p_a2 <- mean(dif_abandon_p_a2)
sd_abandon_p_a2 <- sd(dif_abandon_p_a2)
  
mean_votes_p_a2 <- mean(dif_votes_p_a2)
sd_votes_p_a2 <- sd(dif_votes_p_a2)
  
mean_abandon_a1_a2 <- mean(dif_abandon_a1_a2)
sd_abandon_a1_a2 <- sd(dif_abandon_a1_a2)
 
mean_votes_a1_a2 <- mean(dif_votes_a1_a2)
sd_votes_a2_a2 <- sd(dif_votes_a1_a2)

# step 3 - calculate alpha i -> alpha_total/num of comparisions
alpha_i <- 0.1/6

# step 4 - paired t test

paird_test1 <- t.test(x=current_state[,2],y=Alternative_1[,2],alternative = "two.sided", paired = T, var.equal = T, conf.level = 1-alpha_i)
print(paird_test1)
paird_test2 <- t.test(x=current_state[,4],y=Alternative_1[,4],alternative = "two.sided", paired = T, var.equal = T, conf.level = 1-alpha_i)
print(paird_test2)
paird_test3 <- t.test(x=current_state[,2],y=Alternative_2[,2],alternative = "two.sided", paired = T, var.equal = T, conf.level = 1-alpha_i)
print(paird_test3)
paird_test4 <- t.test(x=current_state[,4],y=Alternative_2[,4],alternative = "two.sided", paired = T, var.equal = T, conf.level = 1-alpha_i)
print(paird_test4)
paird_test5 <- t.test(x=Alternative_1[,2],y=Alternative_2[,2],alternative = "two.sided", paired = T, var.equal = T, conf.level = 1-alpha_i)
print(paird_test5)
paird_test6 <- t.test(x=Alternative_1[,4],y=Alternative_2[,4],alternative = "two.sided", paired = T, var.equal = T, conf.level = 1-alpha_i)
print(paird_test6)

# באופו מובהק חלופה 2 עדיפה על פני חלופה 1 ועל פני המצב הקיים