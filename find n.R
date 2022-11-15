
## Current state
current_state <- read.csv(choose.files(), header=TRUE)

## Alternative 1
Alternative_1 <- read.csv(choose.files(),header=TRUE)

## Alternative 2
Alternative_2 <- read.csv(choose.files(),header=TRUE)

## Data for calculations
gamma<-0.12
alfa<-0.1
alfa_i<-alfa/2
n<-30

#----------------------------- current state -------------------------------------------#
# abandon
test_abandon_current <- t.test(x = current_state$abandon_measure,y=NULL, alternative="two.sided",conf.level=0.95)
avg_current_abandon<-test_abandon_current$estimate
# רווח סמך חלקי
delta_current_abandon<-test_abandon_current$conf.int[2]-avg_current_abandon
# דיוק יחסי
relative_accuracy_current <- delta_current_abandon/avg_current_abandon
print (relative_accuracy_current)
if(relative_accuracy_current<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_current>(gamma/(1+gamma))) paste("amount of runs needed is: "
                                                       ,n*(delta_current_abandon/(avg_current_abandon*gamma/(1+gamma)))^2)

# reject
test_reject_current <- t.test(x= current_state$rejected_measure,y=NULL, alternative="two.sided",conf.level=0.95)
avg_current_reject<-test_reject_current$estimate
# רווח סמך חלקי
delta_current_reject<-test_reject_current$conf.int[2]-avg_current_reject
# דיוק יחסי
relative_accuracy_current2 <- delta_current_reject/avg_current_reject
print (relative_accuracy_current2)
if(relative_accuracy_current2<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_current2>(gamma/(1+gamma))) paste("amount of runs needed is: 
                                                        ",n*(delta_current_reject/(avg_current_reject*gamma/(1+gamma)))^2)
# votes
test_votes_current <- t.test(x= current_state$votes_measure,y=NULL, alternative="two.sided",conf.level=0.95)
avg_current_votes<-test_votes_current$estimate
# רווח סמך חלקי
delta_current_votes<-test_votes_current$conf.int[2]-avg_current_votes
# דיוק יחסי
relative_accuracy_current3 <- delta_current_votes/avg_current_votes
print (relative_accuracy_current3)
if(relative_accuracy_current3<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_current3>(gamma/(1+gamma))) paste("amount of runs needed is: 
                                                          ",n*(delta_current_votes/(avg_current_votes*gamma/(1+gamma)))^2)

#-------------------------------- Alternative 1 -------------------------------------------#
# abandon
test_abandon_1 <- t.test(x= Alternative_1$abandon_measure,y=NULL, alternative="two.sided",conf.level=0.95)
avg_1_abandon<-test_abandon_1$estimate
# רווח סמך חלקי
delta_1_abandon<-test_abandon_1$conf.int[2]-avg_1_abandon
# דיוק יחסי
relative_accuracy_1 <- delta_1_abandon/avg_1_abandon
print (relative_accuracy_1)
if(relative_accuracy_1<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_1>(gamma/(1+gamma))) paste("amount of runs needed is: "
                                                 ,n*(delta_1_abandon/(avg_1_abandon*gamma/(1+gamma)))^2)
                                                       
#reject
test_reject_1 <- t.test(x= Alternative_1$rejected_measure,y=NULL, alternative="two.sided",conf.level=0.95)
avg_1_reject<-test_reject_1$estimate
# רווח סמך חלקי
delta_1_reject<-test_reject_1$conf.int[2]-avg_1_reject
# דיוק יחסי
relative_accuracy_alt1 <- delta_1_reject/avg_1_reject
print (relative_accuracy_alt1)
if(relative_accuracy_alt1<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_alt1>(gamma/(1+gamma))) paste("amount of runs needed is:  "
                                                    ,n*(delta_1_reject/(avg_1_reject*gamma/(1+gamma)))^2)

# votes
test_votes_1 <- t.test(x= Alternative_1$votes_measure,y=NULL, alternative="two.sided",conf.level=0.95)
avg_1_votes<-test_votes_1$estimate
# רווח סמך חלקי
delta_1_votes<-test_votes_1$conf.int[2]-avg_1_votes
# דיוק יחסי
relative_accuracy_13 <- delta_1_votes/avg_1_votes
print (relative_accuracy_13)
if(relative_accuracy_13<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_13>(gamma/(1+gamma))) paste("amount of runs needed is: 
                                                          ",n*(delta_1_votes/(avg_1_votes*gamma/(1+gamma)))^2)

#------------------------------------- Alternative 2 -------------------------------------------#
#abandon
test_abandon_2 <- t.test(x= Alternative_2$abandon_measure,y=NULL, alternative="two.sided",conf.level=0.95)
avg_2_abandon<-test_abandon_2$estimate
# רווח סמך חלקי
delta_2_abandon<-test_abandon_2$conf.int[2]-avg_2_abandon
# דיוק יחסי
relative_accuracy_2 <- delta_2_abandon/avg_2_abandon
print (relative_accuracy_2)
if(relative_accuracy_2<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_2>(gamma/(1+gamma))) paste("amount of runs needed is: "
                                                 ,n*(delta_2_abandon/(avg_2_abandon*gamma/(1+gamma)))^2)

#reject
test_reject_2 <- t.test(x= Alternative_2$rejected,y=NULL, alternative="two.sided",conf.level=0.95)
avg_2_reject<-test_reject_2$estimate
# רווח סמך חלקי
delta_2_reject<-test_reject_2$conf.int[2]-avg_2_reject
# דיוק יחסי
relative_accuracy_alt2 <- delta_2_reject/avg_2_reject
print (relative_accuracy_alt2)
if(relative_accuracy_alt2<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_alt2>(gamma/(1+gamma))) paste("amount of runs needed is:  "
                                                    ,n*(delta_2_reject/(avg_2_reject*gamma/(1+gamma)))^2)
# votes
test_votes_2 <- t.test(x= Alternative_2$votes_measure,y=NULL, alternative="two.sided",conf.level=0.95)
avg_2_votes<-test_votes_2$estimate
# רווח סמך חלקי
delta_2_votes<-test_votes_2$conf.int[2]-avg_2_votes
# דיוק יחסי
relative_accuracy_23 <- delta_2_votes/avg_2_votes
print (relative_accuracy_23)
if(relative_accuracy_23<=gamma/(1+gamma)) {print("amount of runs is OK")}
if (relative_accuracy_23>(gamma/(1+gamma))) paste("amount of runs needed is: 
                                                          ",n*(delta_2_votes/(avg_2_votes*gamma/(1+gamma)))^2)
