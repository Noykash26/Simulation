## ------------------------------------ Alternative 2 ------------------------------------ ##
#בחרנו במפלגת שמאל
# בחרנו להשאיר את מספר הנציגים בכל מתחם כמו שהוא
# ביצענו סדנה לשיפור יכולות השכנוע של הנציגים במפלגה באזור צפון 
# במצב זה הקולות הם: שמאל - 867, מרכז - 864, ימין - 561
# במצב זה, מספר הנוטשים ירד מ449 ל352

## ------------------------------------ ATTRIBUTES AND SETTINGS ------------------------------------ ##

# AREAS      --> 1 = north, 2 = center, 3 = south
# KALPI_TYPE --> 1 = small_kalpi, 2 = large_kalpi OR negishut_kalpi
# KALPI_NUM  --> 1_1 = north&small, 1_2:3 = north&large
#                2_1:3 = center&small, 2_4:9 = center&large
#                3_1:2 = south&small, 3_3:6 = south&large (first index is area)
# TYPE OF VOTER --> 1 = left, 2 = center, 3 = right
# Z --> extreme level of a voter.
# OPINION --> 1 = initial opinion of left party, 2 = center party, 3 = right party.
# NEW_OPINION --> created upon change at the stands. same index as above.
# STAND_ORDER --> 1 = the first party's stand a voter visits, 2 = the second etc.
# MAX_OPINION -> the highest probability of a party of a voter.
# FINAL_OPINION --> 1 = final opinion of left party, 2 = center party, 3 = right party.
##----------------------------------------- 1.  all functions ------------------------------------------------

set_max_opinion <- function (opinion_1, opinion_2, opinion_3){
  opinion_vec <- c(opinion_1, opinion_2, opinion_3)
  max_opinion <- max(opinion_vec)
  return(max_opinion)
} # returns the highest opinion of a voter

my_area <- function () {
  area <- sample(1:3, 1)
  return (area)
} # returns the area of the voter, equal probability to each area 

my_kalpi <- function (area, kalpi_type) {
  kalpi <- switch(
    paste0(area, kalpi_type),
    "11" = 1,
    "12" = sample(2:3, 1),
    "21" = sample(1:3, 1),
    "22" = sample(4:9, 1),
    "31" = sample(1:2, 1),
    "32" = sample(3:6, 1)
  ) # switch
  return (kalpi)
} # returns the number of kalpi of each voter, according to area and type of kalpi

trimmedNorm <- function(mu,sd){
  while(TRUE){
    sample <- rnorm(1, mu, sd)
    if (sample > 0)
      return (sample)
  }
} # trimmedNorm

get_kalpi_resource <- function (area, kalpi_num){
  return (paste0("kalpi_", area, kalpi_num)) 
} # a function that returns the kalpi resource

get_stand_resource <- function (area, kalpi_num, party) {
  return (paste0("stand_", area, kalpi_num, party))
} # a function that returns the stand resource

densityZ <- function (){ # calculations in the final report
  u <- runif(1,0,1)
  if (u < 0.5)
    return (u/8)^(1/3)
  else
    return (u/2)
} # a function that calculates the extreme level of a voter - COMPOSITION of f(x)

opinion_with_Z<-function (type, Z){
  opinion <- c(0.37, 0.44 , 0.19) # initial opinion
  for (i in 1 : 3) {
    if (i == type)
      opinion[i] <- opinion[i] + Z
    else opinion[i] <- opinion[i] - (Z/2)
  } # for
  return (opinion)
} # a function that calculates the opinion vector of a voter with the Z 

stand_order <- function (opinion_1, opinion_2, opinion_3){
  opinion_vec <- c(opinion_1, opinion_2, opinion_3)
  order_vec <- c(1,3,2) # default order, center voter
  max_index <- which(opinion_vec == max(opinion_vec), arr.ind = TRUE) #המפלגה שהדעה שלו מוטית לכיוונה כלומר הסיכוי הכי גבוה
  if (max_index == 1)
    order_vec <- c(3,2,1) # left voter
  if (max_index == 3)
    order_vec <- c(2,1,3) # right voter
  return(order_vec)
} # a function that returns the order of the stands of a voter

isEqual <- function (max_opinion, opinion){
  if (max_opinion == opinion)
    return(1)
  else return(2)
} # returns T/F if there is equal probabilities with the highest party of a voter

updated_values <- function (opinion_1, opinion_2, opinion_3, stand_party) { 
  x <- runif(1, 0, 0.5)
  y <- x/2
  if (stand_party == 1) 
    return(c(opinion_1 + x, opinion_2 - y, opinion_3 - y))
  if (stand_party == 2) 
    return(c(opinion_1 - y, opinion_2 + x, opinion_3 - y))
  if (stand_party == 3) 
    return(c(opinion_1 - y, opinion_2 - y, opinion_3 + x))
  
} # if a voter didn't abandon a stand, we will raise the probabilities of this stand

updated_values_renege <- function (opinion_1, opinion_2, opinion_3, stand_party) { 
  x <- runif(1, 0, 0.25)
  y <- x/2
  if (stand_party == 1) 
    return(c(opinion_1 - x, opinion_2 + y, opinion_3 + y))
  if (stand_party == 2) 
    return(c(opinion_1 + y, opinion_2 - x, opinion_3 + y))
  if (stand_party == 3) 
    return(c(opinion_1 + y, opinion_2 + y, opinion_3 - x))
  
} # if a voter abandon a stand, we will decline the probabilities of the other stands

opinion_check <-function (opinion_1, opinion_2, opinion_3){
  vec <- c(opinion_1, opinion_2, opinion_3)
  if (vec[1] < 0)
    vec[1] <- 0
  if (vec[2] < 0)
    vec[2] <- 0
  if (vec[3] < 0)
    vec[3] <- 0
  return(vec/sum(vec))
} # a function that normalize the vector of opinions

setCapacity <- function (trajectory, area, kalpi_num, capacity_value){
  updatedPath <- NA 
  for (i in 1:kalpi_num)
    updatedPath <- set_capacity(trajectory, resource = paste0("kalpi_", area, i), value = capacity_value)  
  
  return(updatedPath)
  
  # a function that set the capacity of the kalpi, called by the area manager. 
} # assumption: if there is someone in service, he will finish first and then the kalpi will closed.

my_vote <- function (final_opinion_1, final_opinion_2, final_opinion_3){
  final_opinion_vec <- c(final_opinion_1, final_opinion_2, final_opinion_3)
  vote <- rdiscrete(1, probs = final_opinion_vec, values = 1:3)
  return(paste0("party_", vote))
} # returns the winner party of a voter

close_small_kalpi <- function (trajectory) {
  updatedPath <- set_capacity(trajectory, resource = "kalpi_11", value = 0)%>% 
    set_capacity(resource = "kalpi_21", value = 0)%>% 
    set_capacity(resource = "kalpi_22", value = 0)%>% 
    set_capacity(resource = "kalpi_23", value = 0)%>% 
    set_capacity(resource = "kalpi_31", value = 0)%>% 
    set_capacity(resource = "kalpi_32", value = 0)%>% 
    
    set_queue_size(resource = "kalpi_11", value = 0)%>%
    set_queue_size(resource = "kalpi_21", value = 0)%>%
    set_queue_size(resource = "kalpi_22", value = 0)%>%
    set_queue_size(resource = "kalpi_23", value = 0)%>%
    set_queue_size(resource = "kalpi_31", value = 0)%>%
    set_queue_size(resource = "kalpi_32", value = 0)
  
  return(updatedPath)
}# a function that set the capacity and the queue size of the small kalpis to 0

close_large_kalpi <- function (trajectory) {
  updatedPath <- set_capacity(trajectory, resource = "kalpi_12", value = 0)%>% 
    set_capacity(resource = "kalpi_13", value = 0)%>% 
    set_capacity(resource = "kalpi_24", value = 0)%>% 
    set_capacity(resource = "kalpi_25", value = 0)%>% 
    set_capacity(resource = "kalpi_26", value = 0)%>% 
    set_capacity(resource = "kalpi_27", value = 0)%>% 
    set_capacity(resource = "kalpi_28", value = 0)%>% 
    set_capacity(resource = "kalpi_29", value = 0)%>% 
    set_capacity(resource = "kalpi_33", value = 0)%>% 
    set_capacity(resource = "kalpi_34", value = 0)%>% 
    set_capacity(resource = "kalpi_35", value = 0)%>% 
    set_capacity(resource = "kalpi_36", value = 0)%>% 
    
    set_queue_size(resource = "kalpi_12", value = 0)%>%
    set_queue_size(resource = "kalpi_13", value = 0)%>%
    set_queue_size(resource = "kalpi_24", value = 0)%>%
    set_queue_size(resource = "kalpi_25", value = 0)%>%
    set_queue_size(resource = "kalpi_26", value = 0)%>%
    set_queue_size(resource = "kalpi_27", value = 0)%>%
    set_queue_size(resource = "kalpi_28", value = 0)%>%
    set_queue_size(resource = "kalpi_29", value = 0)%>%
    set_queue_size(resource = "kalpi_33", value = 0)%>%
    set_queue_size(resource = "kalpi_34", value = 0)%>%
    set_queue_size(resource = "kalpi_35", value = 0)%>%
    set_queue_size(resource = "kalpi_36", value = 0)
  
  return(updatedPath)
}# a function that set the capacity and the queue size of the large kalpis to 0

cheat <- function () {
  party <- rdiscrete(1, c(1/3, 1/3, 1/3), c(1,2,3))
  return(paste0("party_", party))
} # returns the party to which we add 50 votes

rejected_left <- function (party){
  if (party == 1)
    return (1)
  else return (0)
} 

aband_left <- function (party){
  if (party == 1)
    return (1)
  else return (0)
} 

isLeft<- function (stand, area){
  
  if (stand ==1 & area==1)
    return (1)
  else return (0)
} #Checks if a voter is in the stand of the left party

updated_values_left <- function (opinion_1, opinion_2, opinion_3, stand_party) { 
  x <- runif(1, 0.1, 0.4)
  y <- x/2
  return(c(opinion_1 + x, opinion_2 - y, opinion_3 - y))
} # if a voter didn't abandon a stand, we will raise the probabilities of this stand

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------


simulation_time <- 15*60 + 20
north_agents <- c(5, 5, 4) # number of agents at each stand of each kalpi of each area
south_agents <- c(3, 5, 5)

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

elections <- simmer("elections")

## KALPI(i,j) --> i = area, j = kalpi_num ##
for (i in 1:3)  # north kalpi
  add_resource(elections, paste0("kalpi_1", i), capacity = 1, queue_size = Inf)

for (i in 1:9)  # center kalpi
  add_resource(elections, paste0("kalpi_2", i), capacity = 1, queue_size = Inf)

for (i in 1:6)  # south kalpi
  add_resource(elections, paste0("kalpi_3", i), capacity = 1, queue_size = Inf)

## STAGES(x, i, j) --> x = north, i = kalpi_num, j = party 
for (i in 1:3){ # north stages
  for (j in 1:3) 
    add_resource(elections, paste0("stand_1", i, j), capacity = north_agents[j], queue_size = 0)
}

for (i in 1:9){ # center stages
  for (j in 1:3) 
    add_resource(elections, paste0("stand_2", i, j), capacity = 10, queue_size = 0)
}

for (i in 1:6){ # south stages
  for (j in 1:3) 
    add_resource(elections, paste0("stand_3", i, j), capacity = south_agents[j], queue_size = 0)
}

#--- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

# --------------------------------- mashkifim --------------------------------------------
traj_mashkif_1 <- trajectory("traj_mashkif_1")%>%
  leave(prob = 0.6)%>%
  set_global(key = function () cheat (), value = 50, mod = "+")

traj_mashkif_2 <- trajectory("traj_mashkif_2")%>%
  leave(prob = 0.6)%>%
  set_global(key = function () cheat (), value = 50, mod = "+")

traj_mashkif_3 <- trajectory("traj_mashkif_3")%>%
  leave(prob = 0.6)%>%
  set_global(key = function () cheat (), value = 50, mod = "+")

# ----------------------------- close the kalpi -----------------------------------------
traj_close_small_kalpi <- trajectory("traj_close_small_kalpi")%>%
  close_small_kalpi ()

traj_close_large_kalpi <- trajectory("traj_close_large_kalpi")%>%
  close_large_kalpi ()

# ---------------------------------- voters ----------------------------------------------
traj_vote <- trajectory("traj_vote")%>%
  set_attribute(keys = c("final_opinion_1", "final_opinion_2", "final_opinion_3"), values = function () opinion_check(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3")))%>%
  simmer::select(resources = function () get_kalpi_resource(area = get_attribute(elections, key = "area"), kalpi_num = get_attribute(elections, key = "kalpi_num")), id = 0)%>%
  seize_selected(amount = 1, id = 0)%>%
  timeout (function () rexp (1, 60/40))%>% # identification time
  timeout (function() trimmedNorm (mu = 1,  sd = 6/60))%>% # voting time
  # להוסיף את החלק של למי הוא מצביע בסוף
  set_global(key = function () my_vote(get_attribute(elections, "final_opinion_1"), get_attribute(elections, "final_opinion_2"), get_attribute(elections, "final_opinion_3")), value = 1 , mod = "+", init = 0)%>%
  release_selected(amount = 1, id = 0)

stand3_reject <- trajectory("stand3_reject")%>%
  set_attribute(key = "reject", value = function () rejected_left(get_attribute(elections, "stands_order_3")))%>%
  join(traj_vote)

traj_stand_3 <- trajectory("traj_stand_3")%>% #לא נוטשים את הדוכן השלישי לא משנה מה
  simmer::select(resources =  function () get_stand_resource(get_attribute(elections, key = "area"), get_attribute(elections, key = "kalpi_num"), get_attribute(elections, key = "stands_order_3")), id = 3)%>%
  seize_selected(amount = 1, id = 3, continue = FALSE, reject = stand3_reject)%>%
  timeout(function () runif(1, 2, 6))%>%
  release_selected(amount = 1, id = 3)%>%
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_3")))%>%
  # נירמול וקטור ההעדפות
  join(traj_vote)

stand2_abandon <- trajectory ("trajOfRenege2")%>%
  set_attribute(key = "abandon", value = function () aband_left (get_attribute(elections, "stands_order_2")))%>%
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values_renege(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_2")))%>%
  join(traj_stand_3)

stand2_reject <- trajectory("stand2_reject")%>%
  set_attribute(key = "reject", value = function () rejected_left(get_attribute(elections, "stands_order_2")))%>%
  join(traj_stand_3)

stand2_no_renege <- trajectory ("stand2_no_renege")%>%
  simmer::select(resources =  function () get_stand_resource(get_attribute(elections, key = "area"), get_attribute(elections, key = "kalpi_num"), get_attribute(elections, key = "stands_order_2")), id = 22)%>%
  seize_selected(amount = 1, id = 22, continue = FALSE, reject = stand2_reject)%>%
  timeout(function () runif(1, 2, 6))%>%
  release_selected(amount = 1, id = 22)%>%
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_2")))%>%
  join(traj_stand_3)

stand2_renege_left <- trajectory ("stand2_renege_left")%>%
  simmer::select(resources =  function () get_stand_resource(get_attribute(elections, key = "area"), get_attribute(elections, key = "kalpi_num"), get_attribute(elections, key = "stands_order_2")), id = 21)%>%
  seize_selected(amount = 1, id = 21, continue = FALSE, reject = stand2_reject)%>% 
  renege_in(t = function () runif(1, 3.5, 8.5) , out = stand2_abandon)%>%
  timeout(function () runif(1, 2, 6))%>%
  renege_abort()%>%
  release_selected(amount = 1, id = 21)%>%
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values_left(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_2")))%>%
  join(traj_stand_3)

stand2_renege<- trajectory ("stand2_renege")%>%
  branch(option = function () isLeft (get_attribute(elections, key="stands_order_2"), get_attribute(elections, key="area")), continue = FALSE, stand2_renege_left)%>%
  simmer::select(resources =  function () get_stand_resource(get_attribute(elections, key = "area"), get_attribute(elections, key = "kalpi_num"), get_attribute(elections, key = "stands_order_2")), id = 21)%>%
  seize_selected(amount = 1, id = 21, continue = FALSE, reject = stand2_reject)%>% 
  renege_in(t = function () runif(1, 3.5, 6.5) , out = stand2_abandon)%>%
  timeout(function () runif(1, 2, 6))%>%
  renege_abort()%>%
  release_selected(amount = 1, id = 21)%>%
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_2")))%>%
  join(traj_stand_3)

traj_stand_2 <- trajectory("traj_stand_2")%>%
  branch(option = function ()isEqual (get_attribute(elections, key="max_opinion"), get_attribute(elections, key=paste0("opinion_",get_attribute(elections, key="stands_order_2")))), continue = c(FALSE, FALSE), stand2_no_renege,stand2_renege)

# #אם הוא נוטש באמת
stand1_abandon <- trajectory ("stand1_abandon")%>%
  set_attribute(key = "abandon", value = function () aband_left (get_attribute(elections, "stands_order_1")))%>%
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values_renege(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_1")))%>%
  join(traj_stand_2)

stand1_reject <- trajectory("stand1_reject")%>%
  set_attribute(key = "reject", value = function () rejected_left(get_attribute(elections, "stands_order_1")))%>%
  join(traj_stand_2)

stand1_no_renege <- trajectory ("stand1_no_renege")%>%
  simmer::select(resources =  function () get_stand_resource(get_attribute(elections, key = "area"), get_attribute(elections, key = "kalpi_num"), get_attribute(elections, key = "stands_order_1")), id = 12)%>%
  seize_selected(amount = 1, id = 12, continue = FALSE, reject = stand1_reject)%>%
  timeout(function () runif(1, 2, 6))%>%
  release_selected(amount = 1, id = 12)%>%
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_1")))%>%
  join(traj_stand_2)

stand1_renege_left<- trajectory ("stand1_renege_left")%>%
  simmer::select(resources =  function () get_stand_resource(get_attribute(elections, key = "area"), get_attribute(elections, key = "kalpi_num"), get_attribute(elections, key = "stands_order_1")), id = 11)%>%
  seize_selected(amount = 1, id = 11, continue = FALSE, reject = stand1_reject)%>% 
  renege_in(t = function () runif(1, 3.5, 8.5) , out = stand1_abandon)%>%
  timeout(function () runif(1, 2, 6))%>%
  renege_abort()%>%
  release_selected(amount = 1, id = 11)%>%
  #מעלה הסתברויות
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values_left(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_1")))%>%
  join(traj_stand_2)

stand1_renege<- trajectory ("stand1_renege")%>%
  branch(option = function () isLeft (get_attribute(elections, key="stands_order_1"), get_attribute(elections, key="area")), continue = FALSE, stand1_renege_left)%>%
  simmer::select(resources =  function () get_stand_resource(get_attribute(elections, key = "area"), get_attribute(elections, key = "kalpi_num"), get_attribute(elections, key = "stands_order_1")), id = 11)%>%
  seize_selected(amount = 1, id = 11, continue = FALSE, reject = stand1_reject)%>% 
  renege_in(t = function () runif(1, 3.5, 6.5) , out = stand1_abandon)%>%
  timeout(function () runif(1, 2, 6))%>%
  renege_abort()%>%
  release_selected(amount = 1, id = 11)%>%
  #מעלה הסתברויות
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () updated_values(get_attribute(elections, key = "new_opinion_1"), get_attribute(elections, key = "new_opinion_2"), get_attribute(elections, key = "new_opinion_3"), get_attribute(elections, key = "stands_order_1")))%>%
  join(traj_stand_2)

# here we'll check if there's equal probabilities between stand 1 and stand 3 (which the highest)
traj_stand_1 <- trajectory("traj_stand_1")%>% 
  branch(option = function () isEqual (get_attribute(elections, key="max_opinion"), get_attribute(elections, key=paste0("opinion_",get_attribute(elections, key="stands_order_1")))), continue = c(FALSE, FALSE), stand1_no_renege, stand1_renege)

# trajectory to initialize the specific kalpi the voter will go, according to it's area and type
traj_initialize <- trajectory("traj_initialize")%>%
  set_attribute(key = "area", value = function () my_area ())%>%
  set_attribute(key = "kalpi_num", value = function () my_kalpi (get_attribute(elections, key = "area"), get_attribute(elections, key = "kalpi_type")))%>%
  # voter's agenda 
  set_attribute(key = "type", value = function () rdiscrete(1, c(0.4,0.1,0.5), c(1,2,3)))%>% 
  set_attribute(key = "Z", value = function () densityZ ())%>% # voter's extreme level 
  # opinion including the extreme level
  set_attribute(keys = c("opinion_1", "opinion_2", "opinion_3"), values = function () opinion_with_Z (get_attribute(elections, key = "type"), get_attribute(elections, key = "Z")))%>%
  set_attribute(keys = c("new_opinion_1", "new_opinion_2", "new_opinion_3"), values = function () opinion_with_Z (get_attribute(elections, key = "type"), get_attribute(elections, key = "Z")))%>%
  # the order of stands of a specific voter
  set_attribute(keys = c("stands_order_1", "stands_order_2", "stands_order_3"), values = function () stand_order (get_attribute(elections, key="opinion_1"), get_attribute(elections, key="opinion_2"), get_attribute(elections, key="opinion_3")))%>% 
  set_attribute(key = "max_opinion", value = function () set_max_opinion (get_attribute(elections, key="opinion_1"), get_attribute(elections, key="opinion_2"), get_attribute(elections, key="opinion_3")))%>% # opinion including the extreme level
  join(traj_stand_1)

# trajectories to initialize what type of kalpi the voters will go - small or large
traj_small_kalpi <- trajectory("traj_small_kalpi")%>%
  set_attribute(key = "kalpi_type", value = 1)%>%
  join(traj_initialize)

traj_large_kalpi <- trajectory("traj_large_kalpi")%>%
  set_attribute(key = "kalpi_type", value = 2)%>%
  join(traj_initialize)

traj_negishut_kalpi <- trajectory("traj_negishut_kalpi")%>%
  set_attribute(key = "kalpi_type", value = 2)%>%
  join(traj_initialize)

# -----------------------------managers------------------------------------------
# assumption : the manager closes all the kalpiot in his area, for an equal time
#              and he doesnt close the stages.

traj_north_manager <- trajectory("traj_north_manager")%>% # north = 1 
  setCapacity(area = 1, kalpi_num = 3, capacity_value = 0)%>% 
  timeout(function () rtriangle(1, 3, 15, 9))%>%
  setCapacity(area = 1, kalpi_num = 3, capacity_value = 1)

traj_center_manager <- trajectory("traj_center_manager")%>% # center = 2
  setCapacity(area = 2, kalpi_num = 9, capacity_value = 0)%>% 
  timeout(function () rtriangle(1, 3, 15, 9))%>%
  setCapacity(area = 2, kalpi_num = 9, capacity_value = 1)

traj_south_manager <- trajectory("traj_south_manager")%>% # south = 3
  setCapacity(area = 3, kalpi_num = 6, capacity_value = 0)%>% 
  timeout(function () rtriangle(1, 3, 15, 9))%>%
  setCapacity(area = 3, kalpi_num = 6, capacity_value = 1)

##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

## assumption: when a kalpi is closed, voters stop arrive but voters who are already
##             inside finish to vote and then leave. לבדוק מה קורה עם הספירת קולות

## assumption: when a disabled voter arrives he bypasses the queue (because he has a higher prority of '1'), 
##             however he doesnt take out a voter who is in service (therefore preempitive = False) לא עובד  

elections%>%
  # small kalpi is open between 08:00-20:00
  add_generator("small_kalpi_voter", traj_small_kalpi, from_to(start_time = 60, stop_time = 780, dist = function () rexp(1, 1/1.11)), priority = 0, mon = 2)%>%
  # large kalpi is open between 07:00-22:00
  add_generator("large_kalpi_voter", traj_large_kalpi, from_to(start_time = 0, stop_time = 900, dist = function () rgamma(1, 1.9688020, 0.6994063)), priority = 0, mon = 2)%>%
  # negishut kalpi is the large kalpi, therefore it's open between 07:00-22:00
  add_generator("negishut_kalpi_voter", traj_negishut_kalpi, from_to(start_time = 0, stop_time = 900, dist = function () rgamma(1, 2.023509, 2.835970)), priority = 1, mon = 2)%>%
  
  # assumption : the manager will come at the start of the simulation (time = 0) in intervals of 3 hours,
  #              and it doesnt matter what is the type of the kalpi. 
  #              moreover, there is no need to arrive at 22:00 (i.e time = 900)
  add_generator("north_manager", traj_north_manager, distribution = at(0, 180, 360, 540, 720), mon=2)%>% 
  add_generator("Center_manager", traj_center_manager, distribution = at(0, 180, 360, 540, 720), mon=2)%>%
  add_generator("South_manager", traj_south_manager, distribution = at(0, 180, 360, 540, 720), mon=2)%>%
  
  # a fictive entity that closes the kalpis, each one at it's time
  # assumption : the stands stay open and only the opportunity to vote is closed.
  # assumption : although the kalpis are closed, voters still wait in queue.
  add_generator("fictive_small", traj_close_small_kalpi, distribution = at(780), mon = 0)%>%
  add_generator("fictive_large", traj_close_large_kalpi, distribution = at(900), mon = 0)%>%
  
  # index --> 1 = north area, 2 = center , 3 = south
  add_generator("mashkif_1", traj_mashkif_1, distribution = at(900), mon = 2)%>%
  add_generator("mashkif_2", traj_mashkif_2, distribution = at(900), mon = 2)%>%
  add_generator("mashkif_3", traj_mashkif_3, distribution = at(900), mon = 2)

##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
elections_i <- mclapply(1:30, function(i) {
  set.seed(364 + i)
  reset(elections)%>% 
    run(until=simulation_time) %>%
    wrap()
})

full_arrival_data <- get_mon_arrivals(elections_i) # the full data of all replications
full_attribute_data <- get_mon_attributes(elections_i)
full_arrival_data_resource <- get_mon_arrivals(elections_i, per_resource = T) # the full data of all replications

# ------------------------------------ MEASURES -------------------------------------------
# ----------------------1) abandon / total left stand visitors-----------------------------

visitors_left <- sqldf("select replication, count(resource) as voters_left
           from full_arrival_data_resource
           where resource in ('stand_111', 'stand_121', 'stand_131',
           'stand_211','stand_221','stand_231','stand_241','stand_251','stand_261','stand_271','stand_281','stand_291',
           'stand_311','stand_321','stand_331','stand_341','stand_351','stand_361')
           group by replication")

abandon_left <- sqldf("select replication, count(value) as abanded
                   from full_attribute_data
                   where key = 'abandon' and value = 1
                   group by replication")

abandon_measure <- sqldf("select (abanded*100.00)/(voters_left*100.00) as abandon_measure
                         from visitors_left as v join abandon_left as a on v.replication=a.replication")

# ----------------------2) rejected / total left stand visitors-----------------------------

reject_left <- sqldf("select replication, sum(value) as rejected
                   from full_attribute_data
                   where key = 'reject' 
                   group by replication")

rejected_measure <- sqldf("select (rejected*100.00)/(voters_left*100.00) as rejected_measure
                         from visitors_left as v join reject_left as a on v.replication=a.replication")

# ----------------------3) left_voters / total_voters-----------------------------
total_votes <- sqldf ("select replication, sum(value) as total_votes from (select *
          from full_attribute_data
          where key in('party_1','party_2','party_3')
          group by replication, key
          having max(value)) group by replication")

party_1 <- sqldf("select replication, max(value) as left_votes
                 from full_attribute_data 
                 where key = 'party_1'
                 group by replication")

votes_measure <- sqldf("select left_votes/total_votes as votes_measure
                       from total_votes as t join party_1 as v on t.replication=v.replication")

measures_halufa2 <- cbind(abandon_measure, rejected_measure, votes_measure)
write.csv(measures_halufa2, "C:\\Users\\User\\Desktop\\project\\halufa2.csv", row.names = TRUE)