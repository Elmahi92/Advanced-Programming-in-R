library(markmyassignment)
lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
set_assignment(lab_path)
name<- c("")
liuid<- c("")
#1.1.1 sheldon_game(player1, player2)
sheldon_game <- function (player1, player2) {
  opt <- c("scissors", "paper", "rock","lizard", "spock")
  stopifnot(is.character(player1),is.character(player2), (player1 %in% opt),(player2 %in% opt))
  if (identical(player1, player2)) {
    "draw!"
  }
  else if (player1 == "scissors " && player2 == "paper" 
           |player1 == "paper" && player2 == "rock" 
           | player1 == "rock" && player2 == "lizard"
           | player1 == "lizard" && player2 == "spock"
           | player1 == "spock" && player2 == "scissors") {
    "player 1 wins!"
  }
  else {
    "player 2 wins!"
  }
}
#1.2.1 my_moving_median()
my_moving_median <- function(x,n,...){
  stopifnot(is.vector(x),is.numeric(n))
  na.rm<-list(...)
  if(na.rm==TRUE){
    x <- x[!is.na(x)] 		
  }
  for(i in 1:length(x-n)){
    x[i]<- median(x[i:(i+n)])
    if(is.na(x[i])) break
  }
  x}
#1.2.2 for_mult_table()
for_mult_table <- function(from,to){
  x<-from:to
  z<-length(x)
  mymat <- matrix(0, z, z)
  colnames(mymat)<-x
  rownames(mymat)<-x
  as.numeric(colnames(mymat)[1])
  for(i in 1:dim(mymat)[1]) {
    for(j in 1:dim(mymat)[2]) {
      mymat[i,j] = as.numeric(colnames(mymat)[i]) *as.numeric(rownames(mymat)[j])
    }
  }
  mymat}
#1.3.1 find_cumsum()
find_cumsum <- function(x,find_sum){
  stopifnot(is.numeric(x),is.numeric(find_sum))
  x1<-cumsum(x)
    while(x1[which.max(x1>find_sum)]>find_sum)  {
      return(x1[which.max(x1>find_sum)])
    }
  return(x1[which.max(x1)])
}
find_cumsum(x=1:10, find_sum=1000)
#1.3.2 while_mult_table()
while_mult_table <- function(from,to){
  x<-from:to
  z<-length(x)
  mymat <- matrix(0, z, z)
  colnames(mymat)<-x
  rownames(mymat)<-x
  as.numeric(colnames(mymat)[1])
  i<-1
  while(i <= dim(mymat)[1]) {
    for(j in 1:dim(mymat)[2]) {
      mymat[i,j] = as.numeric(colnames(mymat)[i]) *as.numeric(rownames(mymat)[j])}
    i<-i+1}
  mymat}
#1.4.1 repeat_find_cumsum()
repeat_find_cumsum <- function(x,find_sum){
  stopifnot(is.numeric(x),is.numeric(find_sum))
  x1<-cumsum(x)
  repeat{
    return(x1[which.max(x1)])
  
  if(x1[which.max(x1>find_sum)]>find_sum){
   
     break
  }
  }
}

repeat_find_cumsum(x=1:10, find_sum=1000)
#1.4.2 repeat_my_moving_median()
repeat_my_moving_median <- function(x,n,...){
  stopifnot(is.numeric(x),is.numeric(n))
  repeat{
    na.rm<-list(...)
    if(na.rm==TRUE){
      x <- x[!is.na(x)] 		
    }
    for(i in 1:length(x-n)){
      x[i]<- median(x[i:(i+n)])
      if(is.na(x[i])) break
    }
    x}}
#1.5.1 in_environment()
in_environment<- function(env) {    
  toString(env)}
#1.6.1 cov()
cov<- function(X) {    
lapply(X, function(x) sd(x) / mean(x))}
#1.7.1 moment()
moment <- function(i){
  stopifnot(is.numeric(i))
  function(x)
    mean((x - mean(x)) ** i)}
