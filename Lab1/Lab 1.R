name<- 
liuid<- 
library(markmyassignment)
lab_path <-"https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)


#my_num_vector Function
my_num_vector<- function(){
  c(log10(11),cos(pi/5),exp(pi/3),(1173%%7)/19)
}
#filter_my_vector Function
filter_my_vector<- function(x,leq){
  x_=x
  leq_=leq
 for (i in 1:leq_){
   if (x_[i] >= leq_)
     x_[i]=NA
 
   }  
  return(x_)
}
#dot_prod Function
dot_prod<- function(a,b){
  x=a%*%b
  x[1]
}
#approx_e(N) Function
approx_e<- function(N){
  x=0
  for (i in 0:N){
  x=x+ 1/factorial(i)}
  return(x)
}
#my_magic_matrix Function
my_magic_matrix<- function(){
  matrix(c(4,3,8,9,5,1,2,7,6), nrow = 3, ncol = 3)
}
#calculate_elements Function
calculate_elements<- function(A){
  return(length(A))
}
#row_to_zero function
row_to_zero<- function(A,i){
  A[A %in% A[i,]] <- 0
  return(A)
}
#add_elements_to_matrix Function
add_elements_to_matrix<- function(A,x,i,j){
  A[A %in% A[i,j]] <- A[A %in% A[i,j]] + x
  return(A)
}
#my_magic_list Function
my_magic_list<- function(){
  list_<-list("info"="my own list",my_num_vector(),my_magic_matrix())
  return(list_)
}
#change_info Function
change_info<- function(x,text){
  x[["info"]] <- text
  x
}
#add_note Function
add_note<- function(x,note){
  x_<-list(note)
  names(x_)<-c("note")
  y<-append(x,x_, after = length(x)+1)
  y
}
#sum_numeric_parts Function
sum_numeric_parts<- function(x){
  sm=0 
  for (i in 1:length(x)){if(is.numeric(x[[i]])){for (j in 1:length(x[[i]])) {sm=sm+x[[i]][j]}}}
  sm
}
#my_data.frame Function
my_data.frame<- function(){
  x<- list(1:3,c("John","Lisa","Azra"),c(7.30,0.00,15.21),c(FALSE,FALSE,TRUE))
  names(x)<- c("id","name","income","rich")
  as.data.frame(x)
}
#sort_head Function
sort_head<- function(df,var.name,n){
  df<-df[order(df[,var.name],decreasing = TRUE),]
  df[1:n,]
}
#add_median_variable Function
add_median_variable <- function(df, j) {
  m <- median(df[, j])
  for (i in 1:length(df[, j])) {
    if (df[i, j] > m) {
      df$compared_to_median[i] = "Greater"
    } else if (df[i, j] < m) {
      df$compared_to_median[i] = "Smaller"
    } else df$compared_to_median[i] = "Median" }
  df}
#analyze_columns Function
analyze_columns<- function(df,j){
  c1<-c(
    mean(df[,j[1]]),
    median(df[,j[1]]),
    sd(df[,j[1]]))
  names(c1)<-c("mean","median","sd")
  c2<-c(mean(df[,j[2]]),
        median(df[,j[2]]),
        sd(df[,j[2]]))
  names(c2)<-c("mean","median","sd")
  list_<-list(c1,c2,cor(data.frame(df[,j[1],drop=FALSE],df[,j[2],drop=FALSE])))
  nams1<-c(names(df[j[1]]))
  nams2<-c(names(df[j[2]]))
  names(list_)<- c(nams1,nams2,"correlation_matrix")
  list_
  
}

