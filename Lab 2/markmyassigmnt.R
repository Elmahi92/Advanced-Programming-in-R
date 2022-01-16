mark_my_assignment()

#1.3.2 while_mult_table()
while_mult_table <- function (from,to) {
  stopifnot(is.numeric(from),is.numeric(to))
  x<-1
  while (x <= from){
    print(x ,end='\t')
    x=x + 1
  }
  y <- 1
  while (y <= to){
    print('')
    print(y,end='\t') }
  z = 1  # reset to 1
  while (z <= 12){
    print(y*z ,end='\t')
    z=z + 1
    y=y +1} 
  print()
}
while_mult_table(from = 3, to = 5)



num = 4
for(i in 1:10){
  cat(num," x ",i," = ",num*i, "\n")
}

results = vector(length = 64, mode = 'list')
A<-1:5
B<-1:5
for (i in 1:64) {
  results[i] = B[1:length(A)]*A[i]
}
results

