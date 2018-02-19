#Loading the input file in to R studio
Regression <- read.table(file.choose(), header = TRUE, sep = ',')
View(Regression)
model = lm( y~x,data =Regression)
plot(y~x, data = Regression)
plot(x,y)
abline(model)


cost <- function(X,y, theta){
  sum(X%*%theta - y)^2/2 }

theta = matrix(c(0,0) ,nrow =2)
num_iterations =500
alpha =0.01
cost_hist = double(num_iterations)
theta_hist = list(num_iterations)
X <-cbind(1, matrix(x))
for(i in 1:num_iterations){
  error =(X%*% theta -y)
  delta =t(X)%*% error
  theta = theta-alpha*delta
  cost_hist[i] =cost(X,y,theta)
  theta_hist[[i]] = theta
}
