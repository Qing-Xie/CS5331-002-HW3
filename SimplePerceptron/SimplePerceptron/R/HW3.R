#' A Simple Perceptron Function
#'
#' @param x
#' @param y
#' @keywords perceptron
#' @export
#' @examples
#' perceptron()

# iris dataset
# select the Sepal.Width versus Petal.Width scatter plot
x <- cbind(iris$Sepal.Width,iris$Petal.Width)
# label setosa as positive and the rest as negative
y <- ifelse(iris$Species == "setosa", +1, -1)
# plot all the points
plot(x,cex=0.2)
# use plus sign for setosa points
points(subset(x,y==1),col="black",pch="+",cex=2)
# use minus sign for the rest
points(subset(x,y==-1),col="red",pch="-",cex=2)



# predicting class
distance.from.plane = function(z,w,b) {
  sum(z*w) + b
  }
classify.linear = function(x,w,b) {
  distances =
  apply(x, 1, distance.from.plane, w, b)
  return(ifelse(distances < 0, -1, +1))
}
classify.linear(x,c(-1,1)/sqrt(2),-sqrt(2)/4)


# simple peceptron code
euclidean.norm = function(x) {sqrt(sum(x * x))}
perceptron = function(x, y, learning.rate=1) {
  w = vector(length = ncol(x)) # initialize w
  b = 0 # Initialize b
  k = 0 # count updates
  R = max(apply(x, 1, euclidean.norm))
  made.mistake = TRUE # to enter the while loop
  while (made.mistake) {
    made.mistake=FALSE # hopefully
    yc <- classify.linear(x,w,b)
    for (i in 1:nrow(x)) {
      if (y[i] != yc[i]) {
        w <- w + learning.rate * y[i]*x[i,]
        b <- b + learning.rate * y[i]*R^2
        k <- k+1
        made.mistake=TRUE
        }
      } }
  s = euclidean.norm(w)
  return(list(w=w/s,b=b/s,updates=k))
}


# testing the peceptron
(p <- perceptron(x,y))
sum(abs(classify.linear(x,p$w,p$b) - y))


# replot and view the separation boundary
  plot(x,cex=0.2)
  points(subset(x,y==1),col="black",pch="+",cex=2)
  points(subset(x,y==-1),col="red",pch="-",cex=2)
# compute intercept on y axis of separator from w and b
  intercept <- - p$b / p$w[[2]]
# compute slope of separator from w
  slope <- - p$w[[1]] /p$ w[[2]]
# draw separating boundary
  abline(intercept,slope,col="green")

  