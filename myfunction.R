myfunction <- function(x){
  x=rnorm(100)
  mean(x)
}

second <- function(x)
{
  x=x+rnorm(length(x))
}