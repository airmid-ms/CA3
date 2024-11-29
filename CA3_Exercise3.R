# Question 3
### 1
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

### 2
# ei = yi -xTB
# since there's x1 to x3 we have 4 Betas and a variance for parameters

nll_lm <- function(data, par) {
  Beta <- par[1:4]
  sigma <- par[5]

  # format of ei = yi - B0 + B1x1 + B2x2 + B3x3
  ei <- df$y - (Beta[1] + Beta[2]*df$x1 + Beta[3]*df$x2 + Beta[4]*df$x3)

  # negative log likelihood formula
  n <- nrow(df)
  nll <- -(n/2)*log(1/(2*pi*sigma^2)) - (1/(2*sigma^2)) * sum(ei^2)
  return(nll)
}

### 3
mean(df$y) # 20.09062
parameters <- c(mean(df$y),0.1,0.1,0.1,0.0001)

estimate <- optim(par = parameters, fn = nll_lm,data = df,
                  lower = c(-40, -40, -40, -40, -5),
                  upper = c(40, 40, 40, 40, 5))
estimate$par

### 4

# optim() by default finds the minimum of the input it is given. Minimising the negative log likelihood is the same as maximising the log likelihood

### 5
X <- as.matrix(cbind(1, df[, c("x1", "x2", "x3")]))
y <- as.matrix(df$y)

beta_LS3 <- function(X, y) {
  solve(crossprod(X), crossprod(X, y))
}
beta_LS3(X, y)


### 6
yhat <- X %*% beta_LS3(X,y)
y.hat[1:5, 1]

sigma2 <- sum((y - yhat)^2)/(nrow(X) - ncol(X))
sqrt(sigma2)

### 7


### 8
