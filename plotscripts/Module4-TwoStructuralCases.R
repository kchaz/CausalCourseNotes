
plotter <- function(Y0,Y1, title){
  jitter_amount = 0.05  # Adjust this value to control the amount of jitter
  jitter_x0 = jitter(rep(0, n), amount = jitter_amount)
  jitter_x1 = jitter(rep(1, n), amount = jitter_amount)
  
  lim = c(min(Y0, Y1), max(Y0, Y1))
  plot(jitter_x0, Y0,
       ylim = lim,
       xlim = c(-.5, 1.5),
       xaxt = "n", # removes the x-axis ticks and labels
       xlab = "",
       ylab = "Y(T)",
       col = "blue",
       pch = 20,
       main = title,
       cex.lab = 1.5,
       cex.axis = 1.5,
       cex = 1.5
  )
  points(jitter_x1, Y1, col = "orange", pch = 20, cex = 1.5)
  
  # Adding custom axis labels 
  axis(1, at = c(0, 1), labels = c("T = 0", "T = 1"), cex.axis = 1.5)
  
  # Adding lines connecting elements of Y0 and Y1
  for (i in 1:n) {
    segments(jitter_x0[i], Y0[i], jitter_x1[i], Y1[i], col = "gray")
  }
  
}


set.seed(60)

par(mfrow = c(1,2))
n = 22
alpha = 2
beta = 4

# model 1
epsilons = rnorm(n, 0, 1)
Y0 = alpha + epsilons
Y1 = alpha + beta + epsilons
plotter(Y0,Y1, title = "Constant Additive Treatment Effect")

# model 2
epsilon0 = rnorm(n, 0, 1)
epsilon1 = rnorm(n, 0, 1) #homoskedasticity
Y0 = alpha + epsilon0
Y1 = alpha + beta + epsilon1 
plotter(Y0,Y1, title = "Heterogeneous Treatment Effect")
