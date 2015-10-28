fitbit.predict <- function(data, m, returnfit = FALSE) {
  ### prediction methods: mean, gam, glm, lm
  ### predict minute by minute
  
  ### data should be in long format
  
  ### only predict on days he wears it
  
  ### remove non-wear days
  data <- data[data$wearday == TRUE,]
  ### first get minutes with weartime
  wear <- data$weartime
  data.comp <- data[wear,]
  data.miss <- data[!wear,]
  
  if(m == "mean") {
#    msteps <- ddply(data.comp, .(minute, day), summarize, meansteps = mean(steps))
#  
#    getmeansteps <- function(minute, day, d=msteps){
#      return(d[d$minute == minute & d$day == day,3])
#    }
#    res <- subset(msteps, minute==minute & day==day)$meansteps
#    pred <- apply(data.miss[, c("minute", "day")], 1, function(x) { getmeansteps(as.integer(x[1]), x[2])})
#    fit <- lm(steps ~ minute + day, data=data.comp)
#    pred <- predict(fit, data.miss)
    
    fit = ddply(data.comp[data.comp$weartime==TRUE,], ~ minute, summarize, 
                     mean = mean(steps, na.rm = TRUE), 
                     sd = sd(steps, na.rm = TRUE),
                     n = sum(!is.na(steps))
    )
    pred <- sapply(data.miss[, "minute"], function(x) fit[fit$minute == x,"mean"])
        }
  
  ### fit lm with natural splines
  else if(m == "lm") {
    fit <- lm(steps ~ ns(minute, 30) + factor(day) + factor(month), data=data.comp)
    
    ## predict missing
    pred <- predict(fit, data.miss)
    pred[pred < 0] <- 0 ## steps can not be negative
  }
  
  ## fit with glm(poisson) and splines
  else if(m == "glm") { 
    fit <- glm(steps ~ ns(minute, 30) + factor(day) + factor(month),
               family="poisson", data=data.comp)
    
    pred <- predict(fit, data.miss, type="response")
  }
  
  ## fit with gam
  else if(m == "gam") {
    mod = gam( steps ~ s(minute, bs="cs"), 
               data = data.comp,
               family="poisson")
    p = predict(mod) 
  }
  
  data$steps[!wear] <- pred
  return(list(data = data, fit = fit))
  }

## MSE
fitbit.mse <- function(data, ind, m) {
  data[ind,]$weartime == FALSE
  data.pred <- fitbit.predict(data, m)[[1]]
  return(mean((data[ind,'steps'] - data.pred[ind,'steps'])^2))
}
