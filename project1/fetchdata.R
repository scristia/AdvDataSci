rm(list=ls())
library(rvest)
library(plyr)
library(dplyr)
url = "http://injuryprevention.bmj.com/content/12/6/365/T2.expansion.html"
x = html(url)
tab = html_table(x)[[1]]
tab$Year = as.numeric(tab$Year)
tab = tab[-1, ]
tab = tab[-nrow(tab), ]

# using dplyr syntax
tab = filter(tab, !is.na(Year))

# renaming a column
tab = plyr::rename(tab, c("Year" = "year", "Population" = "population", 
                          "Firearm suicide" = "gun_suicide", "Firearm homicide" = "firearm_homicide",
                          "Homicide minus mass shootings" = "homicide_no_mass","Unintentional" = "unintentional",
                          "Undetermined" = "undetermined", "Total"= "total", "Total non-firearm homicide" = "all_non_gun_homicide",
                          "Suicide by all methods including firearm" = "all_suicide",
                          "Homicide by all methods including firearm" = "all_homicide",
                          "Total non-firearm suicide" = "other_suicide"))
tab$population <- gsub(" ", "", tab$population)
tab$gun_suicide = trimws(tab$gun_suicide)

# Splitting the string on spaces, then taking first element
# then turning that into a number
ss = strsplit(tab$firearm, " ")
ss = sapply(ss, function(x) x[1])

?ss = as.numeric(ss)

# Using a regular expression (regex) to extract the element we want
pop = gsub("(.*) \\(.*\\)", "\\1", tab$firearm)
df <- sapply(1:ncol(tab), function(x) as.numeric(gsub("(.*) \\(.*\\)", "\\1", tab[,x])))
colnames(df) <- colnames(tab)
df <- as.data.frame(df)
write.csv(df, "australia_longitudinal.csv", row.names=FALSE)
df <- read.csv("australia_longitudinal.csv")

df$treatment <- ifelse(df$year < 1997, 0, 1)
## methods for longitudinal data
## xgee
gunhom <- df$homicide_no_mass/df$population * 1e5
allhom <- df$all_homicide/df$population * 1e5
otherhom <- df$all_non_gun_homicide/df$population * 1e5
plot(df$year, allhom, type="l",ylim=c(0,2.4), main="Homicide trends", xlab="Year", ylab="Homicides per 100,000") ## per 100k
lines(df$year, gunhom, lty=6, col="gray40")
lines(df$year, otherhom, lty=2, col="gray40")
legend("topright", c("All Homicides", "Non-firearm homicides", "Firearm homicides"), cex=0.5, lty=c(1,6,2), col=c("black", "gray40", "gray40"), bty="n")
abline(v=1997, col="tomato2")

gunhom.ts <- ts(gunhom, frequency=1, start=c(1979,1))


### segmented regression
df$gunhomratio <- gunhom
df$time <- 1:nrow(df)

### add time covariates. Postslope for interaction intervention and time.
df$time2 <- df$time - 18
df$postslope <- ifelse(df$treatment==0, 0, df$time2)

acf(df$gunhomratio)
acf(df$allhom)


model <- lm(log(gunhomratio) ~ time + treatment + time:treatment, data=df)
model
model2 = gls(log(gunhomratio) ~ time + treatment + postslope, data = df, correlation=corARMA(p=2))
model2
summary(model2)

### plot predicted trends pre and post intervention
pre.inv <- predict(model2, data.frame(treatment= 0, time = 1:18, postslope = 0))
post.inv <- predict(model2, data.frame(treatment = 1, time = 19:25, postslope = 1:7))

plot(log(gunhomratio) ~ time, data=df, cex=0.7)
lines(1:18,pre.inv, col = 'red', lwd = 2)
lines(19:25, post.inv, col = 'red', lwd = 2)

## extrapolate from pre intervention
pre.extrap <- predict(model2, data.frame(treatment= 0, time = 19:25, postslope = 1:7))
lines(19:25, pre.extrap, col = 'blue', lwd = 2, lty = 2)
