#### sak.r
#### R script for SAK project
#### Charles, 09/22/2017
#############################

###############
### Setup R ###
###############

### Clear terminal
cat("\014")

### Clear space
rm(list = ls())

### library packages
library(rio)
library(dplyr)
library(mlr)
library(lattice)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(sandwich)
library(lmtest)
library(lmPerm)
library(coefplot)
library(stargazer)

### Load custom functions
#### Multiplot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

setwd("~/Dropbox/SAK")

##################
### Setup Data ###
##################

dat <- import("data/sak_data.csv")
colnames(dat)
head(dat)
dat$new_id <- 1:nrow(dat)
full <- nrow(dat)

### Drop observations for fast track prosecutors
table(dat$akl_id)
dat <- dat[dat$akl_id != 23, ]
full - nrow(dat) ### Dropped 431 observations
dat$decision_date <- as.Date(dat$decision_date, format = "%d-%b-%Y")
head(dat$decision_date)
drop <- dat[dat$akl_id == 30 & dat$decision_date > "2015-07-01", ]
dat <- dat[!(dat$new_id %in% drop$new_id), ]
full - nrow(dat) ### Dropped 958 observations total
nrow(dat)/full

####################
### Examine Data ###
####################

table(dat$yr_started)
table(dat$yr_completed)
table(dat$akl_id)
table(dat$akl_m)
table(dat$akl_id, dat$akl_m)
dat$akl_m[dat$akl_id == 15] <- 1
table(dat$akl_id, dat$akl_m)
table(dat$decision)
table(dat$vacay)

### Subset dataframe to non-prosecution cases
dat <- dat[dat$decision != 3, ]
table(dat$decision)

### Create new variables
dat$investigate <- NA
dat$investigate[dat$decision == 2] <- 1
dat$investigate[dat$decision == 1] <- 0

dat$decision_words <- NA
dat$decision_words[dat$decision == 1] <- "Not Investigated"
dat$decision_words[dat$decision == 2] <- "Investigated"

dat$sex <- NA
dat$sex[dat$akl_m == 1] <- "Male"
dat$sex[dat$akl_m == 0] <- "Female"

dat$female <- NA
dat$female[dat$akl_m == 0] <- 1
dat$female[dat$akl_m == 1] <- 0
table(dat$female)

#################
### Plot Data ###
#################

par(mfrow=c(1, 3))
### Cases Per Year Plot
dates <- data.frame(cbind(as.vector(table(dat$yr_started)), as.vector(table(dat$yr_completed)), as.numeric(c(2013:2016))))
colnames(dates) <- c("year_start", "year_complete", "year")
library(rccdates)
dates$year <- as.year(dates$year)
par(mfrow=c(1, 3))
plot(dates$year, dates$year_start, type = 'l', lwd = '5', bty = 'n', axes = F,
     xlab = "Year", ylab = "Year Filed", main = "Complaints Filed Per Year",
     cex.lab = 1.3, ylim = c(0, 1000))
axis(side = 1, at = c(2013:2016))
axis(side = 2, at = seq(0, 1000, by = 100))

### Cases by Prosecutor Sex Plot
freqs <- as.numeric(table(dat$akl_m))
ylim <- c(0, 1.1*max(freqs))
xx <- barplot(table(dat$akl_m), 
        names.arg = c("Female", "Male"),
        main = "Complaints by Prosecutor Sex",  
        border = "white", col = "#AAAAAA", yaxt = 'n', width = 0.85, ylim = ylim,
        cex.names = 1.3)
text(x = xx, y = freqs, label = freqs, pos = 3, cex = 1.3, col = "#000000")

### Outcomes Plot
#freqs <- as.numeric(table(dat$decision))
#ylim <- c(0, 1.1*max(freqs))
#xx <- barplot(table(dat$decision), 
#              names.arg = c("Not Investigated", "Investigated"),
#              main = "Complaint Outcomes",  
#              border = "white", col = "#AAAAAA", yaxt = 'n', width = 0.85, ylim = ylim,
#              cex.names = 1.3)
#text(x = xx, y = freqs, label = freqs, pos = 3, cex = 1.3, col = "#000000")

### Cases by Prosecutor Sex Plot
#freqs <- as.numeric(table(dat$akl_m))
#ylim <- c(0, 1.1*max(freqs))
#xx <- barplot(table(dat$akl_m), 
#              names.arg = c("Female", "Male"),
#              main = "Complaints by Prosecutor Sex",  
#              border = "white", col = "#AAAAAA", yaxt = 'n', width = 0.85, ylim = ylim,
#              cex.names = 1.3)
#text(x = xx, y = freqs, label = freqs, pos = 3, cex = 1.3, col = "#000000")

### Outcomes Plot
freqs <- as.numeric(table(dat$decision))
ylim <- c(0, 1.1*max(freqs))
xx <- barplot(table(dat$decision), 
              names.arg = c("Not Investigated", "Investigated"),
              main = "Complaint Outcomes",  
              border = "white", col = "#AAAAAA", yaxt = 'n', width = 0.85, ylim = ylim,
              cex.names = 1.3)
text(x = xx, y = freqs, label = freqs, pos = 3, cex = 1.3, col = "#000000")

### Outcomes by Prosecutor Sex Plot
par(mfrow=c(1, 1))
# Simple Bar Plot 
sciplot::bargraph.CI(x.factor = sex, 
                     response = investigate, 
                     data = dat,
                     ylim = c(0, 1),
                     col = c("#333333", "#AAAAAA"),
                     main = "Complaint Outcomes by Prosecutor Sex", 
                     xlab = "",
                     ylab = "% Complaints Investigated") 

par(mfrow=c(2, 2))
sciplot::bargraph.CI(x.factor = sex, 
                     response = investigate, 
                     data = dat[dat$yr_started == 2013, ],
                     ylim = c(0, 1),
                     col = c("#333333", "#AAAAAA"),
                     main = "Complaint Outcomes by Prosecutor Sex (2013)", 
                     xlab = "",
                     ylab = "% Complaints Investigated") 
sciplot::bargraph.CI(x.factor = sex, 
                     response = investigate, 
                     data = dat[dat$yr_started == 2014, ],
                     ylim = c(0, 1),
                     col = c("#333333", "#AAAAAA"),
                     main = "Complaint Outcomes by Prosecutor Sex (2014)", 
                     xlab = "",
                     ylab = "% Complaints Investigated") 
sciplot::bargraph.CI(x.factor = sex, 
                     response = investigate, 
                     data = dat[dat$yr_started == 2015, ],
                     ylim = c(0, 1),
                     col = c("#333333", "#AAAAAA"),
                     main = "Complaint Outcomes by Prosecutor Sex (2015)", 
                     xlab = "",
                     ylab = "% Complaints Investigated") 
sciplot::bargraph.CI(x.factor = sex, 
                     response = investigate, 
                     data = dat[dat$yr_started == 2016, ],
                     ylim = c(0, 1),
                     col = c("#333333", "#AAAAAA"),
                     main = "Complaint Outcomes by Prosecutor Sex (2016)", 
                     xlab = "",
                     ylab = "% Complaints Investigated") 

####################
### Analyze Data ###
####################

ks.test(as.factor(dat$decision[dat$female == 1]), dat$decision[dat$female == 0])
chisq.test(table(dat$decision_words, dat$sex))
fisher.test(table(dat$decision_words, dat$sex))
library(Barnard)
barnard.test(table(dat$decision_words, dat$sex)[1],
             table(dat$decision_words, dat$sex)[2],
             table(dat$decision_words, dat$sex)[3],
             table(dat$decision_words, dat$sex)[4])

### LM models
#############

mod.1 <- lm(dat$decision ~ dat$female)
summary(mod.1)
mod.1$newse <- vcovHC(mod.1, type = c("HC1"))
coeftest(mod.1, mod.1$newse)
m1_df <- coef(summary(mod.1)) %>% 
  data.frame() %>% 
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m1_df$std.error <- sqrt(diag(mod.1$newse))
m1_df
m1a_df <- mutate(m1_df, 
                 conf.low = estimate - qnorm(.975) * std.error,
                 conf.high = estimate + qnorm(.975) * std.error) %>% 
  # create the lower and upper bounds
  select(-std.error) # remove the std.error

mod.2 <- lm(dat$decision ~ dat$female + 
              as.factor(dat$yr_started))
summary(mod.2)
mod.2$newse <- vcovHC(mod.2, type = c("HC1"))
coeftest(mod.2, mod.2$newse)
m2_df <- coef(summary(mod.2)) %>% 
  data.frame() %>% 
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m2_df$std.error <- sqrt(diag(mod.2$newse))
m2_df
m2a_df <- mutate(m2_df, 
                 conf.low = estimate - qnorm(.975) * std.error,
                 conf.high = estimate + qnorm(.975) * std.error) %>% 
  # create the lower and upper bounds
  select(-std.error) # remove the std.error

mod.3 <- lm(dat$decision ~ dat$female + 
              as.factor(dat$yr_started) + 
              as.factor(dat$vacay))
summary(mod.3)
mod.3$newse <- vcovHC(mod.3, type = c("HC1"))
coeftest(mod.3, mod.3$newse)
m3_df <- coef(summary(mod.3)) %>% 
  data.frame() %>% 
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m3_df$std.error <- sqrt(diag(mod.3$newse))
m3_df
m3a_df <- mutate(m3_df, 
                 conf.low = estimate - qnorm(.975) * std.error,
                 conf.high = estimate + qnorm(.975) * std.error) %>% 
  # create the lower and upper bounds
  select(-std.error) # remove the std.error

mod.4 <- lm(dat$decision[dat$yr_started == 2013] ~ dat$female[dat$yr_started == 2013]
            + dat$vacay[dat$yr_started == 2013])
summary(mod.4)
mod.4$newse <- vcovHC(mod.4, type = c("HC1"))
coeftest(mod.4, mod.4$newse)
m4_df <- coef(summary(mod.4)) %>% 
  data.frame() %>% 
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m4_df$std.error <- sqrt(diag(mod.4$newse))
m4_df
m4a_df <- mutate(m4_df, 
                 conf.low = estimate - qnorm(.975) * std.error,
                 conf.high = estimate + qnorm(.975) * std.error) %>% 
  # create the lower and upper bounds
  select(-std.error) # remove the std.error

mod.5 <- lm(dat$decision[dat$yr_started == 2014] ~ dat$female[dat$yr_started == 2014]
            + dat$vacay[dat$yr_started == 2014])
summary(mod.5)
mod.5$newse <- vcovHC(mod.5, type = c("HC1"))
coeftest(mod.5, mod.5$newse)
m5_df <- coef(summary(mod.5)) %>% 
  data.frame() %>% 
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m5_df$std.error <- sqrt(diag(mod.5$newse))
m5_df
m5a_df <- mutate(m4_df, 
                 conf.low = estimate - qnorm(.975) * std.error,
                 conf.high = estimate + qnorm(.975) * std.error) %>% 
  # create the lower and upper bounds
  select(-std.error) # remove the std.error

mod.6 <- lm(dat$decision[dat$yr_started == 2015] ~ dat$female[dat$yr_started == 2015]
            + dat$vacay[dat$yr_started == 2015])
summary(mod.6)
mod.6$newse <- vcovHC(mod.6, type = c("HC1"))
coeftest(mod.6, mod.6$newse)
m6_df <- coef(summary(mod.6)) %>% 
  data.frame() %>% 
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m6_df$std.error <- sqrt(diag(mod.6$newse))
m6_df
m6a_df <- mutate(m6_df, 
                 conf.low = estimate - qnorm(.975) * std.error,
                 conf.high = estimate + qnorm(.975) * std.error) %>% 
  # create the lower and upper bounds
  select(-std.error) # remove the std.error

mod.7 <- lm(dat$decision[dat$yr_started == 2016] ~ dat$female[dat$yr_started == 2016]
            + dat$vacay[dat$yr_started == 2016])
summary(mod.7)
mod.7$newse <- vcovHC(mod.7, type = c("HC1"))
coeftest(mod.7, mod.7$newse)
m7_df <- coef(summary(mod.7)) %>% 
  data.frame() %>% 
  tibble::rownames_to_column("term") %>%
  rename(estimate = Estimate, std.error = Std..Error)
m7_df$std.error <- sqrt(diag(mod.7$newse))
m7_df
m7a_df <- mutate(m7_df, 
                 conf.low = estimate - qnorm(.975) * std.error,
                 conf.high = estimate + qnorm(.975) * std.error) %>% 
  # create the lower and upper bounds
  select(-std.error) # remove the std.error

estimates <- rbind(m1_df[2, 2:3],
                   m2_df[2, 2:3],
                   m3_df[2, 2:3])
round(estimates, 3)

### Plot estimates
estimates <- rbind(m1_df[2, 2:3],
                   m2_df[2, 2:3],
                   m3_df[2, 2:3])
rownames(estimates) <- c("mod.1", "mod.2", "mod.3")
par(mfrow=c(1,1))
var.names <- rev(c("Model 1", "Model 2", "Model 3"))
par(
  oma = c(0,0,0,0), # Since it is a single plot, I set the outer margins to zero.
  mar = c(5,4,5,4) # margins adjusted to reflect changing the locations of the labels
)
# create an empty plot for total customization
plot(NULL, # create empty plot
     xlim = c(-0.01, 0.3), # set xlim by guessing
     ylim = c(1, length(var.names)), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA) 
abline(v=0, lty=3)
est <- as.numeric(rev(estimates[ ,1])) # conveniently store the estimates (minus the constant)
se <- as.numeric(rev(estimates[ ,2])) # conveniently store the std. errors (minus the constant)
for (i in 1:length(est)) { # loop over a counter the length of the estimate vector
  lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd = 7, col="#666666")
  lines(c(est[i] + 1.64*se[i], est[i] - 1.64*se[i]), c(i, i), lwd = 7, col="#333333")
  points(est[i], i, pch = 19, cex = 1.5, col="white") 
  points(est[i], i, pch = 19, cex = 1.25, col="black") 
  text(est[i], i, var.names[i], xpd = T, cex = .9, pos = 3) # add variable labels above the points
}
axis(side = 1) # add bottom axis
mtext(side = 1, "Estimated Treatment Effects", line = 2.5) 

### Plot estimates
estimates <- rbind(m4_df[2, 2:3],
                   m5_df[2, 2:3],
                   m6_df[2, 2:3],
                   m7_df[2, 2:3])
rownames(estimates) <- c("mod.4", "mod.5", "mod.6", "mod.7")
par(mfrow=c(1,1))
var.names <- rev(c("Model 4 (2013)", "Model 5 (2014)", "Model 6 (2015)", "Model 7 (2016)"))
par(
  oma = c(0,0,0,0), # Since it is a single plot, I set the outer margins to zero.
  mar = c(5,4,5,4) # margins adjusted to reflect changing the locations of the labels
)
# create an empty plot for total customization
plot(NULL, # create empty plot
     xlim = c(-0.04, 0.3), # set xlim by guessing
     ylim = c(1, length(var.names)), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA) 
abline(v=0, lty=3)
est <- as.numeric(rev(estimates[ ,1])) # conveniently store the estimates (minus the constant)
se <- as.numeric(rev(estimates[ ,2])) # conveniently store the std. errors (minus the constant)
for (i in 1:length(est)) { # loop over a counter the length of the estimate vector
  lines(c(est[i] + 1.96*se[i], est[i] - 1.96*se[i]), c(i, i), lwd = 7, col="#666666")
  lines(c(est[i] + 1.64*se[i], est[i] - 1.64*se[i]), c(i, i), lwd = 7, col="#333333")
  points(est[i], i, pch = 19, cex = 1.5, col="white") 
  points(est[i], i, pch = 19, cex = 1.25, col="black") 
  text(est[i], i, var.names[i], xpd = T, cex = .9, pos = 3) # add variable labels above the points
}
axis(side = 1) # add bottom axis
mtext(side = 1, "Estimated Treatment Effects", line = 2.5) 

#########################
### Robustness Checks ###
#########################

mod.logit.1 <- glm(as.factor(dat$decision) 
                 ~ dat$female, 
                 family = "binomial")
summary(mod.logit.1)

mod.logit.2 <- glm(as.factor(dat$decision) 
                 ~ dat$female + 
                   as.factor(dat$yr_started), 
                 family = "binomial")
summary(mod.logit.2)

mod.logit.3 <- glm(as.factor(dat$decision) 
                 ~ dat$female + 
                 as.factor(dat$yr_started) + 
                 as.factor(dat$vacay), 
                 family = "binomial")
summary(mod.logit.3)
#exp(coef(mod.logit))

mod.probit.1 <- glm(as.factor(dat$decision) 
                    ~ dat$female,  
                    family = binomial(link = "probit"))
summary(mod.probit.1)

mod.probit.2 <- glm(as.factor(dat$decision) 
                    ~ dat$female + 
                      as.factor(dat$yr_started),  
                    family = binomial(link = "probit"))
summary(mod.probit.2)

mod.probit.3 <- glm(as.factor(dat$decision) 
                  ~ dat$female + 
                  as.factor(dat$yr_started) + 
                  as.factor(dat$vacay),  
                  family = binomial(link = "probit"))
summary(mod.probit.3)

### Permutationt tests
mod.1ri <- lmp(dat$decision ~ dat$female)
summary(mod.1ri)

### Jackknife
library(bootstrap)
theta <- function(x, dat, coefficient){
  coef(lm(mod.3 , data = dat[unique(dat$akl_id), ]))[coefficient] }
res <- jackknife(1:length(dat$akl_id), theta, dat = dat[], coefficient = "dat$female")
summary(res)
res
