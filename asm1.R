install.packages("lfe")
library(lfe)
install.packages("data.table")
library(data.table) #work with tables
install.packages("lubridate")
library(lubridate) #work with dates
install.packages("ggplot2")
library(ggplot2) 
install.packages("modelsummary")
require(modelsummary)
install.packages("fixest")
require(fixest)
install.packages("dplyr")
require(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("openxlsx")
library(openxlsx)

#load data file
asm1 <- fread("~/Downloads/Unimelb subject/DAF/asm/group asm 1/FNCE90083_2023.csv")

#1.Time series regressions

### create excess return for each variable

for (var in names(asm1)[2:50]) {
  asm1[, paste0("exr_", var) := .SD[[var]] - .SD[["RF"]], .SDcols = c(var, "RF")]
}

### create a table of regression
reg1 <- NULL
for (i in names(asm1)[2:50]){
  if (i %in% colnames(asm1)) {
  reg1[[i]] <- feols(as.formula(paste("exr_", i," ~ `Mkt-RF` + SMB + HML", sep = "")), data = asm1)
  }
}

modelsummary(reg1, stars = T, fmt = 4, statistic = c("t = {statistic}"), out = "R1.html")

#3. Cross-sectional regressions

### get beta and create a beta matrix
beta1 <- list()
beta2 <- list()
beta3 <- list()

for (i in seq_along(reg1)) {
  beta1[[i]] <- unname(reg1[[i]]$coefficients[2])
  beta2[[i]] <- unname(reg1[[i]]$coefficients[3])
  beta3[[i]] <- unname(reg1[[i]]$coefficients[4])
}

mat_beta <- as.matrix(cbind(beta1, beta2, beta3))

### get excess return and create an excess return matrix

exr_list <- vector("list", nrow(asm1))

for (i in 1:595) {
  sub_list <- as.list(asm1[,55:103][i])
  exr_list[[i]] <- sub_list
}

mat_exr <- do.call(rbind, exr_list)

### create new data frame
extend_asm1 <- as.data.frame(cbind(t(mat_exr),mat_beta))
#### convert all variables to numeric
extend_asm1[] <- lapply(extend_asm1, function(x) as.numeric(as.character(x)))

### create a second-stage regression
reg2 <- NULL
for (i in names(extend_asm1)[1:595]){
    reg2[[i]] <- lm(as.formula(paste(i," ~ beta1 + beta2 + beta3", sep = "")), data = extend_asm1)
}
modelsummary(reg2, stars = T, fmt = 4, statistic = c("t = {statistic}", "s.e. = {std.error}"))


### get risk premia & intercept=rpe1

rpre <- list()
for (i in 1:4){
  rpre[[i]] <- list()
for (j in seq_along(reg2)) {
  rpre[[i]][[j]] <- unname(reg2[[j]]$coefficients[i])
  }
}

### final second stage estimator of risk premia & intercept=rpe1

rpre_average <- list()
for (i in 1:4){
rpre_average[[i]] <- mean(sapply(rpre[[i]], mean))
}

rpre_se <- list()
for (i in 1:4){
rpre_se[[i]] <- list()
  for (j in 1:595){
  rpre_se[[i]][[j]] <- (rpre[[i]][[j]] - rpre_average[[i]])^2
  }
}

average_rpre_se <- list()
for (i in 1:4){
average_rpre_se[[i]] <- sqrt(sum(unlist(rpre_se[[i]]))/(595^2))
}

sum(unlist(rpre_se[[1]]))
sum(unlist(rpre_se[[2]]))
sum(unlist(rpre_se[[3]]))
sum(unlist(rpre_se[[4]]))
    
t_rpre <- list()
for (i in 1:4){
  t_rpre[[i]] <- rpre_average[[i]]/average_rpre_se[[i]]
}


### print the result
print(round(as.numeric(rpre_average),4))
print(round(as.numeric(average_rpre_se),4))
print(round(as.numeric(t_rpre),4))


#4. Plot and interpret results

### calculate average excess return, predicted excess return and create name of industry

for (i in 1:49){
  extend_asm1$avg_exr[[i]] <- mean(sapply(extend_asm1[i,1:595], mean))
  extend_asm1$pred_exr[[i]] <- rpre_average[[1]]+rpre_average[[2]]*beta1[[i]]+rpre_average[[3]]*beta2[[i]]+rpre_average[[4]]*beta3[[i]]
  extend_asm1$industry[[i]] <- names(asm1)[i+1]
}

extend_asm1$pred_exr <-as.numeric(extend_asm1$pred_exr)
extend_asm1$avg_exr <-as.numeric(extend_asm1$avg_exr)
extend_asm1$industry <-as.character(extend_asm1$industry)

analysis <- extend_asm1[,599:601]
write.xlsx(analysis, "analysis.xlsx", colNames = TRUE)

### Create a vector of 49 unique colors
my_colors <- vector()
while(length(my_colors) < 49){
  new_color <- sample(colors(), 1)
  if(!(new_color %in% my_colors)){
    my_colors <- c(my_colors, new_color)
  }
}

### Create a plot
plot1 = ggplot(data = extend_asm1, aes(x = pred_exr,y = avg_exr,col=factor(industry)))
plot1 + geom_point(shape="*", size = 12) +
  scale_color_brewer(name = "industry") + 
  scale_color_manual(values = my_colors) +
  geom_abline(slope = 1, intercept = 0, col="black") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(y = "Observed average excess returns (%)", x = "Predicted expected excess returns (%)") 

  
  