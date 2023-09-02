#### DAF Assignment 2 script: Group 19 - Telecommunications ####
library(data.table)
library(ggplot2)
library(modelsummary)
library(openxlsx)
library(dplyr)
library(lubridate)
library(bit64)
library(fixest)

win <- function(x, eps = 0.005){
  up <- quantile(x, na.rm = T, 1 - eps)
  down <- quantile(x, na.rm = T, eps)
  x[x>up] <- up
  x[x<down] <- down
  return(x)
}

na0 <- function(x)
{
  x[is.na(x)] <- 0
  return(x)
}

plotcolours = c("#DDEBF6", "#BDD7EE", "#9CC3E5", "#2E76B5", "#1E4E79")

#### Data #################################################################
library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path)) # set wording directory to current file location

#setwd("C:/Users/aydin/OneDrive/Documents/MCFIN - 2020-2023/- 2023 Semester 1/Data Analysis for Finance - FNCE90083/DAF working directory")
ffdata <- fread("FF_Portfolios.csv")
comp <- fread("compustat.csv")
crsp <- fread("crsp.csv")
inst_own <- fread("institutional_ownership.csv")

# data cleaning
ffdata[, date := ymd(dateff)] # apply date format
ffdata[, year := year(date)] # new year column

crsp[, date := ymd(date)] # apply date format
crsp[, RET := as.numeric(RET)] # change format of RET to numeric.
crsp[["RET"]][is.na(crsp[["RET"]])] <- 0 # replace NAs with 0
crsp[, PRC := abs(PRC)] # make PRC absolute values
crsp[, capitalization := abs(PRC)*SHROUT] # market cap: number of shares in millions

# merging data crsp and FF data
crsp <- merge(crsp, ffdata, by = "date", all.x = T) #all.x = keeping all crsp data
crsp[, retrf := RET - rf] #  calculate excess returns

# scale to monthly data
crsp.dec <- crsp[month(date) == 12] # select only end of year december data

#find all PERMNOs in telecom industry (FF_48 == 32) and filter in crsp, then merge with comp
telecom_PERMNOs <- unique(comp[FF_48 == 32]$PERMNO) # all unique PERMNOs in FF_48 == 32, unique N=724
telecom.crsp <- crsp[PERMNO %in% telecom_PERMNOs] # unique N=603 
telecom <- merge(telecom.crsp, comp, by = c("PERMNO","year"), all.x = T) # unique N=603 merge monthly data to annual comp data
  
# end of year data
telecom.dec <- telecom[month(date) == 12] # change to annual data, unique N=603
telecom.dec[, size := abs(PRC)*SHROUT] # size: number of shares in millions

#### Part 1: Introduction ###############################################
# select telecom industry using combined monthly data
# order monthly telecom data by year then PERMNO
setkey(telecom.dec, year, PERMNO)

# Check number of firms in entire data set
Nfirms_all <- uniqueN(telecom.dec$PERMNO) #N=603

# Number of firms in telecom industry per year
Nfirms <- telecom.dec[, .(Nfirms = uniqueN(PERMNO)), by = year]
print(telecom.dec$PERMNO %>% uniqueN()) #N=603
setkey(Nfirms, year) # order Nfirms vector by year 
mean_NFirms_year <- mean(Nfirms[,Nfirms]) # take mean of all years

# Plot number of firms in telecom industry per year (1990-2022)
Nfirms_plot <- ggplot(Nfirms, aes(x=year,y=Nfirms)) +
  geom_bar(stat = "identity", fill = "#2E76B5") +
  geom_text(aes(label = Nfirms), vjust = -0.5, size = 3) +
  geom_hline(yintercept = colMeans(Nfirms[,2])) +
  ggtitle("Number of firms in Telecom industry across time") +
  labs(caption = "Filtered on monthly CRSP data merged with annual Compstat data") +
  xlab("Year (1990-2022)") + ylab("Number of firms") +
  annotate("text", x=2020, y=(colMeans(Nfirms[,2])+10), label= paste0("Mean = ",round(mean_NFirms_year,4)))# horizontal mean line

# Number of IPOs (new firms)
years <- unique(telecom.dec$year) # vector of years in data
new_permno <- data.table() # blank data.table

for (i in years) { # for loop to make list of all PERMNOs in year i-1,
  # then filter out these PERMNOs in year i to determine number of new entrants.
  
  # determine vector of PERMNO in year i-1
  PERMNO_vector <- c() # empty list in each loop
  tmp.year <- telecom.dec[year == i-1] # filter telecom data by year before
  PERMNO_vector <- unique(tmp.year$PERMNO) # include all unique PERMNOs into vector
  
  # filter next year data.table by PERMNOs !not in! vector of PERMNO in year i-1
  tmp.next_year <- telecom.dec[year == i] # filter telecom data by next year
  tmp.next_year <- tmp.next_year[!(tmp.next_year$PERMNO %in% PERMNO_vector)] # filter by new PERMNOs
  tmp.dt_row = data.table("year" = i, "IPOs" = uniqueN(tmp.next_year$PERMNO)) # create new row of number of new firms to data.table
  new_permno <- rbindlist(list(new_permno,tmp.dt_row)) # add row to existing data.table
}

# add to Nfirms IPO vector data.table
Nfirms[, IPOs := new_permno[,IPOs]]
sum(Nfirms[,IPOs]) #N=624, may be the case that PERMNOs re-enter the dataset.

# Plot number of IPOs in each year
Nfirms_IPO_plot <- ggplot(Nfirms, aes(x=year,y=IPOs)) +
  geom_bar(stat = "identity", fill = "#2E76B5") +
  geom_text(aes(label = IPOs), vjust = -0.5 , size = 3) +
  ggtitle("Number of IPOs in Telecom industry across time") +
  xlab("Year (1990-2022)") + ylab("Number of IPOs")

# remove first year in plot and highlight year with highest number of IPOs
Nfirms2 <- Nfirms[2:33,]
Nfirms_IPO_plot2 <- ggplot(Nfirms2, aes(x=year,y=IPOs)) +
  geom_bar(stat = "identity", fill="#9CC3E5") +
  geom_bar(data = Nfirms2[which.max(Nfirms2$IPOs),], fill="#2E76B5", stat = "identity") +
  geom_text(aes(label = IPOs), vjust = -0.5, size = 2.5, fontface="bold") +
  ggtitle("Number of IPOs in Telecom industry across time") +
  labs(subtitle = "Removed year 1990 since all PERMNOs appear for first time in 1990",
       caption = "1999 had the most IPOs (53 new companies). 
       IPOs prior to 2001 were much higher than pre-2001.") +
  xlab("Year (1990-2022)") + ylab("Number of IPOs")

#### Part 2: Industry composition ##################################################

# What are the top 10 largest companies in the industry?
##### Use 2022 crsp data and sort by capitalization
largest_10_companies <- telecom.dec[year == 2022,] # all observations at end of 2022
largest_10_companies <- largest_10_companies[,.SD[.N], by=PERMNO] # last obs for PERMNO in 2022
setorder(largest_10_companies, cols = - "capitalization") # rank by capitalization
telecom_marketcap_2022 <- sum(largest_10_companies$capitalization) # market size of all companies
largest_10_companies[, market_share := capitalization/telecom_marketcap_2022] # calculate market share for each firm

largest_10_companies <- largest_10_companies[1:10, c("date","PERMNO","COMNAM","PRC", 
                                                     "SHROUT","capitalization","market_share")] # filter list by top 10
largest_10_companies # table included in powerpoint

# export data.table to excel
# fwrite(largest_10_companies, "Telecom-largest_10_companies.csv") 


#### Part 3: Institutional ownership ###########################################

# merging crsp and comp using matching rows
# adjusted according to Kate's advice on discussion board.
m <- match(paste(comp$PERMNO, comp$year), paste(crsp.dec$PERMNO, crsp.dec$year + 1))
comp$size <- crsp.dec$capitalization[m] # create new size column based on crsp
comp$SHROUT <- crsp.dec$SHROUT[m] # create new shares outstanding column based on crsp

inst_own[, year := year(ymd(date))] # create year column in inst_own data

# merge inst_own data using matching of rows
setkey(comp, PERMNO, year) # order comp data by PERMNO, then year
m <- match(paste(comp$year, substr(comp$cusip,1,8)), paste(year(inst_own$date) + 1, inst_own$cusip))
comp$shares.inst.own <- inst_own$InstOwn[m] # create number of shares own by institutions column
comp[, inst_own := 100*shares.inst.own/(10^3*SHROUT)] # create column of % of inst owned shares

# subset of comp data for telecom industry
telecom.comp <- comp[FF_48 == 32] # filter data by telecom
telecom.comp[, prop_inst_own := shares.inst.own/(10^3*SHROUT)] # create column of % of inst owned shares

#summary of variables
#telecom.comp %>% select("inst_own", "SHROUT", "prop_inst_own", "size") %>% datasummary_skim()

telecom.inst_own <- telecom.comp[, .(inst_own = sum(na0(shares.inst.own))), by =year] # vector of number of shares owned by institutions
telecom.shrout <- telecom.comp[, .(shrout = sum(na0(SHROUT))), by = year] #  vector of total shares outstanding


# create telecom inst summary data.table
telecom.inst <- data.table(year=telecom.inst_own[,year], inst_own=telecom.inst_own[,inst_own],
                           shrout=telecom.shrout[,shrout])
telecom.inst <- telecom.inst[2:31,] # remove missing data in 1990, 2021, 2022 and 2023.
telecom.inst[, prop_inst_own := 100*inst_own/(10^3*shrout)] # column of % of inst ownership per year

telecom_inst_plot <- ggplot(telecom.inst[1:31], aes(x=year,y=prop_inst_own)) +
  geom_bar(stat = "identity", fill="#2E76B5") +
  geom_text(aes(label = paste0(round(prop_inst_own,0),"%")), vjust = -0.5, size = 3.5) +
  ggtitle("Porportion of institutional ownership over time") +
  xlab("Year (1990-2022)") + ylab("Proportion of institutional ownership (%)")

# Create a plot to illustrate how institutional ownership is related to firm size
telecom.comp_non_zero <- telecom.comp[,c("PERMNO","prop_inst_own", "size")]

# winzorise inst_own and size at 5% and 95% quantiles
telecom.comp_non_zero$prop_inst_own <- win(telecom.comp_non_zero$prop_inst_own, 0.05)
telecom.comp_non_zero$inst_own <- win(telecom.comp_non_zero$inst_own, 0.05)
telecom.comp_non_zero$inst_own <- win(telecom.comp_non_zero$size, 0.05)

# remove NAs and 0S
telecom.comp_non_zero <- telecom.comp_non_zero[!is.na(prop_inst_own) & !is.na(size)]
telecom.comp_non_zero <- telecom.comp_non_zero[,log_size := log(size)]
#telecom.comp_non_zero <- telecom.comp_non_zero[,log_prop_inst_own := log(prop_inst_own)]

# scatter plot of % of inst ownership and log(size)
prop_inst_own_size_scatter <- ggplot(telecom.comp_non_zero,aes(y=prop_inst_own, x=log_size,
                                                               color = PERMNO)) +
  geom_point(aes(alpha = 0.3)) +
  geom_smooth(method=lm, se= F, color="#2E76B5") +
  geom_text(aes(x= 7.75, y= 0.42, label = "Coefficient 
                of Log(size) = 
                 0.0464")) +
  ggtitle("Proportion of Inst Ownshership v Log(size)")+
  labs(y = "Proportion of Inst. Ownership (%)", x = "Log(size)",
       subtitle="Both variables winzorised at 5% and 95% quantiles") +
  scale_color_gradientn(colors = plotcolours) +
  theme(legend.position = "none")

# regression of prop_inst_own ~ 1 + log_size to calculate coefficient
prop_size_reg <- feols(prop_inst_own ~ 1 + log_size, telecom.comp_non_zero)
summary(prop_size_reg)

### Top 10 companies with highest average inst ownership
avg_prop_inst_own <- telecom.comp[, mean(prop_inst_own, na.rm = T), by = "PERMNO"] # average % inst ownership across stocks' life
avg_prop_inst_own <- avg_prop_inst_own[!is.na(avg_prop_inst_own$V1)] # remove NAs
setorder(avg_prop_inst_own, - V1) # order by avg % inst ownership
top10_avg_prop_inst_own <- head(avg_prop_inst_own,10) # take top 10 firms
top10_avg_prop_inst_own
#fwrite(top10_avg_prop_inst_own, "Telecom-top_10_avg_prop_inst_own.csv") # export to excel to make table

### Part 4 : Company characteristics panel data ##################################

# combine crsp and comp data using matching of rows
# matching size
m <- match(paste(comp$PERMNO, comp$year), paste(crsp.dec$PERMNO, crsp.dec$year + 1))
comp$size <- crsp.dec$capitalization[m]
comp$SHROUT <- crsp.dec$SHROUT[m]


### tobin's q
comp[, tobin := (at - ceq + size/10^3)/at]
### return on assets
comp[, roa := ib/at]
### leverage
comp[, leverage := (na0(dltt) + na0(dlc))/at]
### sales growth
setkey(comp, PERMNO, year)
comp[, lag.sale := shift(sale, 1, type = "lag"), by = "PERMNO"]
comp[, growth := as.numeric(100*(sale/lag.sale - 1))]
# remove infinite sales growth
comp <- comp[!is.infinite(growth)]
### ppe to assets
comp[, ppe_at := ppent/at]
### ebitda to assets
comp[, ebitda_at := ebitda/at]
### calculating book value
comp[, be := seq - na0(pstkrv) + na0(txditc) + na0(dcvt)]
### market-to-book ratio
comp[, MB := (size/10^3)/(be)]

### institutional ownership (re-loading fresh data)
inst_own <- fread("institutional_ownership.csv")
inst_own[, year := year(ymd(date))]
m <- match(paste(comp$year, substr(comp$cusip,1,8)), paste(year(inst_own$date) + 1, inst_own$cusip))

comp$shares.inst.own <- inst_own$InstOwn[m]
comp[, inst_own := 100*shares.inst.own/(10^3*SHROUT)]

# make company*year panel data frame including Tobin’s Q, ROA, leverage, sales growth, 
# PPE/AT, EBITDA/AT variables, institutional ownership, market-to-book ratio.
allcols <- c("year","PERMNO","conm","tobin","roa","leverage", "growth","ppe_at","ebitda_at","inst_own","MB")

cols <- c("tobin","roa","leverage", "growth","ppe_at","ebitda_at","inst_own","MB")

panel <- data.table(comp[,c("year","PERMNO","conm","tobin","roa","leverage", "growth","ppe_at","ebitda_at","inst_own","MB")],
                    key=c("year","PERMNO")) 
setkey(panel, PERMNO, year) # order data set by PERMNO then year

# filter by telecom PERMNO
telecom.panel <- panel[PERMNO %in% telecom_PERMNOs]

telecom.panel %>%
  select("tobin","roa","leverage", "growth", "ppe_at","ebitda_at","inst_own","MB") %>%
  datasummary_skim(title="Summary statistics: No winzorising of outliers")

# extreme outliers >quantile=0.01 do exist, so we should winzorise
telecom.panel_0.01 <- copy(telecom.panel)
cols <- colnames(telecom.panel_0.01)
for(cl in cols[4:11]) telecom.panel_0.01[[cl]] <- win(telecom.panel_0.01[[cl]],0.01)

telecom.panel_0.01 %>%
  select("tobin","roa","leverage", "growth","ppe_at","ebitda_at","inst_own","MB") %>%
  datasummary_skim(title="Summary statistics: Winzorised at 1% and 99% quantile.")

# extreme outliers >quantile=0.05 do exist, so we should winzorise
telecom.panel_0.05 <- copy(telecom.panel)
cols <- colnames(telecom.panel_0.05)
for(cl in cols[4:11]) telecom.panel_0.05[[cl]] <- win(telecom.panel_0.05[[cl]],0.05)

telecom.panel_0.05 %>%
  select("tobin","roa","leverage", "growth","ppe_at","ebitda_at","inst_own","MB") %>%
  datasummary_skim(title="Summary statistics: Winzorised at 5% and 95% quantile.")

#### Part 5: Stock performance ###############################################
### Plot time series of monthly stock performance in the industry.
# specify Telecom data
telecom.crsp <- telecom.crsp # N=603, filtered in part 0, not including comp data.
setkey(telecom.crsp, PERMNO, date)

# create cumulative geometric return sequence
telecom.crsp[, geo_retrf := (1+retrf)] # (1+r) return in each month
telecom.crsp[, cumretrf := cumprod(geo_retrf), by = PERMNO] # cumulative return (1+r)*(1+r) ... = (1+R) by PERMNO

# distribution of returns
retrf_plot <- ggplot(data = telecom.crsp, mapping = aes(x = date, y = retrf, color = PERMNO)) +
  geom_line() +
  scale_color_gradient(low = "#2E76B5", high = "#9CC3E5") +
  geom_line() +
  stat_summary(geom = "line", fun = mean, aes(group = 1), size = 0.5, color = "black") +
  ggtitle("Telecom stocks' Monthly Return since IPO") +
  xlab("Year (1990-2022)") + ylab("Monthly Return (%)") +
  theme(legend.position = "none")

# specify best and worst performing stock by cumulative return from IPO to last trading month
best_performing_stock <- telecom.crsp[PERMNO == 77418]
worst_performing_stock <- telecom.crsp[PERMNO == 14060]

# # time series of cumulative returns of all telecom stocks
cumretrf_plot <- ggplot(data = telecom.crsp,aes(x=date,y=cumretrf, 
                                                group = PERMNO, color = PERMNO)) +
  geom_line(aes(alpha=0.1)) +
  geom_line(data = best_performing_stock, color="green", size = 1) +
  geom_text(aes(x=ymd("2017-12-31"), y=325, label="Best performing stock:
                TIME WARNER INC"), color="green") +
  geom_line(data = worst_performing_stock, color="red", size = 1) +
  geom_text(aes(x=ymd("2017-12-31"), y=-15, label="Worst performing stock:
                VISLINK TECHNOLOGIES INC"), color="red") +
  ggtitle("Telecom stocks' cumulative return since IPO") +
  xlab("Year (1990-2022)") + ylab("Cumulative return (%)") +
  labs(caption="Cumulative return calculated as (1+r)*(1+r)... = (1+R)") +
  theme(legend.position = "none")
  
  geom_text(aes(x= 8, y= 0.4, label = "Coefficient 
                of Size = 0.0464"))

# restrict y axis from 0% to 50%
cumretrf_plot2 <-  cumretrf_plot + 
  geom_text(aes(x=ymd("2017-12-31"), y=-2, label="Worst performing stock:
                VISLINK TECHNOLOGIES INC"), color="red") +
  ylim(c(-3,50))

# last row for each PERMNO
last_row <- telecom.crsp[,.SD[.N], by=PERMNO]
last_row <- last_row[order(-rank(cumretrf))]
highest_performing_stock <- last_row[order(-rank(cumretrf))][1]
lowest_performing_stock <- tail(last_row[order(-rank(cumretrf))], 1)

top5_PERMNOs <- head(last_row,5)$PERMNO
top5_comnam <- head(last_row,5)$COMNAM
bottom5_PERMNOs <- tail(last_row,5)$PERMNO
bottom5_comnam <- tail(last_row,5)$COMNAM

# 5 highest performing stocks
top5_stocks <- ggplot(data = telecom.crsp[PERMNO %in% top5_PERMNOs],aes(x=date,y=cumretrf, 
                               group = PERMNO, color = PERMNO)) +
  geom_line(size = 1) +
  ggtitle("Telecom top 5 stocks' cumulative return since IPO") +
  xlab("Year (1990-2022)") + ylab("Cumulative return (%)") +
  scale_color_gradient(low="lightgreen", high="darkgreen") +
  theme(legend.position = "none")

bottom5_stocks <- ggplot(data = telecom.crsp[PERMNO %in% bottom5_PERMNOs],aes(x=date,y=cumretrf, 
                                                                        group = PERMNO, color = PERMNO)) +
  geom_line(size = 1) +
  ggtitle("Telecom bottom 5 stocks' cumulative return since IPO") +
  xlab("Year (1990-2022)") + ylab("Cumulative return (%)") +
  scale_color_gradient(low="red", high="darkorange") +
  theme(legend.position = "none")

### Highlight best and worst performing stock in industry.
# Below stocks are the best and worst cumulative return at their last appearance in data set
# held from IPO to last trading month.

# plot of best performing stock
best_cumretrf_plot <- ggplot(data = best_performing_stock, mapping = aes(x=date,y=cumretrf)) +
  geom_line(colour="green", size =1) +
  ggtitle("Best performing firm's cumulative excess returns from IPO to last trading month (TIME WARNER INC)") +
  xlab("Year (1990-2022)") + ylab("Cumulative return (%)") + 
  labs(caption = "'TIME WARNER INC' (PERMNO == 77418) originally went public under the name 'AMERICA ONLINE INC' in 1992 changing it's name to 
       'A O L TIME WARNER INC' in 2001 and to 'TIME WARNER INC' in 2003. 'TIME WARNER INC' concluded trading in May 2018 after being acquired by competitor AT&T with 
       a cumulative return of ~277% since IPO.")
# plot of worst performing stock
worst_cumretrf_plot <- ggplot(data = worst_performing_stock, mapping = aes(x=date,y=cumretrf)) +
  geom_line(colour="red", size = 1) +
  ggtitle("Worst performing firm's cumulative excess returns from IPO to last trading month (VISLINK TECHNOLOGIES INC)") +
  xlab("Year (1990-2022)") + ylab("Cumulative return (%)") + 
  labs(caption = "'VISLINK TECHNOLOGIES INC' (PERMNO == 14060) originally went public under the name 'X G TECHNOLOGY INC' in 2013 changing it's name to 
       'VISLINK TECHNOLOGIES INC' in 2019. VISINK was still trading at end of 2022 with a cumulative return of 0%.")

#### Part 6: Panel regressions ####################################################
# filter for Telecom industry
telecom.comp <- comp[FF_48 == 32]
setkey(telecom.comp, PERMNO, year)

# regress ROA (next year) on institutional ownership percentage, leverage, sales growth, 
# log(size), PPE/AT, EBITDA/AT. Do this with two alternative models:

# creating lead ROA variable 
telecom.comp[, lead.roa := shift(roa, n = 1, type = "lead"), by = PERMNO]
# create log(size) variable
telecom.comp[, log_size := log(size)]
# create inst ownership percentage
telecom.comp[, prop_inst_own := shares.inst.own/(10^3*SHROUT)]
# remove infinite sales growth
telecom.comp <- telecom.comp[!is.infinite(growth)]
# drop missing observations and 0's
telecom.comp <- telecom.comp[!is.na(lead.roa) & !is.na(prop_inst_own) 
                             & !is.na(log_size) & !is.na(growth) & sale > 0]

# data summary
telecom.comp %>%
  select("roa", "prop_inst_own", "leverage", "growth",
         "log_size", "ppe_at","ebitda_at") %>%
  datasummary_skim(fmt=fmt_significant(2))

# names for model summary
new.names <- c('(Intercept)' = 'Constant', "inst_own" = "Inst. Ownership", "leverage" = "Leverage", "growth" = "Sales Growth",
               "log_size" = "Log(Size)", "ppe_at" = "PPE/AT","ebitda_at" = "EBITDA/AT")

# check for heterogeneity
library(gplots)
options(warn=-1)
par(col.axis="black")
plotmeans(inst_own + leverage + growth + log_size + ppe_at + ebitda_at ~ PERMNO,
          main="Heterogeneity across firms (PERMNO)",
          ylab="Explanatory variables", n.label=F, data=telecom.comp)

options(warn=-1)
plotmeans(inst_own + leverage + growth + log_size + ppe_at + ebitda_at ~ year, 
          main="Heterogeneity across years", 
          ylab="Explanatory variables", n.label=F, data=telecom.comp)

# regression list
reg <- NULL
reg[["ROA (No Fixed Effects)"]] <- feols(lead.roa ~ inst_own + leverage + growth + log_size + 
                                           ppe_at + ebitda_at, data = telecom.comp)
reg[["ROA (Company Fixed Effects )"]] <- feols(lead.roa ~ inst_own + leverage + growth + log_size + 
                                                         ppe_at + ebitda_at|PERMNO, data = telecom.comp)
modelsummary(reg, stars = T, fmt=fmt_significant(2), gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 ',
             coef_map = new.names, title = "Pooled OLS with IID standard errors")

# clustered standard errors on company level
reg.cluster.se <- NULL
reg.standard.se <- lapply(reg,  function(x) summary(x, cluster= c("PERMNO")))
modelsummary(reg.standard.se, stars = T, fmt=fmt_significant(2), gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 ',
             coef_map = new.names, title = "Pooled OLS with Company clustered standard errors")

#### Part 7: Data Analysis - Factor exposures ##############################################
# Question: what exposures does the Telecom industry have on the fama-french factors? 
# How do these factor exposure change with different Telecom stock characteristics?
# Does size of company impact factor exposures?

# reload fresh crsp data set
crsp7 <- fread("crsp.csv")
crsp7[, date := ymd(date)] # create date column using lubridate
crsp7[, RET := as.numeric(RET)] # make all RET numeric
crsp7[["RET"]][is.na(crsp7[["RET"]])] <- 0 # replace NAs with 0
crsp7[, PRC := abs(PRC)] # get absolute value of prices
crsp7[, capitalization := abs(PRC)*SHROUT] # market capitalization: number of shares in millions

# merging data crsp and FF data
crsp7 <- merge(crsp7, ffdata, by = "date", all.x = T) #all.x = keeping all crsp data
crsp7[, retrf := RET - rf] # excess returns

# filter crsp data base for telecom firms
telecom = crsp7[PERMNO %in% telecom_PERMNOs]

# panel data variable analysis
telecom %>%
  select("PERMNO","year","retrf","mktrf","smb","hml","umd") %>%
  datasummary_skim(title="Summary statistics: Telecom-factor panel data set ")

# CAPM and FF3 and FF4 models, and fixed effects
models <- NULL

models[["CAPM"]] <- feols(retrf ~ 1 + mktrf,
                          data = telecom)
models[["FF3"]] <- feols(retrf ~ 1 + mktrf + smb + hml,
                         data = telecom)
models[["FF4"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd,
                         data = telecom)
models[["FF4: FE Company"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd|PERMNO,
                                     data = telecom)
models[["FF4: FE Year"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd|year,
                                  data = telecom)
models[["FF4: FE Comp&Year"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd|PERMNO+year,
                                      data = telecom)
modelsummary(models, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log',
             title = "Telecom - Monthly excess return regression analysis")

# clustered standard errors
models.clustered_se <- lapply(models,  function(x) summary(x, cluster= c("PERMNO")))

modelsummary(models.clustered_se, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log',
             title = "Telecom - Monthly excess return regression analysis using clustered standard errors")

# yearly factor exposures

years <- unique(telecom$year) # list of years in data
coefficient_list <- list()

for (i in years){
  tmp <- telecom[year == i]
  reg_noFE <- feols(retrf ~ 1 + mktrf + smb + hml + umd,
                                  data = tmp)
  reg_FE <- feols(retrf ~ 1 + mktrf + smb + hml + umd| PERMNO + year,
                  data = tmp)
  coefficient_list[[i]] <- list(reg_noFE$coefficients,reg_FE$coefficients)
}

# data table of factor exposures for FF4 no FE and FF4FE models
factor_exp_year <- data.table()
for (i in years){
  new_row <- data.table(year = i, 
                        mktrf_noFE = coefficient_list[[i]][[1]][[2]], 
                        smb_noFE = coefficient_list[[i]][[1]][[3]],
                        hml_noFE = coefficient_list[[i]][[1]][[4]],
                        umd_noFE = coefficient_list[[i]][[1]][[5]],
                        mktrf_FE = coefficient_list[[i]][[2]][[1]], 
                        smb_FE = coefficient_list[[i]][[2]][[2]],
                        hml_FE = coefficient_list[[i]][[2]][[3]],
                        umd_FE = coefficient_list[[i]][[2]][[4]])
  factor_exp_year <- rbindlist(list(factor_exp_year, new_row))
}

factor_exp_year_melt <- melt(factor_exp_year, id.vars= "year",
                             measure.vars = c("mktrf_noFE","smb_noFE","hml_noFE","umd_noFE",
                                              "mktrf_FE","smb_FE","hml_FE","umd_FE"))
setkey(factor_exp_year_melt, year)
factor_exp_year_melt  

# plots for mktrf, smb, hml and umd factors across time
fact_exp_mktrf <- factor_exp_year_melt[variable %in% c("mktrf_noFE","mktrf_FE")]
fact_exp_smb <- factor_exp_year_melt[variable %in% c("smb_noFE","smb_FE")]
fact_exp_hml <- factor_exp_year_melt[variable %in% c("hml_noFE","hml_FE")]
fact_exp_umd <- factor_exp_year_melt[variable %in% c("umd_noFE","umd_FE")]

fact_exp_mktrf_plot <- ggplot(fact_exp_mktrf, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Market factor risk exposure across time") +
  xlab("Year (1990-2022)") + ylab("Factor exposure coefficient")

fact_exp_smb_plot <- ggplot(fact_exp_smb, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("SMB risk exposure across time") +
  xlab("Year (1990-2022)") + ylab("Factor exposure coefficient")

fact_exp_hml_plot <- ggplot(fact_exp_hml, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("HML risk exposure across time") +
  xlab("Year (1990-2022)") + ylab("Factor exposure coefficient")

fact_exp_umd_plot <- ggplot(fact_exp_umd, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("UMD risk exposure across time") +
  xlab("Year (1990-2022)") + ylab("Factor exposure coefficient")


### Part 7: Data Analysis - Pooled OLS over different size filters ##############
FE_reg <- NULL

FE_reg[["All Telecom"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd|PERMNO+year,
                                 data = telecom)

FE_reg[["Smallest half"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd|PERMNO+year,
                                           data = telecom.size2[size == "Small"])

FE_reg[["Biggest half"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd|PERMNO+year,
                                           data = telecom.size2[size == "Big"])

FE_reg[["Smallest quintile"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd|PERMNO+year,
                                               data = telecom.size10[size == "very small"])

FE_reg[["Biggest quintile"]] <- feols(retrf ~ 1 + mktrf + smb + hml + umd|PERMNO+year,
                                              data = telecom.size10[size == "very big"])

# clustered standard errors by PERMNO
FE_reg.clustered_se <- lapply(FE_reg,  function(x) summary(x, cluster= c("PERMNO")))

modelsummary(FE_reg.clustered_se, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log',
             title = "Telecom - Monthly excess return regression analysis (with company/year fixed effects) using clustered standard errors",
             notes = 'Small/big companies are readjusted every year according to changes in market capitalization')
