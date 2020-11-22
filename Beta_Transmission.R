install.packages('coronavirus')
install.packages('lubridate')
install.packages('deSolve')
install.packages("ggplot2")
install.packages('dplyr')
install.packages('gridExtra')
install.packages('openxlsx')
install.packages("reshape")
install.packages('reactable')

library("ggplot2")
library('dplyr')
library('gridExtra')
library('openxlsx')
library('reshape')
library('reactable')
# STATES STUFF

States = read.csv('~/Downloads/current (2).csv')

States$state = factor(States$state, levels = States$state[order(States$positive)])

# Cases By State Plot

ggplot(States, aes(state, positive, fill=positive)) + geom_bar(stat = 'identity') + 
  labs(x= 'States', y= 'Positive Cases', title = 'Cases by State') +
  theme(axis.text.x = element_text(angle = 90, size =6)) + scale_y_continuous(labels = scales::comma) +
  scale_fill_continuous(low = "rosybrown1", high = "red3", label= scales::comma) + labs(fill='Positive Cumulative Cases')

States2 = subset(States, !is.na(States$hospitalizedCumulative))
States2$state = factor(States2$state, levels = States2$state[order(States2$hospitalizedCumulative)])

#Hospitalized Cases by State Plot

ggplot(States2, aes(state, hospitalizedCumulative, fill=hospitalizedCumulative)) + geom_bar(stat = 'identity') + 
  labs(x= 'States', y= 'Hospitalized Cases', title = 'Hospitalized Cases by State') +
  theme(axis.text.x = element_text(angle = 90, size =6)) + scale_y_continuous(labels = scales::comma) +
  scale_fill_continuous(low = "rosybrown1", high = "red3", label= scales::comma)


# COUNTRY WIDE STUFF

USA <- read.csv("~/Downloads/daily (4).csv")
USA$day = nrow(USA):1

# US Positive Cases Line Plot (CUMULATIVE)

ggplot(USA, aes(day, positive, group = 1)) + geom_line() + labs(x= 'Day Since First Case', y= 'Positive Cases', title = 'US Positive Cases') +
  theme(axis.text.x = element_text(size =6)) + scale_x_continuous(breaks=seq(0, nrow(USA), 10)) + scale_fill_discrete(name = "Positive Cases")

# US Positive Cases Line Plot (BY DAY)

ggplot(USA, aes(day, positiveIncrease, group = 1)) + geom_line() + labs(x= 'Day Since First Case', y= 'Positive Cases Each Day', title = 'US Positive Cases by Day') +
  theme(axis.text.x = element_text(size =6)) + scale_x_continuous(breaks=seq(0, nrow(USA), 10)) + scale_fill_discrete(name = "Positive Cases")


# Do PCA to show that people with old population have high hostpitalized to positve ratio???

# State Policy Data

#Clean Data
Policies <- read.xlsx('~/Downloads/COVID-19 US state policy database 5_28_2020.xlsx')
Policies <- subset(Policies, select = c(State, `End/relax.stay.at.home/shelter.in.place`, Began.to.reopen.businesses, `Stay.at.home/.shelter.in.place`, Percent.at.risk.for.serious.illness.due.to.COVID, `Closed.non-essential.businesses`))
names(Policies) = c('State', 'EndShelterinPlace', 'ReopenBuisness', 'ShelterinPlace', 'AtRisk', 'ClosedBuisness')
Policies[Policies == '0.0'] <- NA
Policies[Policies == '0'] <- NA
Policies$EndShelterinPlace[is.na(Policies$ShelterinPlace)] <- NA
Policies$AtRisk = as.numeric(Policies$AtRisk)
ExcelDate <- function(x){
  return(as.Date(as.numeric(x), origin = "1899-12-30"))
}
Policies[c(2,3,4,6)] <- lapply(Policies[c(2,3,4,6)], ExcelDate)

Policies1 <- subset(Policies, select = c('State', 'EndShelterinPlace', 'ShelterinPlace'))
Policies2 <- subset(Policies, select= c('State', 'ReopenBuisness', 'ClosedBuisness'))
Policies2$ReopenBuisness[is.na(Policies2$ClosedBuisness)] <- NA
MeltPolicies1 <- melt(data = Policies1, id_var = State)
MeltPolicies2 <- melt(data = Policies2, id_var = State)
Policies3 <- subset(Policies, select = c('State', 'AtRisk'))

# Shelter in Place Plot

Policies1$EndShelterinPlace[is.na(Policies2$ShelterinPlace)] <- NA

ggplot(Policies1) + geom_segment(data = Policies1, aes(x=State,xend=State,y=ShelterinPlace, yend=as.Date(ifelse(test= is.na(EndShelterinPlace) == TRUE, 
                                                                                                              yes= as.Date('2020-05-28', format = "%Y-%m-%d"), no= EndShelterinPlace), origin = "1970-01-01"))) +
  theme(axis.text.y = element_text(size=7)) +
  geom_point(data= MeltPolicies1, 
             aes(State, value, color=variable, shape= 'circle'), size=2) + 
  labs(x= 'State', y= 'Date', title = 'Duration of Stay at Home Order', color='Stay at Home Status') + 
  scale_color_manual(values = c("EndShelterinPlace" = "green4", "ShelterinPlace" = "red", "ReopenBuisness"= 'blue'), labels= c('Stay-at-Home Start', 'Stay-at-Home End')) +guides(shape=FALSE) + scale_linetype_manual("segment legend",values=c("Duration of Shelter in Place"=2)) +
  coord_flip() 

# Closed Buisness Plot

ggplot(Policies2) + geom_segment(data = Policies2, aes(x=State,xend=State,y=ClosedBuisness, yend=as.Date(ifelse(test= is.na(ReopenBuisness) == TRUE, 
                                                                                                                yes= as.Date('2020-05-28', format = "%Y-%m-%d"), no= ReopenBuisness), origin = "1970-01-01"))) +
  theme(axis.text.y = element_text(size=7)) +
  geom_point(data= MeltPolicies2, 
             aes(State, value, color=variable, shape= 'circle'), size=2) + 
  labs(x= 'State', y= 'Date', title = 'Duration of Closed Business', color='Business Status') + 
  scale_color_manual(values = c("ReopenBuisness" = "green4", "ClosedBuisness" = "red", "ReopenBuisness"= 'blue'), labels= c('Reopen Business Date', 'Close Business Date')) +guides(shape=FALSE) + scale_linetype_manual("segment legend",values=c("Duration of Closed Business"=2)) +
  coord_flip()

#Risk Bar Plot

Policies3$State = factor(Policies3$State, levels = Policies3$State[order(Policies3$AtRisk)])
ggplot(Policies3, aes(State, AtRisk/100, fill=AtRisk)) + geom_bar(stat = 'identity') + labs(x= 'States', y= 'Risk', title = 'Percent of Population at Risk of Serious Illness by State') +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90, size =9))  + scale_y_continuous(labels = scales::percent)  +
  scale_fill_continuous(low = "deepskyblue", high = "dodgerblue4")

# FIND PERCENTAGE OF STATES WITH BUISNESSES CLOSED

DateRange <- seq(as.Date('2020-01-22'), by = "day", length.out = nrow(USA))
DateRange <- as.Date(DateRange, format = "%Y-$m-%d")
Buisness <- Policies2[!is.na(Policies2$ClosedBuisness), ]
Buisness$ReopenBuisness[is.na(Buisness$ReopenBuisness)] <- '2030-01-01'

NumberStaybyDate <- c()
for (date in DateRange){
  NumberofStatesinStay = 0
  for (state in Buisness$State){
    if (Buisness$ClosedBuisness[Buisness$State == state] <= as.Date(date, origin = '1970-01-01')){
      if(Buisness$ReopenBuisness[Buisness$State == state] >= as.Date(date, origin = '1970-01-01')){
        NumberofStatesinStay = NumberofStatesinStay + 1}}}
NumberStaybyDate <- c(NumberStaybyDate, NumberofStatesinStay)}

BuisnessbyDate <- data.frame(cbind(DateRange, NumberStaybyDate))
colnames(BuisnessbyDate) <- c('Date', 'NumStates')
ToDate <- function(x){
  return(as.Date(as.numeric(x), origin = "1970-01-01"))
}
BuisnessbyDate$Date <- ToDate(BuisnessbyDate$Date)
BuisnessbyDate$PercentofStates <- BuisnessbyDate$NumStates/51 #Include District of Columbia
BuisnessbyDate$Day <- 1:nrow(USA)


#Plot Percent of States Closed Buisness

ggplot(BuisnessbyDate, aes(Day, PercentofStates, group = 1)) + geom_line() + labs(x= 'Date', y= 'Percent of States', title = 'Percent of US States with Mandatory Buisness Closings') +
  theme(axis.text.x = element_text(size =6)) + scale_x_continuous(breaks=seq(0, 117, 10))

# FIND PERCENTAGE OF STATES WITH STAY AT HOME

StayAtHome <- Policies1[!is.na(Policies1$ShelterinPlace), ]
StayAtHome$EndShelterinPlace[is.na(StayAtHome$EndShelterinPlace)] <- '2030-01-01'

NumberStaybyDate <- c()
for (date in DateRange){
  NumberofStatesinStay = 0
  for (state in StayAtHome$State){
    if (StayAtHome$ShelterinPlace[StayAtHome$State == state] <= as.Date(date, origin = '1970-01-01')){
      if(StayAtHome$EndShelterinPlace[StayAtHome$State == state] >= as.Date(date, origin = '1970-01-01')){
        NumberofStatesinStay = NumberofStatesinStay + 1}}}
NumberStaybyDate <- c(NumberStaybyDate, NumberofStatesinStay)}

StatesStaybyDate <- data.frame(cbind(DateRange, NumberStaybyDate))
colnames(StatesStaybyDate) <- c('Date', 'NumStates')
ToDate <- function(x){
  return(as.Date(as.numeric(x), origin = "1970-01-01"))
}
StatesStaybyDate$Date <- ToDate(StatesStaybyDate$Date)
StatesStaybyDate$PercentofStates <- StatesStaybyDate$NumStates/51 #Include District of Columbia
StatesStaybyDate$Day <- 1:nrow(USA)


#Plot Percent of States on Stay at home

ggplot(StatesStaybyDate, aes(Day, PercentofStates, group = 1)) + geom_line() + labs(x= 'Date', y= 'Percent of States', title = 'Percent of US States on Stay at Home Order') +
  theme(axis.text.x = element_text(size =6)) + scale_x_continuous(breaks=seq(0, 117, 10))






# BIG PLOT: Percent of States Stay at Home, Percent w closed buisness
StatesStaybyDate1 = StatesStaybyDate
colnames(StatesStaybyDate1) <- c('Date', 'SNumStates', 'SPercentofStates', 'Day')
BuisnessbyDate1 = BuisnessbyDate
colnames(BuisnessbyDate1) <- c('Date', 'BNumStates', 'BPercentofStates', 'Day')
TotalbyDate <- merge(StatesStaybyDate1, BuisnessbyDate1,by=c("Date", 'Day'))
TotalbyDate = TotalbyDate[-c(1,3,5)]
TotalbyDate <- melt(TotalbyDate, id = "Day")

ggplot(TotalbyDate, aes(x =Day, y= value, color = variable)) + 
  geom_line() +
  labs(x= 'Days after First US Case', y= 'Percent of States', title = 'US State COVID Policies', color = "Policy") +
  theme(axis.text.x = element_text(size =8)) + scale_x_continuous(breaks=seq(0, nrow(BuisnessbyDate1), 10)) +
  scale_color_manual(labels = c("Stay at Home", "Closed Buisnesses"), values = c("orchid2", "deepskyblue"))


# REPEAT: US Positive Cases Line Plot (BY DAY) PUT SIDE BY SIDE W ABOVE

ggplot(USA, aes(day, positiveIncrease, group = 1)) + geom_line() + labs(x= 'Day Since First Case', y= 'Positive Cases Each Day', title = 'US Positive Cases by Day') +
  theme(axis.text.x = element_text(size =6)) + scale_x_continuous(breaks=seq(0, nrow(USA), 10))

### BEGIN SIR MODEL WORK: 
library(coronavirus) 
library(lubridate)
library(deSolve)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - 0.2 * I
    dR <- 0.2 * I
    list(c(dS, dI, dR))
  })
}

USA$recovered[is.na(USA$recovered)] <- 0
USA$positiveSIR <- USA$positive-USA$recovered

# Let's do for US from day 1 to fit our model

##intial value
N = 329697910 
t <- 1:nrow(USA)
init <- c(S = (N-1), I = 1, R = 0)


# Define Least-Squares error function
RSS <- function(parameters) {
  names(parameters) <- "beta"
  out <- ode(y = init, times = t, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((rev(USA$positiveSIR) - fit)^2)
}

## Fit to appropriate AVERAGE parameters
USA2 <- USA
USA2 <- subset(USA2, select=c(positiveSIR))
USA2$day <- nrow(USA):1
USA2 <- USA2[order(USA2$day),]

Opt <- optim(c(0),
             RSS,
             method = "L-BFGS-B",
             lower = c(0),
             upper = c(1)
)

Opt_par <- setNames(Opt, c("beta"))
Opt_par 

options(scipen = 999)
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
fit <- subset(fit, select=c(time, I))
colnames(fit) <- c('day', 'Predicted')

fit <- merge(fit, USA2, by= "day")
fit <- melt(fit, id.vars = 'day')

## GRAPH (MIGHT WANNA CHECK THIS AND TRY DIFFERENT PARAMETER MAX/min etc)
ggplot(fit, aes(day, y= value, color = variable)) + 
  geom_line() +
  labs(x= 'Days since Day 1', y= 'Measurement', title = 'US COVID SIR Modeling Total: β=0.37', color = "Attribute") +
  theme(axis.text.x = element_text(size =6))+ scale_x_continuous(breaks=seq(0, 117, 10)) + 
  scale_y_continuous(labels = scales::comma)+scale_color_discrete(name = "Attribute", labels = c("SIR Predicted", "True Positive"))


# Clearly, the representation isnt accurate, becaus our parameters have changed with the 
# the policies implimented over time... our goal is to find the effect of these policies
# on the parameters over time and to predict the parameters in the future based on policy 
# trends in different states

## Do hypothesis testing to see how parameters change with 0% SAH/CH v 80% v 50%

# FIRST HYPOTHESIS TEST FOR NO STAY AT HOME/CB, DO SO WITH Different periods of time in US
# Find SIR model fit predictions for each time period, and do hypothesis tests to decide 
# H0: Parameters are barely higher w policy implimented, H1: Parameters are much higher
#( NOTE GAMMA IS CONSTANT)

# For beta = num of avg close contacts

# 1. For 0% of states having policies implimented: Day 0-60

USA0 <-  subset(USA2, 0 <= USA2$day & USA2$day <= 60)
USA0 <- subset(USA0, select=c(positiveSIR))

N = 329697910 
t0 <- 1:60
init0 <- c(S = (N-1), I = 1, R = 0)


RSS0 <- function(parameters) {
  names(parameters) <- c("beta")
  out <- ode(y = init0, times = t0, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((USA0 - fit)^2)
}

Opt0 <- optim(c(0),
             RSS0,
             method = "L-BFGS-B",
             lower = c(0),
             upper = c(1),
             hessian = TRUE
)

Opt_par0 <- setNames(Opt0$par, c("beta"))
Opt_par0

fit0 <- data.frame(ode(y = init0, times = t0, func = SIR, parms = Opt_par0))
fit0 <- subset(fit0, select=c(time, I))
colnames(fit0) <- c('day', 'Predicted')
fit0 <- merge(fit0, USA2, by= "day")
fit0 <- melt(fit0, id.vars = 'day')

ggplot(fit0, aes(day, y= value, color = variable)) + 
  geom_line() +
  labs(x= 'Days since Day 1', y= 'Measurement', title = 'US SIR Modeling 0% Policy: β=0.37', color = "Attribute") +
  theme(axis.text.x = element_text(size =9)) + scale_x_continuous(breaks=seq(0, 117, 10)) + 
  scale_y_continuous(labels = scales::comma)+scale_color_discrete(name = "Attribute", labels = c("SIR Predicted", "True Positive"))




# 2. For 80% of states having policies implimented: Day 75-95

USA3 <- USA
USA3 <- USA3[order(USA3$day),]
USA3 <- subset(USA3, select=c(day, positiveSIR, positive, recovered))


USA80 <-  subset(USA3, 75 <= USA3$day & USA3$day <= 95)

N = 329697910 
t80 <- 75:95
init80 <- c(S = (N-USA80$positive[USA80$day == 75]), I = USA80$positiveSIR[USA80$day == 75], R = USA80$recovered[USA80$day == 75])


RSS80 <- function(parameters) {
  names(parameters) <- c("beta")
  out <- ode(y = init80, times = t80, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((USA80$positiveSIR - fit)^2)
}

Opt80 <- optim(c(0),
              RSS80,
              method = "L-BFGS-B",
              lower = c(0),
              upper = c(1),
              hessian = TRUE
)

Opt_par80 <- setNames(Opt80$par, c("beta"))
Opt_par80

fit80 <- data.frame(ode(y = init80, times = t80, func = SIR, parms = Opt_par80))
fit80 <- subset(fit80, select=c(time, I))
colnames(fit80) <- c('day', 'Predicted')
fit80 <- merge(fit80, USA2, by= "day") 
fit80 <- melt(fit80, id.vars = 'day')

ggplot(fit80, aes(day, y= value, color = variable)) + 
  geom_line() +
  labs(x= 'Days since Day 1', y= 'Measurement', title = 'US SIR Modeling 80% Policy: β=0.25', color = "Attribute") +
  theme(axis.text.x = element_text(size =10)) + scale_x_continuous(breaks=seq(0, 117, 2)) + 
  scale_y_continuous(labels = scales::comma)+scale_color_discrete(name = "Attribute", labels = c("SIR Predicted", "True Positive"))


# 3. For approximately 50% of states having policies implimented: Day 95-131 (ie. Need 2 graphs)

#95-131
USA50_2 <-  subset(USA3, 95 <= USA3$day & USA3$day <= 131)

N = 329697910 
t50_2 <- 95:131
init50_2 <- c(S = (N-USA50_2$positive[USA50_2$day == 95]), I = USA50_2$positiveSIR[USA50_2$day == 95], R = USA50_2$recovered[USA50_2$day == 95])


RSS50_2 <- function(parameters) {
  names(parameters) <- c("beta")
  out <- ode(y = init50_2, times = t50_2, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((USA50_2$positiveSIR) - fit)^2
}

Opt50_2 <- optim(c(0),
                 RSS50_2,
                 method = "L-BFGS-B",
                 lower = c(0),
                 upper = c(1),
                 hessian = TRUE
)

Opt_par50_2 <- setNames(Opt50_2$par, c("beta"))
Opt_par50_2

fit50_2 <- data.frame(ode(y = init50_2, times = t50_2, func = SIR, parms = Opt_par50_2))
fit50_2 <- subset(fit50_2, select=c(time, I))
colnames(fit50_2) <- c('day', 'Predicted')
fit50_2 <- merge(fit50_2, USA2, by= "day") 
fit50_2 <- melt(fit50_2, id.vars = 'day')

ggplot(fit50_2, aes(day, y= value, color = variable)) + 
  geom_line() +
  labs(x= 'Days since Day 1', y= 'Measurement', title = 'US SIR Modeling 50% Policy: β=0.22', color = "Attribute") +
  theme(axis.text.x = element_text(size =8)) + scale_x_continuous(breaks=seq(0, 140, 5)) + 
  scale_y_continuous(labels = scales::comma)+scale_color_discrete(name = "Attribute", labels = c("SIR Predicted", "True Positive"))

# FINAL BETA VALUES AT 0,50,50,80% state policy implimentation (NUMBER OF PEOPLE INFECTED CONTACT) 

c(Opt_par0, Opt_par50_2, Opt_par80)

#Find confidence intervals of the above using standard error from hession matrix

SE0 <- sqrt(diag(solve(Opt0$hessian)))
SE80 <- sqrt(diag(solve(Opt80$hessian)))
SE50_1 <- sqrt(diag(solve(Opt50_1$hessian)))
SE50_2 <- sqrt(diag(solve(Opt50_2$hessian)))

upper0<-Opt0$par+1.96*SE0
lower0<-Opt0$par-1.96*SE0
CI<-data.frame(value=Opt0$par, upper=upper0, lower=lower0)

upper80<-Opt80$par+1.96*SE80
lower80<-Opt80$par-1.96*SE80
ci80= list(value=Opt80$par, upper= upper80, lower=lower80)
CI = rbind(CI,ci80, stringsAsFactors=FALSE)

upper50_2<-Opt50_2$par+1.96*SE50_2
lower50_2<-Opt50_2$par-1.96*SE50_2
ci50_2= list(value=Opt50_2$par, upper= upper50_2, lower=lower50_2)
CI = rbind(CI,ci50_2, stringsAsFactors=FALSE)

CI['Beta Type'] = c('0% of State closures', '80% of State closures', 'Last 40 days')

View(CI)

# NOW DO HYPOTHESIS TESTING TO DECIDE WHETHER THESE ARE SIGNIFICANT DIFFERENCES (Alternative to modeling)
# DO THIS LATER

# Model for current beta value (Over last 10 days)

USAC <-  subset(USA3, 120 <= USA3$day & USA3$day <= 131)

N = 329697910 
tC <- 120:131
initC <- c(S = (N-USAC$positive[USAC$day == 120]), I = USAC$positiveSIR[USAC$day == 120], R = USAC$recovered[USAC$day == 120])


RSSC <- function(parameters) {
  names(parameters) <- c("beta")
  out <- ode(y = initC, times = tC, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((USAC$positiveSIR - fit)^2)
}

OptC <- optim(c(0),
                 RSSC,
                 method = "L-BFGS-B",
                 lower = c(0),
                 upper = c(1),
                 hessian = TRUE
)

Opt_parC <- setNames(OptC$par, c("beta"))
Opt_parC

fitC <- data.frame(ode(y = initC, times = tC, func = SIR, parms = Opt_parC))
fitC <- subset(fitC, select=c(time, I))
colnames(fitC) <- c('day', 'Predicted')
fitC <- merge(fitC, USA2, by= "day") 
fitC <- melt(fitC, id.vars = 'day')

ggplot(fitC, aes(day, y= value, color = variable)) + 
  geom_line() +
  labs(x= 'Days since Day 1', y= 'Measurement', title = 'US SIR Modeling Current Policy: β=0.22', color = "Attribute") +
  theme(axis.text.x = element_text(size =6)) + scale_x_continuous(breaks=seq(120, 131, 2)) + 
  scale_y_continuous(labels = scales::comma)+scale_color_discrete(name = "Attribute", labels = c("SIR Predicted", "True Positive"))

SEC <- sqrt(diag(solve(OptC$hessian)))

upperC<-OptC$par+1.96*SEC
lowerC<-OptC$par-1.96*SEC
ciC= list(OptC$par, upperC, lowerC, 'Last 10 days')
CI = rbind(CI, ciC)
ConfidenceInts= c("[0.3700007,0.3700018]", "[0.2520433, 0.2520434]", "[0.2187852, 0.2187854]", "[0.2078906, 0.2078907]")
ConfidenceInts= data.frame(ConfidenceInts)
CI= cbind(CI, ConfidenceInts)
CI=subset(CI,select=-c(upper,lower))
colnames(CI)=c('BetaValue', 'BetaType', 'CI95')

reactable(CI, columns = list(
  BetaValue = colDef(name = "Beta Value", align= 'left'),
  BetaType = colDef(name = "Beta Type"),
  CI95 = colDef(name = "95% Confidence Interval")),
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px"
  ))

# Make plot of predicted levels in future (CUMULATIVE)

tP <- 131:231 # time in days
initP <- c(S = (N-USAC$positive[USAC$day == 131]), I = USAC$positiveSIR[USAC$day == 131], R = USAC$recovered[USAC$day == 131])

paramP0=c(Opt_par0,0.2)
paramP80=c(Opt_par80,0.2)
paramP50_1=c(Opt_par50_2,0.2)
paramPC=c(Opt_parC,0.2)
names(paramP0)=c('beta','gamma')
names(paramP80)=c('beta','gamma')
names(paramP50_1)=c('beta','gamma')
names(paramPC)=c('beta','gamma')


fitP0 <- data.frame(ode(y = initP, times = tP, func = SIR, parms = paramP0))
fitP0['BetaType'] <- '0% of States with Stay at Home: β=0.37'
fitP80 <- data.frame(ode(y = initP, times = tP, func = SIR, parms = paramP80))
fitP80['BetaType'] <- '80% of States with Stay at Home: β=0.25'
fitP50 <- data.frame(ode(y = initP, times = tP, func = SIR, parms = paramP50_1))
fitP50['BetaType'] <- 'Last 40 days: β=0.22'
fitPC <- data.frame(ode(y = initP, times = tP, func = SIR, parms = paramPC))
fitPC['BetaType'] <- 'Last 10 Days: β=0.20'

fitP = rbind(fitP0,fitP80,fitP50,fitPC)

ggplot(fitP, aes(time, I, color= BetaType)) + geom_line() + labs(x= 'Day', y= 'Predicted Positive Cases', title = 'US Predicted Positive Cases for Different Transmission Rates') +
  theme(axis.text.x = element_text(size =6)) + scale_x_continuous(breaks=seq(0, nrow(fitP), 10))

USAfinal <- subset(USA, select=c(day, positiveSIR))
colnames(USAfinal) <- c('time', 'I')
USAfinal['BetaType']= 'True Data'
USAfinal['line'] <- 's'
USAfinal= USAfinal[order(USAfinal$time),]
fitPfinal <- subset(fitP, select=-c(S,R))
fitPfinal <- fitPfinal[2:404,]
fitPfinal['line'] <- 
FINAL <- rbind(USAfinal, fitPfinal)

ggplot(FINAL, aes(time, I, color= BetaType, linetype= line)) + geom_line(size=1.1) + labs(x= 'Day', y= 'Predicted Positive Cases', title = 'US Predicted Positive Cases for Different Transmission Rates') +
  theme(axis.text.x = element_text(size =6)) + scale_x_continuous(breaks=seq(0, nrow(fitP), 20)) + scale_linetype_manual('Data Type', labels= c('Predicted', 'True Data'),values=c("dotdash","solid")) +
  scale_color_manual('Beta Type', values=c("red1", "magenta1", "springgreen3", "steelblue2", "grey70"))
