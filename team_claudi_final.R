## Load deSolve package
library(deSolve)

rm(list=ls())
## Data
bundeslaender <- c('Schleswig-Holstein','Hamburg','Niedersachsen','Bremen','NRW','Hessen','Rheinland-Pfalz','Baden-Wuerttemberg','Bayern','Saarland','Berlin','Brandenburg','Mecklenburg-Vorpommern','Sachsen','Sachsen-Anhalt','Thueringen')
intensiv_betten <- c(867,802,2362,294,6148,1845,1343,3262,3790,548,1450,745,1014,1859,972,730)
intensiv_betten_auslastungsquote <- c(0.74,0.773,0.774,0.661,0.846,0.787,0.683,0.756,0.785,0.793,0.83,0.838,0.774,0.795,0.785,0.787)
infiziert <- c(359,664,1505,127,6806,1153,1047,4238,3821,268,868,324,167,595,242,233)
tote <- c(1,1,0,0,25,2,2,17,19,0,1,0,0,1,0,1)
genesen <- c(0,0,11,3,107,10,0,37,3,0,0,22,3,0,0,3)
einwohner <- c(2896712,1841179,7982448,682986,17932651,6265809,4084844,11069533,13076721,990509,3644826,2511917,1609575,4077937,2208321,2143145)



out_final <- 0

## Create an SIR function
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}


#i<-1
#j<-5
intensivanteil <- 0.0275
beta_max <- 3
#kontaktfaktor <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

kontaktfaktor <- c(0.2,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.8,1)
infektionsdauer <- 10
gamma <- 0.11

for (i in 1:16) {
  for (j in 1:10) {
### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init       <- c(S = (einwohner[i]-infiziert[i]-tote[i]-genesen[i])/einwohner[i], I = infiziert[i]/einwohner[i], R = (genesen[i]+tote[i])/einwohner[i])
#init       <- c(S = 0.8, I = 0.02, R = 0.18)

## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = kontaktfaktor[j]*beta_max/infektionsdauer, gamma)
## Time frame
times      <- seq(0, 730, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)
out <- out*einwohner[i]
out['I_intensiv'] <- out['I']*intensivanteil
out['I_Betten_ges'] <- rep(intensiv_betten[i])
out['I_Betten_frei'] <- out['I_Betten_ges']*(1-intensiv_betten_auslastungsquote[i])
out['Name'] <- bundeslaender[i]
out['Kontaktfaktor'] <- kontaktfaktor[j]
out['Tag']<-1:dim(out)[1]
out_final <- rbind(out_final,out)

}
}


pl_Out<- out['I_intensiv']
pl_Out['I_Betten_frei'] <-out['I_Betten_ges']*(1-intensiv_betten_auslastungsquote[i])

## Plot
matplot(x = times, y = pl_Out, type = "l",
        xlab = "Time", ylab = "Susceptible and Recovered", main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:3)

## Plot
#Ziel: betrachte Time Horizone bis infizierten nur noch 0.001% ausmachen
#stop <- 0.00001
#index <- match(FALSE, pl_Out$I_intensiv > stop*einwohner[i])
#times_new <- seq(0, index, by=1)
#out_new <- pl_Out[1:(index+1),]
#Plot nur bis index'
#matplot(x = times_new, y = out_new, type = "l",
#        xlab = "Time", ylab = "Susceptible and Recovered", main = "SIR Model",
#        lwd = 1, lty = 1, bty = "l", col = 2:4)

## Add legend
legend(40, 0.7, c("Infected", "Available I_Beds"), pch = 1, col = 2:3, bty = "n")


###############################################################################################################################

library("dplyr")
data <- read.csv(file = 'C:/Users/Nutzer/OneDrive/Desktop/hist_data.csv',sep=";")
data_new <- structure(list(character()), class = "data.frame")

for (i in 0:15){
  data_temp <- data[data$Zahl%%16==i,]
  data_new <- rbind(data_new,data_temp)
}
data<-data_new
remove(data_new)

data$Name<-as.character(data$Name)
data[data$Name=="Baden-Württemberg ", ][3]<- rep("Baden-Wuerttemberg",18)
data[data$Name=="Bavaria ", ][3]<- rep("Bayern",18)
data[data$Name=="Hesse ", ][3]<- rep("Hessen",18)
data[data$Name=="Lower Saxony ", ][3]<- rep("Niedersachsen",18)
data[data$Name=="Mecklenburg-Western Pomerania ", ][3]<- rep("Mecklenburg-Vorpommern",18)
data[data$Name=="North Rhine-Westphalia ", ][3]<- rep("NRW",18)
data[data$Name=="Rhineland-Palatinate ", ][3]<- rep("Rheinland-Pfalz",18)
data[data$Name=="Saxony ", ][3]<- rep("Sachsen",18)
data[data$Name=="Saxony-Anhalt ", ][3]<- rep("Sachsen-Anhalt",18)
data[data$Name=="Thuringia ", ][3]<- rep("Thueringen",18)

data$Tote<-NULL
data$Zahl<-NULL
data$Date<-NULL

#Sortieren wie in out_final:
data <- data[c( (15*18+1):(16*18), (6*18+1):(7*18), (8*18+1):(9*18), (5*18+1):(6*18), (10*18+1):(11*18), (7*18+1):(8*18), (11*18+1):(12*18), (1*18+1):(2*18), (2*18+1):(3*18), (12*18+1):(13*18), (3*18+1):(4*18), (4*18+1):(5*18), (9*18+1):(10*18), (13*18+1):(14*18), (14*18+1):(15*18), (0*18+1):(1*18) ),]
data$Tag <- rep(-17:0,16)

out_final2<-out_final[-c(1),]
out_final <- out_final[1,] #structure(list(character()), class = "data.frame")

for (i in 1:16){
  for (j in 1:length(kontaktfaktor)){
    data_temp2 <- data[((i-1)*18+1):(i*18),]
    data_temp2$Kontaktfaktor <- kontaktfaktor[j]
    data_temp2$I_Betten_ges <- intensiv_betten[i]
    data_temp2$I_Betten_frei <- intensiv_betten[i]*(1-intensiv_betten_auslastungsquote[i])
    
    out_final <- bind_rows(out_final, data_temp2)
    out_final <- bind_rows(out_final, out_final2[((i-1)*7310+(j-1)*731+1):((i-1)*7310+(j)*731),])
  }
}

out_final$I_intensiv <- out_final$I * intensivanteil

kon <- c(0,rep(rep(c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.85,1),each=749),16))
out_final$Kontaktfaktor <- kon

openxlsx::write.xlsx(out_final,path="C:/Users/Nutzer/OneDrive/Desktop",file = "TTDS.csv",sep=";")
