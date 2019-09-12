
library(Matrix)
library(MASS)
library(deSolve)

######## MUTUALISM ASSUMPTIONS COMMON FOR ALL THE MODELS ########
#                 (Graves et al. 2006)
#A1: each species behaves according to a logistic model
#A2: each species affects the other species' per capita growth 
#    rate but not directly its self limitation
#A3: the increase in each species cannot harm the other species 
#################################################################

# Read parameters
setwd("~/A_Angel/MasterAgroEco/Ecosystems Modelling/Paper analysis")
source("Params_Sens.R")

# Define time steps
ts = seq(from=0,length=m,by=hs)

####################################################################################
################# First case: Potentially unlimited growth rate ####################
####################################################################################

#### Further assumptions: #######################################
# A4: the change in per capita birth rate of each species depends
#    linearly on the other
#################################################################
if (run == 1 || run == 0) {
First <- function(t,x, params){
  with(as.list(params),
       {
         R1y = c1+b1*x[2]
         R2x = c2+b2*x[1]
         
         dx= R1y*x[1] - a1*x[1]*x[1]
         dy= R2x*x[2] - a2*x[2]*x[2]
         list(c(dx,dy))
       }
  )
}

out.1 <- as.data.frame(lsoda(x0.First, ts, First, params))

miny=min(out.1$`1`,out.1$`2`)
maxy=max(out.1$`1`,out.1$`2`)
plot(ts[1:length(out.1$`1`)],out.1$`1`,
     type='l',main=title,ylab="x(t)",xlab="time",col="black",ylim=c(miny,maxy))
lines(ts[1:length(out.1$`1`)],out.1$`2`,col="red")

}


##################################################################################
################# Second case: limited per capita growth rate ####################
##################################################################################

#### Further assumptions: #############################################
# A4: there is a maximum per capita growth (limited benefit assumption)
# A5: rate of change of per capita growth due to mutualism is 
#     proportional to the difference between maximum growth rate and
#     current growth rate (proportional benefit assumption)
#######################################################################
if (run == 2 || run == 0) {
Second <- function(t,x, params){
  with(as.list(params),
       {
         # Assumption A4+A5
         R1y = c1 + (m1 - c1)*(1 - exp(-d1*x[2]))*x[2]
         R2x = c2 + (m2 - c2)*(1 - exp(-d2*x[1]))*x[1]
         
         dx= R1y*x[1] - a1*x[1]*x[1]
         dy= R2x*x[2] - a2*x[2]*x[2]
         list(c(dx,dy))
       }
  )
}

out.2 <- as.data.frame(lsoda(x0.Second, ts, Second, params))

miny=min(out.2$`1`,out.2$`2`)
maxy=max(out.2$`1`,out.2$`2`)
plot(ts[1:length(out.2$`1`)],out.2$`1`,
     type='l',main=title,ylab="x(t)",xlab="time",col="black",ylim=c(miny,maxy))
lines(ts[1:length(out.2$`1`)],out.2$`2`,col="red")

}


#################################################################
##### Sensitivity analysis: analytical analysis of the PUGR #####
#################################################################
if (run == 3 || run == 0) {
Sens <- function(t,x, params){
  with(as.list(params),
       {
         R1y = c1+b1*x[2]
         R2x = c2+b2*x[1]
         
         dx= R1y*x[1] - a1*x[1]*x[1]
         dy= R2x*x[2] - a2*x[2]*x[2]
         dsx.a1 = b1*(x[4]*x[1]+x[3]*x[2])  -2*a1*x[1]*x[3] +c1*x[3]  -x[1]*x[1]
         dsy.a1 = b2*(x[4]*x[1]+x[3]*x[2])  -2*a2*x[2]*x[4] +c2*x[4]
         dsx.a2 = b1*(x[6]*x[1]+x[5]*x[2])  -2*a1*x[1]*x[5] +c1*x[5]
         dsy.a2 = b2*(x[6]*x[1]+x[5]*x[2])  -2*a2*x[2]*x[6] +c2*x[6]  -x[2]*x[2]
         dsx.b1 = b1*(x[8]*x[1]+x[7]*x[2])  -2*a1*x[1]*x[7] +c1*x[7]  +x[1]*x[2]
         dsy.b1 = b2*(x[8]*x[1]+x[7]*x[2])  -2*a2*x[2]*x[8] +c2*x[8]
         dsx.b2 = b1*(x[10]*x[1]+x[9]*x[2]) -2*a1*x[1]*x[9] +c1*x[9]
         dsy.b2 = b2*(x[10]*x[1]+x[9]*x[2]) -2*a2*x[2]*x[10]+c2*x[10] +x[1]*x[2]
         dsx.c1 = b1*(x[12]*x[1]+x[11]*x[2])-2*a1*x[1]*x[11]+c1*x[11] +x[1]
         dsy.c1 = b2*(x[12]*x[1]+x[11]*x[2])-2*a2*x[2]*x[12]+c2*x[12]
         dsx.c2 = b1*(x[14]*x[1]+x[13]*x[2])-2*a1*x[1]*x[13]+c1*x[13]
         dsy.c2 = b2*(x[14]*x[1]+x[13]*x[2])-2*a2*x[2]*x[14]+c2*x[14] +x[2]
         list(c(dx,dy,dsx.a1,dsy.a1,dsx.a2,dsy.a2,
                dsx.b1,dsy.b1,dsx.b2,dsy.b2,
                dsx.c1,dsy.c1,dsx.c2,dsy.c2))
       }
  )
}

out.3 <- as.data.frame(lsoda(x0.Sens, ts, Sens, params))
sens.data <- data.frame(time=out.3$time, x=out.3$`1`,y=out.3$`2`,
                        sx.a1=out.3$`3`,sy.a1=out.3$`4`,sx.a2=out.3$`5`,sy.a2=out.3$`6`,
                        sx.b1=out.3$`7`,sy.b1=out.3$`8`,sx.b2=out.3$`9`,sy.b2=out.3$`10`,
                        sx.c1=out.3$`11`,sy.c1=out.3$`12`,sx.c2=out.3$`13`,sy.c2=out.3$`14`)

# Write functions in a table
write.table(sens.data,sep=" ",file="sensdata.csv",row.names=FALSE)

# Plot sensitivity function for specific parameter..
#plot(ts[1:length(out.3$`1`)],out.3$`3`,type='l')

# Calculate maximum error function at t:
max.err.x = err.par* (abs(sens.data$sx.a1)+
                    abs(sens.data$sx.a2)+
                    abs(sens.data$sx.b1)+
                    abs(sens.data$sx.b2)+
                    abs(sens.data$sx.c1)+
                    abs(sens.data$sx.c2))

max.err.y = err.par* (abs(sens.data$sy.a1)+
                     abs(sens.data$sy.a2)+
                     abs(sens.data$sy.b1)+
                     abs(sens.data$sy.b2)+
                     abs(sens.data$sy.c1)+
                     +abs(sens.data$sy.c2))


# Plot x(t), y(t) +- error
up.x = sens.data$x + max.err.x
low.x = sens.data$x - max.err.x
up.y = sens.data$y + max.err.y
low.y = sens.data$y - max.err.y
miny = min(low.x, low.y)
maxy = max(up.x, up.y)
plot(ts,sens.data$x,
     type='l',main=title,ylab="x(t) +- MaxErr",xlab="time",col="black",ylim=c(miny,maxy))
lines(ts,sens.data$y,col="red")
segments(ts,low.x , ts, up.x, col = "black")
segments(ts,low.y , ts, up.y, col = "red")


# Calculate error function with regard to a1:
err.x.a1 = err.a1*abs(sens.data$sx.a1)
err.y.a1 = err.a1*abs(sens.data$sy.a1)

# Plot x(t), y(t) +- error
up.x.a1 = sens.data$x + err.x.a1
low.x.a1 = sens.data$x - err.x.a1
up.y.a1 = sens.data$y + err.y.a1
low.y.a1 = sens.data$y - err.y.a1
miny.a1 = min(low.x.a1, low.y.a1)
maxy.a1 = max(up.x.a1, up.y.a1)
plot(ts,sens.data$x,
     type='l',main=title,ylab="x(t) +- Err a1",xlab="time",col="black",ylim=c(miny.a1,maxy.a1))
lines(ts,sens.data$y,col="red")
segments(ts,low.x.a1 , ts, up.x.a1, col = "black")
segments(ts,low.y.a1 , ts, up.y.a1, col = "red")

}








