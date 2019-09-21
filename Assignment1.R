T=600
C=100
y = c(500,300,200)
class = c(1,2,3)
alpha = c(0.001,0.015,0.05)
beta = c(0.01,0.005,0.0025)

V=array(NA,c((C+1),T,length(class)))

V[,T,]=0
V[1,,]=0

booking_class=matrix(nrow=C+1,ncol=T)
booking_class[,T]=1
booking_class[1,]=1

avoid_price_drop = FALSE
expected_revenue = 0
for(t in (T-1):1){
  for(x in 2:(C+1)){
    for(j in class){
      prob_accept = 0
      prob_reject = 1
      prob = 0
      max_revenue = 0
      for(i in (1:j)){
        prob = alpha[i]*exp(beta[i]*t)
        prob_accept = prob_accept + prob
        prob_reject = prob_reject - prob
      }
      total_revenue=0
      for(k in (ifelse(avoid_price_drop,j,1):length(class))){
        total_revenue = prob_accept*(y[j]+V[x-1,t+1,k])  + prob_reject*V[x,t+1,k]
        if(total_revenue>max_revenue){
          max_revenue = total_revenue
        }
      }
      V[x,t,j] = max_revenue
    }
  }
}

expected_revenue = max(V[C+1,1,])

for(i in (1:(C+1))){
  for(j in(1:T)){
    booking_class[i,j] = which.max(V[i,j,])
  }
}


booking_class_reversed  <- apply(apply(booking_class,1,rev),2,rev)
cols <-c('1' = "blue",
         '2' = "white",
         '3' = "red")
image(1:nrow(booking_class_reversed), 1:ncol(booking_class_reversed), as.matrix(booking_class_reversed), col=cols)


##simulation
paths = 10000
sum_revenue = 0

for(p in (1:paths)){
  booking_class_simulation <- booking_class
  
  booking_class_simulation[C+1,1] = -1
  
  capacity_left <- matrix(C,nrow=1,ncol=T)

  simulation_revenue_per_step <-matrix(0,nrow=1,ncol=T)
  
  booking_class_per_step <- matrix(nrow=1,ncol=T)
  
  
  for(t in 1:(T)){
    j = booking_class[capacity_left[t]+1,t]
    booking_class_per_step[t] = j
    
    if(capacity_left[t]==0){
      break
    }
    
    if(avoid_price_drop & (t!=T)){
      if((booking_class[capacity_left[t]+1,t+1]<=j) & (booking_class[capacity_left[t],t+1]>j)) {
        booking_class_simulation[capacity_left[t],t]=-1
        capacity_left[col(capacity_left)>=(t+1)]=capacity_left[t]-1
        simulation_revenue_per_step[t] = y[j]
        next
      }
      if((booking_class[capacity_left[t]+1,t+1]>j) & (booking_class[capacity_left[t],t+1]<=j)) {
        booking_class_simulation[capacity_left[t]+1,t]=-1
        next
      }
    }

    prob_accept=0
    for(i in (1:j)){
      prob = alpha[i]*exp(beta[i]*t)
      prob_accept = prob_accept + prob
    }
    
    if(prob_accept>runif(1, 0, 1)){
      booking_class_simulation[capacity_left[t],t]=-1
      capacity_left[col(capacity_left)>=(t+1)]=capacity_left[t]-1
      simulation_revenue_per_step[t] = y[j]
    }
    else{
      booking_class_simulation[capacity_left[t]+1,t]=-1
    }
  }
  sum_revenue = sum_revenue + sum(simulation_revenue_per_step)
}

sum_revenue/paths


booking_class_simulation_reversed  <- apply(apply(booking_class_simulation,1,rev),2,rev)
cols <-c('1' = "black",
         '2' = "red",
         '3' = "white",
         '-1' = "blue"
)
image(1:nrow(booking_class_simulation_reversed), 1:ncol(booking_class_simulation_reversed), as.matrix(booking_class_simulation_reversed), col=cols)
#plot((1:length(booking_class_per_step)),booking_class_per_step)