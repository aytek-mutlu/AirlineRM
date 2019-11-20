#Initialize given variables
library(fields)

T=600
C=100
y = c(500,300,200)
class = c(1,2,3)
alpha = c(0.001,0.015,0.05)
beta = c(0.01,0.005,0.0025)

colors = c('red','white','blue')
colors_simulation = c('grey','red','white','blue')
breaks = c("1st Class","2nd Class","3rd Class","")
breaks_simulation = c("Simulation","1st Class","2nd Class","3rd Class","")


#value matrix (C+1)x(T+1)xlength(class)
V=array(NA,c((C+1),(T+1),length(class)))

#expected revenue is zero when all seats are sold or when the plane is departing
V[,T+1,]=0
V[1,,]=0

#booking class matrix
booking_class=matrix(nrow=C+1,ncol=T+1)
booking_class[,T+1]=1
booking_class[1,]=1

#boolean in order to avoid price drops
avoid_price_drop = TRUE

expected_revenue = 0

#for each time left for departure
for(t in T:1){
  #for each capacity level
  for(x in 2:(C+1)){
    #for each class
    for(j in class){
      prob_accept = 0
      prob_reject = 1
      prob = 0
      max_revenue = 0
      
      #calculate total probability of accepting and rejecting the demand
      for(i in (1:j)){
        prob = alpha[i]*exp(beta[i]*t)
        prob_accept = prob_accept + prob
        prob_reject = prob_reject - prob
      }
      total_revenue=0
      
      #calculate potential revenue with given probabilities
      for(k in (ifelse(avoid_price_drop,j,1):length(class))){
        total_revenue = prob_accept*(y[j]+V[x-1,t+1,k])  + prob_reject*V[x,t+1,k]
        if(total_revenue>max_revenue){
          max_revenue = total_revenue
        }
      }
      
      #fill in the maximum revenue possible
      V[x,t,j] = max_revenue
    }
  }
}

#expected total revenue 
expected_revenue = max(V[C+1,1,])

#optimal policy matrix
for(i in (1:(C+1))){
  for(j in(1:(T+1))){
    booking_class[i,j] = which.max(V[i,j,])
  }
}

#plot optimal policy lines
booking_class_reversed  <- apply(apply(booking_class,1,rev),2,rev)
col_len<-length(table(booking_class_reversed))
col_unique <-unique(c(booking_class_reversed))
image.plot(1:nrow(booking_class_reversed), 1:ncol(booking_class_reversed), as.matrix(booking_class_reversed),
           col=colors[1:col_len],xlab='Time',ylab='Capacity',lab.breaks=breaks[append(col_unique,4)])

##simulation

#run simulation for 10,000 times
paths = 10000
sum_revenue = matrix(0,1,paths)
num_class1 = matrix(0,1,paths)
num_class2 = matrix(0,1,paths)
num_class3 = matrix(0,1,paths)
booking_class_simulation <- booking_class
booking_class_simulation[C+1,1] = 0


for(p in (1:paths)){
  
  #prepare matrices for simulation plot, capacity left at each time, 
  #revenue at eact time and the booking class at each time
  
  capacity_left <- matrix(C,nrow=1,ncol=T)
  simulation_revenue_per_step <-matrix(0,nrow=1,ncol=T)
  booking_class_per_step <- matrix(nrow=1,ncol=T)
  
  #for each time
  for(t in 1:(T)){
    
    #extract current booking class from optimal policy
    j = booking_class[capacity_left[t]+1,t]
    booking_class_per_step[t] = j
    
    #finalize if all tickets are sold
    if(capacity_left[t]==0){
      break
    }
    
    #in case price never drops, simulation is unnecessary in cases
    #where either selling ticket or not selling ticket would lead to a 
    #lower booking class. Since it is not possible, the other option must
    #be executed
    
    if(avoid_price_drop & (t!=T)){
      if((booking_class[capacity_left[t]+1,t+1]<j) & (booking_class[capacity_left[t],t+1]>=j)) {
        booking_class_simulation[capacity_left[t],t]=0
        capacity_left[col(capacity_left)>=(t+1)]=capacity_left[t]-1
        simulation_revenue_per_step[t] = y[j]
        next
      }
      if((booking_class[capacity_left[t]+1,t+1]>=j) & (booking_class[capacity_left[t],t+1]<j)) {
        booking_class_simulation[capacity_left[t]+1,t]=0
        next
      }
    }
  
    #otherwise, simulate based on probabilities and then update matrices
    prob_accept=0
    for(i in (1:j)){
      prob = alpha[i]*exp(beta[i]*t)
      prob_accept = prob_accept + prob
    }
    
    if(prob_accept>runif(1, 0, 1)){
      booking_class_simulation[capacity_left[t],t]=0
      capacity_left[col(capacity_left)>=(t+1)]=capacity_left[t]-1
      simulation_revenue_per_step[t] = y[j]
    }
    else{
      booking_class_simulation[capacity_left[t]+1,t]=0
    }
  }
  
  #calculate total revenue and selling counts of each class
  sum_revenue[1,p] = sum(simulation_revenue_per_step)
  num_class1[1,p] = rowSums(simulation_revenue_per_step == y[1])
  num_class2[1,p] = rowSums(simulation_revenue_per_step == y[2])
  num_class3[1,p] =rowSums(simulation_revenue_per_step == y[3])
  
}
#plot histograms of total revenue and number of tickets sold per class
hist(sum_revenue,main="Histogram for Total Revenue",col='green',breaks=40,xlab='Total Revenue')
hist(num_class1,main="Histogram for Number of 1st Class Tickets Sold",col='red',breaks=40,xlab='Number of 1st Class Tickets Sold')
hist(num_class2,main="Histogram for Number of 2nd Class Tickets Sold",col='yellow',breaks=40,xlab='Number of 2nd Class Tickets Sold')
hist(num_class3,main="Histogram for Number of 3rd Class Tickets Sold",col='blue',breaks=40,xlab='Number of 3rd Class Tickets Sold')



#calculate average revenue and class counts of all paths
avg_revenue = mean(sum_revenue)
avg_num_class1 = mean(num_class1)
avg_num_class2 = mean(num_class2)
avg_num_class3 = mean(num_class3)

#plot simulation on optimal policy
booking_class_simulation_reversed  <- apply(apply(booking_class_simulation,1,rev),2,rev)
col_len<-length(table(booking_class_simulation_reversed))
col_unique <-sort(unique(c(booking_class_simulation_reversed)))


image.plot(1:nrow(booking_class_simulation_reversed), 1:ncol(booking_class_simulation_reversed), 
           as.matrix(booking_class_simulation_reversed),col=colors_simulation[1:col_len],
           xlab='Time',ylab='Capacity',
           lab.breaks=breaks_simulation[append(col_unique+1,5)])


#optimal booking class plot
plot((1:length(booking_class_per_step)),booking_class_per_step,xlab='Time',ylab='Booking Class')

#capacity left plot
plot((1:length(capacity_left)),capacity_left,xlab='Time',ylab='Capacity Left')


