setwd('C:/Users/admin/Desktop/capstone')

## Reading the Datasets
Travel_Time <- read.csv('TravelTime.csv', stringsAsFactors = F)
head(Travel_Time)
nrow(Travel_Time)
str(Travel_Time)

Job_Time <- read.csv('JobExecutionTime.csv', stringsAsFactors = F)
head(Job_Time)
nrow(Job_Time)
str(Job_Time)
## Adding the Depot as one of the stops.
## We are assuming that the Job Execution time at depot is 0
Job_Time[69,]<-c('Depot',0)
tail(Job_Time)

library(utils)
## Finding all the combinations of stops
## Example: If there are 4 stops (s1,s2,s3,s4), the below function
## will return [(s1,s2), (s1,s3), (s1,s4), (s2,s3), (s2,s4), (s3,s4)]
comb <- combn(Job_Time$StopId,2)
dim(comb)
#class(comb)
View(comb)


## Including all the permutations of the combinations list
## This will return [(s1,s2), (s1,s3), (s1,s4), (s2,s3), (s2,s4), (s3,s4)
##                   (s2,s1), (s3,s1), (s4,s1), (s3,s2), (s4,s2), (s4,s3)]
comb <- data.frame('stopId1'=c(comb[1,],comb[2,]),'stopId2'=c(comb[2,],comb[1,]))
#class(comb)
head(comb)


## Merging the Travel time Dataframe with the combinations. The combinations
## that are not present in Travel time will take NA values as time
new_df <- merge(comb, Travel_Time, by=c('stopId1','stopId2'), all = T)

head(new_df)

View(new_df)


## The below code will impute the NA values in the dataframe with the
## maximum time of the starting point and ending point
for (i in which(is.na(new_df$time))){
  new_df$time[i] <- max(na.omit(c(new_df$time[new_df$stopId1==new_df$stopId1[i]],new_df$time[new_df$stopId2==new_df$stopId2[i]])))
}

## Combining the Job Execution time to the dataframe
new_df$Job_Time <- sapply(new_df$stopId2,function(x) Job_Time$EstimatedDurationMinutes[Job_Time$StopId==x])
head(new_df$Job_Time)
## Type conversion
new_df$Job_Time <- as.integer(new_df$Job_Time)
head(new_df)
## Calculatimg the total time taken to travel and complete the job 
new_df$Total_Time <- new_df$time+new_df$Job_Time

#write.csv(new_df,file = 'transformedData.csv')
## setting the total number of stops
no_stops = nrow(Job_Time)

## Initializing time for using in the loop
time=0
new_df$stopId1<-as.character(new_df$stopId1)
new_df$stopId2<-as.character(new_df$stopId2)
give_route<-function(x){
  start=1
  i=1
  end=sample(5:7,1)
  route = list()
  while(start<69){
    route[[i]]<-c('Depot',x[start:end],'Depot')
    i=i+1
    print (start)
    print (end)
    start=end+1
    if(start>61){
      end = 68
    } else{
      end = start + sample(5:7,1)
    }
  }
  return (route)
}

calculate_penalty<-function(x){
  penal<-ifelse(x>660,1,0)
  penal[x>660]<-as.integer((x[x>660]-660)/60)+1
  penal[penal==1]<-66
  penal[penal==2 | penal==3]<-132
  penal[penal>3] <- 198
  return (penal)
}

calculate_time<-function(x){
  time=as.numeric()
  for(i in 1:length(x)){
    time[i]=0
    for(j in 2:length(x[[i]])){
      time[i]=time[i]+new_df$Total_Time[new_df$stopId1==x[[i]][j-1] & new_df$stopId2==x[[i]][j]]
    }
  }
  return (time+calculate_penalty(time))
}

mutate<-function(x){
  number_changes<-sample(1:(length(x)/2),1)
  number_changes<-sample(1:length(x),number_changes)
  for (i in number_changes){
    if(length(x[[i]]<4)) next
    if(length(x[[i]]<6)){
      shuffles=2
    } else{
      shuffles = sample(2:(length(x[[i]])/2))
    }
    shuffles=sample(2:(length(x[[i]])-1),shuffles)
    x[[i]][shuffles]=sample(x[[i]][shuffles])
  }
  return (x)
}

crossover<-function(x){
  t=calculate_time(x)
  c1=which.max(t)
  c2=which.min(t)
  
  minim<-x[[c2]]
  maxim<-x[[c1]]
  
  if(length(x[[c2]])>3){
    x[[c1]]=c(maxim[1:(length(maxim)/2)],
              minim[as.integer(length(minim)/2 +1):length(minim)])
    x[[c2]]=c(minim[1:(length(minim)/2)],
              maxim[as.integer(length(maxim)/2 +1):length(maxim)])
  }
  return (x)
}

create_solution<-function(x){
  if(length(x)<8){
    return (list(x))
  }
  number_genes = sample(5:7,1)
  chromosome = sample(1:length(x),number_genes)
  soln = list(x[chromosome])
  x=x[-chromosome]
  soln = append(soln,create_solution(x))
  return (soln)
}

create_chromosomes<-function(x){
  soln=create_solution(x)
  for (i in 1:length(soln)){
    soln[[i]]<-c('Depot',soln[[i]],'Depot')
  }
  return (soln)
}
final_times=integer()
final_plot=integer()
final_solution = list()
final_time = 999999
iter=0
converge=0
while(converge==0){
  iter=iter+1
  if(iter>5000){    # 5000 iterations
    converge=1
    break
  }
  soln=create_chromosomes(Job_Time$StopId[1:68])
  time = calculate_time(soln)
  if(sum(time)<sum(final_time)){
    final_time=time
    final_times=append(final_times,sum(final_time))
    final_solution=soln
    iter=0
  }
  
  for(j in 1:100){
    iter=iter+1
    if(iter>5000){
      converge=1
      break
    }
    final_plot=append(final_plot,sum(final_time))
    soln1 = mutate(soln)
    if(sum(calculate_time(soln1))<sum(time)){
      time = sum(calculate_time(soln1))
      soln=soln1
      if(sum(time)<sum(final_time)){
        final_time=time
        final_times=append(final_times,sum(final_time))
        final_solution=soln
        iter=0
      }
    }
  }
  for(j in 1:100){
    iter=iter+1
    if(iter>5000){
      converge=1
      break
    }
    final_plot=append(final_plot,sum(final_time))
    soln1=crossover(soln)
    if(sum(calculate_time(soln1))<sum(time)){
      time = sum(calculate_time(soln1))
      soln=soln1
      if(sum(time)<sum(final_time)){
        final_time=time
        final_times=append(final_times,sum(final_time))
        final_solution=soln
        iter=0
      }
    }
  }
  final_plot=append(final_plot,sum(final_time))
  print(iter)
}
plot.ts(final_times)
plot.ts(final_plot)

tail(final_plot)


final_times[1:5]
tail(final_times)

final_solution

