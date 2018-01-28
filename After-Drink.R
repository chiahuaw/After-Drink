library(ggmap)
library(dplyr)
library(magrittr)
library(data.table)
library(ggplot2)
library(geosphere)

#### 建立隨機路徑 ####

restaurant = read.csv(file("kinmen-restaurant-point.csv")) # 匯入金門餐聽的坐標。資料來源是 OpenStreepMap。
km = read.csv(file("金門隨機點_依10趴人口utf8.csv")) #匯入金門的隨機坐標點。資料是使用 QGIS 依金門人口數比例產生的。

taxi = list()
for (i in 1:2399) {
  
  startres = sample(nrow(restaurant),1,replace=F) # 從 restaurant 資料集中隨機取一筆，做為起點。
  target = sample(nrow(km),1,replace = F) # 從 km 資料集中隨機取一筆，做為終點。
  
  x1 = restaurant$X[startres[1]]
  y1 = restaurant$Y[startres[1]]
  x2 = km$x[target[1]]
  y2 = km$y[target[1]]
  
  d = distm(c(x1,y1),c(x2,y2),fun=distHaversine)[1]
  
  t1 = ifelse(x1<118.275200,"烈嶼鄉","") # "烈嶼鄉" 在另一個島，開車無法到達。因此需要篩掉。但 OpenStreetMap 的資料裡沒有鄉鎮，所以自己加上去。
  t2 = km$TName[target[1]]
  
  r.model = "driving"
  r.plustime = F
  
  if (d<=1000) { #choice walking or drving,if distance between 2 point over 1000m.
    r.model = "walking"
  } 
  
  #平均老年人步行的速度是3.2 km/h ~ 3.9 km/h，年輕人則為3.75 km/h ~ 5.43 km/h
  #Google maps 導航步行速度是5km/h
  
  if (any(grepl("烈嶼鄉",t1),grepl("烈嶼鄉",t2))) {
    
    if (t1 == "烈嶼鄉" & t2!="烈嶼鄉") {
      x1 = 118.286501
      y1 = 24.415096
      r.plustime = T
    }
    
    if (t2 == "烈嶼鄉" & t1 != "烈嶼鄉") {
      x2 = 118.286501
      y2 = 24.415096
      r.plustime = T
    }
  }
  
  temp<-route(from=paste(y1,x1,sep=","),
              to=paste(y2,x2,sep=","),
              mode=r.model,structure = "route",output = "all") #,key=""
  if (temp$status=="OK") {
    
    if (r.plustime==T) {
      temp$routes[[1]]$legs[[1]]$duration$value = temp$routes[[1]]$legs[[1]]$duration$value+1800
    }
    temp$target = target
    taxi[[length(taxi)+1]] = temp
    
  } else {
    Sys.sleep(5)
    next
  }
  
  
  if (i%%100==0) {
    print(paste("run at ",i))
    save(taxi,file="taxi.RData")
    if (routeQueryCheck()<=100) {Sys.sleep(86450)}
    # if (routeQueryCheck()<=100) {break}
  }
  
  wait<-round(sample((c(1:3)),1)+abs(rnorm(1,mean=2.5,sd=3)),2)
  print(paste(i,"of",2399,",wait",wait,"sec..."))
  Sys.sleep(wait)
  
  rm(temp)
  rm(target)
  rm(x1)
  rm(x2)
  rm(y1)
  rm(y2)
  rm(d)
  rm(t1)
  rm(t2)
  rm(r.model)
  rm(r.plustime)
}
save(taxi,file="taxi.RData")
#load(file="/data/taxi.RData")

#### research ####

# 從 Google Maps API 導航的資料裡，取得開車的部分。

taxi.trip = data.frame()

for (i in 1:length(taxi)) {
  x1 = taxi[i][[1]]$routes[[1]]$legs[[1]]$start_location$lat[1]
  x2 = taxi[i][[1]]$routes[[1]]$legs[[1]]$end_location$lat[1]
  y1 = taxi[i][[1]]$routes[[1]]$legs[[1]]$start_location$lng[1]
  y2 = taxi[i][[1]]$routes[[1]]$legs[[1]]$end_location$lng[1]
  
  if (taxi[i][[1]]$routes[[1]]$legs[[1]]$steps[[1]]$travel_mode != "DRIVING") {
    next
  }
  
  Temp = data.frame(i = i,
                    d = distm(c(y1,x1),c(y2,x2),fun=distHaversine)[1],
                    m = taxi[i][[1]]$routes[[1]]$legs[[1]]$distance$value[1],
                    t = round(taxi[i][[1]]$routes[[1]]$legs[[1]]$duration$value[1]/60,2))
  taxi.trip = rbind(taxi.trip,Temp,stringsAsFactors=F)
  rm(x1);rm(y1);rm(x2);rm(y2);rm(Temp)
  
}



taxi.trip = mutate(taxi.trip,fee = round((100+round(ifelse((m-1000)<0,0,m-1000)/250,0)*5),0),
                     nightfee=round((100+round(ifelse((m-800)<0,0,m-800)/200,0)*5),0))
summary(taxi.trip)

ggplot(taxi.trip,aes(x=fee))+geom_density()

filter(taxi.trip,fee<=255) %>% 
  ggplot(.,aes(x=fee))+geom_density()

filter(taxi.trip,fee>255) %>% 
  ggplot(.,aes(x=fee))+geom_density()

#cost fomula

daycount = c(seq(1,500,1))
mountcount = daycount*30
yearcount= daycount*365

rate = c(seq(0.01,1,0.01))

feecout = data.frame(cars=sample(c(1:500),365,replace=T),
                     taxiFee=0
                     )
feecout$taxiFee = mean(taxi.trip$fee[sample(nrow(taxi.trip),feecout$cars,replace=T)])
#stratege fomula,fix

ggplot(feecout,aes(x=taxiFee))+geom_density()

qnorm(0.1,mean=mean(feecout$taxiFee),sd=sd(feecout$taxiFee))
