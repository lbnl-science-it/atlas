# step 8: disposed vehicles: scrappaged or go back to market
scrappage <- function(data, dispose_data){
library(data.table)
  # calculate the total controls
  data <- data[,vintage_nextwave:= evoyear - deltayear]
  dispose_data <- dispose_data[,vintage_nextwave:= evoyear - deltayear]

  # LJ: recoding some of this to make it more compatible with future time steps
  # 
  
  veh_num <- table(data$vehtype, data$vintage_nextwave)
  veh_num <- cbind(veh_num[,as.numeric(colnames(veh_num))<25], as.vector(rowSums(veh_num[,as.numeric(colnames(veh_num))>=25])))
  colnames(veh_num)[ncol(veh_num)] <- 25
  
  if(length(scrap_car)>1){
    veh_num_car <- rbind(colSums(veh_num[scrap_car,]), colnames(veh_num))
    
  }else{
    veh_num_car <- rbind(veh_num[scrap_car, ], colnames(veh_num))
  }
  veh_num_car <- as.data.frame(t(matrix(as.numeric(veh_num_car), ncol = ncol(veh_num_car))))
  names(veh_num_car) <- c("number", "vintage")
#  veh_num_truck <- rbind(veh_num[scrap_truck, ], colnames(veh_num))
  if(length(scrap_truck)>1){
    veh_num_truck <- rbind(colSums(veh_num[scrap_truck,]), colnames(veh_num))
    
  }else{
    veh_num_truck <- rbind(veh_num[scrap_truck, ], colnames(veh_num))
  }
  veh_num_truck <- as.data.frame(t(matrix(as.numeric(veh_num_truck), ncol = ncol(veh_num_truck))))
  names(veh_num_truck) <- c("number", "vintage")
  # calculate the number of scrappaged cars, i.e. number - Nsurvived vehicles
  veh_num_car <- veh_num_car %>% mutate(scrap=number-round((carA*vintage^4+carB*vintage^3+carC*vintage^2+carD*vintage+carE)*number/100,0))
  veh_num_truck <- veh_num_truck %>% mutate(scrap=number-round((truckA*vintage^4+truckB*vintage^3+truckC*vintage^2+truckD*vintage+truckE)*number/100,0))
  
  print(paste('scale the total scrappage by %vehicles in continuing hh','car',car.contHH.ratio, 'truck',truck.contHH.ratio))
  veh_num_car$scrap_contHH = floor(veh_num_car$scrap * car.contHH.ratio) # scale to the continuing hh portion of the vehicles
  # correct typo 2/28/2023: should scale by truck.contHH.ratio
  veh_num_truck$scrap_contHH = floor(veh_num_truck$scrap * truck.contHH.ratio) # scale to the continuing hh portion of the vehicles
  
  # veh_num_truck$scrap_contHH = floor(veh_num_truck$scrap * car.contHH.ratio) # scale to the continuing hh portion of the vehicles
  # 
  # plot
  # kk = gather(veh_num_car, 'counttype','number', -vintage)
  # ggplot(kk, aes(x = vintage, y = number, color = countype)) +geom_point()
  # 
  vehicles_dispose_scrape <- dispose_data[0,]
  for (i in unique(c(veh_num_truck$vintage, veh_num_car$vintage))){
    if (i<25){
      car <- dispose_data[vehtype%in%scrap_car & vintage_nextwave==i]
      truck <- dispose_data[vehtype%in%scrap_truck & vintage_nextwave==i]
    }else{
      car <- dispose_data[vehtype%in%scrap_car & vintage_nextwave>=i]
      truck <- dispose_data[vehtype%in%scrap_truck & vintage_nextwave>=i]
    }
    #LJ: 10/18/2022: revise the code to handle cases when the vintage is not continous
    if(veh_num_car$scrap_contHH[veh_num_car$vintage == i]<nrow(car)){
      print(paste('dispose cars more than scrapped cars for vintage',i, 'random sample'))
      # random select veh to scrap if the dispose total is greater than scrap total, otherwise scrap all of the disposed vehicles
      car <- car[sample(nrow(car), veh_num_car$scrap_contHH[veh_num_car$vintage == i]), ]
    }
    if(veh_num_truck$scrap_contHH[veh_num_truck$vintage == i]<nrow(truck)){
      # random select veh to scrap if the dispose total is greater than scrap total, otherwise scrap all of the disposed vehicles
      print(paste('dispose trucks more than scrapped trucks for vintage',i, 'random sample'))
      truck <- truck[sample(nrow(truck), veh_num_truck$scrap_contHH[veh_num_truck$vintage == i]), ]
    }
    
 #    car <- car[sample(nrow(car), veh_num_car$scrap[(i-1)]), ] # --> this will create problem if i is not continous in 2-25
#    truck <- truck[sample(nrow(truck), veh_num_truck$scrap[(i-1)]), ]
    vehicles_dispose_scrape <- bind_rows(vehicles_dispose_scrape, car, truck)
  }
  returnlist <- list(vehicles_dispose_scrape, veh_num_car, veh_num_truck)
  return(returnlist)
}


