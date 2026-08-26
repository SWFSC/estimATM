bootstrap.V2 <- function(nasc.file , stratum.file, nboot =1, survey = "2407RL" ,seed = 1 , trim.edges = T){ #need to add the area
    require(sp)
    require(geosphere)
                                        #the resulting data frame has to have the following columns species, point estimate, low CI, high CI, SD, CV,

    nasc.file.original <- nasc.file
    transect<-        unlist(lapply(split(        nasc.file$transect , nasc.file$transect), FUN = unique))
    mid.long<-        unlist(lapply(split(        nasc.file$long , nasc.file$transect), FUN = median))
     mid.lat<-        unlist(lapply(split(        nasc.file$lat , nasc.file$transect), FUN = median))

                                        #this needs to be done to not include transect that might have one observation in a different stratum. It was a lucky finding when looking a herring stratum 2
estimate.matrix <-    species <-  stock <-  stratum <- area <- NULL


for( j in sort(unique(stratum.file$sci))){ # it starts with clupea

   print(j)
    for(i in unique(     stratum.file$strat[stratum.file$sci == j])){
    print(i)#3
    area <-c( area ,              areaPolygon(cbind(stratum.file$long[stratum.file$sci ==j &  stratum.file$strat == i] , stratum.file$lat[stratum.file$sci ==j &  stratum.file$strat == i])) / (1852^2))

    temporary.transects <-     transect[         point.in.polygon(      mid.long, mid.lat ,    stratum.file$long[stratum.file$sci ==j &  stratum.file$strat == i],    stratum.file$lat[stratum.file$sci ==j &  stratum.file$strat == i]) ==1]

    if(trim.edges ==T)
    nasc.file <- nasc.file[point.in.polygon(      nasc.file$long, nasc.file$lat ,    stratum.file$long[stratum.file$sci ==j &  stratum.file$strat == i],    stratum.file$lat[stratum.file$sci ==j &  stratum.file$strat == i]) ==1,] #only keeps observations within the stratum

    if(j == "Clupea pallasii"){

                                        #point.estimmate

        estimate.vector <-        round(  rev(area)[1] *  mean(nasc.file$her.dens[                  !is.na(match(nasc.file$transect ,     temporary.transects)) ]) ,1)#density is already in tons per square nautical mile
                                        #nboot
               set.seed(seed)
        for(b in 1:nboot){
            boot.transects <-     sample(temporary.transects,  size = length(temporary.transects) , replace =T) #creates the resample transects
                                        #need to create a recursive vector because I couldn't use match to repeat transects
            boot.densities <- NULL
            for (l in boot.transects)
                boot.densities <- c(boot.densities , nasc.file$her.dens[nasc.file$transect == l])
            estimate.vector<-c( estimate.vector,    round(rev(area)[1] * mean(            boot.densities),1))

}
 }

    if(j == "Sardinops sagax"){

                                        #point.estimmate
        estimate.vector <-       round(   rev(area)[1] *  mean(nasc.file$sar.dens[                  !is.na(match(nasc.file$transect ,     temporary.transects)) ]), 1)#density is already in tons per square nautical mile
                                        #nboot
               set.seed(seed)
        for(b in 1:nboot){
            boot.transects <-     sample(temporary.transects,  size = length(temporary.transects) , replace =T) #creates the resample transects
                                        #need to create a recursive vector because I couldn't use match to repeat transects
            boot.densities <- NULL
            for (l in boot.transects)
                boot.densities <- c(boot.densities , nasc.file$sar.dens[nasc.file$transect == l])
            estimate.vector<-c( estimate.vector,    round(rev(area)[1] * mean(            boot.densities),1))

}
    }

            if(j == "Scomber japonicus"){

                                        #point.estimmate
        estimate.vector <-        round(  rev(area)[1] *  mean(nasc.file$mack.dens[                  !is.na(match(nasc.file$transect ,     temporary.transects)) ]),1)#density is already in tons per square nautical mile
                                        #nboot
               set.seed(seed)
        for(b in 1:nboot){
            boot.transects <-     sample(temporary.transects,  size = length(temporary.transects) , replace =T) #creates the resample transects
                                        #need to create a recursive vector because I couldn't use match to repeat transects
            boot.densities <- NULL
            for (l in boot.transects)
                boot.densities <- c(boot.densities , nasc.file$mack.dens[nasc.file$transect == l])
            estimate.vector<-c( estimate.vector,    round(rev(area)[1] * mean(            boot.densities),1))

}
        }

        if(j == "Trachurus symmetricus"){

                                        #point.estimmate
        estimate.vector <-        round(  rev(area)[1] *  mean(nasc.file$jack.dens[                  !is.na(match(nasc.file$transect ,     temporary.transects)) ]),1)#density is already in tons per square nautical mile
                                        #nboot
               set.seed(seed)
        for(b in 1:nboot){
            boot.transects <-     sample(temporary.transects,  size = length(temporary.transects) , replace =T) #creates the resample transects
                                        #need to create a recursive vector because I couldn't use match to repeat transects
            boot.densities <- NULL
            for (l in boot.transects)
                boot.densities <- c(boot.densities , nasc.file$jack.dens[nasc.file$transect == l])
            estimate.vector<-c( estimate.vector,   round( rev(area)[1] * mean(            boot.densities), 1))

}
        }



        if(j == "Engraulis mordax"){

                                        #point.estimmate
        estimate.vector <-        round(  rev(area)[1] *  mean(nasc.file$anch.dens[                  !is.na(match(nasc.file$transect ,     temporary.transects)) ]), 1)#density is already in tons per square nautical mile
                                        #nboot
               set.seed(seed)
        for(b in 1:nboot){
            boot.transects <-     sample(temporary.transects,  size = length(temporary.transects) , replace =T) #creates the resample transects
                                        #need to create a recursive vector because I couldn't use match to repeat transects
            boot.densities <- NULL
            for (l in boot.transects)
                boot.densities <- c(boot.densities , nasc.file$anch.dens[nasc.file$transect == l])
            estimate.vector<-c( estimate.vector,  round(  rev(area)[1] * mean(            boot.densities) ,1))

}
        }


            if(j == "Etrumeus acuminatus"){

                                        #point.estimmate
        estimate.vector <-         round( rev(area)[1] *  mean(nasc.file$rher.dens[                  !is.na(match(nasc.file$transect ,     temporary.transects)) ]) , 1)#density is already in tons per square nautical mile
                                        #nboot
               set.seed(seed)
        for(b in 1:nboot){
            boot.transects <-     sample(temporary.transects,  size = length(temporary.transects) , replace =T) #creates the resample transects
                                        #need to create a recursive vector because I couldn't use match to repeat transects
            boot.densities <- NULL
            for (l in boot.transects)
                boot.densities <- c(boot.densities , nasc.file$rher.dens[nasc.file$transect == l])
            estimate.vector<-c( estimate.vector,    round(rev(area)[1] * mean(            boot.densities) ,1))

}
        }




    species <- c( species , j)
                stock <- c(stock, unique(                       stratum.file$stock[stratum.file$scie  == j & stratum.file$strat == i]))
                       stratum <- c( stratum , i)
       estimate.matrix <- rbind(estimate.matrix , estimate.vector)
    row.names(estimate.matrix) <- NULL

    nasc.file <- nasc.file.original #resets the nasc file to the original
    }


    output<-        data.frame(scientificName = species, stock  =stock ,stratum =stratum, area = round(area))
    output <-         data.frame(  output ,        estimate.matrix)
    names(output)[5] <- "point.estimate"
output<-   data.frame(survey = survey , output)
} #species
    return(output)

}
