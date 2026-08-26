summarize.bootstraps<- function(bootstrap.file){#this will be done by survey

  #  names(bootstrap.file)
point.estimate<- mean<- sdev <-  ci.025 <- ci.975 <-  scientificName <-  stock <-  stratum <- area <- NULL


    for( j in sort(unique(bootstrap.file$sci))){
print(paste("Species", j))



for(d in  unique(           bootstrap.file$stock[bootstrap.file$sci ==j])){
    print(paste("Stock", d))

    area.temporary <- NULL
point.estimate.temporary <- NULL
    sdev.temporary <- NULL
    mean.temporary <- NULL
boot.estimates.temporary <- NULL
 boot.estimates.temporary <- numeric(0)

for(i in unique(          bootstrap.file$stratum[bootstrap.file$sci ==j & bootstrap.file$stock ==d ])){
    print(i)
    scientificName <- c(scientificName, j)
    stock     <- c(stock, d)
    stratum     <- c(stratum, as.character(i))
    area <- c(area ,  bootstrap.file$area[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d ])
    area.temporary <- c(area.temporary ,  bootstrap.file$area[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d ])

    point.estimate <- c(point.estimate ,  bootstrap.file$point[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d ])
    point.estimate.temporary <-c( point.estimate.temporary , bootstrap.file$point[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d ])

    names(bootstrap.file)
    sdev <- c( sdev , sd(as.numeric(bootstrap.file[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d  ,                    seq(which (names(bootstrap.file) == "X2"), ncol( bootstrap.file))])))
    sdev.temporary <-  c(sdev.temporary ,                                    sd(as.numeric(bootstrap.file[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d  ,                    seq(which (names(bootstrap.file) == "X2"), ncol( bootstrap.file))])))

    mean <- c( mean , mean(as.numeric(bootstrap.file[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d  ,                    seq(which (names(bootstrap.file) == "X2"), ncol( bootstrap.file))])))

    mean.temporary <-  c(mean.temporary ,                                    mean(as.numeric(bootstrap.file[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d  ,                    seq(which (names(bootstrap.file) == "X2"), ncol( bootstrap.file))])))



    ci.025 <- c( ci.025 , quantile(as.numeric(bootstrap.file[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d  ,                    seq(which (names(bootstrap.file) == "X2"), ncol( bootstrap.file))]) , 0.025))

    ci.975 <- c( ci.975 , quantile(as.numeric(bootstrap.file[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d  ,                    seq(which (names(bootstrap.file) == "X2"), ncol( bootstrap.file))]) , 1- 0.025))


    boot.estimates.temporary <- rbind(boot.estimates.temporary ,                                   as.numeric(bootstrap.file[ bootstrap.file$stratum == i & bootstrap.file$sci ==j & bootstrap.file$stock ==d  ,                    seq(which (names(bootstrap.file) == "X2"), ncol( bootstrap.file))]))


}

#this combines everything that was done for each scientificName, and stock into a single line
scientificName <- c(scientificName, rev(scientificName)[1])
stock <- c(stock, rev(stock)[1])
    stratum <- c(stratum , "All")
area <- c(area , sum(area.temporary))
point.estimate <- c(point.estimate , sum(point.estimate.temporary))
    sdev <- c(sdev , sqrt(sum(              sdev.temporary^2)) )
        mean <- c(mean ,sum(          mean.temporary))
ci.025 <- c(ci.025 , quantile(apply( boot.estimates.temporary, FUN =sum, MAR=2) , 0.025))
ci.975 <- c(ci.975 , quantile(apply( boot.estimates.temporary, FUN =sum, MAR=2) , 1- 0.025))
}
    }

    output<-data.frame(survey = unique(bootstrap.file$survey) , scientificName  , stock , stratum, area, point.estimate, sdev, ci.025, ci.975 , cv =  round(sdev/point.estimate*100,1), mean , cv2 = round(sdev/mean*100, 1))

#    output<-data.frame(survey = unique(bootstrap.file$survey) , scientificName  , stock , stratum, point.estimate, sdev, ci.025, ci.975 , cv =  round(sdev/point.estimate*100,1) )
return(output)
}
