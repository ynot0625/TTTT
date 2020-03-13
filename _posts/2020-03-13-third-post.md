---
title: "Untitled"
author: "YJunKim"
date: '2020 3 13 '
output:
  html_document:
    fig_height: 6
    fig_width: 10
    highlight: textmate
    theme: cosmo
    toc: yes
    toc_depth: 3
    toc_float: yes
  pdf_document:
    fig_height: 6
    fig_width: 10
    toc: no
  word_document:
    fig_height: 6
    fig_width: 9
    toc: no
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown 테스트

이 페이This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r, message=F}
library(dplyr)
library(ggplot2)
```

```{r, message=F}
rm(list=ls())
setwd("D:/RWORK/2019_study_part1")
```

## 자료 열기

```{r, message=F}
file.name <- paste("./DAOU/monthly/2010-2019_ASI.RData", sep = "")
load(file.name)
```

```{r}
head(df)
```

## Including Plots
```{r, include=F}
df1 <- filter(df, lat < 38.5 & lat > 34 &  lon > 126 & lon < 130 )
#df1 <- df1[-c(1),]
min_lat <- min(df1$lat)
max_lat <- max(df1$lat)
min_lon <- min(df1$lon)
max_lon <- max(df1$lon)
```

```{r, echo = FALSE,include=F}
df1$date <- as.character(df1$date)
df1$YYYY <- substr(df1$date,0,4)
```


```{r}
df2 <- df1 %>%
  group_by(date) %>%
  mutate(SUM_ASI = sum(na.omit(ASI)))
#AVE_ASI = SUM_ASI/max(as.numeric(DD))*100)
#max(as.numeric(df4$DD))
#df2 <- distinct(df, lon,lat,SUM_ASI, AVE_ASI)
```

```{r, echo = FALSE,include=F}
df3 <- distinct(df2, date ,SUM_ASI)
head(df3)
```

```{r, echo = FALSE, include=F}
findruns <- function(x,k) {
  n <- length(x)
  runs <- NULL
  for(i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)] > 1)) runs <- c(runs,i)
  }
  return(runs)
}
```


```{r, echo = FALSE, include=F}
dat <- NULL
for (iy in seq(min_lat, max_lat, 0.5)) {
  for (ix in seq(min_lon, max_lon, 0.5)) {
    #iy <- 37.25
    #ix <- 128.25
    #print(paste0(ix,"   ",iy))
    dummy1 <- filter(df1, lat == iy & lon == ix)
    
    dummy1 <- dummy1 %>%
      group_by(YYYY) %>%
      mutate(annual_stag_day=sum(ASI))
    dummy1
    #dummy1$annual_stag_day
    dummy1$stag_case <- 0
    ind <- findruns(dummy1$ASI,4)

    if(!is.null(ind)) {dummy1[ind,]$stag_case <-1}
    #if(is.null(ind)) {break}
    dummy1$id <- NA
    dummy1$annual_stag_case_duration <- NA
    stag_id <- 1
    ii<-Position(function(x) x > 0, dummy1$stag_case)
    if(!is.null(ind)) {
    while(ii <= nrow(dummy1)){
      
      i2 <- ii + Position(function(x) x == 0, dummy1$stag_case[ii:nrow(dummy1)])
      dummy1[(ii):(i2-2),]$id <- stag_id
      dummy1[(ii):(i2-2),]
      
      ii <- i2 - 1
      stag_id <- stag_id + 1
    
      i2 <- ii + Position(function(x) x > 0, dummy1$stag_case[ii:nrow(dummy1)])
      ii <- i2 - 1
#      print(ii)
      if(is.na(ii)) {break}
    }
    }

    #dummy1$annual_stag_case_duration <- 0
    
    #head(data.frame(dummy1))
    
    dummy2 <- dummy1 %>%
      group_by(YYYY,id) %>%
      tally() %>%
      filter(id > 0) %>%
      ungroup() %>%
      group_by(YYYY) %>%
      mutate(annual_stag_case_duration = mean(n + 3)) 
   
    dummy3 <- dummy2 %>%
      group_by(YYYY) %>%
      tally()
    #names(dummy3) <- c("YYYY"," annual_stag_case")
    
    dummy2 <-distinct(dummy2, YYYY, annual_stag_case_duration)
    dummy2$annual_stag_case <- dummy3$n
    

    dummy1 <- distinct(dummy1, YYYY, lon, lat, annual_stag_day)
    dummy1 <- merge(dummy1, dummy2, by = "YYYY", all=TRUE)
    
    #dummy2 <- merge(dummy2, dummy1, by="YYYY", all=TRUE)
    
    #distinct(dummy1, YYYY, annual_stag_day)
    #dummy2 <- merge(dat, dummy2, by="YYYY", all=TRUE ) 
  
    #distinct(dummy1,YYYY, lon, lat, annual_stag_day)
    dat <- rbind(dat, dummy1)
    #dat <- merge(dat, dummy3, by="YYYY", all=TRUE ) 
    #dummy2
    
    #dummy3 <- dummy1 %>%
    #  group_by(YYYY) %>%
    #  mutate(annual_stag_case=length(unique(id)),
    #         annual_stag_case_duration = mean(na.omit(annual_stag_case_duration)),
    #         start_stag_day=min(date))
    
    #dat <- rbind(dat, distinct(dummy2, YYYY, lon, lat, annual_stag_day, annual_stag_case, annual_stag_case_duration))
    
    rm(dummy1, dummy2, dummy3)
  }
}
```

```{r}
head(dat)
```

```{r, echo = FALSE,include=F}
library(maps)
WorldData <- map_data('world')
```

```{r, echo = FALSE,include=F}
cols<- c("#e7f0fa", #lighter than light blue
         "#c9e2f6", #light blue
         "#95cbee", #blue
         "#0099dc", #darker blue
         "#4ab04a", #green
         "#ffd73e", #yellow
         "#eec73a", #mustard
         "#e29421", #dark khaki (?)
         "#f05336", #orange red
         "#ce472e") #red
#install.packages("extrafont")
#extrafont::loadfonts()
```

```{r, echo=FALSE, include=FALSE}
file.name <- paste0("./DAOU/east_asia_2010_2019_ASI.R", sep = "")
load(file.name)
```

```{r,echo = FALSE}
library(ggplot2)
#colbr <- (brewer.pal(11, "RdBu"))
ggplot(dat[dat$YYYY=="2010",]) +
  geom_tile(aes(lon,lat,fill=annual_stag_day))+ #temperature data
  scale_fill_gradientn(colours =cols, limits=c(1,150),
                       #breaks=c(0,15,35,50,75,Inf),
                       #low = "White",high="#800026",
                       na.value = "white",
                       #na.value = "#081D58",
                       #na.value = "#3F007D",
                       guide=guide_colourbar(ticks=T, nbin=10,
                                             barheight=.5, label=T, 
                                             barwidth=10)) +
  coord_sf(ylim=c(25,55),xlim=c(105,145))+
  #coord_sf(ylim=c(33,38.5),xlim=c(126,130))+
  #labs(x="", y="", fill="") +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "NA", colour = "black", size=1) +
  theme(legend.position=c(.5, -.13),
        legend.direction="horizontal",
        legend.text=element_text(colour="black"),
        plot.margin=grid::unit(c(.5,.5,1.5,.5), "cm"),
        #axis.text.y=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())
#title=element_text(element_blank()),
#text=element_text(element_blank()))
```