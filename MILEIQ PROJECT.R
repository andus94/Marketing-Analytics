
### 1. Read the Data

data <- read.csv("C:/Users/wkong_000/Desktop/MileIQ/Marketing Analytics Internship Data.csv")

### 2. Creating Percent Classified Variable

indicator <- 1

Percent.Classified <-vector()

while(indicator <= length(data$Drives.Classified)){
  
  Percent.Classified[indicator] <- data$Drives.Classified[indicator] / data$Drives.Captured[indicator]
  
  indicator <- indicator + 1   
}

### 3. Creating total Percent Classified Variable

indicator <- 1

Percent.Classified.Total <- vector()

total.classified <- 0
total.captured <- 0

while(indicator <= length(data$Drives.Classified)){
  
  total.classified <- total.classified + data$Drives.Classified[indicator]
  total.captured <- total.captured + data$Drives.Captured[indicator]
  Percent.Classified.Total[indicator] <- total.classified/total.captured
  
  indicator <- indicator + 1  
}

### 4. Creating Average Classified

indicator <- 1

Average <- vector()

while(indicator <= length(data$Drives.Classified)){
  
  Average[indicator] <- (data$Drives.Classified[indicator] + data$Drives.Classified[indicator + 1]) / 2
  
  indicator <- indicator + 1   
}

### 5. Creating Percentage Classified over Open

indicator <- 1

Percent.Open <- vector()

while(indicator <= length(data$Drives.Classified)){
  
  Percent.Open[indicator] <- data$Drives.Classified[indicator] / data$Number.of.Opens[indicator]
  
  indicator <- indicator + 1
  
}

### 6. Merging Data

data <- cbind(data,Percent.Classified, Percent.Classified.Total, Average, Percent.Open)

write.csv(data, file = "MileIQ.Date.csv")

### 7. Graphing Date vs. All new Variables

g <- ggplot(data, aes(Date, Percent.Classified)) ### Date vs. Percent.Classified

g + geom_point() + geom_line() + coord_cartesian(xlim = c(1,30), ylim = c(0,1))

g <- ggplot(data, aes(Date, Percent.Classified.Total)) ### Date vs. Percent.Classified.Total

g + geom_point() + geom_line() + coord_cartesian(xlim = c(1,365))

g <- ggplot(data, aes(Date, Average)) ### Date vs. Average.Classified

g + geom_point() + geom_line() +coord_cartesian(xlim = c(1:30))

g <- ggplot(data, aes(Date, Percent.Open)) ### Date vs. Percent.Open

g + geom_point() + geom_line() +coord_cartesian(xlim = c(1:30), ylim = c(0,8))


### 7. Correlation Matrix

data <- read.csv("C:/Users/wkong_000/Desktop/MileIQ/Marketing Analytics Internship Data.csv")

cor(data)

### 8. Graphing Open vs. Captured

library(ggplot2)

g <- ggplot(data, aes(Number.of.Opens, Drives.Captured))

g + geom_point() + geom_smooth() 

### 9. Graphing 3 Month window 

TriMonth1 <- read.csv("C:/Users/wkong_000/Desktop/MileIQ/TriMonth1.csv")

g <- ggplot(TriMonth1, aes(Date, Drives.Classified)) ### Date vs Classified

g + geom_point() + geom_line() + geom_smooth() 

g <- ggplot(TriMonth1, aes(Date, Number.of.Opens)) ### Date vs Opens

g + geom_point() + geom_line() + geom_smooth() 

### 10. Calculating Distance between Local Max and Min

distance <- vector()

indicator <- 1
indicator3 <-1
indicator4 <- 0

while(indicator <= length(data$Number.of.Opens)){
  
  
  indicator2 <- indicator + 29
  
  Max.Position <- which.max(data$Number.of.Opens[indicator: indicator2]) + indicator4 * 30
  
  Min.Position <- which.min(data$Number.of.Opens[Max.Position: indicator2]) + Max.Position - 1
  
  distance[indicator3] <- Min.Position - Max.Position
  
  
  indicator <- indicator +  30  
  indicator3 <- indicator3 + 1
  indicator4 <- indicator4 + 1
}

### 11. Calculating Average Distance in each 30 day time frame

mean(distance)




local.max <-max(data$Number.of.Opens[indicator: indicator2])

local.max

which.max(data$Number.of.Opens[indicator: indicator2])

(which.min(data$Number.of.Opens[which.max(data$Number.of.Opens[indicator: indicator2]): indicator2]) + 
  which.max(data$Number.of.Opens[indicator: indicator2]) - 1)


min(data$Number.of.Opens[which.max(data$Number.of.Opens[indicator: indicator2]): indicator2])

local.max <- max(data$Number.of.Opens[indicator: indicator2])

local.min <- min(data$Number.of.Opens[which.max(data$Number.of.Opens[indicator: indicator2]): indicator2])
                               
                               
                               
                               