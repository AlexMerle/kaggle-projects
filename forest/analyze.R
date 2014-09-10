summary(forest.train)
table(forest.train$Cover)
table(forest.train$Wilderness1)
table(forest.train$Wilderness2)
table(forest.train$Wilderness3)
table(forest.train$Wilderness4)

cor(forest.train$Cover, forest.train$Wilderness1)
cor(forest.train$Cover, forest.train$Wilderness2)
cor(forest.train$Cover, forest.train$Wilderness3)
cor(forest.train$Cover, forest.train$Wilderness4)

boxplot(forest.train$Elevation ~ forest.train$Cover)
boxplot(forest.train$RoadwaysHorDist ~ forest.train$Cover)
plot(forest.train$Elevation ~ forest.train$RoadwaysHorDist, col=forest.train$Cover)
plot(forest.train$Elevation ~ forest.train$FirePointsHorDist, col=forest.train$Cover)
plot(forest.train$Elevation ~ forest.train$HydrologyHorDist, col=forest.train$Cover)
plot(forest.train$Elevation ~ forest.train$HydrologyVerDist, col=forest.train$Cover)
boxplot(forest.train$Hillshade3pm ~ forest.train$AspectN)
plot(density(forest.train$Slope))
plot(density(forest.train$Aspect))

plot(density(forest.train$Hillshade9am))
plot(density(forest.train$HillshadeNoon))
plot(density(forest.train$Hillshade3pm))

plot(forest.train$HillshadeNoon ~ forest.train$AspectCos, col=forest.train$Cover)
plot(forest.train$Elevation ~ forest.train$HydrologyHorDist, col=forest.train$Cover)


boxplot(forest.train$Slope ~ forest.train$Cover)

plot(forest.train$Elevation ~ forest.train$Slope, col=forest.train$Cover)

qplot(HydrologyHorDist, Elevation, data=train, facets=.Wilderness~, color=Cover)



subsetSoil <- function(data, type) {
  subset = data[ data[paste0("Soil",type)]==1, ]
  return(subset)
}

# Soil7, Soil15 - empty
# kick 8 and 25?
# 1+5+6+14
# 10+11+16+17
# 9+12
# 23+24+31+33
# 19+26
# 22+27+35+38+39+40
# 18+28
# 29+30


table(subsetSoil(forest.train, 1)$Cover) # 3,4,6
table(subsetSoil(forest.train, 2)$Cover) # 2,3,4,5,6
table(subsetSoil(forest.train, 3)$Cover) # 2,3,4,6
table(subsetSoil(forest.train, 4)$Cover) # 1,2,3,4,5,6,7
table(subsetSoil(forest.train, 5)$Cover) # 3,4,6
table(subsetSoil(forest.train, 6)$Cover) # 3,4,6
table(subsetSoil(forest.train, 7)$Cover) # ----
table(subsetSoil(forest.train, 8)$Cover) # 2
table(subsetSoil(forest.train, 9)$Cover) # 1,2
table(subsetSoil(forest.train, 10)$Cover) # 1,2,3,4,5,6
table(subsetSoil(forest.train, 11)$Cover) # 1,2,3,4,5,6
table(subsetSoil(forest.train, 12)$Cover) # 1,2
table(subsetSoil(forest.train, 13)$Cover) # 1,2,3,5,6
table(subsetSoil(forest.train, 14)$Cover) # 3,4,6
table(subsetSoil(forest.train, 15)$Cover) # ----
table(subsetSoil(forest.train, 16)$Cover) # 1,2,3,4,5,6
table(subsetSoil(forest.train, 17)$Cover) # 1,2,3,4,5,6
table(subsetSoil(forest.train, 18)$Cover) # 2,5
table(subsetSoil(forest.train, 19)$Cover) # 1,2,5
table(subsetSoil(forest.train, 20)$Cover) # 1,2,5,6
table(subsetSoil(forest.train, 21)$Cover) # 1,7
table(subsetSoil(forest.train, 22)$Cover) # 1,2,7
table(subsetSoil(forest.train, 23)$Cover) # 1,2,5,6,7
table(subsetSoil(forest.train, 24)$Cover) # 1,2,5,6,7
table(subsetSoil(forest.train, 25)$Cover) # 2
table(subsetSoil(forest.train, 26)$Cover) # 1,2,5
table(subsetSoil(forest.train, 27)$Cover) # 1,2,7
table(subsetSoil(forest.train, 28)$Cover) # 2,5
table(subsetSoil(forest.train, 29)$Cover) # 1,2,5,7
table(subsetSoil(forest.train, 30)$Cover) # 1,2,5,7
table(subsetSoil(forest.train, 31)$Cover) # 1,2,5,6,7
table(subsetSoil(forest.train, 32)$Cover) # 1,2,3,5,6,7
table(subsetSoil(forest.train, 33)$Cover) # 1,2,5,6,7
table(subsetSoil(forest.train, 34)$Cover) # 2,5,6,7
table(subsetSoil(forest.train, 35)$Cover) # 1,2,7
table(subsetSoil(forest.train, 36)$Cover) # 2,7
table(subsetSoil(forest.train, 37)$Cover) # 7
table(subsetSoil(forest.train, 38)$Cover) # 1,2,7
table(subsetSoil(forest.train, 39)$Cover) # 1,2,7
table(subsetSoil(forest.train, 40)$Cover) # 1,2,7
