## Soil duplicates

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

forest.train.orig = forest.train

forest.train$Soil1 = forest.train$Soil1+forest.train$Soil5+forest.train$Soil6+forest.train$Soil14
forest.train$Soil10 = forest.train$Soil10+forest.train$Soil11+forest.train$Soil16+forest.train$Soil17
forest.train$Soil9 = forest.train$Soil9+forest.train$Soil12
forest.train$Soil23 = forest.train$Soil23+forest.train$Soil24+forest.train$Soil31+forest.train$Soil33
forest.train$Soil19 = forest.train$Soil19+forest.train$Soil26
forest.train$Soil22 = forest.train$Soil22+forest.train$Soil27+forest.train$Soil35+forest.train$Soil38+forest.train$Soil39+forest.train$Soil40
forest.train$Soil18 = forest.train$Soil18+forest.train$Soil28
forest.train$Soil29 = forest.train$Soil29+forest.train$Soil30

forest.train = subset(forest.train, select=-c(Soil5,Soil6,Soil14,Soil11,Soil16,Soil17,Soil12,
                Soil24,Soil31,Soil33,Soil26,Soil27,Soil35,Soil38,Soil39,Soil40,
                Soil28,Soil30,Soil7,Soil15))

any(forest.train$Soil1 > 1)
any(forest.train$Soil9 > 1)
any(forest.train$Soil10 > 1)
any(forest.train$Soil18 > 1)
any(forest.train$Soil19 > 1)
any(forest.train$Soil22 > 1)
any(forest.train$Soil23 > 1)
any(forest.train$Soil29 > 1)

forest.test.orig = forest.test

forest.test$Soil1 = forest.test$Soil1+forest.test$Soil5+forest.test$Soil6+forest.test$Soil14
forest.test$Soil10 = forest.test$Soil10+forest.test$Soil11+forest.test$Soil16+forest.test$Soil17
forest.test$Soil9 = forest.test$Soil9+forest.test$Soil12
forest.test$Soil23 = forest.test$Soil23+forest.test$Soil24+forest.test$Soil31+forest.test$Soil33
forest.test$Soil19 = forest.test$Soil19+forest.test$Soil26
forest.test$Soil22 = forest.test$Soil22+forest.test$Soil27+forest.test$Soil35+forest.test$Soil38+forest.test$Soil39+forest.test$Soil40
forest.test$Soil18 = forest.test$Soil18+forest.test$Soil28
forest.test$Soil29 = forest.test$Soil29+forest.test$Soil30

forest.test = subset(forest.test, select=-c(Soil5,Soil6,Soil14,Soil11,Soil16,Soil17,Soil12,
                                              Soil24,Soil31,Soil33,Soil26,Soil27,Soil35,Soil38,Soil39,Soil40,
                                              Soil28,Soil30,Soil7,Soil15))


dim(train[train$Soil18 == 1,])


## Soil types

forest.train = subset(forest.train, select=-c(SoilType))
st_idxs <- grep("Soil", names(forest.train))
forest.train$SoilType <- apply(forest.train, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.train$SoilType = as.integer(forest.train$SoilType)
#levels(forest.train$SoilType) = union(levels(forest.train$SoilType), levels(forest.test$SoilType))

forest.train.orig = subset(forest.train.orig, select=-c(SoilType))
st_idxs <- grep("Soil", names(forest.train.orig))
forest.train.orig$SoilType <- apply(forest.train.orig, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.train.orig$SoilType = as.integer(forest.train.orig$SoilType)
#levels(forest.train.orig$SoilType) = union(levels(forest.train.orig$SoilType), levels(forest.test.orig$SoilType))


forest.test = subset(forest.test, select=-c(SoilType))
st_idxs <- grep("Soil", names(forest.test))
forest.test$SoilType <- apply(forest.test, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.test$SoilType = as.integer(forest.test$SoilType)

forest.test.orig = subset(forest.test.orig, select=-c(SoilType))
st_idxs <- grep("Soil", names(forest.test.orig))
forest.test.orig$SoilType <- apply(forest.test.orig, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.test.orig$SoilType = as.integer(forest.test.orig$SoilType)



## Aspect

forest.train$AspectN = ifelse((forest.train$Aspect <= 22.5 | forest.train$Aspect > 337.5),1,0)
forest.train$AspectNE = ifelse((forest.train$Aspect > 22.5 & forest.train$Aspect <= 67.5),1,0)
forest.train$AspectE = ifelse((forest.train$Aspect > 67.5 & forest.train$Aspect <= 112.5),1,0)
forest.train$AspectSE = ifelse((forest.train$Aspect > 112.5 & forest.train$Aspect <= 157.5),1,0)
forest.train$AspectS = ifelse((forest.train$Aspect > 157.5 & forest.train$Aspect <= 202.5),1,0)
forest.train$AspectSW = ifelse((forest.train$Aspect > 202.5 & forest.train$Aspect <= 247.5),1,0)
forest.train$AspectW = ifelse((forest.train$Aspect > 247.5 & forest.train$Aspect <= 292.5),1,0)
forest.train$AspectNW = ifelse((forest.train$Aspect > 292.5 & forest.train$Aspect <= 337.5),1,0)

forest.test$AspectN = ifelse((forest.test$Aspect <= 22.5 | forest.test$Aspect > 337.5),1,0)
forest.test$AspectNE = ifelse((forest.test$Aspect > 22.5 & forest.test$Aspect <= 67.5),1,0)
forest.test$AspectE = ifelse((forest.test$Aspect > 67.5 & forest.test$Aspect <= 112.5),1,0)
forest.test$AspectSE = ifelse((forest.test$Aspect > 112.5 & forest.test$Aspect <= 157.5),1,0)
forest.test$AspectS = ifelse((forest.test$Aspect > 157.5 & forest.test$Aspect <= 202.5),1,0)
forest.test$AspectSW = ifelse((forest.test$Aspect > 202.5 & forest.test$Aspect <= 247.5),1,0)
forest.test$AspectW = ifelse((forest.test$Aspect > 247.5 & forest.test$Aspect <= 292.5),1,0)
forest.test$AspectNW = ifelse((forest.test$Aspect > 292.5 & forest.test$Aspect <= 337.5),1,0)

forest.train = subset(forest.train, select=-c(AspectFact))
st_idxs = grep("Aspect", names(forest.train))
st_idxs = st_idxs[-1] # remove Aspect column
forest.train$AspectFact <- apply(forest.train, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.train$AspectFact = as.integer(forest.train$AspectFact)

forest.train.orig = subset(forest.train.orig, select=-c(AspectFact))
st_idxs = grep("Aspect", names(forest.train.orig))
st_idxs = st_idxs[-1] # remove Aspect column
forest.train.orig$AspectFact <- apply(forest.train.orig, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.train.orig$AspectFact = as.integer(forest.train.orig$AspectFact)

forest.test = subset(forest.test, select=-c(AspectFact))
st_idxs = grep("Aspect", names(forest.test))
st_idxs = st_idxs[-1] # remove Aspect column
forest.test$AspectFact <- apply(forest.test, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.test$AspectFact = as.integer(forest.test$AspectFact)

forest.test.orig = subset(forest.test.orig, select=-c(AspectFact))
st_idxs = grep("Aspect", names(forest.test.orig))
st_idxs = st_idxs[-1] # remove Aspect column
forest.test.orig$AspectFact <- apply(forest.test.orig, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.test.orig$AspectFact = as.integer(forest.test.orig$AspectFact)



## Wilderness Area

forest.train = subset(forest.train, select=-c(Wilderness))
st_idxs = grep("Wilderness", names(forest.train))
forest.train$Wilderness <- apply(forest.train, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.train$Wilderness = as.integer(forest.train$Wilderness)

forest.train.orig = subset(forest.train.orig, select=-c(Wilderness))
st_idxs = grep("Wilderness", names(forest.train.orig))
forest.train.orig$Wilderness <- apply(forest.train.orig, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.train.orig$Wilderness = as.integer(forest.train.orig$Wilderness)

forest.test = subset(forest.test, select=-c(Wilderness))
st_idxs = grep("Wilderness", names(forest.test))
forest.test$Wilderness <- apply(forest.test, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.test$Wilderness = as.integer(forest.test$Wilderness)

forest.test.orig = subset(forest.test.orig, select=-c(Wilderness))
st_idxs = grep("Wilderness", names(forest.test.orig))
forest.test.orig$Wilderness <- apply(forest.test.orig, MARGIN=1, FUN=function(x){
  grep(1, x[st_idxs])
})
forest.test.orig$Wilderness = as.integer(forest.test.orig$Wilderness)

# Hillshade

forest.train$Hillshade = forest.train$Hillshade9am + forest.train$HillshadeNoon + forest.train$Hillshade3pm
forest.train.orig$Hillshade = forest.train.orig$Hillshade9am + forest.train.orig$HillshadeNoon + forest.train.orig$Hillshade3pm
forest.test$Hillshade = forest.test$Hillshade9am + forest.test$HillshadeNoon + forest.test$Hillshade3pm
forest.test.orig$Hillshade = forest.test.orig$Hillshade9am + forest.test.orig$HillshadeNoon + forest.test.orig$Hillshade3pm



# Soil types

forest.train$SoilStony = forest.train$Soil6 + forest.train$Soil12
forest.train$SoilVeryStony = forest.train$Soil2 + forest.train$Soil9 + forest.train$Soil18
+ forest.train$Soil26
forest.train$SoilExtremStony = forest.train$Soil1 + forest.train$Soil24 + forest.train$Soil25
+ forest.train$Soil27 + forest.tlibrary(ggplot2)rain$Soil28 + forest.train$Soil29 + forest.train$Soil30
+ forest.train$Soil31 + forest.train$Soil32 + forest.train$Soil33 + forest.train$Soil34
+ forest.train$Soil36 + forest.train$Soil37 + forest.train$Soil38 + forest.train$Soil39
+ forest.train$Soil40
forest.train$SoilRubbly = forest.train$Soil3 + forest.train$Soil4 + forest.train$Soil5
+ forest.train$Soil10 + forest.train$Soil11 + forest.train$Soil13 + 
  forest.train$SoilCryaquolis = forest.train$Soil17 + forest.train$Soil19
forest.train$SoilCryaquolls = forest.train$Soil20 + forest.train$Soil21 + forest.train$Soil23
forest.train$SoilBouldery = forest.train$Soil22
# Soil7, Soil8, Soil14, Soil15, Soil16, Soil35

forest.test$SoilStony = forest.test$Soil6 + forest.test$Soil12
forest.test$SoilVeryStony = forest.test$Soil2 + forest.test$Soil9 + forest.test$Soil18 + forest.test$Soil26
forest.test$SoilExtremStony = forest.test$Soil1 + forest.test$Soil24 + forest.test$Soil25 + forest.test$Soil27 + forest.test$Soil28 + forest.test$Soil29 + forest.test$Soil30 + forest.test$Soil31 + forest.test$Soil32 + forest.test$Soil33 + forest.test$Soil34 + forest.test$Soil36 + forest.test$Soil37 + forest.test$Soil38 + forest.test$Soil39 + forest.test$Soil40 
forest.test$SoilRubbly = forest.test$Soil3 + forest.test$Soil4 + forest.test$Soil5 + forest.test$Soil10 + forest.test$Soil11 + forest.test$Soil13
forest.test$SoilCryaquolis = forest.test$Soil17 + forest.test$Soil19
forest.test$SoilCryaquolls = forest.test$Soil20 + forest.test$Soil21 + forest.test$Soil23
forest.test$SoilBouldery = forest.test$Soil22
# Soil7, Soil8, Soil14, Soil15, Soil16, Soil35

