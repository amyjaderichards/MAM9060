#########Reading in and exploring the 2012 Single Measurements data

#Why isn't this way working??
#DATA.2012 <- read.csv("../Data/2012_Single_Measurements.csv")

DATA.2012 <- read.csv("C:/Users/amyja/OneDrive/Desktop/MSc Dissertation/Data/2012_Single_Measurements.csv")

#Print size of the data
dim(DATA.2012)

#Print head of the dataframe (MESSY)
head(DATA.2012)

#Print columns of the dataframe
colnames(DATA.2012)

#########PLANT ID#########
#Save a list of the plant IDs
###There are columns with ID like this - aBR 15 fS B1-R9 C9
###Find the row number of these IDs and remove from subsequent lists of values??
#Should I remove the 2 from the start of the IDs?
PlantID <- DATA.2012$X.2

PlantID <- levels(PlantID)[PlantID]
PlantID <- tail(PlantID, -5)
#PlantID <- as.numeric(PlantID)

###SPLIT INTO BLOCKS 1, 2 AND 3??

#########STEM DIAMETERS#########
# 3 stems chosen RANDOMLY, preferably using marked stick, 1 measurement per stem, half way up canopy height, between nodes, no preference for min/ max.

StemDiam.1 <- DATA.2012$Stem
StemDiam.2 <- DATA.2012$Stem.1
StemDiam.3 <- DATA.2012$Stem.2

head(StemDiam.1)

#Removing first 5 rows from the Stem Diameter data
StemDiam.1 <- tail(StemDiam.1, -5)
StemDiam.2 <- tail(StemDiam.2, -5)
StemDiam.3 <- tail(StemDiam.3, -5)

head(StemDiam.1)

StemDiam.1 <- levels(StemDiam.1)[StemDiam.1]
StemDiam.1 <- as.numeric(StemDiam.1)
StemDiam.2 <- levels(StemDiam.2)[StemDiam.2]
StemDiam.2 <- as.numeric(StemDiam.2)
StemDiam.3 <- levels(StemDiam.3)[StemDiam.3]
StemDiam.3 <- as.numeric(StemDiam.3)

###Creating a dataframe of all stem diamaters and the corresponding plant ID
StemDiam.df <- do.call(rbind, Map(data.frame, ID=PlantID, StemDiam.1=StemDiam.1, StemDiam.2=StemDiam.2, StemDiam.3=StemDiam.3))

#Removing the rows which have non-integer plant Ids
#SuppressWarnings removes the warning message that NAs are introduced 
index1 <- which(is.na(suppressWarnings(as.numeric(as.character(StemDiam.df[[1]])))))
StemDiam.df.subset <- StemDiam.df[-index1,]

#Summary statistics for the three stem diameters - MORE HERE!
#There are 0's in each stem diameter list > ask Kerrie if those values should be discarded?
summary(StemDiam.df.subset$StemDiam.1)
summary(StemDiam.df.subset$StemDiam.2)
summary(StemDiam.df.subset$StemDiam.3)

#Plot of Stem Diameter 1
plot(unlist(StemDiam.df.subset$ID), unlist(StemDiam.df.subset$StemDiam.1), main="Stem Diameter 1 For Every Plant", xlab="Plant ID", ylab="Stem Diameter 1")

#Plot of Stem Diameter 2
plot(unlist(StemDiam.df.subset$ID), unlist(StemDiam.df.subset$StemDiam.2), main="Stem Diameter 2 For Every Plant", xlab="Plant ID", ylab="Stem Diameter 2")

#Plot of Stem Diameter 3
plot(unlist(StemDiam.df.subset$ID), unlist(StemDiam.df.subset$StemDiam.3), main="Stem Diameter 3 For Every Plant", xlab="Plant ID", ylab="Stem Diameter 3")



##########CLUMP DIAMETER#########
#Using a measuring stick, measure size of base from one side to another, to be done randomly - no preference for longest/ shortest side.

#Saving the clump diameter column into the variable ClumpDiam
ClumpDiam <- DATA.2012$Clump

#Removing the first 5 entries
ClumpDiam <- tail(ClumpDiam, -5)

#levels(ClumpDiam)
#nlevels(ClumpDiam) 46 LEVELS???

ClumpDiam <- levels(ClumpDiam)[ClumpDiam]
ClumpDiam <- as.numeric(ClumpDiam)


###CREATING A DATAFRAME WITH PLANT ID AND CLUMP DIAMETER AS COLUMNS
clump.df <- do.call(rbind, Map(data.frame, A=PlantID, B=ClumpDiam))

#Removing the rows which have non-integer plant Ids
#SuppressWarnings removes the warning message that NAs are introduced 
index1 <- which(is.na(suppressWarnings(as.numeric(as.character(clump.df[[1]])))))
clump.df.subset <- clump.df[-index1,]

#Plotting clump diameter against plant ID
plot(unlist(clump.df.subset$A), unlist(clump.df.subset$B), main="Clump Diameter For Every Plant", xlab="Plant ID", ylab="Clump Diameter (mm)")

""" This isn't the most useful graph as there is no way to spread out the ~300 plant IDs along the x-axis,
it is useful to see the spread of diameters for every plant though. Ask Kerrie if the 0 values are correct """

#Summary statistics for the clump diameter - MORE HERE??
summary(clump.df.subset$B)

