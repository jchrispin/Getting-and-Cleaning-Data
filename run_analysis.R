library(tools)
library(dplyr)
library(tidyr)
library(sqldf)
# Setting the repository paths
root <- "C:/Download/Big Data/WrkD_DateSce/Getting and Cleaning Data/UCI HAR Dataset"
pTrain <- "C:/Download/Big Data/WrkD_DateSce/Getting and Cleaning Data/UCI HAR Dataset/train"
pTest <- "C:/Download/Big Data/WrkD_DateSce/Getting and Cleaning Data/UCI HAR Dataset/test"

# Storing the list of full names of the file to read in. sFile will contain the list of file group by train/test
# as columns. One extra column is added that will contain the name of the data frames merged.
fTrain <- list.files(pTrain, full.names=TRUE)
fTest <- list.files(pTest, full.names=TRUE)
rm(list = c("pTest","pTrain"))
t <- basename(file_path_sans_ext(fTest))
t <- gsub("_test","",t)
sFile <- tbl_df(data.frame(df_name=t, df_train=fTrain, df_test=fTest,stringsAsFactors=FALSE))
sFile <- sFile[sFile$df_name != "Inertial Signals", ]
# loading the activity table
activity <- read.table(paste(root,"activity_labels.txt",sep="/"),stringsAsFactors =FALSE, colClasses=c("numeric","character"))
# Loading the feature table
features <- read.table(paste(root,"features.txt",sep="/"),stringsAsFactors =FALSE, colClasses=c("numeric","character"))
features$V2 <- gsub("BodyBody","Body",features$V2)
features$V2 <- gsub("-","_",features$V2)
features$V2 <- gsub(",","_",features$V2)
features$V2 <- make.names(features$V2, unique=TRUE, allow_=TRUE)
features$V2 <- gsub("\\.","",features$V2)
#features$V2 <- gsub("mean_","mean",features$V2)
#features$V2 <- gsub("std_","std",features$V2)
features$V2 <- gsub("Body","_Body_",features$V2)
features$V2 <- gsub("Gravity","_Gravity_",features$V2)
#features$V2 <- gsub("Acc","Acc_",features$V2)
#features$V2 <- gsub("AccJerk","Acc_Jerk",features$V2)
#features$V2 <- gsub("GyroJerk","Gyro_Jerk",features$V2)
features$V2 <- gsub("JerkMag","Jerk_Mag",features$V2)
features$V2 <- gsub("AccMag","Acc_Mag",features$V2)
features$V2 <- gsub("GyroMag","Gyro_Mag",features$V2)
features$V2 <- gsub("Mag_mean","mean_Mag",features$V2)
features$V2 <- gsub("Mag_std","std_Mag",features$V2)
#features$V2 <- gsub("GyroMag","Gyro_Mag",features$V2)
rm(list = c("t","fTest","fTrain","root"))

# Loading and merging the data. The merge data will then have 561+2 columns.
for (i in 1:length(sFile$df_name)) {
    df <- rbind(read.table(sFile$df_train[i]),read.table(sFile$df_test[i]))
    assign(sFile$df_name[i],df)
}
rm(sFile, df, i)

# Extracting the needed column from X
# 1- Use column 2 of features to name the columns of X
# 2- Replace class labels with their corresponding activity names.
# 3- Select the needed columns from X and merge them with y and subject.
y <- data.frame(sqldf("select * from activity left join y using (V1)")$V2, stringsAsFactors=FALSE)
names(y) <- "Activity_Num"
names(subject) <- "Subject_Num"
names(X) <- features$V2
measure <- cbind(y,subject,select(X,contains("_mean",ignore.case=FALSE)),select(X,contains("_std",ignore.case=FALSE)))
measure <- select(measure,-contains("meanF"))
measure <- select(measure,-contains("MagF"))
rm(X,subject,features,y,activity)
#measure <- gather(measure,Dom_Cat_Sig_Mean_Cord, Mean_Cord, contains("_mean",ignore.case=FALSE))
measure <- gather(measure,Dom_Cat_Sig_Stat_Cord, Stat_Cord, -Activity_Num, -Subject_Num)
i <- sapply(measure, is.factor)
measure[i] <- lapply(measure[i], as.character)
rm(i)
measure <- separate(data=unique(measure), col=Dom_Cat_Sig_Stat_Cord, into=c("Domain","Motion","Sig_Label","Statistic","Axial_Mag"))

# Generating the Tidy dataset
# This dataset is composed of SignalT, Activity, X_AxialT, Y_AxialT, Z_AxialT and MagnitudeT (For more information see Code Book)
SignalT <- unique(select(measure, Domain:Sig_Label))
SignalT$Sig_ID <- 1:nrow(SignalT)
measure <- sqldf("select * from measure left join SignalT using (Domain, Motion, Sig_Label)")
Activity <- unique(sqldf("select Sig_ID, Activity_Num, Subject_Num from measure"))
Activity$Act_ID <- 1:nrow(Activity)
measure <- sqldf("select * from measure left join Activity using (Sig_ID, Activity_Num, Subject_Num)")
measure <- select(measure,Statistic:Act_ID)
X_AxialT <- select(filter(measure,Axial_Mag == "X"),-Axial_Mag)
Y_AxialT <- select(filter(measure,Axial_Mag == "Y"),-Axial_Mag)
Z_AxialT <- select(filter(measure,Axial_Mag == "Z"),-Axial_Mag)
MagnitudeT <- select(filter(measure,Axial_Mag == "Mag"),-Axial_Mag)
rm(measure)
write.table(SignalT, "SignalT.txt", row.name=FALSE)
write.table(Activity, "Activity.txt", row.name=FALSE)
write.table(X_AxialT, "X_AxialT.txt", row.name=FALSE)
write.table(Y_AxialT, "Y_AxialT.txt", row.name=FALSE)
write.table(Y_AxialT, "Y_AxialT.txt", row.name=FALSE)
write.table(MagnitudeT, "MagnitudeT.txt", row.name=FALSE)