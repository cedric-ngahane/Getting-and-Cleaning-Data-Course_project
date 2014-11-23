#download data
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile="getdata_projectfiles_UCI HAR Dataset.zip")

#unzip data
a<-unzip("getdata_projectfiles_UCI HAR Dataset.zip")

# 1*** Merges the training and the test sets to create one data set.

#-- reading X_train file ---
# we set buffersize to have enough memory
# we avoid any conversion type because of reading whitespace
fichier<-"UCI HAR Dataset/train/X_train.txt"
n<-nchar(readLines(fichier,1))/16 #number of columns
xtrain<-read.fwf(fichier, widths=rep(16,n), colClasses ="character",  buffersize = 700)
# suppression of trim leading space in values of variables and 
#conversion into numeric type
f<-function(x){as.numeric(sub("^\\s+","",x))} #trim leading space and convert
xtrain<-as.data.frame(lapply(xtrain,f))

#-- reading X_test file
# we set buffersize to have enough memory
# we avoid any conversion type because of reading whitespace
fichier<-"UCI HAR Dataset/test/X_test.txt"
n<-nchar(readLines(fichier,1))/16 #number of columns
xtest<-read.fwf(fichier, widths=rep(16,n), colClasses ="character",  buffersize = 700)
# suppression of trim leading space in values of variables and 
#conversion into numeric type
xtest<-as.data.frame(lapply(xtest,f))

#-- merge train and testfile 
d<-rbind(xtrain,xtest)
#d$group<-rep(c("train","test"),each=c(nrow(train),nrow(test))) 

#*__________________________________________________________

# 2*** Extracts only the measurements on the mean and standard deviation for each measurement. 

#--reading feature to have variables names
nomvar<-readLines("UCI HAR Dataset/features.txt")
#searching for variable which contain "mean(" or "std("
pos<-grep("mean\\(|std\\(",nomvar,F)
d<-d[,pos]

#*__________________________________________________________


# 3*** Uses descriptive activity names to name the activities in the data set

#reading activity in training data and testing data
activity<-readLines("UCI HAR Dataset/train/Y_train.txt")
activity<-c(activity, readLines("UCI HAR Dataset/test/Y_test.txt"))
#reading activities labels
lab.act<-readLines("UCI HAR Dataset/activity_labels.txt")
#trim leading digit and whitespace activity labels
lab.act<-sub("^\\d\\s", "",lab.act)
#inserting activity in dataset and labelling
activity<-factor(activity, labels=lab.act)

#*__________________________________________________________

# 4*** Appropriately labels the data set with descriptive variable names. 

#trim leading digit and whitespace before variable name
nomvar<-nomvar[pos]
nomvar<-sub("^\\d+\\s+", "",nomvar)
nomvar<-make.names(nomvar)
nomvar<-gsub("\\.+",".",nomvar)
names(d)<-nomvar
#*__________________________________________________________


# 5 *** From the data set in step 4, creates a second, independent tidy data
#   set with the average of each variable for each activity and each subject.

#reading subject in training data and testing data
subject<-readLines("UCI HAR Dataset/train/subject_train.txt")
subject<-c(subject, readLines("UCI HAR Dataset/test/subject_test.txt"))

#the average of each variable for each activity and each subject.
tidydata<-aggregate(d,by=list(subject,activity),FUN=mean)
names(tidydata)[1:2]<-c("subject","activity")
write.table(tidydata, file="tidydata.txt", row.names =F)





