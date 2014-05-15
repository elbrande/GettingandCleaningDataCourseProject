##Label lookups for measure names and activity names
label.measures = read.table("data/features.txt", header=F, sep=" ",col.names=c('id','label'), stringsAsFactors=F)
label.activity = read.table("data/activity_labels.txt", header=F, sep=" ",col.names=c('activity_id','activityName'), stringsAsFactors=F)
##Fixed column width definition
colWidths = rep(16,561)

##Add a sequence generated ID column
addRowID <- function (m) {
   id = seq(along=m[ ,1])
   cbind(id, m) 
}

##pType: test | train
getData <- function(pType) {
   
   ##Define which table columns to load by setting std() or mean() columns to numeric 
   ##Set all others to NULL.  This will skip the columns and reduce the data set
   tl = grepl('std()|mean()',label.measures$label)
   tc = as.character(tl)
   tc = gsub("TRUE","numeric",tc)
   tc = gsub("FALSE","NULL",tc)
   
   ##Read the main X data file and add a seqeuence id
   ##set the buffersize to conserve memory
   data = read.fwf(paste('data/',pType,'/X_',pType,'.txt',sep=''), widths=colWidths, header=F, colClasses=tc, col.names=label.measures$label, buffersize=250) 
   data=addRowID(data)
   
   ##Read the subject file and add an ID
   subject = read.table(paste('data/',pType,'/subject_',pType,'.txt',sep=''), col.names=c("subject_id"))
   subject=addRowID(subject)
   
   ##Read the activity file and add an ID
   activity = read.table(paste('data/',pType,'/y_',pType,'.txt',sep=''), col.names=c("activity_id"))
   activity=addRowID(activity)
   
   ##Merge the subject and activity
   combinedData = merge(subject, activity, by.x="id", by.y="id", all=T)
   
   ##Merge subject/activity witht the X data
   merge(combinedData, data, by.x="id", by.y="id", all=T)	

}


runAnalysis <- function () {
   #Get the two data sets
   data.test = getData('test')
   data.train = getData('train')
   
   #Combine the two data sets
   data.combined = rbind(data.test,data.train)

   ##Calculate the mean by person and activity
   data.aggregate = aggregate(data.combined, by=list(data.combined$subject_id, data.combined$activity_id), FUN=mean)   
   
   ##Fix up the column names by applying the column map from the codebook
   t = gsub("\\.{2,}",'.',(colnames(data.aggregate)))
   t = gsub('\\.$','',t)
   t  =gsub("^(t|f)",'',t)   
   colnames(data.aggregate)<-t
  
   ##Lookup activity names
   data.tidy = merge(label.activity, data.aggregate, by.x="activity_id", by.y="activity_id",  all.y=T)
   
   ##Write tidy data to file
   write.csv(data.tidy, file='dataTidy.txt')
}

