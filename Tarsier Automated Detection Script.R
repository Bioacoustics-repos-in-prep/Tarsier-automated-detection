
# Load relevant packages -----------------------------------------------------

# install.packages("devtools")
# devtools::install_github("DenaJGibbon/gibbonR")

library(gibbonR)
library(stringr)
library(tuneR)
library(seewave)
library(ROCR)
set.seed(13)

# Set path to BoxDrive
BoxDrivePath <- '/Users/denaclink/Library/CloudStorage/Box-Box/Tangkoko/Tangkoko_August_29/Rolling PAM/'

# Prepare selection tables ------------------------------------------------

# List selection table full names
SelectionTables <-
  list.files('data/Tarsier_Raven_Selections',pattern = '.txt',full.names = T)

# List selection table short names
SelectionTablesShort <-
  list.files('data/Tarsier_Raven_Selections',pattern = '.txt')

# Isolate just file name
SelectionTablesID <- str_split_fixed(SelectionTablesShort,pattern = '.Table',n=2)[,1]

# Divide into training and test data sets ---------------------------------
# We will focus on a small number of files to start
n.files <- length(SelectionTables)

# Extract this number of files
RandomSample <- sample(length(SelectionTables), n.files, replace=F)

# Get full file path
SelectionTablesSub <- SelectionTables[RandomSample ]

# Extract this number of files from ID
SelectionTablesSubID <- SelectionTablesID[ RandomSample]

# Divide into 80% training
TrainingSelectionIDs <- sample(1:n.files,round(n.files*.8),replace = F)

# Subset 80% of selection tables for training
TrainingSelections <- SelectionTablesSub[TrainingSelectionIDs]
TrainingSelectionFileNames <- SelectionTablesSubID[TrainingSelectionIDs]
TrainingSelectionRecorders <- str_split_fixed(TrainingSelectionFileNames,pattern = '-',n=2)[,1]

# Use remaining 20% for testing
TestSelections <- SelectionTablesSub[-TrainingSelectionIDs]
TestSelectionFileNames <- SelectionTablesSubID[-TrainingSelectionIDs]
TestSelectionRecorders <- str_split_fixed(TestSelectionFileNames,pattern = '-',n=2)[,1]


# Prepare training data ---------------------------------------------------
source('R/DetectBLED.R')
TrainingOutputTarsier <- 'TrainingFiles/BLED/'

# Loop to identify sound events
for(c in 1:length(TrainingSelections)){
  TempTrainingFileInfo <- TrainingSelections[c]
  PAMRecorder <- TrainingSelectionRecorders[c]
  PAMDate <- str_split_fixed(TrainingSelectionFileNames[c],pattern = '-',n=2)[,2]
  Searchpath <- paste(BoxDrivePath,PAMRecorder,sep='')
  Temp.file.list <- list.files(Searchpath)
  wav.path <- Temp.file.list[which(str_detect(Temp.file.list,PAMDate))]
  inputfile <- paste(Searchpath,wav.path,sep='/')
  output.dir <- paste(TrainingOutputTarsier,PAMRecorder,sep='')


  print(paste('processing',TempTrainingFileInfo))
  DetectBLED(input=inputfile,
             file.type='wav',
             min.freq = 4000,
             max.freq = 15000,
             noise.quantile.val=0.5,
             spectrogram.window =512,
             pattern.split = ".wav",
             min.signal.dur = 6,
             max.sound.event.dur = 30,
             swift.time=FALSE,
             output = "wav",
             wav.output = TRUE,
             output.dir = output.dir,
             write.csv.output=FALSE,
             verbose=TRUE,
             random.sample=FALSE)
}


# Supervised classification -----------------------------------------------
TrainingWavFilesDir <-
  'TrainingFiles/TarsierTraining/'

trainingdata <- gibbonR::MFCCFunction(input.dir=TrainingWavFilesDir, min.freq = 4000, max.freq = 16000,win.avg='standard')

trainingdata$class <- as.factor(trainingdata$class)

ml.model.svm <- e1071::svm(trainingdata[, 2:ncol(trainingdata)], trainingdata$class, kernel = "radial",
                           cross = 25,
                           probability = TRUE)

print(paste('SVM accuracy',ml.model.svm$tot.accuracy))


# Visualize training data -------------------------------------------------

library(gibbonR)
library(ggpubr)
gibbonID(input.dir='TrainingFiles/TarsierTraining/',output.dir='TrainingFiles/TarsierTraining/Thumbnails',
         win.avg='mean.sd',add.spectrograms=TRUE,min.freq=4000,max.freq=16000,class='no.clustering')

gibbonID(input.dir='TrainingFiles/TarsierTraining/',output.dir='TrainingFiles/TarsierTraining/Thumbnails',
         win.avg='mean.sd',add.spectrograms=TRUE,min.freq=4000,max.freq=16000,class="fixed")



# Prepare test data -------------------------------------------------------
TarsierTestFilesListPath <- list()
# Loop to find full file name
for(c in 1:length(TestSelections)){
  TempTrainingFileInfo <- TestSelections[c]
  PAMRecorder <- TestSelectionRecorders[c]
  PAMDate <- str_split_fixed(TestSelectionFileNames[c],pattern = '-',n=2)[,2]
  Searchpath <- paste(BoxDrivePath,PAMRecorder,sep='')
  Temp.file.list <- list.files(Searchpath)
  wav.path <- Temp.file.list[which(str_detect(Temp.file.list,PAMDate))]
  inputfile <- paste(Searchpath,wav.path,sep='/')
  TarsierTestFilesListPath[[c]] <- inputfile
}


# Run the detector/classifier ---------------------------------------------

OutputDirectory <-  "gibbonRoutput/"
source('R/gibbonR.R')

gibbonR(input=TarsierTestFilesListPath,
        feature.df=trainingdata,
        model.type.list=c('SVM','RF'),
        tune = TRUE,
        short.wav.duration=300,
        target.signal = c("T"),
        min.freq = 4000, max.freq = 16000,
        noise.quantile.val=0.3,
        minimum.separation =3,
        n.windows = 9, num.cep = 12,
        spectrogram.window =160,
        pattern.split = ".wav",
        min.signal.dur = 6,
        max.sound.event.dur = 25,
        maximum.separation =1,
        probability.thresh.svm = 0.15,
        probability.thresh.rf = 0.15,
        wav.output = "TRUE",
        output.dir =OutputDirectory,
        swift.time=FALSE,
        write.table.output=FALSE,verbose=TRUE,
        random.sample='NA')


Temp.files <- list.files(OutputDirectory,full.names = F)
Temp.files.full <- list.files(OutputDirectory,full.names = T)

for(d in 1:length(Temp.files)){
  TempName <- Temp.files[[d]]
  tempNameSplit <- str_split_fixed(TempName,pattern ='-',n=2)[,2]
  tempDate <- paste(str_split_fixed(tempNameSplit,pattern ='_',n=3)[,1],
                    str_split_fixed(tempNameSplit,pattern ='_',n=3)[,2],sep='_')
  FullName <-TarsierTestFilesListPath[[ which(str_detect(TarsierTestFilesListPath,tempDate))]]
  nslash <- str_count(FullName, pattern = '/')
  Recorder <-
    str_split_fixed(FullName, pattern = '/', nslash+1)[, nslash]

  TempWav <- readWave(Temp.files.full[[d]])
  NewWavName <- paste("gibbonRoutput/updateNames/",Recorder,'_',tempNameSplit,sep='')
  writeWave(TempWav,NewWavName,extensible = F)
}


# Tarsier Performance Metrics -----------------------------------------------------

CombinedTestSelections <- data.frame()
for(a in 1:length(TestSelections)){
  TempTable <- read.delim(TestSelections[a])
  TempName <- str_split_fixed(TestSelections[a],pattern = '/',n=3)[,3]
  TempNameSplit <- str_split_fixed(TempName,pattern = '-',n=2)
  Recorder <- TempNameSplit[,1]
  Date <- str_split_fixed(TempNameSplit[,2],pattern = '_',n=2)[,1]
  Time <- str_split_fixed(TempNameSplit[,2],pattern = '_',n=2)[,2]
  Time <- str_split_fixed(Time,pattern = '[.]',n=2)[,1]

  TempTable <- cbind.data.frame(TempTable,Recorder,Date,Time)

  CombinedTestSelections <- rbind.data.frame(CombinedTestSelections,TempTable)
}

CombinedTestSelections$file.name <- paste(CombinedTestSelections$Recorder,CombinedTestSelections$Date,CombinedTestSelections$Time,sep='_')

all.detections <- subset(CombinedTestSelections,Tarsier_Vocal=='Y')

# Validation --------------------------------------------------------------

all.combinedprecision.recall.randomiter <- data.frame()

output.dir <- "gibbonRoutput/updateNames/"

### Detections using band-limited energy summation
gibbondetects <- output.dir
list.ml <-  list.files(gibbondetects, full.names = T, pattern='.wav')
# list.txt <- list.files(gibbondetects, full.names = F, pattern='.txt')
# list.txt.names <- str_split_fixed(list.txt,pattern = '_timing',n=2)[,1]
#
# Need to focus on gibbons for this validation
nslash <- str_count(list.ml[[1]],'/')+1
list.ml.signals <- str_split_fixed(list.ml,pattern = '/',n=nslash)[,nslash]

list.ml.signals <- str_split_fixed(list.ml.signals,pattern = '_',n=5)[,4]


list.ml <-
  list.ml[which(list.ml.signals=='T')]


ml.detection.df <- data.frame()

for(y in 1:length(list.ml)){
  L.wav <- list.ml[[y]]
  n.slash  <- str_count(L.wav, pattern = "/")[1] + 1

  det.file.name <- str_split_fixed(L.wav,"/",n=n.slash)[,n.slash]
  det.file.name <- str_split_fixed(det.file.name,".wav",n=2)[,1]

  file.name <- paste(str_split_fixed(det.file.name,"_",n=5)[,1],str_split_fixed(det.file.name,"_",n=5)[,2],
                     str_split_fixed(det.file.name,"_",n=5)[,3], sep='_')
  det.date <- str_split_fixed(det.file.name,"_",n=5)[,2]
  det.time <- str_split_fixed(det.file.name,"_",n=5)[,3]
  det.swift <- str_split_fixed(det.file.name,"_",n=5)[,1]
  det.time.start <- as.numeric(str_split_fixed(det.file.name,"_",n=9)[,6])
  det.time.end <- as.numeric(str_split_fixed(det.file.name,"_",n=9)[,7])
  probability <- str_split_fixed(det.file.name,"_",n=8)[,8]
  ml.algorithm <- str_split_fixed(det.file.name,"_",n=7)[,5]

  detections.df <- cbind.data.frame(file.name,det.swift, det.date, det.time,det.time.start,det.time.end,probability,ml.algorithm)

  ml.detection.df <- rbind.data.frame(ml.detection.df,detections.df)
}

recall.snr.all.df <- data.frame()
for(x in 1:nrow(ml.detection.df)){
  all.detections.subset <- ml.detection.df[x,]
  validate.detect.subset <-subset(all.detections,file.name==as.character(all.detections.subset$file.name))
  validate.detect.subset$Begin.Time..s. <- as.numeric(validate.detect.subset$Begin.Time..s.)
  min.start.time <- as.numeric(all.detections.subset$det.time.start)-range.secs.start
  max.start.time <- as.numeric(all.detections.subset$det.time.start)+range.secs.end

  detections.ml <- subset(validate.detect.subset,  det.time.start  >= Begin.Time..s. &  det.time.end  <= End.Time..s. )

  if(nrow(detections.ml)>0){
    all.detections.subset$class.label <- '1'
  } else{
    all.detections.subset$class.label <- '-1'
  }


  #print(detections.ml.adj)
  recall.snr.all.df <- rbind.data.frame(recall.snr.all.df,all.detections.subset)
}



auc.df <- data.frame()
performance.df <- data.frame()


ml.index <- unique(recall.snr.all.df$ml.algorithm)
for(m in 1:length(ml.index)){

  temp.subset <-
    subset(recall.snr.all.df,
           ml.algorithm==ml.index[m])

  predictions <- as.numeric(temp.subset$probability)
  labels <- (temp.subset$class.label)
  pred <- prediction(predictions, labels)
  perf <- performance(pred, "rec", "prec")
  perfauc <- performance(pred, "aucpr")
  Precision <- perf@x.values[[1]]
  Recall <- perf@y.values[[1]]
  Threshold <- perf@alpha.values[[1]]
  AUC <- perfauc@y.values[[1]]
  perfF1 <- performance(pred, "f")
  F1 <-  perfF1@y.values[[1]]
  plot(perfF1)
  print(AUC)
  ml.algorithm <- ml.index[m]
  tempauc <- cbind.data.frame(AUC,ml.algorithm)
  auc.df <- rbind.data.frame(auc.df,tempauc)

  temp.performance <- cbind.data.frame(Precision,Recall,Threshold,F1,ml.algorithm)
  performance.df <- rbind.data.frame(performance.df,temp.performance)
}

plot(perfF1)

performance.df$Threshold <- as.numeric(performance.df$Threshold)
ggline(data=performance.df,x='Threshold',y='F1',color = 'ml.algorithm')



