
# Load relevant packages -----------------------------------------------------

# install.packages("devtools")
# devtools::install_github("DenaJGibbon/gibbonR")

library(gibbonR)
library(stringr)
library(tuneR)
library(seewave)
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

trainingdata <- gibbonR::MFCCFunction(input.dir=TrainingWavFilesDir, min.freq = 4000, max.freq = 16000,win.avg='mean.sd')

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

