# This program will convert raw data into a Data Frame that is easy and intuitive to
# look at and is visually simplistic.

#Load the libraries
library("tidyr")
library("dplyr")
require("magrittr")

#-----------------------------------------------------------------------------
# Load data
# Everyones route will be differnet
raw_data <- read.table(file = "~/Desktop/Work/Project#2_NewSpreadsheet/genes.read_group_tracking.read_group_tracking", 
                       comment.char = "", skip = 0, sep = "\t", header = T)

# Only the info that we want to look at is seleted from the raw_data and put
# into the Data Frame named 
data <- subset(raw_data, select = -c(raw_frags, 
                                             internal_scaled_frags, 
                                             external_scaled_frags, 
                                             effective_length, 
                                             status))

# The condtion and replicate column are combnined into 1 column
data$condition_replicate = paste(data$condition, data$replicate, sep="_")

# A Data Frame is created with this new column
updated_data = subset(data, select = -c(condition, replicate))

# A new Data Frame that with the tracking id is created
tracking_idTable = subset(data, select = c(tracking_id))

# Only every 10th value will be saved, all other values are discarded
c = 1:nrow(tracking_idTable)
NFT1 = as.data.frame(tracking_idTable[(c%%10==0),])

# A Data Frame is created for 1 line of the updated data. This outlines the structure
# of all new created Data Frames
fooData.T = as.data.frame(t(updated_data[1:10,2,1:ncol(updated_data)]))

# Column names are assigned
colnames(fooData.T) = c('GermFreeColon_0', 
                         'GermFreeColon_1',
                         'HealthyFFColon_0',
                         'HealthyFFColon_1',
                         'HealthyFFColon_2',
                         'HealthyFFColon_3',
                         'CMAFFColon_0',
                         'CMAFFColon_1',
                         'CMAFFColon_2',
                         'CMAFFColon_3')

# A Data Frame is created with all of the FPKM values on one row
FPKMTable = t(updated_data[1:NROW(na.omit(raw_data)),2,1:ncol(updated_data)])

# A Data Frame is created with the second line of the FPKM Table to get the outline
# for that table set
moreData = rbind(fooData.T, FPKMTable[11:20])

# A counter is set to the size of the original data
counter = NROW(na.omit(raw_data)) + 1

# The first 20 lines of the FPKM are used, so we start at 21 and go by 10's creating
# new rows every time. The limit of rows is set to the number of rows from raw_data
firstNumber = 21
secondNumber = 30
while (secondNumber < counter) {
  moreData = rbind(moreData, FPKMTable[firstNumber:secondNumber])
  firstNumber = firstNumber + 10
  secondNumber = secondNumber + 10
}

# moreData is converted into a Data Frame
moreData = as.data.frame(moreData)

# The Final Data Frame is created with a combination of the NFT1 Data Frame
# and the moreData Data Frame
Final = cbind(NFT1, moreData)

# The column names are set in the Final Data Frame
colnames(Final) = c('tracking_id',
                    'GermFreeColon_0',
                    'GermFreeColon_1',
                    'HealthyFFColon_0',
                    'HealthyFFColon_1',
                    'HealthyFFColon_2',
                    'HealthyFFColon_3',
                    'CMAFFColon_0',
                    'CMAFFColon_1',
                    'CMAFFColon_2',
                    'CMAFFColon_3')
