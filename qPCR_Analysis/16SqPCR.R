# Program
library("tidyr")
library("dplyr")
library("xlsx")


#-----------------------------------------------------------------------------

# Load data
# Everyones route will be differnet
# setwd("")
# Load the fecal weight
Fecal_Weight = read.table(file = "~/Desktop/Work/qPCR_Analysis/filesToTest/Fecal_Weights_06162016_Clostridia_Initial_Colonization_Bact_Load_16S_data_James.csv", 
                          comment.char = "", skip = 0, sep = ",", header = T)
# Load the raw data table from the qPCR machine
raw_data = read.table(file = "~/Desktop/Work/qPCR_Analysis/filesToTest/06162016_Clostridia_Initial_Colonization_Bact_Load_16S_data_James.csv", 
                      comment.char = "", skip = 7, sep = ",", header = T)

#-----------------------------------------------------------------------------

# Removes all of the empty rows
raw_data <- raw_data[!apply(is.na(raw_data) | raw_data == "", 1, all),]

#-----------------------------------------------------------------------------

# Get rid of the extra lines at the end of the Data Frame:
raw_data <- raw_data[1:(length(row.names(raw_data))-4),]

#-----------------------------------------------------------------------------

# Change the column names
colnames(raw_data) <- c('Well',
                        'Sample.Name',
                        'Target.Name',
                        'Task',
                        'Reporter',
                        'Quencher',
                        'RQ',
                        'RQ.Min',
                        'RQ.Max',
                        'Ct',
                        'Ct.Mean',
                        'Ct.SD',
                        'Delta.Ct',
                        'Delta.Ct.Mean',
                        'Delta.Ct.SE',
                        'HK.Control.Delta.Ct.Mean',
                        'HK.Control.Delta.Ct.SE',
                        'Delta.Delta.Ct',
                        'Automatic.Ct.Threshold',
                        'Ct.Threshold',
                        'Automatic.Baseline',
                        'Baseline.Start',
                        'Baseline.End',
                        'Efficiency',
                        'Tm1',
                        'Tm2',
                        'Tm3',
                        'Comments',
                        'High.SD',
                        'EXP.Fail',
                        'MTP')

#-----------------------------------------------------------------------------

# Subset just the columns that we need for the time being
data = subset(raw_data, select = c(Well,
                                   Sample.Name,
                                   Target.Name,
                                   Ct,
                                   Ct.Mean,
                                   High.SD))

#-----------------------------------------------------------------------------

# Shortens "data" so that I can cbind it with the new values
# There will always be 24 data values so we can hard code this
shortData = as.data.frame(data[1:24,])

#-----------------------------------------------------------------------------

# Fixes the index number on the Data Frame to be in the right order
rownames(shortData) <- 1:nrow(shortData)

#-----------------------------------------------------------------------------

# Creates a new column with just the High.SD data
High.SD = shortData[6]

# Subsets the rows that have a "Y" in the yes/no column
rows_With_High_SD = which(High.SD == "Y", arr.ind=TRUE)

# Just gets the column(s)
rows_With_High_SD = as.data.frame(rows_With_High_SD[1:nrow(rows_With_High_SD),1])

# Remove the Data Frames we no longer need
rm(High.SD)

#-----------------------------------------------------------------------------

# Create a Data Frame with all of the log16S_Copies values
log16S_Copies = as.data.frame(c(7.5,7,6.5,6,5.5,5,4.5,4,3.5,3,2.5,2))

# Combines the 2 dataFrames
A = cbind(shortData, log16S_Copies)

# Changes the names of the columns
colnames(A) <- c('Well', 
                 'Sample.Name', 
                 'Target.Name', 
                 'Ct', 
                 'Ct.Mean', 
                 'High.SD', 
                 'log16S_Copies')

# Gets rid of the Data Frames we do not need anymore
rm(shortData)
rm(log16S_Copies)

#-----------------------------------------------------------------------------

# Display Graph
# Set Linear Model
lm.r <- lm(log16S_Copies ~ Ct.Mean, data = A)
# Plot the standard curve and fit line
plot(log16S_Copies ~ Ct.Mean, data = A)
# Include the linear model fit into the plot
abline(lm.r)
# Now we have a graph/plot with a linear model/standard curve/prediction line

#-----------------------------------------------------------------------------

# Prints the values that have a higher standard deviation
i = 0
counter = 1

print("Here are the values that have a high standard deviation:")

while (i < nrow(rows_With_High_SD)) {
  print(A[rows_With_High_SD[counter,1],])
  i = i + 1
  counter = counter + 1
}
rm(i)
rm(counter)
rm(rows_With_High_SD)

# Asks how many rows the user wants to remove
# Removes the rows that the user wants to remove
# How many wells would you like to remove?
NWellsToRemove = as.numeric(readline("How many wells would you like to remove?: "))

i = 0
well = 0
counter = 1
print(A)
while (i < NWellsToRemove) {
  well = as.numeric(readline("Which index (number on the far left) you would like to remove?: "))
  A = A[-well,]
  i = i + 1
  rownames(A) <- 1:nrow(A)
  print(A)
}
rm(counter)
rm(i)
rm(NWellsToRemove)
rm(well)

#-----------------------------------------------------------------------------

# Set Linear Model
lm.r <- lm(log16S_Copies ~ Ct.Mean, data = A)
# Plot the standard curve and fit line
plot(log16S_Copies ~ Ct.Mean, data = A)
# Include the linear model fit into the plot
abline(lm.r)
# Now we have a graph/plot with a linear model/standard curve/prediction line

#-----------------------------------------------------------------------------

# Asks the user if the graph looks good and then gives then an option to remove additional values
checkGraph = readline("Does this graph look good? If you want to remove additional samples, type 'no'. If not, type 'yes': ")
if(checkGraph == "no") {
  NWellsToRemove = as.numeric(readline("How many wells would you like to remove?: "))
  i = 0
  well = 0
  counter = 1
  print(A)
  while (i < NWellsToRemove) {
    well = as.numeric(readline("Which index (number on the far left) you would like to remove?: "))
    A = A[-well,]
    i = i + 1
    rownames(A) <- 1:nrow(A)
    print(A)
  }
  rm(counter)
  rm(i)
  rm(NWellsToRemove)
  rm(well)
  
  # Set Linear Model
  lm.r <- lm(log16S_Copies ~ Ct.Mean, data = A)
  # Plot the standard curve and fit line
  plot(log16S_Copies ~ Ct.Mean, data = A)
  # Include the linear model fit into the plot
  abline(lm.r)
  # Now we have a graph/plot with a linear model/standard curve/prediction line
}

#-----------------------------------------------------------------------------

# To check the formula/coefficients/intercept. Run: ->
print(lm.r)

#-----------------------------------------------------------------------------

# To check the formula used, the R^2, the Residuals, the Coefficients, The Intercept, 
# The p-value, and many other parameters. Run: ->
print(summary(lm.r))

#-----------------------------------------------------------------------------

# Shows the user information on the linear fit
checkData = readline("Above in the information on the linear model fit line. Type anything to continue: ")

#-----------------------------------------------------------------------------

# Creates a Data Frame with just the values that we are looking for
dataShortened = data[25:nrow(data),]

#-----------------------------------------------------------------------------

# Get rid of the Positive | Negative | Blank controls
# Fixes the index number on the Data Frame to be in the right order
rownames(dataShortened) <- 1:nrow(dataShortened)

# Subsets all of the control values
Negative = subset(dataShortened, Sample.Name == "Negative")
Positive = subset(dataShortened, Sample.Name == "Positive")
Blank = subset(dataShortened, Sample.Name == "Blank")

# Prints all of the control values to the console
print("The Negative Control(s)")
print(Negative)
print("The Positive Control(s)")
print(Positive)
print("The Blank Control(s)")
print(Blank)

# Stops the program to show the user the control values
stopper.NotNeeded = readline("Controls will be removed. Type anything to continue: ")
if(stopper.NotNeeded == "no") {
  print("Demasiado. Se van a eliminar de todos modos")
}

#-----------------------------------------------------------------------------

# Removes the Negative controls
# Has to go backwards so that the numbers behind it do not get messed up when the index change
negativeToRemove = nrow(Negative)
i = 0
counter = nrow(Negative)
while (i < negativeToRemove) {
  delete = as.numeric(row.names(Negative[counter,]))
  dataShortened = dataShortened[-delete,]
  i = i + 1
  counter = counter - 1
}
rownames(dataShortened) <- 1:nrow(dataShortened)
rm(negativeToRemove)
rm(i)
rm(counter)

#-----------------------------------------------------------------------------

# Removes the Positive controls
# Has to go backwards so that the numbers behind it do not get messed up when the index change
# re-subset the Positive controls because the index values will be different
Positive = subset(dataShortened, Sample.Name == "Positive")
positiveToRemove = nrow(Positive)
i = 0
counter = nrow(Positive)
while (i < positiveToRemove) {
  delete = as.numeric(row.names(Positive[counter,]))
  dataShortened = dataShortened[-delete,]
  i = i + 1
  counter = counter - 1
}
rownames(dataShortened) <- 1:nrow(dataShortened)
rm(positiveToRemove)
rm(i)
rm(counter)

#-----------------------------------------------------------------------------

# Removes the Blank controls
# Has to go backwards so that the numbers behind it do not get messed up when the index change
# re-subset the Blank controls because the index values will be different
Blank = subset(dataShortened, Sample.Name == "Blank")
blankToRemove = nrow(Blank)
i = 0
counter = nrow(Blank)
while (i < blankToRemove) {
  delete = as.numeric(row.names(Blank[counter,]))
  dataShortened = dataShortened[-delete,]
  i = i + 1
  counter = counter - 1
}
rownames(dataShortened) <- 1:nrow(dataShortened)
rm(blankToRemove)
rm(i)
rm(counter)
rm(delete)
rm(stopper.NotNeeded)
rm(Negative)
rm(Positive)
rm(Blank)

#-----------------------------------------------------------------------------

# Creates a Data Frame with the Ct.Mean Values
Ct.Mean = subset(dataShortened, select = c(Ct.Mean))

#-----------------------------------------------------------------------------

# Creates a line of fit using the Data From the logDataFrame
fit = lm(log16S_Copies ~ Ct.Mean, A)

#-----------------------------------------------------------------------------

# The 16S values are predicted using the Ct.Mean and the line of fit
new16S = as.data.frame(predict(fit, Ct.Mean))

#-----------------------------------------------------------------------------

# Remove the individual Data Frames
rm(Ct.Mean)

#-----------------------------------------------------------------------------

# A function that takes values and takes them to the power of themselves is created
# This is created because we need to "un-log" the values that we have
# A new Data Frame is created that with the new values and the column is named accordingly
power_function = function(x) {10^x}
new16S = power_function(new16S)
colnames(new16S) <- c('16S')

#-----------------------------------------------------------------------------

# DilutionsFactors
# Dilutions factors applied to the samples *10 | *50 | *2.46
# A function that takes values and multiplys them by 10 | 50| 2.46 is created
# A new Data Frame is created that with the new values and the column is named accordingly
dilutionsFactors = function(x) {(((x*10)*50)*2.46)}
DilutionsFactors = dilutionsFactors(new16S)
colnames(DilutionsFactors) <- c('Dilutions_Factors')

#-----------------------------------------------------------------------------

# Bind the Data | 16S | Dilution Data Frame into one Data Frame
Combined = cbind(dataShortened, new16S, DilutionsFactors)
# Get rid of the individual Data Frames
rm(new16S)
rm(dataShortened)
rm(DilutionsFactors)
rm(A)
rm(data)
rm(checkData)
rm(checkGraph)

#-----------------------------------------------------------------------------

# Create one combined Data Frame with the fecal data
Combined = cbind(Combined, Fecal_Weight)

#-----------------------------------------------------------------------------

# Divide the adjusted number of 16S copies by the fecal weight of the sample.
Combined = mutate(Combined, new = Dilutions_Factors / Fecal.Weight)

#-----------------------------------------------------------------------------

# For some reason. The "mutate" command does not like the certain column names so we set them
# to what we want after the "mutate" command has been completed
colnames(Combined) <- c('Well', 'Sample.Name','Target.Name','Ct','Ct.Mean','High.SD','16S','Dilutions_Factors','Sample.Name','Fecal_Weight','16S_Copies/gr_of_Feces')

#-----------------------------------------------------------------------------

# Allows the user to name the output .xlsx file
fileName = readline("What do you want to name the file?(Has to end in .xlsx) ")
write.xlsx(Combined, fileName)
rm(fileName)
#-----------------------------------------------------------------------------