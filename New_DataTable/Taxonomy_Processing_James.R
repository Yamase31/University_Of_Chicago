# This program will take data from a excel spreadsheet 
# in a .txt file and output the names of the targeted bacteria 
# along with their values. It will check to make sure all of the values add to 1. 
# This program will adapt to whatever data is inputed.
# This program will also tell you the names of bacteria that are observed with a value
# of over 1 percent.
# I also implemented the change where the family column does not have the "f__" anymore.

# Load the libraries
###setwd("/Users/pedrobelda-ferre/Documents/Sequencing/Argonne_July_2016/Australia_Samples/core_analysis_1000/taxa_plots/")

library("tidyr")
library("dplyr")
#-----------------------------------------------------------------------------

# Load data
# You will have to change this route to match where your file is stored

# Change the name of the input and output file

#input <- c("Group_otu_table_sorted_L5.txt")

raw_data <- read.table(file = "~/Desktop/Work/Project#1_Spreadsheet/Group_otu_table_sorted_L5.txt", 
                       comment.char = "", skip = 1, sep = "\t", header = T)
#-----------------------------------------------------------------------------

# Separate the first column into the different taxonomy levels
tax_data <- separate(data = raw_data, col = "X.OTU.ID" , 
  into = c("Root", "Phylum", "Class", "Order", "Family"), 
  sep = ";", extra = "merge")
#-----------------------------------------------------------------------------

#Actinobacteria
# Subset rows for a specific taxa (Actinobacteria)
Actinobacteria = filter(tax_data, Phylum == "p__Actinobacteria")
#-----------------------------------------------------------------------------

#Proteobacteria
# Subset rows for a specific taxa (Enterobacteriaceae)
Enterobacteriaceae = filter(tax_data, Phylum == "p__Proteobacteria" & 
                      Family == "f__Enterobacteriaceae")
# Subset rows for a specific taxa (Pasteurellaceae)
Pasteurellaceae = filter(tax_data, Phylum == "p__Proteobacteria" &
                   Family == "f__Pasteurellaceae")
# Subset rows for a specific taxa (Proteobacteria_Other)
Proteobacteria_Other = filter(tax_data, Phylum == "p__Proteobacteria" & 
                        Family != "f__Pasteurellaceae" & 
                        Family != "f__Enterobacteriaceae")
#-----------------------------------------------------------------------------

#Firmicutes
#Bacilli
# Subset rows for a specific taxa (Streptococcaceae)
Streptococcaceae = filter(tax_data, Phylum == "p__Firmicutes" & 
                    Family == "f__Streptococcaceae")

# Subset rows for a specific taxa (Lactobacillaceae)
Lactobacillaceae = filter(tax_data, Phylum == "p__Firmicutes" & 
                    Family == "f__Lactobacillaceae")

# Subset rows for a specific taxa (Enterococcaceae)
Enterococcaceae = filter(tax_data, Phylum == "p__Firmicutes" & 
                   Family == "f__Enterococcaceae")

# Subset rows for a specific taxa (Bacilli_Other)
Bacilli_Other = filter(tax_data, Phylum == "p__Firmicutes" & 
                 Class == "c__Bacilli" & 
                 Family != "f__Enterococcaceae" &
                 Family != "f__Lactobacillaceae" &
                 Family != "f__Streptococcaceae")
#-----------------------------------------------------------------------------

#Clostridia
# Subset rows for a specific taxa (Ruminococcaceae)
Ruminococcaceae = filter(tax_data, Phylum == "p__Firmicutes" & 
                   Family == "f__Ruminococcaceae")

# Subset rows for a specific taxa (Peptostreptococcaceae)
Peptostreptococcaceae = filter(tax_data, Phylum == "p__Firmicutes" & 
                         Family == "f__Peptostreptococcaceae")

# Subset rows for a specific taxa (Clostridiaceae_1)
Clostridiaceae_1 = filter(tax_data, Phylum == "p__Firmicutes" & 
                    Family == "f__Clostridiaceae_1")

# Subset rows for a specific taxa (Lachnospiraceae)
Lachnospiraceae = filter(tax_data, Phylum == "p__Firmicutes" & 
                   Family == "f__Lachnospiraceae")

# Subset rows for a specific taxa (Other_Clostridia)
Other_Clostridia = filter(tax_data, Phylum == "p__Firmicutes" & 
                    Class == "c__Clostridia" & 
                    Family != "f__Ruminococcaceae" &
                    Family != "f__Clostridiaceae_1" &
                    Family != "f__Peptostreptococcaceae" &
                    Family != "f__Lachnospiraceae")
#-----------------------------------------------------------------------------

#Other
# Subset rows for a specific taxa (Veillonellaceae)
Veillonellaceae = filter(tax_data, Phylum == "p__Firmicutes" & 
                   Family == "f__Veillonellaceae")

# Subset rows for a specific taxa (Erysipelotrichaceae)
Erysipelotrichaceae = filter(tax_data, Phylum == "p__Firmicutes" & 
                       Family == "f__Erysipelotrichaceae")

# Subset rows for a specific taxa (Unclassified_Firmicutes)
Unclassified_Firmicutes = filter(tax_data, Phylum == "p__Firmicutes", Class !="c__Clostridia" & Class != "c__Bacilli" & 
            Family != "f__Erysipelotrichaceae" & Family != "f__Veillonellaceae")
#-----------------------------------------------------------------------------

#Bacteroidetes
# Subset rows for a specific taxa (Porphyromonadaceae)
Porphyromonadaceae = filter(tax_data, Phylum == "p__Bacteroidetes" & 
                      Family == "f__Porphyromonadaceae")

# Subset rows for a specific taxa (Bacteroidaceae)
Bacteroidaceae = filter(tax_data, Phylum == "p__Bacteroidetes" & 
                  Family == "f__Bacteroidaceae")

# Subset rows for a specific taxa (Bacteroidetes_Other/Unclassifed)
Bacteroidetes_Other_Unclassifed = filter(tax_data, Phylum == "p__Bacteroidetes" & 
                                   Family != "f__Porphyromonadaceae" &
                                   Family != "f__Bacteroidaceae")
#-----------------------------------------------------------------------------

#Verrucomicrobia
# Subset rows for a specific taxa (Verrucomicrobia)
Verrucomicrobia = filter(tax_data, Phylum == "p__Verrucomicrobia")
#-----------------------------------------------------------------------------

#Other/Unclassified
# Subset rows for a specific taxa (Other/Unclassified)
Other_Unclassified = filter(tax_data, 
                    Phylum != "p__Actinobacteria" & 
                      Phylum != "p__Proteobacteria" & 
                      Phylum != "p__Firmicutes" &
                      Phylum != "p__Bacteroidetes" &
                      Phylum != "p__Verrucomicrobia")
#-----------------------------------------------------------------------------

# The data from each individual Data Frame is summed and then added to a new Data Frame
standardTable <- as.data.frame(colSums(Actinobacteria[6:length(colnames(tax_data))]))
standardTable <- cbind(standardTable, as.data.frame(colSums(Verrucomicrobia[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Streptococcaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Lactobacillaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Enterococcaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Bacilli_Other[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Ruminococcaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Peptostreptococcaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Clostridiaceae_1[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Lachnospiraceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Other_Clostridia[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Erysipelotrichaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Veillonellaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Unclassified_Firmicutes[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Bacteroidetes_Other_Unclassifed[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Porphyromonadaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Bacteroidaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Proteobacteria_Other[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Enterobacteriaceae[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Other_Unclassified[6:length(colnames(tax_data))])))
standardTable <- cbind(standardTable, as.data.frame(colSums(Pasteurellaceae[6:length(colnames(tax_data))])))

# Verrucomicrobia_Other is added to the DataFrame if its value is greater than 1%


#-----------------------------------------------------------------------------

# The columns are appropriately named
colnames(standardTable) <- c("Actinobacteria",
                          "Verucomicrobia",
                          "Streptococcaceae",
                          "Lactobacillaceae",
                          "Enterococcaceae",
                          "Bacilli_Other",
                          "Ruminococcaceae",
                          "Peptostreptococcaceae",
                          "Clostridiaceae_1",
                          "Lachnospiraceae",
                          "Other_Clostridia",
                          "Erysipelotrichaceae",
                          "Veillonellaceae",
                          "Unclassified_Firmicutes",
                          "Bacteroidetes_Other_Unclassified",
                          "Porphyromonadaceae",
                          "Bacteroidaceae",
                          "Proteobacteria_Other",
                          "Enterobacteriaceae",
                          "Other/Unclassified",
                          "Pasteurellaceae")
#-----------------------------------------------------------------------------

# Each row is summed and a new columed called "Total" is added to the Data Frame.
# The sum of the row is added to the "Total" column.
# This is done to make sure all of the data is correct mathematically by checking
# if it sums to 1.
standardTable$Total = rowSums(standardTable)
#-----------------------------------------------------------------------------

# This creates a new Data Frame from the tax_data that only takes every
# sub category of bacteria and its value.
manyBacteriaTable <- tax_data[6:length(colnames(tax_data))]

# This adds the row names from the tax_data Data Frame.
rownames(manyBacteriaTable) <- tax_data[,5]

# This adds the column names to the manyBacteriaTable.
colnames(manyBacteriaTable)[] = "Family"

# This deletes the "f__" from the Family column making the
# column easier to read
rownames(manyBacteriaTable) = gsub("f__", "", rownames(manyBacteriaTable))

# This creates a new Data Frame with data that is over 1%
over1Percent = manyBacteriaTable[apply(manyBacteriaTable, 1, function(x) any(x > 0.01)),]

# Prints just the row names of the selection Data Frame
print(rownames(over1Percent))

### If the total sum of any row is less or bigger than 1, print a warning message
number = sum(standardTable$Total) 
number = round(number,digits=10)
if (number == dim(standardTable)[1]) {
  print('It all adds to 1!')
} else {
  print('It does not all add to 1. ERRRRRROOOOOOOOORRRRRRRRRR!!!')
}


write.csv(standardTable, file = "TOPRISM_table_mc1650_sorted_L5.txt")