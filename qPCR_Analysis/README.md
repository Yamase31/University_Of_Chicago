# qPCR Analysis Protocol
# need to change this

Tuesday, November 29th, 2016

This program will take outputted spreadsheets from qPCRs and manipulates them in the following ways:

Displays a graph with all of the data from the standard curve
Displays all of the standard curve data points with a high standard deviation and allows you to remove values of your choosing, then displays the new graph without the removed points
Sets linear model with the new graph
Displays and Removes Controls
16S Values for remaining data is predicted using the linear model from the standard curve
Dilutions factors applied to the samples *10 | *50 | *2.46
Adjusted number of 16S copies divided by the fecal weight of the samples are calculated
New spreadsheet outputted as a .xlsx file (excel file) and is saved to the folder where the program is located

----------------------------------------------------------------------------------------------------------------------------------
If you have not already, install R Studio. 

Save the excel file from the qPCR output as a comma separated values (csv) file. You can do this from excel. Click “save as” and then select (csv).
Save the Fecal Weights file as a (csv) file
Save both of those files into the same folder that is easily accessible.
Open the RStudio file named “qPCR16S”  file.
Lines 21 and 24 needed to be changed with the route of where your files are stored. An easy way to figure this out is to open the folder where your file is stored and look at the bottom bar of your finder window. You only need to include everything after your username (the house icon). The last part of the line is the name of your file.
Check the outputted files from the qPCT and check the following:
Controls must be names exactly as follows "Positive" | "Negative" | "Blank"
High.SD columns must be labeled as either Y or N
Check for typos. These will confuse the program and will produce error messages.
Click source.
Follow the on screen instructions and enter information when prompted.
The new spreadsheet will be saved in the same folder as the RStudio program is located
Save the new spreadsheet into a folder with your raw data and your raw Fecal Data. You will need all 3 files later.
 
 
These R packages should be pre-installed: 
If an error messages comes up saying that a package cannot be found. Proceed as follows
To install R packages, delete the “#” symbol before lines 4-10. Click source. The program should run without errors. Put the “#” back in front of lines 4, 6-10.

R Packages:
Tidyr
Plyr
Dplyr
Reshape2
Ggplot2
Hexbin
Gcookbook
Lattice
Xlsx (Need to have updated Java for .xlsx)
