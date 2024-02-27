#Testing staRdom with the provided example data
#https://cran.r-project.org/web//packages/staRdom/vignettes/PARAFAC_analysis_of_EEM.html
#February 27, 2024
# Sydney Shelton

library(tidyverse)
library(staRdom)

#First, read in EEMs data
folder <- system.file("extdata/EEMs/", package = "staRdom") # folder containing example EEMs
eem_list <- eem_read(folder, recursive = TRUE, import_function = eem_csv) # in case you use your own data, 
#just replace folder by a path. e.g. "C:/folder/another folder" and change import_function according to instrument. - for us this would be "aqualog"
#Whether excitation/emission wavelengths are in the columns can matter here - for us excitation is in column

#Take a look at the EEMs
eem_overview_plot(eem_list, spp=9, contour = TRUE)

#Load in absorbance
#Example absorbane path
absorbance_path = system.file("extdata/absorbance", package = "staRdom") # load example data, set a path without using system.
#file to use your own data e.g. "C:/folder/another folder"
absorbance <- absorbance_read(absorbance_path) # load csv or txt tables in folder


#Import a metadata table
metatable <- system.file("extdata/metatable_dreem.csv",package = "staRdom") # path to example data, can be replaced by a path to your own data
meta <- read.table(metatable, header = TRUE, sep = ",", dec = ".", row.names = 1) # load data

#Or make one
eem_metatemplate(eem_list, absorbance) %>%
  write.csv(file="metatable.csv", row.names = FALSE)

#Then check that the EEMs, absorbance, and metatable all match
problem <- eem_checkdata(eem_list,absorbance,meta,metacolumns = c("dilution"),error=FALSE)




#Load all the absorbance data into one dataframe - not in correct format for metatable
# then call in spectra
rawabs <- sapply(filenames, FUN = read.delim, simplify = FALSE, USE.NAMES = TRUE)

# Need to clean up the names in the list
# Change filenames to remove the canned part we don't want
# then make these object names in rawabs
filenames <- sub(" .*", "", basename(filenames))
names(rawabs) <- filenames
# Now, compile all of these columns into one dataframe
# Remove all columns except for the absorbance
rawabs <- lapply(rawabs, function(x) {select(x, -c("I1", "I1.dark", "R1", "R1dark", "XCorrect", "I1c", "R1c", "I1c.R1c", "Percent.T"))})
#Make the Absorbance column name the sample name and remove extra columns
for(i in unique(1:length(filenames))){
  colnames <- c("Wavelength", filenames[i])
  colnames(rawabs[[i]]) <- colnames
}
#Combine into one dataframe, joining by Wavelength column
absorbance <- reduce(rawabs, left_join, by = "Wavelength")
#Remove the first two rows, which just lists units and equations
absorbance <- absorbance[-c(1:2),]
#And make absorbance numeric
absorbance[,] <- lapply(absorbance[,], as.numeric)
#Change name of Wavelength so "W" is lowercase
absorbance <- absorbance %>% rename_at('Wavelength', ~'wavelength')
#Arrange so it's listed by wavelength from low to high
absorbance <- arrange(absorbance, wavelength)


write.csv(absorbance, file.path(fp.lowa,"losresabsorbancecompliation.csv"), row.names = F)