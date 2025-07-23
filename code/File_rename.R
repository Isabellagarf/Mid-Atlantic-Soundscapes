library(tidyverse)
library(tcltk)

# Select working directory with files to rename
setwd(tk_choose.dir(caption="Users/isabella.garfield/Documents/MA-RI-renamingtest"))


# get list of files in folder
old_filename <- list.files()

# search for pattern in old file list 
# and generate new list with desired replacement
new_filename <- gsub(pattern = "_",
                     replacement = "",
                     old_filename)

# double check everything looks correct
head(old_filename)
head(new_filename)

# rename files
file.rename(from = old_filename, to = new_filename)
