# Social Network analysis of authors who publish research papers together 

# Takes publication records downloaded from Web of Science and produces count matrix of times authors 
# have published together, which can be exported for use in Gephi, or other SNA packages

rm(list=ls())

install.packages(c("dplyr", "tidyr", "tm", "curl", "stringr"))
library(dplyr)
library(tidyr)
library(tm)
library(stringr)
library(curl)

# import publication data
Publications <- read.csv(curl("https://raw.githubusercontent.com/sergiomarrocoli/Publication_author_SNA/master/ExampleData.csv"), stringsAsFactors = F)

# Remove uppercase, whitespace etc to remove formatting inconsistencies
AuthorsList <- distinct(Publications[, c(1,3)])           # select useful columns (Author and Title)
AuthorsList$Author <- tolower(str_replace_all(AuthorsList$Author,"[^[:graph:]]", ""))    # remove graphical characters and make lowercase
AuthorsList$Author <- gsub(" ", "", AuthorsList$Author, fixed = TRUE)   # remove white space
AuthorsList$Author <- gsub("\\.*", "", AuthorsList$Author) # remove final period, which can be missing sometimes
AuthorsList$Author <- gsub("'|-", "", AuthorsList$Author)

# Create list of authors
# Inconsistencies in naming authors i.e. one or two initials, full first etc, so deal with those. 
# Care required as this could merge several authors under a single name
Authors <- sort(unique(unlist(strsplit(AuthorsList$Author, split = ";"))))
Authors <- unique(sub(",([a-z]).*", ",\\1", Authors))

# for loop that searches each paper for authors and puts them into a table
for (i in Authors){
  AuthorsList[i] <- (grepl(i, AuthorsList$Author)) * 1
}

# Make empty association matrix 
AssociationMatrix <- matrix(data = 0, nrow =  length(Authors), ncol =  length(Authors))
Authors <- gsub("," , ".", Authors)
colnames(AssociationMatrix) <- Authors
rownames(AssociationMatrix) <- Authors

# use nested for loop to populate matrix
AuthorsList <- AuthorsList[-c(1:2)]
for (i in 1:ncol(AssociationMatrix)){
  for(j in 1:ncol(AssociationMatrix)){
    AssociationMatrix[i,j] <- sum(AuthorsList[i] * AuthorsList[j]) 
  }
}

# Create create table of authors and number of papers authored
PaperCount <- data.frame(Author = Authors, Count = 0)
for (i in 1:nrow(PaperCount)){
  PaperCount[i, 2] <- max(AssociationMatrix[i, ])
}

# output tables with ; seperator for use in Gephi (or other SNA packages). see Github repository for example figure
write.table(AssociationMatrix, "Filename.csv", sep = ";", quote = F, row.names = T)
write.table(PaperCount, "Filename.csv", sep = ";", quote = F, row.names = F)

# save workspace
save.image("SaveDirectory.RData")

