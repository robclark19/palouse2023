# import and clean 2022 data

# libraries
library("tidyverse")
library("readxl") #https://readxl.tidyverse.org/

# Import each sheet from 2022 surveys on vetch 
# As of 4 20 2023, this lacks 33 transects worth of aphid count data
# Transects should be completed in May 2023 and will be added as a separate csv to merge
raw_2022_1 <- read_xlsx("./Data/Originals/AphidCounts_2022.xlsx", sheet=1) # most up to date counts
raw_2022_2 <- read_xlsx("./Data/Originals/AphidCounts_2022.xlsx", sheet=2) # has pemv testing data
raw_2022_3 <- read_xlsx("./Data/Originals/AphidCounts_2022.xlsx", sheet=3) # has wider data set but missing some of the counts completed by students

# remaining counts for the last 33 samples


