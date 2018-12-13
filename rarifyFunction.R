# This script rarifies bug counts to 110 from VDEQ's EDAS database
# R version 3.5.1
# Adapted from code by Lou Reynolds and Leah Ettema (US EPA Region III)

library(tidyverse)
library(lazyeval)


# Read in raw bug data
#bugdat <- readxl::read_excel("VSCI_TAXA_FAMILY_GENUS.xlsx", "TaxaGenus")

# make long form for function cutting out unnecessary columns for now
#bugdatlong <- select(bugdat, -c(StationID,CollDate,Target_Count,RepNum)) %>% # get rid of unnecessary columns
#  gather(FinalID,Count,-BenSampID) %>% # long format
#  filter(!is.na(Count)) # lose redundant NA counts

# Function arguments for testing
#UID <- "BenSampID"
#finalID <- 'FinalID'
#count <- 'Count'


rarify <- function( # inputs necessary to run function
                   bugdatlong, # TABLE: long data.frame, tbl, or tbl_df of bug counts organized by unique sample ID
                   #stationName, # COLUMN NAME: name of station
                   #date, # COLUMN NAME: sample date
                   #targetCount, # 
                   UID, # COLUMN NAME: unique sample identifier for each separate sampling event
                   finalID, # COLUMN NAME: Lowest taxonomic level identified to
                   count){ # COLUMN NAME: number of occurrences of each lowest taxonomic level  
  # Total up bug counts to see what needs rarification
  bugtotals <- bugdatlong %>% 
    group_by_(UID) %>% 
    summarise_(bugtotal = interp(~sum(v), v = as.name(count))) %>% # must use purrr::interp() bc standard evaluation in dplyr
    select(UID, bugtotal) %>% #bugtotal is total
    left_join(bugdatlong,by=UID)  
  
  # Separate UID's that are already rarified from ones that need to be rarified
  rarified <- filter(bugtotals, bugtotal <= 110) %>%
    arrange_(UID) %>%
    mutate(SampleCount=bugtotal, Count = .[[4]]) %>%
    select(UID, finalID, SampleCount, Count)
  rarifyMe <- filter(bugtotals, bugtotal > 110) %>%
    arrange_(UID) %>%
    mutate_(count=count) # make generic count variable to make loop work faster
  
  # Rarification Part 1
  #this loop copies and pastes a new row totn-1 number of times, making a unique row for each individual taxa
  if(nrow(rarifyMe)>0){
    for(s in 1:nrow(rarifyMe)){
      if(rarifyMe$count[s] > 1){
        reps=rarifyMe$count[s]-1 #number of individuals per FinalID -1
        for (t in 1:reps){
          rarifyMe <- rbind(rarifyMe, rarifyMe[s,]) #paste the row "reps" number of times
        }
      }
    }
    # reorganize
    final <- rarifyMe %>%
      group_by_(UID) %>%
      select_(UID,finalID) %>% #Count is now meaningless (should be 1 for each row) so remove it
      sample_n(110, replace = FALSE) %>% #randomly selects 110 rows (individuals) for each BenSampID
      group_by_(UID, finalID) %>%
      summarise(Count=n()) %>%
      mutate(SampleCount= sum(Count)) %>% #These BenSampID's were subsampled
      bind_rows(rarified)
  }else{
    final <- rarified
  }
  
  return(final)
}
 

#test1 <- rarify(bugdatlong, "BenSampID",'FinalID','Count')
#test2 <- rarify(bugdatlong, "BenSampID",'FinalID','Count')
#test3 <- rarify(bugdatlong, "BenSampID",'FinalID','Count')

  
 