library(knitr)
library(readr)
library(dplyr)
library(ggplot2)

#Storyboard Narrative: 
# 1. Start with introducing the SDG of interest and some background on the issue/area
# 2. Reference some sources and talk about the conflict between economic prosperity
#    and the reduction of inequity and developing clean energy sources
# 3. Introduce data on the decline of Coal production (Coal production by year, 
#    maybe have production by surface vs underground mines vs total as a dodged
#    bar chart.)
# 4. Next to Coal Production, include the data on mine employment over the years 
#    (same data format as the above data, include a kabled table for both)
# 5. (Explore mine dataset...) NOTES: Maybe try to summarize the data in its 
#    current format and create charts based on the summarized tables. Also we 
#    might want to include data on the the total number of mines in the state, 
#    Just so we can understand the scale of the mining operation in KY (maybe 
#    we can have the contry wide data and make a visualization for how coal
#    production is concentrated in Eastern KY/WV.) 

CoalProd <- read.csv("../data/Coal_Prod.csv")
knitr::kable(head(CoalProd))
View(CoalProd)

Mines <- read.csv("../data/KY_Mines.csv")
knitr::kable(head(Mines))
View(Mines)

Employment <- read.csv("../data/Avg_Employees.csv")
knitr::kable(head(Employment))
View(Employment)

## Mines Data Exploration

Mines$Status %>% unique

Mines %>% head

ggplot(Mines, 
       aes(x=Status))+
  geom_bar()

ggplot(Mines,
       aes(x=Type))+
  geom_bar()

################################################################################


### Looking at the bulk download (in pipe delim)

# Mines <- read_delim("Mines.txt", 
#                  delim = "|",
#                  locale = locale(encoding = "latin1"),
#                  trim_ws = TRUE)
# Mines$STATE %>% unique


addresses <- read_delim("AddressOfRecord.txt", 
                 delim = ",",
                 locale = locale(encoding = "latin1"),
                 trim_ws = TRUE)

addresses$STATE %>% unique
addresses$PRIMARY_SIC_CD %>% unique

View(addresses)
addresses$STATE %>% unique()

getwd()
### After this, we now have a file containing all of the mines in the data set  
### including their state and the type of material that they produce
################################################################################
library(ggplot2)
library(dplyr)


#### AN ATTEMPT AT QUANTIFYING ALL THIS CATEGORICAL DATA
MinesSum <- addresses %>%
  group_by(STATE, PRIMARY_SIC_CD, MINE_STATUS) %>%
  summarise(count = n(), .groups = "drop" ) %>% 
  filter((PRIMARY_SIC_CD %in% c("Coal (Bituminous)",
                               "Coal (Lignite)", 
                               "Coal (Anthracite)")))
MinesSum %>% View 
# This shows a data set for coal mines grouped and organized by state and status  
# Has a count column that measures number of mines for unique values by state and
# status.


### Working with Dates =========================================================
# mdy(), converts the date from a text format to a date foramt
# year(), grabs the year from the mdy formatted date
library(lubridate)

addresses <- addresses %>% 
  mutate(MINE_STATUS_DT = mdy(MINE_STATUS_DT)) %>% 
  mutate(mine_year = year(MINE_STATUS_DT)) %>%
  filter((PRIMARY_SIC_CD %in% c("Coal (Bituminous)",
                               "Coal (Lignite)", 
                               "Coal (Anthracite)")))

# The above code converts the text date foramt into a useable data format and adds 
# the column with just the year for each mine.

addresses %>% View
addresses$mine_year %>% range()

### plotting Dates===============================================================

library(ggplot2)

addresses_timeline <- addresses %>% 
  group_by(mine_year) %>% 
  summarise(count = n(), .groups = "drop")

# the above code groups data by year and counts the number of mines per year.

ggplot(addresses_timeline,
       aes(x=mine_year,
           y=count))+
  geom_area(alpha = 0.5,
            fill = "skyblue")+
  labs(title = "Timeline of Coal Mining Activity",
       y = "Active Mines",
       x = " ")+
  theme_classic()

### Filters

library(dplyr)
library(ggplot2)

### Filters to Regions =========================================================
# We define categorized vectors containing the data we want to classify, use
# mutate() and case_when() to categorize the data as we need.

Appalachia <- c("Alabama", "Kentucky", "Tennessee", "West Virginia", "North Carolina", "Virginia")
SE         <- c("Florida", "South Carolina", "Louisiana", "Mississippi", "Georgia", "Arkansas")
NE         <- c("Maryland", "Pennsylvania", "New Hampshire", "Vermont", "New York", "New Jersey",
                "Delaware", "Maine", "Rhode Island", "Connecticut", "Massachusetts")
SW         <- c("Texas", "Arizona", "New Mexico", "California", "Nevada", "Utah", "Oklahoma")
Midwest    <- c("Ohio", "Indiana", "Michigan", "Illinois", "Minnesota", "Wisconsin",
                "Iowa", "Kansas", "North Dakota", "South Dakota")
NW         <- c("Idaho", "Montana", "Wyoming", "Colorado", "Oregon", "Washington", "Alaska")

MinesSumRegion <- MinesSum %>%
  filter(PRIMARY_SIC_CD %in% c("Coal (Bituminous)", "Coal (Lignite)", "Coal (Anthracite)")) %>%
  mutate(region = case_when(
    STATE %in% Appalachia ~ "Appalachia",
    STATE %in% SE         ~ "Southeast",
    STATE %in% NE         ~ "Northeast",
    STATE %in% SW         ~ "Southwest",
    STATE %in% Midwest    ~ "Midwest",
    STATE %in% NW         ~ "Northwest",
    TRUE                  ~ "Other"))

MinesSumRegion <- MinesSumRegion %>% rename(Status = MINE_STATUS)

# case_when allows you to assign already created vector classification based on 
# data in another column. This function adds a column and assigns it a value 
# (Appalachia, SE, NE, etc.) based on its "STATE" value (which we have defined in
# our grouping vectors). If the data in the grouping vectors match the STATE value,
# the STATE row gets assigned that vector value.

### Plot Coal mines by Region
ggplot(MinesSumRegion,
       aes(x = region,
           y = count,
           fill = region)) +
  geom_col(position = "dodge") +
  labs(title = "Coal Mines by Region",
       x     = "Region",
       y     = "Number of Mines") +
  theme_classic()+
  theme(legend.position = 'none')

### Plot Coal mines by Region and Status
ggplot(MinesSum1,
       aes(x = region,
           y = count,
           fill = Status)) +
  geom_col(position = "stack") +
  labs(title = "Coal Mines by Region and Status",
    x     = "Region",
    y     = "Number of Mines") +
  theme_classic()+
  theme(legend.position = 'none')



### Timeline of Coal Mining Activity ===========================================
# Eric Ezell
library(tidyr)
library(plotly)

# Make a new version that simplifies the MINE_STATUS column
mines <- addresses %>%
  mutate(status = case_when(MINE_STATUS %in% c('Active') ~ 'active',
                            MINE_STATUS %in% c('Abandoned', 'Abandoned and Sealed') ~ 'abandoned',
                            .default = 'other'))

# This renamed and reclassified our column names to be simpler for analzing
# check it out
mines$status %>% table

# To do what you need to do, I think you need to iterate through each year
# that means FOR LOOP

# Figure out range of years
mines$mine_year %>% range
# Make a vector of years
(years <- 1925:2026)

yi <- 1973 # dummy year to use in coding up the for loop
mr <- data.frame() # stage an empty dataframe to take results

for(yi in years){ # loop through each year
  
  # Get all mines with updates PRIOR to year yi
  (mini <- mines %>% filter(as.numeric(mine_year) <= yi))
  # Get all mines with updates AFTER year yi
  (post_mini <- mines %>% filter(as.numeric(mine_year) > yi))
  
  # Get all mines who have come on line prior to this year
  became_active <- mini %>% filter(mine_year <= yi, status == 'active')
  
  # Get all mines currently active that WILL BE ABANDONED in future years
  will_be_abandoned <- post_mini %>% filter(status %in% c('abandoned', 'other'))
  
  # Get all mines that have already been abandoned
  abandoned <- mini %>% filter(mine_year <= yi, status == 'abandoned')
  
  other <- mini %>% filter(mine_year <= yi, status == 'other')
  
  # Now save sample sizes
  mri <-
    data.frame(year = yi,
               active = (became_active %>% nrow) + (will_be_abandoned %>% nrow),
               abandoned = abandoned %>% nrow,
               other = other %>% nrow)
  
  # Print result for this year to help with debugging
  print(mri)
  
  # Add to results dataframe
  mr <- rbind(mr, mri)
} # end for loop

# Check out result
mr

# Pivot to make it plot-able
mr_long <-
  mr %>%
  pivot_longer(cols=active:other,
               names_to='status',
               values_to='n')

# Plot it
ggplotly(ggplot(mr_long,
       aes(x=year,
           y=n,
           color=status)) +
  geom_path(size = 1)+
  labs(title = "Timeline of Mines Seperated by their Status",
       y = "Mines",
       x = "Year")+
    annotate(geom='text',
             x=1950, y=1250, 
             label='Active mines begin\n to decline around 1975 ->')+
  theme_bw())

