# Task1: Using the iri dateset
## 1. Get a subset with STATE_CODE 6 and SHRP_ID starting with 050.
library(dplyr)

# Read csv. data
iri <- read.csv("C:\\Users\\23367\\Desktop\\R assignment2\\LTPP\\iri.csv")

# Filter the data that meets the conditions
iri_sub <- subset(iri, STATE_CODE == 6 & substr(SHRP_ID,1,3) == '050')

# Show
iri_sub


## 2. Obtain the summary statistics of IRI of each section: min, max, and mean.
# Calculate the summary statistics
iri_stat <- iri |>
  group_by(STATE_CODE, SHRP_ID) |>
  summarise(
    observation = n(),
    iri_min = min(IRI),
    iri_max = max(IRI),
    iri_mean = mean(IRI)
  ) |>
  ungroup()

# Show
iri_stat


## 3. Sort the summarized data by the averaged IRI in a descending order.
# Sort in a descending order
iri_sort <- iri_stat[order(iri_stat$iri_mean, decreasing = T),]

# Show
iri_sort


## 4.1. Generate a scatter plot for the averaged IRI against the time for a selected section.
# Generate a subset with STATE_CODE=6 and SHRP_ID=0504
sel <- subset(iri, STATE_CODE == 6 & SHRP_ID == '0504')

# Calculate the summary statistics grouped by VISIT_DATE
sel_stat <- sel |>
  group_by(VISIT_DATE) |>
  summarise(
    iri_mean = mean(IRI)
  ) |>
  ungroup()

# Split VISIT_DATE
library(tidyr) 
sel_stat <- separate(sel_stat, VISIT_DATE, c("DATE","TIME"), sep=",") |>
  mutate(
    DATE = as.Date(DATE,"%m/%d/%y")
  )
sel_stat <- sel_stat[order(sel_stat$DATE),]

# Show
sel_stat

# Generate a scatter plot
jpeg(filename = "assignment2_Task1.jpg",
    width = 640,
    height = 480)
plot(x=sel_stat$DATE,
     y=sel_stat$iri_mean, 
     col=adjustcolor("darkred", 1/2),
     xlab="time", ylab="mean IRI",
     main = "mean IRI-time Scatter Plot")
dev.off()


## 4.2. Give your interpretation of the plot.


# Task2: Using the CRSS datasets in 2017
## 1. Get the intersection of the datasets accident and person.
# Read csv. data
accident <- read.csv("C:\\Users\\23367\\Desktop\\R assignment2\\CRSS\\ACCIDENT.csv")  
person <- read.csv("C:\\Users\\23367\\Desktop\\R assignment2\\CRSS\\PERSON.csv")
vehicle <- read.csv("C:\\Users\\23367\\Desktop\\R assignment2\\CRSS\\VEHICLE.csv")

# Get the intersection
intersection <- 
  inner_join(
    x = accident,
    y = person
  )

# Show
intersection


## 2. Tabulate the total number of observations in each injury severity (INJ_SEV).
# Calculate the summary statistics grouped by INJ_SEV
injury_severity <- person |>
  group_by(INJ_SEV) |>
  summarise(
    obversation = n()
  ) |>
  ungroup()

# Show
injury_severity


## 3. Merge the accident dataset with the vehicle dataset, and report the dimension of your results and number of missing values in one variable of the right dataset.
library(naniar)
# Merge ACCIDENT with the VEHICLE
merge <- left_join(
  x = accident,
  y = vehicle
)

# Report the dimension of results & number of missing values
merge |> miss_var_summary()
