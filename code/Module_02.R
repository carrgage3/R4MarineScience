# Housekeeping
## Inventory every active object currently residing in session RAM
objects()
## Purge global environment
rm(list = ls())
# Confirm that global session memory now completely vacant
objects()

## Try and set up a starter script for future data sets
library(tidyverse)
library(readxl)

# Practice Import A: Loading a standard comma-separated plain text file
benthic_cover <- read_csv(here::here("data/workshop1/reef_cover_log.csv"))

# Practice Import B: Parsing a tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(here::here("data/workshop1/acoustic_telemetry_stream.txt"))

# Practice Import C: Targeting a specific sheet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(here::here("data/workshop1/fish_catch_data.xlsx"), sheet = "Commercial_2026")

# Read in mangrove data
mangrove_data <- read_csv(file = here::here("data/workshop1/mangrove_survey_raw.csv"))

# Use args within read_csv to skip headers and declare missing flags
mangrove_data <-  read_csv(
  here::here("data/workshop1/mangrove_survey_raw.csv"),
  skip = 5, # Skip the first 5 lines of field notes
  na = c(".", "NA", "9999", "ND", "blank")) # Convert known text alts to true NA


# Tibbles vs. Legacy Tables ####

# Force a modern tibble to degreade into a legacy base R data frame structure
benthic_cover_df <-  as.data.frame(benthic_cover)

# Print the old-style dataframe structure to view
print(benthic_cover_df)
# And compare with tibble alternative
print(benthic_cover)

# Load palmerpenguins data into active memory
library(palmerpenguins)
data("penguins")

# Examine structure of dataset - always when loading a new dataset
glimpse(penguins) # tidyverse version (from dplyr package)
str(penguins) # base R version

# Generate exploratory summary matrix
summary(penguins)

# Isolating Attributes with select() ####

# Vertically slice specific morphometric variables by explicit name
morphology_metrics <- select(penguins, species, bill_length_mm, bill_depth_mm, body_mass_g)
glimpse(morphology_metrics)

# Retain a continuous block of attributes using the colon operator
spatial_block <-  select(penguins, species:island)

#Discard logistics tracking attributes while preserving everything else using the minus("-") sign
clean_scientific_fields <- select(penguins, -year)


# Sifting rows with filter() ####
# Isolate observations belonging to a single categorical target group
adelie_cohort <-  filter(penguins, species == "Adelie")
# Sift out individuals using continuous numerical boundary thresholds
#Preserves only large penguins whose mass exceeds 4500 grams
heavy_penguins <- filter(penguins, body_mass_g > 4500)

# Combine multiple conditional parameters across separate attributes
# Preserves records matching Gentoo penguins sampled explicitly on Biscoe Island
biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe")

# Sift records matching multiple targeting flags within an explicit set
sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))


# Ordering Sequences with Arrange() ####

# Sort penguins by ascending body mass (Default setting: Smallest mass first)
lightest_first <- arrange(penguins, (body_mass_g))

# Sort penguins in descending sequence using the desc() layout wrapper
heaviest_first <- arrange(penguins, desc(body_mass_g))

#Execute nested sorting criteria: Group by species, then sort by descending bill length
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))


# Introducing the Pipe (|>) ####

# Example
penguins_final <- penguins |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  filter(species == "Adelie")

# Calculate a new morphological ratio in the environment
penguin_ratios <-  penguins |>
  mutate(body_mass_kg = body_mass_g/1000, # COnvert grams to kilograms
         bill_ratio = bill_length_mm / bill_depth_mm # Bill ratio
         )

# View newly engineered variables to the far-right columns
glimpse(penguin_ratios)


# Data Aggregation and Ecological Summarisation ####

# Grouping our active memory penguins by species
grouped_penguins <- group_by(penguins, species)

# Table looks identical, but metadata notes 'Groups: species [3]'
print(grouped_penguins)

# Collapsing buckets into explicit summary metrics
species_mass_summary <- summarise(grouped_penguins,
                                  mean_mass_g = mean(body_mass_g)
                                  )

print(species_mass_summary)

# Overcoming the missing value trap using na.rm = TRUE (%>% = |>(?))
biological_signal <-  penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(), # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE), # Calculate mean ignoring missing cells
    sd_mass_g = sd(body_mass_g, na.rm = TRUE) # Standard deviation calculation
  )

print(biological_signal)


# Integrating sata grammar with visual disgnostics in qmd ####

# Pipe directly from aggregation to plotting with error bars
mass_compare_plot <-  penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = species, y = mean_mass, colour = island))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = mean_mass - sd_mass,
                    ymax = mean_mass + sd_mass),
                width = 0.2) +
  labs(title = "Mean Body Mass by Species and Island",
       subtitle = "Error bars represent standard deviation",
       y = "Mean Body Mass (g)",
       x = "Species") +
  theme_minimal()

mass_compare_plot

# Challenge 1: Boxplot
penguins %>%
  ggplot(aes(x = species, y = body_mass_g, fill = island)) +
           geom_boxplot(na.rm = TRUE)+
         labs(
           x = "species",
           y = "Mean Mass (g)",
           fill = "Island"
         )+
           theme_minimal()

# Challenge 2:
mass_summary <-  penguins|>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = sum(!is.na(body_mass_g)),
    .groups = "drop"
  )|>
  ggplot(aes(x = species, y = mean_mass, colour = island))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = mean_mass - sd_mass,
                    ymax = mean_mass + sd_mass),
                width = 0.2) +
  labs(title = "Mean Body Mass by Species and Island",
       subtitle = "Error bars represent standard deviation",
       y = "Mean Body Mass (g)",
       x = "Species") +
  theme_minimal()

mass_summary

# 1. Exporting collapsed summary table as a universal flat text file
write_csv(biological_signal, "outputs/penguin_species_mass_summary.csv")

# 2. Saving our cleaned morphological cohort table as a native R binary file
saveRDS(clean_scientific_fields, "outputs/clean_penguin_morphology_cohort.rds")

ggsave("outputs/mass_compare_plot.png",
       plot = mass_compare_plot,
       width = 120, height = 120,
       units = "mm", dpi = 300)
