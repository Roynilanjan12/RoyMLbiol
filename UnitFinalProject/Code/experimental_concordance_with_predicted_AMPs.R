# Load necessary libraries
library(tidyverse)
## assuming that the working directory is set as the project directory
# Read in sample metadata CSV file 
sample_metadata <- read_csv("./Data/SraRunTable.csv")

# Inspect the first and last few rows of the sample metadata
head(sample_metadata)
tail(sample_metadata)

# Read in the merged TPM gene expression data
gene_expression_tpm <- read.delim("./Data/rsem.merged.gene_tpm.tsv", header = TRUE)

# Inspect the first and last few rows of the TPM data
head(gene_expression_tpm)
tail(gene_expression_tpm)
dim(gene_expression_tpm)

# Remove the 'transcript_id.s.' column as it is not needed
gene_expression_tpm$transcript_id.s. <- NULL

# The following line is commented out, but shows how to filter for FBte:
# gene_expression_tpm <- gene_expression_tpm[grep("FBte", gene_expression_tpm$gene_id), ]

# Inspect the data again after removing the column
head(gene_expression_tpm)
tail(gene_expression_tpm)
dim(gene_expression_tpm)

# Extract the gene identifier from the 'gene_id' column by removing everything after the underscore
gene_expression_tpm$gene_id <- sub("_.*", "", gene_expression_tpm$gene_id)

# Extract the numeric 'Hour' from 'Sample Name' in the metadata
sample_metadata$hour <- as.numeric(str_extract(sample_metadata$`Sample Name`, "^\\d+"))

# Remove rows with missing hour values (if any)
sample_metadata <- na.omit(sample_metadata)

# Subset 'gene_expression_tpm' based on the 'Run' column present in sample_metadata
selected_columns <- c("gene_id", sample_metadata$Run)
gene_expression_tpm <- gene_expression_tpm %>%
  select(all_of(selected_columns))

dim(gene_expression_tpm)
dim(sample_metadata)

# Remove the prefix (e.g., "0h ", "12h ") from 'Sample Name' to clean up sample names
sample_metadata$`Sample Name` <- sub("^\\d+h ", "", sample_metadata$`Sample Name`)


# Reshape the wide TPM data to long format for easier analysis and plotting
gene_expression_long <- gene_expression_tpm %>%
  pivot_longer(cols = -gene_id, names_to = "Run", values_to = "Expression")

# Join the long-format TPM data with the sample metadata based on the 'Run' column
gene_expression_combined <- gene_expression_long %>%
  left_join(sample_metadata, by = "Run")

# Inspect the combined data
head(gene_expression_combined)
dim(gene_expression_combined)

# Rename the columns in the combined data for clarity
colnames(gene_expression_combined) <- c("TE","Run", "Expression", "Treatment","Sex", "Strain","Tissue","Hour")

# Check the first and last few rows again after renaming
head(gene_expression_combined)
tail(gene_expression_combined)

# Examine the structure of the combined data frame
str(gene_expression_combined)

# Inspect a few unique TE identifiers for sanity check
head(unique(gene_expression_combined$TE))

# Load dplyr (already loaded via tidyverse, but just to be explicit)
library(dplyr)

###############################################################################
# Define the list of predicted antimicrobial peptides (predicted_amps) to include
###############################################################################
predicted_amps <- c(
  "CG17108", "CG34166", "CG34215", "CG34331", "CG43071",
  "CG43109", "CG43175", "CG11413", "CG43920", "CG32284",
  "CG32276", "CG16713"
)

###############################################################################
# Filter the data for the selected predicted_amps
###############################################################################
predicted_amps <- gene_expression_combined %>%
  filter(TE %in% predicted_amps)

# Convert 'Hour' and 'TE' columns to factors for consistent ordering in plots
predicted_amps$Hour <- factor(predicted_amps$Hour)
predicted_amps$TE <- factor(predicted_amps$TE, levels = c(
  "CG17108", "CG34166", "CG34215", "CG34331", "CG43071",
  "CG43109", "CG43175", "CG11413", "CG43920", "CG32284",
  "CG32276", "CG16713"
))

# Define the desired order of Treatment levels
treatment_levels <- c(
  "Unchallenged",
  "clean prick",
  "M. luteus",
  "S. aureus",
  "E. faecalis live",
  "E. faecalis heat-killed",
  "Ecc15",
  "P. rettgeri live",
  "S. marcescens Type",
  "E. coli",
  "P. rettgeri heat-killed",
  "P. entomophila",
  "S. marcescens Db11",
  "P. sneebia"
)

# Reorder the Treatment factor levels
predicted_amps$Treatment <- factor(predicted_amps$Treatment, levels = treatment_levels)

# Map each Treatment to a broader 'TreatmentGroup' category, useful for coloring
treatment_groups <- c(
  "Unchallenged" = "Unchallenged",
  "clean prick" = "clean prick",
  "M. luteus" = "Gram-positive bacteria",
  "S. aureus" = "Gram-positive bacteria",
  "E. faecalis live" = "Gram-positive bacteria",
  "E. faecalis heat-killed" = "Gram-positive bacteria",
  "Ecc15" = "Gram-negative bacteria",
  "P. rettgeri live" = "Gram-negative bacteria",
  "S. marcescens Type" = "Gram-negative bacteria",
  "E. coli" = "Gram-negative bacteria",
  "P. rettgeri heat-killed" = "Gram-negative bacteria",
  "P. entomophila" = "Gram-negative bacteria",
  "S. marcescens Db11" = "Gram-negative bacteria",
  "P. sneebia" = "Gram-negative bacteria"
)

# Create a new column 'TreatmentGroup' in 'predicted_amps' that groups treatments
predicted_amps$TreatmentGroup <- treatment_groups[as.character(predicted_amps$Treatment)]
predicted_amps$TreatmentGroup <- factor(predicted_amps$TreatmentGroup, 
                                        levels = c("Unchallenged", "clean prick", 
                                                   "Gram-positive bacteria", 
                                                   "Gram-negative bacteria"))

# Define the colors for each treatment group
treatment_group_colors <- c(
  "Unchallenged" = "black",
  "clean prick" = "gray50",
  "Gram-positive bacteria" = "blue",
  "Gram-negative bacteria" = "red"
)

# Filter the data so we only keep Hour 0 and Hour 12
predicted_amps <- predicted_amps %>%
  filter(Hour %in% c(0, 12))

###############################################################################
# Rename te_order to amps_order
###############################################################################
amps_order <- c(
  # Upregulated genes
  "CG43071", "CG43920", "CG43175", "CG32276", "CG16713", 
  "CG34215", "CG11413","CG32284","CG43109",
  # Downregulated genes
  "CG34166", "CG34331", "CG17108"
)

# Convert TE to a factor with the specified order
predicted_amps$TE <- factor(predicted_amps$TE, levels = amps_order)

###############################################################################
# Create the plot object using ggplot2
# Plotting 'predicted_amps' data, which was renamed from 'filtered_te_data'
###############################################################################
p <- ggplot(predicted_amps, 
            aes(x = interaction(Hour, Treatment), y = Expression, fill = TreatmentGroup)) +
  geom_boxplot(outlier.shape = NA) +        # Box plot without outlier points
  geom_point(
    alpha = 0.4,
    aes(color = "black")                    # Overlay points with some transparency
  ) +
  facet_wrap(~ TE, nrow = 4, scales = 'free_y') +  # Facet by TE, free y-axis scale, 4 rows
  labs(
    x = "Hour and Treatment",               # X-axis label
    y = "Expression (TPM)",                 # Y-axis label
    fill = "Treatment Group"                # Legend title for fill
  ) +
  theme_minimal() +
  scale_fill_manual(values = treatment_group_colors) +
  scale_color_manual(values = treatment_group_colors) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),  # Bold x-axis text, rotated
    axis.text.y = element_text(face = "bold"),                         # Bold y-axis text
    axis.title.x = element_text(size = 10, face = "bold"),             # Bold x-axis title
    axis.title.y = element_text(size = 10, face = "bold"),             # Bold y-axis title
    legend.position = "bottom",                                        # Legend at the bottom
    legend.text = element_text(size = 8, face = "bold"),               # Bold legend text
    legend.title = element_text(size = 10, face = "bold"),             # Bold legend title
    strip.text = element_text(size = 12, face = "bold")                # Bold facet strip text
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE),  # Single-row legend
    color = "none"                                # Remove color legend, only fill legend used
  )

# Display the final plot
p
