#### Loading packages
library(tidyverse)
library(reshape2)

#---------------------------------------------------------------
# Exploratory Data Analysis
#---------------------------------------------------------------

# Create a numeric-only version of the data for correlation analysis and plotting
numeric_data <- mouse_data
numeric_data$Genotype <- NULL

# Compute the correlation matrix of the numeric variables
protein_correlation_matrix <- as.data.frame(cor(numeric_data))

# Reshape the numeric dataframe from wide to long format for plotting
df_long <- melt(numeric_data)

# Plot histograms for each numeric variable using ggplot2
# Plot histograms for each numeric variable using ggplot2
plot <- ggplot(df_long, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  facet_wrap(~variable, scales = "free_y") +  # Separate histograms for each variable
  theme_classic() +
  labs(x = "Protein Expression", y = "Count")+theme(text = element_text(size = 15))

# Create "Results" folder if it doesn't exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

# Save the correlation matrix as a CSV file
write.csv(protein_correlation_matrix[1:10,1:10], file = "Results/protein_correlation_matrix.csv", row.names = TRUE)
# Save the plot
ggsave(filename = "Results/protein_expression_histograms.png", plot = plot, width = 13, height = 10)

