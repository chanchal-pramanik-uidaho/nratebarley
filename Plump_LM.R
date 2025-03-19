want <- c("soiltestcorr","readxl","dplyr","tidyverse","ggplot2","tidyr","foreign","patchwork")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

# Define varieties
varieties <- c("Klages", "Gemcraft", "M69", "Voyager", "Merem", "Harrington")

# Read data
maltbarley <- read_excel("Rog_Malt_Barley_Final.xlsx", sheet="MeanRY")

# Process data
maltbarley <- maltbarley %>%
  mutate(variety = factor(variety, levels = varieties))

maltbarley_filtered <- maltbarley %>%
  filter(variety %in% varieties) %>%
  filter(siteyear_pub != 12)

# Define custom colors and fills
custom_fills <- c(
  "Klages" = "black",
  "Merem" = "black", 
  "Gemcraft" = "black",
  "M69" = "black",
  "Voyager" = "black",
  "Harrington" = "black"
)

custom_colors <- c(
  "Klages" = "black",
  "Merem" = "black",
  "Gemcraft" = "black",
  "M69" = "black",
  "Voyager" = "black",
  "Harrington" = "black"
)

# Define shapes
variety_shapes <- c(21, 21, 21, 21, 21, 21)

# Function to create a plot for a single variety
create_plot <- function(data, variety_name, plot_letter) {
  plot_data <- data %>% filter(variety == variety_name)
  
  # Fit linear model
  lm_result <- lm(plumps ~ nsupply, data = plot_data)
  
  # Extract coefficients and R-squared
  intercept <- coef(lm_result)[1]
  slope <- coef(lm_result)[2]
  r_squared <- summary(lm_result)$r.squared
  
  p <- ggplot(plot_data, aes(nsupply, plumps, color = variety, shape = variety, fill = variety)) +
    geom_point(size = 2.75, stroke = 1) +
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_fills) +
    scale_shape_manual(values = setNames(variety_shapes, varieties)) +
    scale_x_continuous(limits = c(0, 450), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.000001, 140), expand = c(0, 0)) +
    geom_abline(intercept = intercept, slope = slope, size = 1.5, colour = 'black') +
    labs(title = variety_name,
         x = bquote('N-Supply' ~'('(kg ~N ~ha^-1) ),
         y = bquote("Plumpness (%)")) +
    
    # Label Letter
    annotate("text", x = 10, y = 130, label = plot_letter, hjust = 0, family = "serif", size = 6, face = "bold") +
    
    # R2 Line
    annotate("text", x = 250 + 2, y = 35,  
             label = sprintf("R² = %.2f", r_squared),
             family = "serif", size = 6, face = "bold", hjust = 0) + 
    
    # Equation line
    annotate("text", x = 250 + 2, y = 20, 
             label = sprintf("y = %.2f + %.2fx", intercept, slope),
             hjust = 0, family = "serif", size = 6, face = "bold") +
    
    theme_bw() +
    coord_cartesian(xlim = c(0, 460), ylim = c(0.000001, 140))
  
  # Apply theme modifications after creating the plot
  p <- p + theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black", size = 0.5),
    plot.title = element_text(hjust = 0.5, family = "serif", size = 18, face = "bold"),
    axis.title = element_text(family = "serif", size = 16),
    axis.text.x = element_text(family = "serif", size = 12, face = "bold"),
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 5)
  )
  
  return(p)
}

# Create a list of plots for each variety
plot_letters <- letters[1:length(varieties)]
variety_plots <- mapply(function(v, l) create_plot(maltbarley_filtered, v, paste0(l, ")")),
                        varieties, plot_letters, SIMPLIFY = FALSE)

# Combine plots using patchwork
combined_plot <- wrap_plots(variety_plots, ncol = 2)

# Display the combined plot
print(combined_plot)

### Table ###

# Function to extract parameters
extract_parameters <- function(data, variety_name) {
  lm_result <- lm(plumps ~ nsupply, data = data)
  
  intercept <- coef(lm_result)[1]
  slope <- coef(lm_result)[2]
  r_squared <- summary(lm_result)$r.squared
  
  return(data.frame(
    Variety = variety_name,
    Equation = sprintf("y = %.2f + %.2fx", intercept, slope),
    R_squared = round(r_squared, 3)
  ))
}

# Extract parameters for each variety
variety_params <- do.call(rbind, lapply(varieties, function(var) {
  data <- maltbarley_filtered %>% filter(variety == var)
  extract_parameters(data, var)
}))

# Display the table
kable(variety_params, caption = "Linear Model Equations and R² for Each Variety",
      align = c('l', 'l', 'r'),  # Left-align text, right-align R-squared
      col.names = c("Variety", "Equation", "R²"))

