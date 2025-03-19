# Load required libraries
want <- c("soiltestcorr","readxl","dplyr","tidyverse","ggplot2","tidyr","foreign","patchwork", "cowplot","knitr","gtsummary")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)

# Load data
maltbarley <- read_excel("Rog_Malt_Barley_Final.xlsx", sheet="MeanRY")

# Define the desired order of varieties
varieties <- c("Klages", "Harrington", "M69", "Merem", "Voyager", "Gemcraft")

maltbarley_filtered <- maltbarley %>%
  filter(variety %in% varieties) %>%
  filter(siteyear_pub != 12) %>%
  mutate(variety = factor(variety, levels = varieties))

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

# Define a single shape (21) for all varieties
uniform_shape <- 21

# Function to create a plot for a single variety
create_plot <- function(data, variety_name, plot_letter) {
  plot_data <- data %>% filter(variety == variety_name)
  
  result <- linear_plateau(
    data = plot_data,
    ry = RGY,
    stv = nsupply,
    target = 95,
    tidy = TRUE,
    plot = FALSE
  )
  
  p <- ggplot(plot_data, aes(nsupply, RGY, color = variety, fill = variety)) +
    geom_point(size = 2.75, stroke = 1, color = "black", shape = uniform_shape) +  # Use uniform shape
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_fills) +
    scale_x_continuous(limits = c(0, 450), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.000001, 104), expand = c(0, 0)) +
    geom_function(size = 1.5, colour = 'black', fun = function(x) result$intercept + result$slope * x, xlim = c(5, result$CSTV)) +
    geom_function(size = 1.5, colour = 'black', fun = function(x) result$plateau, xlim = c(result$CSTV, 470)) +
    geom_vline(xintercept = result$STVt, linetype = 'dashed', size = 1, colour = 'black') +
    labs(title = variety_name,
         x = bquote('N-Supply' ~'('(kg ~N ~ha^-1) ),
         y = bquote("RGY (%)")) +
    
    annotate("text", x = 10, y = 98, label = plot_letter, hjust = 0, family = "serif", size = 6) +
    annotate("text", x = result$STVt + 5.5, y = 4,
             label = bquote(.(round(result$STVt,0))~~'kg N' ~ ha^-1 ~'at 95% RGY'),
             family = "serif", size = 4,face = "bold", angle = 90, hjust = 0) +
    annotate("text", x = 250 + 2, y = 40,
             label = sprintf("R² = %.2f", result$R2),
             family = "serif", size = 6, face = "bold", hjust = 0) +
    annotate("text", x = 250 + 2, y = 30,
             label = bquote('Plateau =' ~.(round(result$plateau,0)) *'%'),
             family = "serif", size = 6,face = "bold", hjust = 0) +
    annotate("text", x = 250 + 2, y = 20, label = result$equation, hjust = 0,
             family = "serif", size = 6, face = "bold") +
    annotate("text", x = 250 + 2, y = 10,
             label = bquote('95% CI =' ~.(round(result$lowerCL, 0)) *' - ' *.(round(result$upperCL, 0)) ~'kg N' ~ ha^-1),
             hjust = 0, family = "serif", size = 6, face = "bold") +
    theme_bw() +
    coord_cartesian(xlim = c(0, 460), ylim = c(0.000001, 104))
  
  p <- p + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = 0.5),
    plot.title = element_text(hjust = 0.5, family = "serif", size = 18, face = "bold"),
    axis.title.x = element_text(family = "serif", size = 16),
    axis.title.y = element_text(family = "serif", size = 16),
    axis.text.x = element_text(family = "serif", size = 12, face = "bold"),
    axis.text.y = element_text(family = "serif", size = 12, face = "bold"),
    legend.position = "none",  # Remove legend
    legend.title = element_blank(),
    legend.text = element_text(family = "serif", size = 16, face = "bold"),
    plot.margin = margin(5, 10, 5, 5)
  )
  return(p)
}

# Create a list of plots for each variety in the specified order
plot_letters <- letters[1:length(varieties)]
variety_plots <- mapply(function(v, l) create_plot(maltbarley_filtered, v, paste0(l, ")")),
                        varieties, plot_letters, SIMPLIFY = FALSE)

# Combine plots using patchwork
combined_plot <- wrap_plots(variety_plots, ncol = 2)

# Display the combined plot
combined_plot

# Function to extract parameters for linear plateau model
extract_parameters <- function(data, variety_name) {
  result <- linear_plateau(
    data = data,
    ry = RGY,
    stv = nsupply,
    target = 95,
    tidy = TRUE,
    plot = FALSE
  )
  
  return(data.frame(
    Variety = variety_name,
    Intercept = round(result$intercept, 2),
    Slope = round(result$slope, 4),
    Plateau = round(result$plateau, 2),
    CSTV = round(result$CSTV, 1),
    STVt = round(result$STVt, 1),
    R2 = round(result$R2, 3),
    LowerCL = round(result$lowerCL, 1),
    UpperCL = round(result$upperCL, 1)
  ))
}

# Extract parameters for each variety
variety_params <- do.call(rbind, lapply(varieties, function(var) {
  data <- maltbarley_filtered %>% filter(variety == var)
  extract_parameters(data, var)
}))

# Display the table
kable(variety_params, caption = "Linear Plateau Model Parameters for Each Variety",
      align = c('l', rep('r', 8)),
      digits = c(0, 2, 4, 2, 1, 1, 3, 1, 1))

