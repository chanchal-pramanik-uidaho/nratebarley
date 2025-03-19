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

maltbarley_filtered <- maltbarley_filtered %>% mutate(yield_kg_ha_100=yield_kg_ha/100)

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

# Function to create a plot for a single variety using quadratic plateau
create_plot_quadratic <- function(data, variety_name, plot_letter) {
  plot_data <- data %>% filter(variety == variety_name)
  
  result <- quadratic_plateau(
    data = plot_data,
    ry = yield_kg_ha_100,
    stv = nsupply,
    target = 95,
    tidy = TRUE,
    plot = FALSE
  )
  
  stv_value <- if(is.na(result$STVt) || result$STVt < 0) result$CSTV else result$STVt
  
  p <- ggplot(plot_data, aes(nsupply, yield_kg_ha_100, color = variety, fill = variety)) +
    geom_point(size = 2.75, stroke = 1, color = "black", shape = uniform_shape) +
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_fills) +
    scale_x_continuous(limits = c(0, 450), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.000001, 129), expand = c(0, 0)) +
    geom_function(size = 1.5, colour = 'black', 
                  fun = function(x) result$intercept + result$slope * x + result$quadratic * x^2, 
                  xlim = c(5, result$CSTV)) +
    geom_function(size = 1.5, colour = 'black', fun = function(x) result$plateau, xlim = c(result$CSTV, 470)) +
    geom_vline(xintercept = stv_value, linetype = 'dashed', size = 1, colour = 'black') +
    labs(title = variety_name,
         x = bquote('N-Supply' ~'('(kg ~N ~ha^-1) ),
         y = bquote("Yield ('00 kg/ha)")) +
    
    annotate("text", x = 10, y = 98, label = plot_letter, hjust = 0, family = "serif", size = 6) +
    annotate("text", x = stv_value + 5.5, y = 4,
             label = bquote(.(round(stv_value,0))~~'kg N' ~ ha^-1 ~'at 95% Yield'),
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
    coord_cartesian(xlim = c(0, 460), ylim = c(0.000001, 129))
  
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
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(family = "serif", size = 16, face = "bold"),
    plot.margin = margin(5, 10, 5, 5)
  )
  return(p)
}

# Create a list of plots for each variety using quadratic plateau
plot_letters <- letters[1:length(varieties)]
variety_plots_quadratic <- mapply(function(v, l) create_plot_quadratic(maltbarley_filtered, v, paste0(l, ")")),
                                  varieties, plot_letters, SIMPLIFY = FALSE)

# Combine plots using patchwork
combined_plot_quadratic <- wrap_plots(variety_plots_quadratic, ncol = 2)

# Display the combined plot
combined_plot_quadratic

# Function to extract parameters for quadratic plateau model
extract_parameters_quadratic <- function(data, variety_name) {
  result <- quadratic_plateau(
    data = data,
    ry = yield_kg_ha_100,
    stv = nsupply,
    target = 95,
    tidy = TRUE,
    plot = FALSE
  )
  
  return(data.frame(
    Variety = variety_name,
    Intercept = round(result$intercept, 2),
    Slope = round(result$slope, 4),
    Quadratic = round(result$quadratic, 6),
    Plateau = round(result$plateau, 2),
    CSTV = round(result$CSTV, 1),
    STVt = round(result$STVt, 1),
    R2 = round(result$R2, 3),
    LowerCL = round(result$lowerCL, 1),
    UpperCL = round(result$upperCL, 1)
  ))
}

# Extract parameters for each variety using quadratic plateau
variety_params_quadratic <- do.call(rbind, lapply(varieties, function(var) {
  data <- maltbarley_filtered %>% filter(variety == var)
  extract_parameters_quadratic(data, var)
}))

# Display the table
kable(variety_params_quadratic, caption = "Quadratic Plateau Model Parameters for Each Variety",
      align = c('l', rep('r', 9)),
      digits = c(0, 2, 4, 6, 2, 1, 1, 3, 1, 1))

