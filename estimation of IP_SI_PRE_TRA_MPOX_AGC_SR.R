# Estimation of the Mpox Incubation period vs the density distribution 

####################################################################
# Load necessary libraries
library(ggplot2)
library(survival)
library(viridis)
library(grid)
tdata
# Example data (replace with your actual data)
# tdata <- your_data_frame
# threedist <- list(myfit = fitted_weibull_model, myfit_gamma = fitted_gamma_model, myfit_lnorm = fitted_lognormal_model)

# Kaplan-Meier fit
km_fit <- survfit(Surv(tdata$minIncTimes, tdata$maxIncTimes, type = "interval2") ~ 1, data = tdata)

# Kaplan-Meier plot data
km_data <- data.frame(
  time = km_fit$time,
  surv = km_fit$surv,
  lower = km_fit$lower,
  upper = km_fit$upper
)

# Data for survival distribution lines
days <- seq(0, 25, by = 0.05)
pdata <- data.frame(
  days = rep(days, 3),
  fitsurv = c(
    1 - pweibull(days, shape = exp(threedist$myfit$coefficients[1]), scale = exp(allthree$myfit$coefficients[2])),
    1 - pgamma(days, shape = exp(threedist$myfit_gamma$coefficients[1]), scale = exp(allthree$myfit_gamma$coefficients[2])),
    1 - plnorm(days, meanlog = threedist$myfit_lnorm$coefficients[1], sdlog = exp(threedist$myfit_lnorm$coefficients[2]))
  ),
  distn = c(rep("Weibull", length(days)), rep("Gamma", length(days)), rep("Log normal", length(days)))
)

# Kaplan-Meier survival plot using ggsurvplot
ggs = ggsurvplot(
  fit = km_fit,
  legend.title = "KM with 95ci",
  xlab = "Days",
  ylab = "Overall probability of no symptoms yet",
  palette = 'uchicago',
  legend = "none",  # No legend for the Kaplan-Meier plot
  conf.int = TRUE,  # Show the 95% CI
  conf.int.fill = "darkred"  # Color for the 95% CI
)

# Kaplan-Meier plot object with survival distribution lines
final_plot <- ggs$plot + 
  geom_line(data = pdata, aes(x = days, y = fitsurv, color = distn), size = 1, show.legend = TRUE) +  # Add survival distribution lines
  geom_step(data = km_data, aes(x = time, y = surv, color = "Kaplan-Meier"), size = 1) +  # Kaplan-Meier curve
  scale_color_manual(values = c("Kaplan-Meier" = "black","Gamma" = "royalblue2","Log normal" = "#FDE725FF", "Weibull" = "#21908CFF")) +  # Unified color scale
  theme(
    legend.position = "right",  # Position the legend to the right
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  labs(color = "")  # Unified legend (without 95% CI)

# Create the second plot (Density plot for Gamma, Weibull, Log normal distributions)
p1 <- ggplot(data = data.frame(x = c(0, 25)), aes(x = x)) +
  stat_function(fun = dgamma, n = 1001, args = list(shape = 3.39, scale = 2.49), aes(color = "Gamma")) +
  stat_function(fun = dweibull, n = 1001, args = list(shape = 1.93, scale = 9.61), aes(color = "Weibull")) +
  stat_function(fun = function(x) dlnorm(x, meanlog = 1.99, sdlog = 0.56), n = 1001, aes(color = "Log normal")) +
  scale_y_continuous(name = "Density") +  # Add y-axis label as "Density"
  scale_color_manual(values = c("Gamma" = "royalblue2", "Weibull" = "#21908CFF", "Log normal" = "#FDE725FF")) +  # Matching colors
  theme_classic() +
  labs(x = "Incubation period", color = "Distribution") +
  theme(legend.position = "none")  # Remove the legend for the density plot

# Create the density plot as a grob (graphical object)
p1_grob <- ggplotGrob(p1)

# Combine the Kaplan-Meier plot and density plot in one single plot
final_combined_plot <- final_plot +
  annotation_custom(grob = p1_grob, xmin = 10, xmax = 20, ymin = 0.5, ymax = 1)  # Adjust size and position of the density plot

# Display the combined plot
print(final_combined_plot)

# Save the combined plot
ggsave(filename = "/Users/yehyaalthobaity/Desktop/monkeybox/figure/combined_plot_km_with_density_ip.pdf", plot = final_combined_plot, width = 8, height = 6)
#######################################
# Estimation of the Mpox Serial interval vs the density distribution 

library(ggplot2)
library(survival)
library(viridis)
library(survminer)
library(grid)

# Kaplan-Meier plot with extended x-axis to 25 days
km_plot <- ggsurvplot(
  fit = km_fit,
  conf.int = TRUE,
  conf.int.style = "ribbon",
  conf.int.fill = "#800020",  # Burgundy for 95% CI
  conf.int.alpha = 0.3,       # Adjust transparency for CI
  xlab = "Days between symptom onset of secondary and index cases",
  ylab = "Probability of secondary cases with no symptoms yet",
  xlim = c(0, 22),            # Ensure x-axis is extended to 25
  ggtheme = theme_minimal(),
  palette = "black"           # Set Kaplan-Meier curve color to black
)

# Convert Kaplan-Meier plot to ggplot object
km_ggplot <- km_plot$plot +
  scale_x_continuous(limits = c(0, 22)) +  # Enforce x-axis range for the survival curve
  theme(
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  )

# Combine survival curve with density curves
final_plot_km_si <- km_ggplot +
  geom_line(data = surv_df, aes(x = Time, y = Survival, color = Distribution), size = 0.6) +
  scale_color_manual(
    values = c(
      "Gamma" = "royalblue2",        # Soft teal
      "Lognormal" = "#FDE725FF",    # Deep steel blue
      "Weibull" = "#21908CFF",      # Burnt orange
      "All" = "black"               # Black for Kaplan-Meier curve
    ),
    labels = c(
      "Gamma" = "Gamma",
      "Lognormal" = "Lognormal",
      "Weibull" = "Weibull",
      "All" = "Kaplan-Meier"        # Ensure this matches the legend
    ),
    breaks = c("Gamma", "Lognormal", "Weibull", "All")  # Define legend order here
  ) +
  labs(color = "") +
  theme(
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.2)
  )

# Density plot for inset (with x-axis range extended to 25)
p2 <- ggplot(data = data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = dgamma, args = list(shape = 1.23, rate = 0.17), size = 0.8, colour = "royalblue2", show.legend = FALSE) + 
  stat_function(fun = dlnorm, args = list(meanlog = 1.52, sdlog = 1.26), size = 0.8, colour = "#FDE725FF", show.legend = FALSE) +
  stat_function(fun = dweibull, args = list(shape = 1.22, scale = 7.69), size = 0.8, colour = "#21908CFF", show.legend = FALSE) +
  labs(x = "Serial interval", y = "Density") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  # Set white background
    panel.grid = element_blank()  # Remove grid lines
  )

# Convert density plot to grob for insertion into Kaplan-Meier plot
p2_grob <- ggplotGrob(p2)

# Combine the plots
final_combined_plot <- final_plot_km_si +
  annotation_custom(grob = p2_grob, xmin = 10, xmax = 20, ymin = 0.5, ymax = 1)  # Adjust position of the inset plot

# Display the combined plot
print(final_combined_plot)

# Save the combined plot
ggsave(filename = "~/Desktop/monkeybox/figure/combined_plot_km_and_density_serial_interval.pdf", plot = final_combined_plot)
#####################################3


library(ggplot2)
library(survival)
library(viridis)
library(survminer)
library(grid)
library(unikn)  # For UnivChicago palette
km_fit
# Extract UnivChicago palette colors
unichicago_colors <- usecol(pal_unikn)
unichicago_colors
selected_color <- unichicago_colors[1]  # Choose a color (e.g., 3rd color in the palette)
selected_color

# Kaplan-Meier plot with extended x-axis to 25 days
km_plot <- ggsurvplot(
  fit = km_fit,
  conf.int = TRUE,
  conf.int.style = "ribbon",
  conf.int.fill = "#8B0000",  # Dark Red color for 95% CI
  conf.int.alpha = 0.3,           # Adjust transparency for CI
  xlab = "Days between symptom onset of secondary and index cases",
  ylab = "Probability of secondary cases with no symptoms yet",
  xlim = c(0, 22),                # Ensure x-axis is extended to 25
  ggtheme = theme_minimal(),
  palette = "black"               # Set Kaplan-Meier curve color to black
)

# Convert Kaplan-Meier plot to ggplot object
km_ggplot <- km_plot$plot +
  scale_x_continuous(limits = c(0, 22)) +  # Enforce x-axis range for the survival curve
  theme(
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  )

# Combine survival curve with density curves
final_plot_km_si <- km_ggplot +
  geom_line(data = surv_df, aes(x = Time, y = Survival, color = Distribution), size = 0.6) +
  scale_color_manual(
    values = c(
      "Gamma" = "royalblue2",        # Soft teal
      "Lognormal" = "#FDE725FF",    # Deep steel blue
      "Weibull" = "#21908CFF",      # Burnt orange
      "All" = "black"               # Black for Kaplan-Meier curve
    ),
    labels = c(
      "Gamma" = "Gamma",
      "Lognormal" = "Lognormal",
      "Weibull" = "Weibull",
      "All" = "Kaplan-Meier"        # Ensure this matches the legend
    ),
    breaks = c("Gamma", "Lognormal", "Weibull", "All")  # Define legend order here
  ) +
  labs(color = "") +
  theme(
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(hjust = 0.2)
  )

# Density plot for inset (with x-axis range extended to 25)
p2 <- ggplot(data = data.frame(x = c(0, 20)), aes(x)) +
  stat_function(fun = dgamma, args = list(shape = 1.23, rate = 0.17), size = 0.8, colour = "royalblue2", show.legend = FALSE) + 
  stat_function(fun = dlnorm, args = list(meanlog = 1.52, sdlog = 1.26), size = 0.8, colour = "#FDE725FF", show.legend = FALSE) +
  stat_function(fun = dweibull, args = list(shape = 1.22, scale = 7.69), size = 0.8, colour = "#21908CFF", show.legend = FALSE) +
  labs(x = "Serial interval", y = "Density") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  # Set white background
    panel.grid = element_blank()  # Remove grid lines
  )

# Convert density plot to grob for insertion into Kaplan-Meier plot
p2_grob <- ggplotGrob(p2)

# Combine the plots
final_combined_plot <- final_plot_km_si +
  annotation_custom(grob = p2_grob, xmin = 10, xmax = 20, ymin = 0.5, ymax = 1)  # Adjust position of the inset plot

# Display the combined plot
print(final_combined_plot)

# Save the combined plot
ggsave(filename = "~/Desktop/monkeybox/figure/combined_plot_km_and_density_serial_interval.pdf", plot = final_combined_plot)
#################################################################################
# estimation of pre-symptomatic transmission of mpox in the agc 

# Required Libraries
library(ggplot2)
library(gridExtra)
library(mvtnorm)
library(lcmix)
library(viridis)

# Parameters for serial interval (Gamma distribution) and incubation period (Gamma distribution)
agc = list(median = 7.19, shape = 1.23, scale = 7.69)  # Serial interval parameters (Gamma)
incubation = list(mean = 8.52, shape = 3.39, scale = 2.49)  # Incubation period parameters (Gamma)

# Function to generate differences between serial interval and incubation period
generate_diff_data <- function(Nsamp, agc, incubation) {
  # Generate incubation times using Gamma distribution (for the incubation period)
  inctimesU = rgamma(Nsamp, shape = incubation$shape, rate = 1 / incubation$scale)  # scale = 1/mean
  
  # Generate serial interval using Gamma distribution
  sertimes = rgamma(Nsamp, shape = agc$shape, rate = 1 / 5.88)  # scale = 1/0.17
  
  # Return data frame with differences between serial interval and incubation period
  return(data.frame(timedifferance = sertimes - inctimesU, group = "Unstratified"))
}
# Define Nsamp
Nsamp = 1000  # Number of samples to generate

# Generate data
a1 = generate_diff_data(Nsamp, agc, incubation)

# Portion pre-symptom: unstratified
portion_pre_symptom_unstratified = sum(a1$timedifferance < 0) / length(a1$timedifferance)
portion_pre_symptom_unstratified
mean_time_diff = mean(a1$timedifferance)
mean_time_diff
# Load external data for further processing
load("threedist_and_data.Rdata") 

# Function to generate differences based on statistical fits
getMyDiffs <- function(statfit, cormean = 0.389) {
  incparsamps = exp(mvtnorm::rmvnorm(n = 100, mean = statfit$coefficients, sigma = statfit$var))
  sishapes = rnorm(100, mean = 7.16, sd = 1.25)
  corvals = rnorm(100, mean = cormean, sd = 0.04)
  
  bigsamps = lapply(1:100, function(x) 
    lcmix::rmvgamma(n = 500, shape = c(incparsamps[x, 1], sishapes[x]), 
                    rate = c(1 / incparsamps[x, 2], 1 / 2.23),
                    corr = matrix(c(1, corvals[x], corvals[x], 1), nrow = 2)))
  
  bigsamps = do.call(rbind, bigsamps)
  return(data.frame(incs = bigsamps[, 1], sis = bigsamps[, 2], diffs = bigsamps[, 2] - bigsamps[, 1]))
}

# Call the function for different fits
anydiffs = getMyDiffs(threedist$myfit_gamma)

# Combine the data for plotting
a1 = data.frame(timedifferance = anydiffs$diffs, group = "Unstratified")

# Plot density of timedifferance
p3 = ggplot(data = a1, aes(x = timedifferance, fill = group)) +
  geom_density(alpha = 0.5) + 
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 1.5) +
  xlim(c(-20, 10)) + 
  ggtitle("Estimation of Pre-symptomatic transmission") +
  theme(plot.title = element_text(hjust = 0.5))  # Center title
ggsave(file = "portion_pre_pre_symp_corr.pdf", height = 4, width = 6)
p3
# Portion pre-symptom: unstratified
portion_pre_symptom_unstratified

# Mean timedifferance
mean_time_diff

# Load another dataset for bootstrapping
load("~/Desktop/monkeybox/interbooty2.Rdata")

# Function for bootstrapped differences
getDiffsInter <- function(myboot, cormean = 0.389) {
  gamshape = sample(myboot$gsboots, 100)
  incshape = sample(myboot$isboots, 100)
  corvals = rnorm(100, mean = cormean, sd = 0.04)
  
  bigsamps = lapply(1:100, function(x) 
    lcmix::rmvgamma(n = 500, shape = c(gamshape[x], incshape[x]), 
                    rate = c(1 / 2.2, 1 / 2.2),
                    corr = matrix(c(1, corvals[x], corvals[x], 1), nrow = 2)))
  
  bigsamps = do.call(rbind, bigsamps)
  return(data.frame(gens = bigsamps[, 1], incs = bigsamps[, 2],
                    timedifferance = bigsamps[, 1] - bigsamps[, 2], rate = myboot$rate[1])) 
}

# Get differences for bootstrapped datasets
diff1 = getDiffsInter(boot1)
diff2 = getDiffsInter(boot2)
diff3 = getDiffsInter(boot3)
diff4 = getDiffsInter(boot4)

# Combine bootstrapped data for plotting
diffd = rbind(diff1, diff2, diff3, diff4)
diffd$rate = as.factor(diffd$rate)


# Calculate portion pre-symptoms for the different bootstrapped datasets
portion_pre_symptom_boot1 = sum(diff1$timedifferanceerance < 0) / nrow(diff1)
mean(diff1$timedifferance)
portion_pre_symptom_boot2 = sum(diff2$timedifferance < 0) / nrow(diff2)
mean(diff1$timedifferance)
portion_pre_symptom_boot3 = sum(diff3$timedifferance < 0) / nrow(diff3)
mean(diff3$timedifferance)
portion_pre_symptom_boot4 = sum(diff4$timedifferance < 0) / nrow(diff4)
mean(diff4$timedifferance)

# Load necessary libraries
library(ggplot2)

# Combine the data for plotting
d3 = data.frame(timedifferance = anydiffs$diffs, group = "")
df = d3  # No need to use rbind if there's no additional data

# Plot density of timedifferance with dark blue color scheme
p3 = ggplot(data = df, aes(x = timedifferance, fill = group)) +
  geom_density(alpha = 0.7) +  # Increased opacity for better visualization
  scale_fill_manual(values = c("darkred")) +  # Use a dark blue color for the fill
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 1.5) +  # Corrected line style
  xlim(c(-20, 30)) + 
  ggtitle("Estimation of Pre-symptomatic Transmission") +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(size = 12),   # Adjust x-axis label size
        axis.title.y = element_text(size = 12)) + # Adjust y-axis label size
  xlab("Time Difference") +  # Updated x-axis label
  labs(fill = "Density")  # Set the legend title to "Density"

# Display the plot
print(p3)
p3
# Save the plot as PDF
ggsave(file = "portion_pre_pre_symp_corr.pdf", plot = p3, height = 4, width = 6)


