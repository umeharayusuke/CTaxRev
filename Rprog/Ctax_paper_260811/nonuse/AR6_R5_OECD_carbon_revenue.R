# AR6 R5 OECD90+EU: carbon price x net CO2 emissions -----------------------
library(data.table)
library(ggplot2)
library(readxl)
library(scales)

# Run from the project root or from this script's directory.
data_dir <- if (dir.exists("data/AR6database")) "data/AR6database" else "../../data/AR6database"
if (!dir.exists(data_dir)) stop("Cannot find data/AR6database.")
output_dir <- if (dir.exists("output")) "output/Figure" else "../../output/Figure"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

r5_file <- file.path(data_dir, "AR6_Scenarios_Database_R5_regions_v1.1.csv")
metadata_file <- file.path(data_dir, "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx")
region <- "R5OECD90+EU"
price_variable <- "Price|Carbon"
emissions_variable <- "Emissions|CO2|Energy and Industrial Processes"
years <- seq(2020, 2050, by = 5)
year_cols <- as.character(years)
categories <- paste0("C", 1:4)

if (!file.exists(r5_file) || !file.exists(metadata_file)) stop("AR6 R5 CSV or metadata workbook is missing.")

# Read only the variables, region, and years needed for the calculation.
raw <- fread(r5_file, header = TRUE,
             select = c("Model", "Scenario", "Region", "Variable", "Unit", year_cols),
             showProgress = TRUE)
raw <- raw[Region == region & Variable %in% c(price_variable, emissions_variable)]
if (nrow(raw) == 0) stop("Neither requested variable was found for R5OECD90+EU.")

actual_units <- raw[, .(Unit = unique(Unit)), by = Variable]
expected_units <- c("Price|Carbon" = "US$2010/t CO2",
                    "Emissions|CO2|Energy and Industrial Processes" = "Mt CO2/yr")
if (nrow(actual_units) != 2 ||
    any(actual_units$Unit != unname(expected_units[actual_units$Variable]))) {
  stop("Unexpected price or emissions unit; inspect the R5 CSV before converting revenue.")
}

long <- melt(raw, id.vars = c("Model", "Scenario", "Variable"), measure.vars = year_cols,
             variable.name = "Year", value.name = "Value", variable.factor = FALSE)
long[, Year := as.integer(Year)]
long <- long[is.finite(Value)]
if (anyDuplicated(long, by = c("Model", "Scenario", "Variable", "Year"))) {
  stop("Duplicate model-scenario-variable-year records found; pairing is ambiguous.")
}

paired <- dcast(long, Model + Scenario + Year ~ Variable, value.var = "Value")
paired <- paired[is.finite(get(price_variable)) & is.finite(get(emissions_variable))]
if (nrow(paired) == 0) stop("No model-scenario-year has both price and emissions.")

# Use the AR6 climate-category metadata to avoid mixing unlike scenario classes.
metadata <- as.data.table(read_excel(metadata_file, sheet = "meta_Ch3vetted_withclimate"))[
  , .(Model, Scenario, Category)]
if (anyDuplicated(metadata, by = c("Model", "Scenario"))) {
  stop("Duplicate model-scenario keys found in the AR6 metadata.")
}
paired <- merge(paired, metadata[Category %in% categories], by = c("Model", "Scenario"))
if (nrow(paired) == 0) stop("No paired observations match vetted AR6 C1-C4 scenarios.")

# US$2010/t CO2 x Mt CO2/yr = million US$2010/yr; divide by 1e6 for trillion.
paired[, Revenue := get(price_variable) * get(emissions_variable) / 1e6]

# Give each model one vote per category and year, regardless of scenario count.
model_year <- paired[, .(Revenue = median(Revenue)), by = .(Category, Year, Model)]
summary_year <- model_year[, .(
  Models = .N,
  Median = median(Revenue),
  P10 = as.numeric(quantile(Revenue, 0.10)),
  P25 = as.numeric(quantile(Revenue, 0.25)),
  P75 = as.numeric(quantile(Revenue, 0.75)),
  P90 = as.numeric(quantile(Revenue, 0.90))
), by = .(Category, Year)]
summary_year <- summary_year[Models >= 3]
summary_year[, Category := factor(Category, levels = categories)]
if (nrow(summary_year) == 0) stop("Fewer than three models are available in every category-year.")

category_labels <- c(
  C1 = "C1  1.5 C, no/limited overshoot", C2 = "C2  1.5 C, high overshoot",
  C3 = "C3  2 C (>67%)", C4 = "C4  2 C (>50%)"
)
n2050 <- summary_year[Year == 2050, .(Category = as.character(Category), Models)]
category_labels <- setNames(
  paste0(unname(category_labels), "  (n=", n2050$Models[match(categories, n2050$Category)], ")"),
  categories
)

ink <- "#173A4A"
teal <- "#297F83"
fill_teal <- "#A8D0CB"
axis_title <- "Price x net CO2 emissions (trillion US$2010/yr)"
method_note <- paste0(
  "One value per model (median over its scenarios); n is model count in 2050.\n",
  "Negative values reflect net negative emissions. This proxy is not observed tax receipts."
)
plot_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, color = ink),
        plot.title = element_text(face = "bold", size = 16, color = ink),
        plot.subtitle = element_text(color = "#53616B"),
        plot.caption = element_text(hjust = 0, color = "#53616B", size = 8.5),
        legend.position = "bottom")

save_pair <- function(plot, stem, width, height) {
  png_file <- file.path(output_dir, paste0(stem, ".png"))
  svg_file <- file.path(output_dir, paste0(stem, ".svg"))
  ggsave(png_file, plot, device = ragg::agg_png,
         width = width, height = height, dpi = 300, bg = "white")
  ggsave(svg_file, plot, device = svglite::svglite,
         width = width, height = height, bg = "white")
  cat("Saved:", png_file, "and", svg_file, "\n")
}

# View 1: keep the middle 50% band so very high model values do not flatten the median.
median_focus <- ggplot(summary_year, aes(x = Year)) +
  geom_hline(yintercept = 0, color = "#8B969C", linewidth = 0.35) +
  geom_ribbon(aes(ymin = P25, ymax = P75), fill = fill_teal, alpha = 0.8) +
  geom_line(aes(y = Median), color = ink, linewidth = 1.15) +
  geom_point(aes(y = Median), color = ink, size = 1.7) +
  facet_wrap(~Category, ncol = 2, scales = "free_y", labeller = as_labeller(category_labels)) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(title = "Implied carbon-price revenue in OECD90+EU",
       subtitle = "C1-C4 scenarios | model median (line) and interquartile range (shading)",
       x = "Year", y = axis_title,
       caption = paste0("Y scales differ by category. ", method_note)) + plot_theme
save_pair(median_focus, "AR6_R5_OECD_carbon_revenue", 12, 9)

# View 2: percentile boxplots at five-year intervals; extreme values do not set the axis.
boxplot_5yr <- ggplot(summary_year, aes(x = factor(Year, levels = years))) +
  geom_hline(yintercept = 0, color = "#8B969C", linewidth = 0.35) +
  geom_boxplot(aes(lower = P25, upper = P75, middle = Median, ymin = P10, ymax = P90),
               stat = "identity", width = 0.62, fill = fill_teal, color = ink,
               linewidth = 0.45) +
  geom_point(aes(y = Median), color = "#C65D4C", size = 2.2) +
  facet_wrap(~Category, ncol = 2, scales = "free_y", labeller = as_labeller(category_labels)) +
  scale_y_continuous(trans = pseudo_log_trans(sigma = 0.5),
                     breaks = c(-1, 0, 0.5, 1, 2, 5, 10, 20),
                     labels = label_number(accuracy = 0.1)) +
  labs(title = "Model distributions every five years",
       subtitle = "C1-C4 scenarios | red dots mark model medians; compressed vertical scale",
       x = "Year", y = axis_title,
       caption = paste0("Signed pseudo-log y scale (linear near zero). Boxes: 25th-75th; whiskers: 10th-90th percentiles.\n",
                        "Values outside the whiskers are not shown; y scales differ by category.\n", method_note)) + plot_theme
save_pair(boxplot_5yr, "AR6_R5_OECD_carbon_revenue_boxplot", 12, 9)

# View 3: exact model medians, with a common color scale across all categories.
value_map <- ggplot(summary_year, aes(x = factor(Year, levels = years), y = Category)) +
  geom_tile(aes(fill = Median), color = "white", linewidth = 1.3) +
  geom_text(aes(label = sprintf("%.2f", Median),
                color = abs(Median) >= 1.5), fontface = "bold", size = 4.5) +
  scale_y_discrete(labels = category_labels, limits = rev(categories)) +
  scale_fill_gradient2(low = "#B26859", mid = "#E6F0EE", high = teal,
                       midpoint = 0, name = "Trillion US$2010/yr") +
  scale_color_manual(values = c(`FALSE` = ink, `TRUE` = "white"), guide = "none") +
  labs(title = "Median implied carbon-price revenue",
       subtitle = "C1-C4 scenarios | figures are model medians (trillion US$2010/yr)",
       x = "Year", y = NULL, caption = method_note) +
  plot_theme + theme(panel.grid = element_blank(), axis.text.y = element_text(color = ink),
                     legend.position = "right")
save_pair(value_map, "AR6_R5_OECD_carbon_revenue_values", 12, 5.7)

cat("Paired model-scenarios:", uniqueN(paired, by = c("Model", "Scenario")), "\n")
cat("Models by category in 2050:\n")
print(summary_year[Year == 2050, .(Category, Models, Median, P25, P75)])
