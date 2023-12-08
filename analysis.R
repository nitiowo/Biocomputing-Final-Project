library(tidyverse)
library(patchwork)
source("supportingFunctions.R")

# converting all files in countryY to csv format
convert_dir_to_csv("./countryY", sep = " ")

# compiling data across directories/countries
dirs = list.dirs(".")
country_dirs = dirs[grepl("country", dirs)]

compiled_data = data.frame()
for (i in 1:length(country_dirs)) {
  compiled_data = rbind(compiled_data, compile_data(country_dirs[i], drop_na = F))
}

# summarising data in a table and a plot
summarise_compiled_data(compiled_data)

# for every marker, computing its frequency in each country
marker_freq = compiled_data %>%
  select(starts_with("marker"), country) %>%
  pivot_longer(!country, names_to = "marker") %>%
  group_by(country, marker) %>%
  summarise(value = sum(value)) %>%
  mutate(freq = value/sum(value)) %>%
  select(-value) %>%
  pivot_wider(names_from = "marker", values_from = "freq") %>%
  column_to_rownames("country")

write.csv(marker_freq, "marker_freq_table.csv")

pdf("marker_freq_heatmap.pdf", width = 5, height = 4)
heatmap(as.matrix(marker_freq))
dev.off()