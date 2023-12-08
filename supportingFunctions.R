convert_dir_to_csv = function(dir_path, sep = "\t") {
  # this function converts all files with separator `sep` in directory `dir path` into csv files
  file_names = list.files(dir_path, full.names = T)
  file_names = file_names[grep("txt$", file_names)]
  
  for (i in 1:length(file_names)) {
    file = read.table(file_names[i], sep = sep, header = T)
    file_name_csv = paste0(substr(file_names[i], 1, nchar(file_names[1])-4), ".csv")
    write.csv(file, file_name_csv, row.names = FALSE)
  }
}

compile_data = function(direct, drop_na = TRUE) {
  # this function compiles data in directory `direct` and drops rows with NA values, given that drop_na is set to TRUE
  # directory names must contain "country" word, followed by an identifier
  file_names = list.files(direct, full.names = T)
  file_names = file_names[grep("csv$", file_names)]
  
  compiled_data = data.frame()
  
  for (i in 1:length(file_names)) {
    file = read.csv(file_names[i])
    file$country = strsplit(direct, "country")[[1]][2]
    file$dayofYear = as.numeric(strsplit(strsplit(file_names[i], "_")[[1]][2], "\\.")[[1]][1])
    compiled_data = rbind(compiled_data, file)
  }
  
  if (!drop_na & any(is.na(compiled_data))) {
    warning("The data contains NA values and drop_na is set to FALSE")
  } else if (drop_na) {
    compiled_data = compiled_data[rowSums(is.na(compiled_data))==0,]
  }
  
  return(compiled_data)
}


summarise_compiled_data = function(compiled_data) {
  #this function generates summary plots and tables based on compiled data
  compiled_data$infected = as.numeric(rowSums(compiled_data[,grep("^marker", colnames(compiled_data))])>0)
  compiled_data$is_male = as.numeric(compiled_data$gender=="male")
  compiled_data$is_female = as.numeric(compiled_data$gender=="female")
  compiled_data$dayofYear_binned = cut(compiled_data$dayofYear, breaks = 10)
  
  # percent infected (incidence) per country
  percent_infected = compiled_data %>%
    group_by(country, dayofYear_binned) %>%
    summarise(value = sum(infected)/n())
  
  summary_table = compiled_data %>%
    group_by(country) %>%
    summarise(n_screens = n(),
              percent_infected = sum(infected)/n(),
              percent_male = sum(is_male)/n(),
              percent_female = sum(is_female)/n())
  
  summary_plot_1 = ggplot(compiled_data,
         aes(x = age)) +
    geom_histogram(binwidth = 5) +
    facet_wrap(~country) +
    labs(x = "Age, years", y = "Count") +
    theme_classic(18)
  
  summary_plot_2 = ggplot(percent_infected,
                          aes(x = dayofYear_binned, y = value, fill = country)) +
    geom_bar(stat="identity", position=position_dodge()) +
    labs(x = "Day of year", y = "Incidence") +
    theme_classic(18) +
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
  
  write.csv(summary_table, "summary_table.csv", row.names = FALSE)
  ggsave(summary_plot_1 + summary_plot_2, path=".", filename= "summary_plot.pdf", width=11, height=4, dpi = 700)
  
  message("Summary table saved as summary_table.csv")
  message("Summary plot saved as summary_plot.pdf")
}




  
