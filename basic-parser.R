library(ggplot2)
library(dplyr)
library(reshape2)
library(readr)

parse_sar_file <- function(filename) {
  sar_raw <- read.table(filename, skip = 1, header = TRUE, fill = TRUE)
  sar_filtered <- sar_raw[sar_raw$CPU == "all",]
  rm(sar_raw)
  sar_date <- as.character(read.table(filename, nrows = 1, header=FALSE)[1,4])
  print(paste(filename, sar_date))
  sar_cpu <- sar_filtered[,c(1,2,4,6,7)]
  
  names(sar_cpu) <- c("time", "ampm","usr", "sys", "iowait")
  sar_cpu$timestamp <- paste(sar_date, sar_cpu$time, sar_cpu$ampm)
  sar_cpu$usr <- as.numeric(sub(" ", x = sar_cpu$usr, ""))
  sar_cpu$sys <- as.numeric(sub(" ", x = sar_cpu$sys, ""))
  sar_cpu$iowait <- as.numeric(sub(" ", x = sar_cpu$iowait, ""))
  sar_cpu$total <- sar_cpu$usr + sar_cpu$sys + sar_cpu$iowait
  output_csv <- paste("exported-", filename, ".csv", sep = "")
  output_png <- paste("exported-", filename, ".png", sep = "")
  write.csv(file=output_csv, x = sar_cpu, row.names = FALSE)
  sar_cpu <- read_csv(file = output_csv,
                      col_types = cols(timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S %p")));

  s_melted <- melt(sar_cpu[,c(6,3,4,5)], id="timestamp")
  ggplot(data = s_melted, aes(timestamp, y=value)) + 
    geom_bar(stat = "identity", aes(fill = variable)) +
    labs(title=sar_date, x = "time", y = "total") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  ggsave(output_png, width = 10, units = "in", limitsize = FALSE, scale = 2)
  rm(sar_cpu)
}

sar_files <- list.files(pattern = "^sar*")
for (file in sar_files) {
   parse_sar_file(file);
}