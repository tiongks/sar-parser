# library(lubridate)
library("ggplot2")
library("dplyr")

parse_sar_file <- function(filename) {
  # print(filename)
  sar_raw <- read.table(filename, skip = 1, header = TRUE, fill = TRUE)
  sar_filtered <- sar_raw[sar_raw$CPU == "all",]
  # sar_filtered <- sar_raw[sar_raw$CPU %in% (0:23),]
  rm(sar_raw)
  sar_header <- read.table(filename, nrows = 1, header=FALSE)
  sar_date <- as.character(sar_header[1,4])
  print(paste(filename, sar_date))
  rm(sar_header)
  sar_cpu <- sar_filtered[,c(1,3,4,6,7)]
  names(sar_cpu) <- c("time", "cpu","usr", "sys", "iowait")
  sar_cpu$usr <- as.numeric(sub(" ", x = sar_cpu$usr, ""))
  sar_cpu$sys <- as.numeric(sub(" ", x = sar_cpu$sys, ""))
  sar_cpu$iowait <- as.numeric(sub(" ", x = sar_cpu$iowait, ""))
  sar_cpu$total <- sar_cpu$usr + sar_cpu$sys + sar_cpu$iowait
  output_csv <- paste("exported-", filename, ".csv", sep = "")
  output_png <- paste("exported-", filename, ".png", sep = "")
  write.csv(file=output_csv, x = sar_cpu)
  ggplot(data =  sar_cpu) + geom_point() +
    aes(x = time, y = total, color = cpu) +
    labs(title=sar_date, x = "time", y = "total") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(output_png, width = 10, units = "in", limitsize = FALSE, scale = 1.5)
  rm(sar_cpu)
}

sar_files <- list.files(pattern = "^sar*")
for (file in sar_files) {
   parse_sar_file(file);
}