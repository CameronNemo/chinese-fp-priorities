trade_data_file <- "../data/year_origin_destination_hs02_4.tsv"

test_read <- read.csv(trade_data_file, sep='\t', nrows=1000)

test_read$export_val <- as.numeric(gsub("NULL", "0", as.character(test_read$export_val)))
test_read$import_val <- as.numeric(gsub("NULL", "0", as.character(test_read$import_val)))

test_read %>% group_by(year, origin, dest) %>% summarise(exports = sum(export_val), imports = sum(import_val))