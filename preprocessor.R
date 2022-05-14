# Summary: preprocesses the source file containing events by removing the unnecessary 
# columns.

args = commandArgs(trailingOnly=TRUE)

if (length(args) != 2) 
{
    stop("Usage: preprocessor.R <file_name> <directory where to write WITH trailish slash>.\n", 
         call. = FALSE)
} 
prefix = args[2]
cat("Reading file.\n")

occurr = read.csv(
    args[1], 
    sep = ",", 
    stringsAsFactors = F,
    check.names = T)

occurr <- occurr[, c("scientificName", "vernacularName", "latitudeDecimal", "longitudeDecimal", "eventDate", "eventTime")]
# TODEL
occurr <- occurr[2:nrow(occurr), ]

# I'm not conducting file structure checks here because I assume, the file provided
# is correct. But, of course, for a production-ready app tests would be needed. 
write.csv(occurr, paste0(prefix, "occurr_final_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv", collapse = ""))