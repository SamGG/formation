# Merging panels

# We analyze tissues of subjects with two complementary panels
# Some tissues are analyzed with a panel only
# The results are exported as CSV files and are splitted by panel
# Each row summarizes a FCS file, ie a tissue of a subject
# Each row reports the percentage of a tissue for all populations of interest
# We want to get an overall population profile for each tissue
# We need to merge/join the percentages of the two panels

# Read files
# Files are CSV (See conversion note at the end)
# title of columns is unchanged (check.names = FALSE)
# file names could be replaced by file.choose() function in order to interactively select a file
mix.LT <- read.csv("mix LT activation CD4+.csv", check.names = FALSE)
mix.Treg <- read.csv("mix Treg CD4+.csv", check.names = FALSE)

# Join
# Join means that each row of the second table is appended to the right of the same row of the first table checking that both rows have the same key (ie identifier)
# The key that identifies a row in each table is "Name"
# all = TRUE reports every row even if it is not found in one of table
compil = merge(mix.LT, mix.Treg, by = "Name", all = TRUE)

# Cleaning
# We remove statistics that have been exported as extra rows
# This can be done just after reading the CSV file
compil = subset(compil, Name != "SD")
compil = subset(compil, Name != "Mean")
# We remove columns that are in common in both tables
# Such columns are suffixed by ".x" for the first table, and ".y" for the second one
compil = compil[, -grep(".x", colnames(compil), fixed = TRUE)]
compil = compil[, -grep(".y", colnames(compil), fixed = TRUE)]

# If there is more than one panel:
# read the CSV export of next panel
# merge it with compil
# repeat until no more panels


# Transform columns (Optional)
# We remove ".fcs" extension by replacing it with an empty string
# This can be done just after reading the CSV file
compil$Name = gsub(".fcs", "", compil$Name)
# The key string is a pattern "tissue_status_numeric"
# status is either "cre" or "crelox"
# status_numeric identifies a unique subject
# We use the replacement function
# The pattern must match the complete string thus
# We use the power of regular exressions to retrieve specific parts
# Characters within paranthesis are stored  and reinserted using "\\1"
# We match the beginning of the string
compil$tissue = gsub("^(.+?)_.*", "\\1", compil$Name, perl = TRUE)
# We match the rest of the string excluding the tissue part
compil$id = gsub("^.+?_(.*)", "\\1", compil$Name, perl = TRUE)
# We match the second part, ie the status
compil$cre = gsub("^.+?_(.*)_.*", "\\1", compil$Name, perl = TRUE)


# Save as CSV
# Don't export name of row (here there is none)
# Use empty string as missing values (NA)
# Don't encapsulate strings within quote unless necessary
write.csv(compil, file = "compil.csv", row.names = FALSE, na = "", quote = FALSE)
# Then we check, clean, annotate and export the data using Excel
# The data must be exported as "text tabulated" format in order to fulfill MeV requirements



# Wide to long
# We transform the two-way table (ie wide format) in one-way table (long format)
# One-way table fits perfectly database storage and analysis
# In wide format, some columns identifies the studied sample, and others columns hold measurements of specific characteristics, such as percentage of specific populations. Population names are stored in column titles. Measured percentages are value stored at the intersection of a specific column and a specific row.
# In long format, the column title (ie population name) is reported in the "variable" column, and the measure (ie percentage) is reported in the "value" column. Thus the variable column is highly redundant.
library(reshape2)
# The operation is called melt in reshape2 vocabulary
# id.vars informs which columns are identifying a row and are not melt
# All others columns are melt, ie the column title is placed in the "variable" column and the percentage is placed in the "value" column
# The title of the "variable" column is changed as population, because all columns are populations
# The title of the "value" column is changed as percentage
compil1d = melt(data = compil, id.vars = c("Name", "tissue", "id", "cre"), variable.name = "pop", value.name = "percentage")
# Save as CSV
write.csv(compil1d, file = "compil1d.csv", row.names = FALSE, na = "", quote = FALSE)


# Dispatching
# We want a wide format, in order to compare "crelox" vs "crelox" subjects for every tissue and populations combination
# subjects (aka status_numeric) are columns
# tissue and population combinations are rows
# the intersections are percentage
# Aggregation (ie mean, sum...) is possible but not used here
# The function is called dcast
# A formula (x ~ y) allows defining which data is in row (x) and in column (y)
compil2 = dcast(data = compil1d, formula = tissue + pop ~ id, value.var = "percentage")
# We build a new identifier that holds both tissue and population information
compil2$id2 = paste(compil2$tissue, compil2$pop, sep = " ")

# Save as CSV
write.csv(compil2, file = "compil2.csv", row.names = FALSE, na = "", quote = FALSE)
# Then we check, clean, annotate and export the data using Excel
# The data must be exported as "text tabulated" format in order to fulfill MeV requirements


# Conversion notes
# FlowJo gating (at least boolean gating) adds unicode characters to report positive and negative events. Those characters look like upperscript plus and minus. While such characters are fancy, they are not ver informative as they are lost in a CSV export. When exporting statistics as gating as Excel, those characters are kept and could be replaced by standard plus and minus. This could be done within R, once the Excel file has been exported as "text unicode". The following code replaces those character and simplifying the gate labels by removing a constant part.

# input file name
i.file = "original/mix LT activation CD4+.txt"
# alternatively, you can use a dialog box
i.file = file.choose()

# The Excel file was exported as "unicode text" in Windows using by default UTF-16LE code
# The file function explicitely converts unicode code to R unicode code
# The file is processed as a lines because read.table is problematic
# Read lines
report = readLines(file(i.file, encoding = "UTF-16LE"))
# Convert uggly + and -
report[1] = gsub("\u207A", "+", report[1], fixed = TRUE)
report[1] = gsub("\u207B", "-", report[1], fixed = TRUE)
# Add a column title to the first column
report[1] = paste0("Name", report[1], collapse = "")
# Remove
report[1] = gsub(" | Freq. of Parent", "", report[1], fixed = TRUE)
# Simplify strategy labelling
report[1] = gsub("Cells/CD45+/Single Cells/Single Cells/CD4+/Tconv/", "", report[1], fixed = TRUE)
report[1] = gsub("Cells/CD45+/Single Cells/Single Cells/CD4+/", "", report[1], fixed = TRUE)
report[1] = gsub("Cells/CD45+/Single Cells/Single Cells/", "", report[1], fixed = TRUE)
# Create an output file name, replacing extension with a suffix
o.file = gsub(".txt", "-mod", i.file, fixed = TRUE)
# Save converted file
if (!length(grep(",", report))) {
  # There is no comma in any line
  # Mimic a CSV file by replacing tabulations with comma
  message("Input file has been processed and saved as CSV.")
  report = gsub("\t", ",", report)
  writeLines(report, sprintf("%s.csv", o.file))
} else {
  # Save as text file
  message("Input file has been processed and saved as TXT.")
  writeLines(report, sprintf("%s.txt", o.file))
}
