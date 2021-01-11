# Download CM 2.6 reshaped projections from google drive
# 8/20/2020
# JGM

library (googledrive)

cm_files <- drive_find (pattern = "_projection_")

for (file in cm_files$name) {
  file_path <- paste0 ("../Documents/MATLAB/CM2_6/CM26_", file)
  drive_download (file, file_path, overwrite = TRUE)
}

# again for the fake deltas I recalculated from the projections:

cm_files <- drive_find (pattern = "_delta_")

for (file in cm_files$name) {
  file_path <- paste0 ("../Documents/MATLAB/CM2_6/CM26_", file)
  drive_download (file, file_path, overwrite = TRUE)
}
