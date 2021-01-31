# Download CM 2.6 reshaped projections from google drive
# 8/20/2020
# JGM

library (googledrive)

cm_files <- drive_find (pattern = "_Borm_")

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

# for other period prediction bricks
# test if ok
test_br <- brick ("Models/Prediction_bricks/Bricks_2021/Leptoclinus_maculatus_Borm_14_alltemp_CM26_585_2021_2040.grd")


cm_files <- drive_find (pattern = "_Borm_")

for (file in cm_files$name) {
  file_path <- file.path("Models/Prediction_bricks", file)
  drive_download (file, file_path, overwrite = TRUE)
}

files_21 <-  list.files (path = "Models/Prediction_bricks/", pattern = "2021") # want 882
files_41 <- list.files (path = "Models/Prediction_bricks/", pattern = "2041")
files_81 <- list.files (path = "Models/Prediction_bricks/", pattern = "2081")
save (files_21, file = "files_21_local.RData")
save (files_41, file = "files_41_local.RData")
save (files_81, file = "files_81_local.RData")

extra_21 <- drive_find (pattern = "Lepidorhombus_whiffiagonis_Borm_14_alltemp_gfdl_245_2021_2040.gri")
