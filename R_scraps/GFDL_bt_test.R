library (ncdf4)

gfdl_245_bt <- nc_open ("Data/gfdl_245_bt_crop.nc")

print (gfdl_245_bt)

g2_bt <- ncvar_get (gfdl_245_bt, "thetao")

image (g2_bt[,,100])
