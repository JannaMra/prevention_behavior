###########################################################
# Calculation of visual angles

#rm(list=ls())

# Screen dimensions: LG 24MB65PY-B
# Pixel size
wx <- 1920; wy <- 1200
# Screen size (in cm)
swx <- 51.694; swy <- 32.309
# Screen distance sdist (in cm)
sdist <- 59

# Calculate visual angles for whole screen
vax <- 2*(atan((swx/2)/sdist)/pi*180)
vay <- 2*(atan((swy/2)/sdist)/pi*180)

# Pixels per 1 deg of visual angle
xdeg <- wx/vax
ydeg <- wy/vay


# Calculate distance for monitor in Trier
# Screen dimensions: 
# Pixel size
wx <- 1920; wy <- 1080
# Screen size (in cm)
swx <- 53; swy <- 30
# Screen distance sdist (in cm)
sdist <- 61

# Calculate visual angles for whole screen
vax <- 2*(atan((swx/2)/sdist)/pi*180)
vay <- 2*(atan((swy/2)/sdist)/pi*180)

# Pixels per 1 deg of visual angle
xdeg <- wx/vax
ydeg <- wy/vay
