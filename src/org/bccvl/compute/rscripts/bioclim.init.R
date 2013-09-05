# This is python string template which is currently tightly tied to
# ../bioclim.py

.libPaths("{rlibdir}")
wd = "{workdir}"
species = "{species}"
occur.data = "{occurence}"
bkgd.data = {background}
enviro.data.names = c({enviro[names]})
enviro.data.current = c({enviro[data]})
enviro.data.type = c({enviro[type]})
enviro.data.future= c({future[data]})

model.bioclim = TRUE
project.bioclim = TRUE
model.brt = FALSE
project.brt = FALSE

opt.tails = c("both")
opt.ext = NULL
