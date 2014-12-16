####################################
# Initialise functions and variables
####################################


# Set these to match your local Java and Maxent directories
java.dir = '/usr/bin/'
maxent = '/home/dave/Downloads/maxent.jar'   

# Defined as in Supp Materials 1 for calibrating mean distance
chi.mean = sqrt(2) * gamma(1.5)/gamma(1)

# Install required packages
options(guiToolkit="RGtk2")
packages = c("tcltk", "gWidgets","gWidgetsRGtk2","raster","rgdal","fields","RGtk2","cairoDevice","akima","dismo")
#packages = c("raster","rgdal","fields","akima","dismo")
for(the.package in packages){
  if (!require(the.package,character.only=T)) {
    inst.text=paste("install.packages(",the.package,")",sep="")
    eval(parse(text=inst.text))
  }
  if (!require(the.package,character.only=T)) {
    print(paste("Could not install package '",the.packge,"', please contact your sysadmin.",sep=""))
    return()
  }
}

# Initialise variables
all.error.reps = 0
obs.error.reps = 0
env.error.reps = 0
sb.reps = 0
var.reps = 0
dl.reps = 0
curr.t = 1
species = "None"
methods = "None"

# File dialog functions

# File chooser dialog
fileChoose <- function(action="print", text = "Select a file...", type="open", ...) {
   gfile(text=text, type=type, ..., action = action, handler =function(h,...) {
     if(h$action != ""){
       print(h$action)
       do.call(h$action, list(h$file))
     }
   })}
   
# Folder chooser dialog
folderChoose <- function(action="print", text = "Select a file...", type="selectdir", ...) {
   gfile(text=text, type=type, ..., action = action, handler =function(h,...) {
     if(h$action != ""){
       print(h$action)
       do.call(h$action, list(h$file))
     }
   })}   
   
# reads monthly raster files, converts them and writes rasters of the relevant 
# 6 bioclim variables using the biovars function from the dismo package
monthly.to.bio6 = function(dir, write = FALSE, ...)
{
  for (i in 1:3)
  {
    layers = vector('list', 12)
    for (j in 1:12)
    {
      X = raster(sprintf('%s/s%02dm%02d.asc', dir, i, j))
      layers[[j]] = X
    }
    if (i == 1) tmax = stack(layers[[1]], layers[[2]],layers[[3]],layers[[4]],layers[[5]],layers[[6]],layers[[7]],layers[[8]],layers[[9]],layers[[10]],layers[[11]],layers[[12]])
    if (i == 2) tmin = stack(layers[[1]], layers[[2]],layers[[3]],layers[[4]],layers[[5]],layers[[6]],layers[[7]],layers[[8]],layers[[9]],layers[[10]],layers[[11]],layers[[12]])
    if (i == 3) prec = stack(layers[[1]], layers[[2]],layers[[3]],layers[[4]],layers[[5]],layers[[6]],layers[[7]],layers[[8]],layers[[9]],layers[[10]],layers[[11]],layers[[12]])
  }
  out = biovars(prec, tmin, tmax)
  out = list(bio01 = out$bio1, bio05 = out$bio5, bio06 = out$bio6, bio12 = out$bio12, bio18 = out$bio18, bio19 = out$bio19)
  
  if (write)
  {
    for (n in names(out))
      writeRaster(out[[n]], file = sprintf('%s/%s.asc', dir, n), ...)
  }
  
  out
}


##########
# Load GUI
##########

# Draw window
window = gwindow("Uncertainty modelling", visible=T)
pane = ggroup(container=window,horizontal=F)
notebook = gnotebook(container=pane,expand=T)

# Maxent settings pane
maxent.group = ggroup(container=notebook,label="Maxent settings",horizontal=F)
a.group = ggroup(container=maxent.group, horizontal = TRUE)

b.group = ggroup(container=a.group, horizontal = FALSE)

load.samples = gbutton("Load samples file", container=b.group, handler = function(h,...)
  {samples.filename <<- fileChoose(); setwd(dirname(samples.filename)); svalue(load.samples.label) = samples.filename})
load.samples.label = glabel("Samples file", container=b.group)

load.layers = gbutton("Select layers directory", container=b.group, handler = function(h,...)
  {layers.filename <<- folderChoose(); setwd(layers.filename);
  svalue(load.layers.label) = layers.filename})
load.layers.label = glabel("Environmental layers", container=b.group)

load.layer.errors = gbutton("Select layer errors directory", container=b.group, handler = function(h,...)
  {layer.errors.filename <<- folderChoose(); setwd(layer.errors.filename);
  svalue(load.layer.errors.label) = layer.errors.filename})
load.layer.errors.label = glabel("Environmental errors layers", container=b.group)

c.group = ggroup(container=a.group, horizontal = FALSE)
output.dir.button = gbutton("Select output directory", container=c.group, handler = function(h,...)
  {output.filename <<- folderChoose(); setwd(output.filename);
  svalue(output.dir.label) = output.filename})
output.dir.label = glabel("Output directory", container=c.group)

proj.dir = gbutton("Select projection layers", container=c.group, handler = function(h,...)
  {proj.filename <<- folderChoose(); setwd(proj.filename);
  svalue(proj.dir.label) = proj.filename})
enabled(proj.dir) = FALSE
proj.dir.label = glabel("Projection layers", container=c.group)

run = gbutton("Run Maxent", container=c.group, handler = function(h,...) run.maxent())

a.group = ggroup(container=maxent.group, horizontal = TRUE)
b.group = ggroup(container=a.group, horizontal = FALSE)
auto.check = gcheckbox('Auto features', checked = TRUE, container = b.group, handler = function(h,...)
  {for (i in 1:N.features) enabled(features.checks[[i]]) = !svalue(h$obj)})
names = c('Linear', 'Quadratic', 'Product', 'Threshold', 'Hinge')
N.features = length(names)
features.checks = vector('list', N.features)
for (i in 1:N.features)
{
  features.checks[[i]] = gcheckbox(names[i], checked = TRUE, container = b.group)
  enabled(features.checks[[i]]) = FALSE
}

c.group = ggroup(container=a.group, horizontal = FALSE)
isproj <<- 'Current'
proj.check = gcheckbox('Projection', checked = FALSE, container = c.group, 
  handler = function(h,...) {
    enabled(proj.dir) = svalue(h$obj);
    if (svalue(h$obj)) 
      isproj <<- c('Current', 'Projected')
    else
      isproj <<- 'Current'
    proj.droplist[] = isproj
  })
names = c('Response curves', 'Pictures', 'Jackknife', 'Cross-validation')
N.outputs = length(names)
output.checks = vector('list', N.outputs)
settings = c(FALSE, TRUE, FALSE)
for (i in 1:(N.outputs-1))
{
  output.checks[[i]] = gcheckbox(names[i], checked = settings[i], container = c.group)
}

output.checks[[N.outputs]] = gcheckbox(names[N.outputs], checked = FALSE, container = c.group, handler = function(h,...)
  {
    if (svalue(h$obj)) 
    {
      reps <<- as.integer(winDialogString('Number of CV replicates:', '5'));
      svalue(cv.label) = sprintf('%d cross-validation folds', reps)
    }
    else 
    {
      reps <<- 1
      svalue(cv.label) = 'No replication'
    }
  })
  
cv.label = glabel('No replication', container = c.group)

# Uncertainty pane
unc.group = ggroup(container=notebook,label="Uncertainty",horizontal=F)
a.group = ggroup(container=unc.group, horizontal = TRUE)
b.group = ggroup(container=a.group, horizontal = FALSE)

obs.check = gcheckbox('Observation location uncertainty', checked = FALSE, container = b.group, handler = function(h,...){
              enabled(obs.error) = svalue(h$obj);
              enabled(dist.error) = svalue(h$obj);
              if (!svalue(h$obj)) {obs.error.reps <<- 0; svalue(obs.error) = 0}
          })
d.group = ggroup(container = b.group, horizontal = TRUE)
samps.label = glabel('Number of samples:', container = d.group)
obs.error = gedit('0', container = d.group, handler = function(h,...){
  obs.error.reps <<- as.integer(svalue(h$obj)); svalue(runs.label) = sprintf('Number of Maxent runs: %d', all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps + dl.reps)})
enabled(obs.error) = FALSE
d.group = ggroup(container = b.group, horizontal = TRUE)
dist.label = glabel('Standard error (km):', container = d.group)
dist.error = gedit('1', container = d.group)
enabled(dist.error) = FALSE

env.check = gcheckbox('Environmental layers uncertainty', checked = FALSE, container = b.group, handler = function(h,...){
              enabled(env.error) = svalue(h$obj);
              if (!svalue(h$obj)) {env.error.reps <<- 0; svalue(env.error) = 0}
          })
d.group = ggroup(container = b.group, horizontal = TRUE)
samps.label = glabel('Number of samples:', container = d.group)
env.error = gedit('0', container = d.group, handler = function(h,...){
  env.error.reps <<- as.integer(svalue(h$obj)); svalue(runs.label) = sprintf('Number of Maxent runs: %d', all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps + dl.reps )})
enabled(env.error) = FALSE


sb.check = gcheckbox('Spatial bias', checked = FALSE, container = b.group, handler = function(h,...){
              enabled(sb.samples) = svalue(h$obj);
              enabled(sb.prop) = svalue(h$obj);
              if (!svalue(h$obj)) {sb.reps <<- 0; svalue(sb.samples) = 0}
          })
d.group = ggroup(container = b.group, horizontal = TRUE)
samps.label = glabel('Number of samples:', container = d.group)
sb.samples = gedit('0', container = d.group, handler = function(h,...){
  sb.reps <<- as.integer(svalue(h$obj)); svalue(runs.label) = sprintf('Number of Maxent runs: %d', all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps + dl.reps )})
enabled(sb.samples) = FALSE
d.group = ggroup(container = b.group, horizontal = TRUE)
prop.label = glabel('Proportion of original sample:', container = d.group)
sb.prop = gedit('0.5', container = d.group)
enabled(sb.prop) = FALSE

data.check = gcheckbox('Data loss uncertainty', checked = FALSE, container = b.group, handler = function(h,...){
              enabled(dl.samples) = svalue(h$obj);
              enabled(dl.prop) = svalue(h$obj);
              if (!svalue(h$obj)) {dl.reps <<- 0; svalue(dl.samples) = 0}
          })
d.group = ggroup(container = b.group, horizontal = TRUE)
samps.label = glabel('Number of samples:', container = d.group)
dl.samples = gedit('0', container = d.group, handler = function(h,...){
  dl.reps <<- as.integer(svalue(h$obj)); svalue(runs.label) = sprintf('Number of Maxent runs: %d', all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps + dl.reps )})
enabled(dl.samples) = FALSE
d.group = ggroup(container = b.group, horizontal = TRUE)
prop.label = glabel('Proportion of original sample:', container = d.group)
dl.prop = gedit('0.5', container = d.group)
enabled(dl.prop) = FALSE

var.check = gcheckbox('Model variance', checked = FALSE, container = b.group, handler = function(h,...){
              enabled(var.samples) = svalue(h$obj);
              if (!svalue(h$obj)) {var.reps <<- 0; svalue(var.samples) = 0}
          })
d.group = ggroup(container = b.group, horizontal = TRUE)
samps.label = glabel('Number of samples:', container = d.group)
var.samples = gedit('0', container = d.group, handler = function(h,...){
  var.reps <<- as.integer(svalue(h$obj)); svalue(runs.label) = sprintf('Number of Maxent runs: %d', all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps + dl.reps )})
enabled(var.samples) = FALSE
  
c.group = ggroup(container=a.group, horizontal = FALSE)
all.check = gcheckbox("All sources combined", checked = FALSE, container = c.group, handler = function(h,...) {
    enabled(all.error) = svalue(h$obj)
    if (!svalue(h$obj)) {all.error.reps <<- 0; svalue(all.error) = 0}
  })
d.group = ggroup(container = c.group, horizontal = TRUE)
all.label = glabel('Number of samples:', container = d.group)
all.error = gedit('0', container = d.group, handler = function(h,...)
  {all.error.reps <<- as.integer(svalue(h$obj)); svalue(runs.label) = sprintf('Number of Maxent runs: %d', all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps + dl.reps )})
enabled(all.error) = FALSE


run.unc = gbutton("Run Maxent with uncertainty", container= c.group, handler = function(h,...) {
  outputs.dir <<- svalue(output.dir.label)
  print("RUN MAXENTS")
  run.maxents()
})
runs.label = glabel('Number of Maxent runs: 0', container = c.group)  # obs.error.reps + all.error.reps

output.radio = gradio(c('Habitat suitability', 'Presence-absence'), container = c.group)

# Draws current model run distribution of current method/species combination
draw.curr = function(h, ...)
{
  visible(raster.page)=T
  i = svalue(methods.droplist, index=TRUE)
  j = svalue(species.droplist, index=TRUE)
  if (length(i)>0 & length(j)>0)
  {
    if (!draw.proj)
      tmp=(DATA[[i]][[j]][[curr.t]])
    else
      tmp=(DATA.PROJ[[i]][[j]][[curr.t]])
  
    if (length(tmp)>0) 
      plot(tmp, main = curr.t)
    else
    {
      plot(1,1,type='n', axes=FALSE, xlab='', ylab='')
      text(1,1, 'No image')
    }
  }
}

# Outputs pane
outputs.group = ggroup(container=notebook,label="Outputs",horizontal=F)
raster.page <- ggraphics(container=outputs.group,label="Population TS")
outputs.buttons <- ggroup(container=outputs.group)
methods.droplist = gdroplist(methods, container=outputs.buttons,
                    handler= function(h, ...) {curr.t <<- 1; draw.curr(h,...)})
species.droplist = gdroplist(species, container=outputs.buttons, 
                    handler= function(h, ...) {curr.t <<- 1; draw.curr(h,...)})

draw.proj = FALSE
proj.droplist = gdroplist(isproj, container = outputs.buttons,
                  handler = function(h, ...){
                    if (!is.null(svalue(proj.droplist))) # hasn't just been reinitialised by proj.check
                    {
                      if (svalue(proj.droplist) == 'Current')
                        draw.proj <<- FALSE
                      else
                        draw.proj <<- TRUE                                          
                      if ('DATA' %in% ls(.GlobalEnv)) draw.curr(h,...) # if data to plot (avoids error in checking projection button before data available)
                    }
})

prev.but <- gbutton("Previous",container=outputs.buttons,
                    handler=function(h,...){curr.t <<- max(1, curr.t - 1); # set time back by 1 (unless it's already at the first frame)
                      draw.curr(h,...)})
next.but <- gbutton("Next",container=outputs.buttons,
            handler=function(h,...){
              k = svalue(methods.droplist, index=TRUE)
              if (methods[k] == 'All sources')  
                reps = all.error.reps
              else if (methods[k] == 'Observation error') 
                reps = obs.error.reps
              else if (methods[k] == 'Environmental error') 
                reps = env.error.reps
              else if (methods[k] == 'Spatial bias') 
                reps = sb.reps
              else if (methods[k] == 'Model variance') 
                reps = var.reps                
              else if (methods[k] == 'Data loss') 
                reps = dl.reps
              curr.t <<- min(reps, curr.t + 1) # set time forward by 1 (unless it's already at the last frame)
              draw.curr(h,...)
            })
mean.but <- gbutton("Mean",container=outputs.buttons,
            handler = function(h,...)
            {
              visible(raster.page)=T
              i = svalue(methods.droplist, index=TRUE)
              j = svalue(species.droplist, index=TRUE)
              if (length(i)>0 & length(j)>0)
              {              
                if (!draw.proj)
                  tmp = (MEANS[[i]][[j]])
                else
                  tmp = (MEANS.PROJ[[i]][[j]])

                if (length(tmp)>0) 
                  plot(tmp, main = "Mean")
                else
                {
                  plot(1,1,type='n', axes=FALSE, xlab='', ylab='')
                  text(1,1, 'No image')
                }
              }
            })
sd.but <- gbutton("Standard deviation",container=outputs.buttons,
            handler = function(h,...)
            {
              visible(raster.page)=T
              i = svalue(methods.droplist, index=TRUE)
              j = svalue(species.droplist, index=TRUE)
              if (length(i)>0 & length(j)>0)
              {                 
                if (!draw.proj)
                  tmp=(SDS[[i]][[j]])
                else
                  tmp=(SDS.PROJ[[i]][[j]])
                if (length(tmp)>0) 
                  plot(tmp, main = "SD")
                else
                {
                  plot(1,1,type='n', axes=FALSE, xlab='', ylab='')
                  text(1,1, 'No image')
                }
              }
            })


##################
# Run Maxent
##################

run.maxent = function()
{
  # Get filenames from GUI objects
  output.dir = svalue(output.dir.label)
  dir.create(output.dir, showWarnings = FALSE)
  env.file = svalue(load.layers.label)
  proj.file = svalue(proj.dir.label)
  train.file = svalue(load.samples.label)

  # initialise objects
  options = NULL

  # add information about features to options
  names = c('linear', 'quadratic', 'product', 'threshold', 'hinge')

  if (svalue(auto.check)) options = paste(options, '-A') else{
    for (i in 1:N.features)
      if (svalue(features.checks[[i]])) options = paste(options, names[i]) else options = paste(options, paste('no', names[i], sep=''))
  }

  # add information about outputs to options
  names = c('responsecurves', 'pictures', 'jackknife')

  for (i in 1:(N.outputs - 1))
    if (svalue(output.checks[[i]])) options = paste(options, names[i]) else options = paste(options, paste('no', names[i], sep=''))

  # add information about replicates to options
  if (svalue(output.checks[[N.outputs]])) options = paste(options, paste('replicates=', reps,sep=''))

  # rutil/un maxent
  if (!svalue(proj.check)) # no projection
    system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
    '" -e "', env.file, '" -s "', train.file,
    '" -o "', output.dir, '" -a ', options, ' threads=8', sep=''))
  else # with projection
    system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
    '" -e "', env.file, '" -s "', train.file,
    '" -o "', output.dir, '" -a ', options, ' -j "', proj.file, '"  threads=8', sep=''))
}

##############################################
# Prepare output for multiple runs
##############################################
# INPUTS:
# k = method, i = species, reps = number of replicates in method, folder = method code (e.g. "OE"),
# done = data for progress bar, cv = is this a cross-validation dataset (i.e. "MV"),
# proj = is this a projection? (if so, extract information from GUI)
# OUTPUTS:
# No function outputs - output data is saved to global variables DATA and DATA.PROJ

prep.output = function(k, i, reps, folder, done, cv = FALSE, proj = FALSE)
{
  # initialise the method/species sublist in DATA or DATA.PROJ
  if (!proj)
    DATA[[k]][[i]] <<- vector('list', reps)
  else
  {
    DATA.PROJ[[k]][[i]] <<- vector('list', reps)
    pd = svalue(proj.dir.label)    
    pd = gsub('\\', '/', pd, fixed = TRUE) # make sure all path symbols facing the right way
    pd = strsplit(pd, '/')[[1]]    
    pd = pd[length(pd)] # extract last bit of pathname
  }
    
  # for each replicate in the given method
  for (j in 1:reps)
  {
    # work out filename to load
    if (cv) # if cross-validation is used, file format is a bit different
    {
      if (!proj)
        filename = sprintf('%s/test.%s/%s_%d.asc', outputs.dir, folder, gsub(' ', '_', species[i]), j-1)  
      else
        filename = sprintf('%s/test.%s/%s_%d_%s.asc', outputs.dir, folder, gsub(' ', '_', species[i]), j-1, pd) 
    }
    else
    {
      if (!proj)
        filename = sprintf('%s/test.%s.%d/%s.asc', outputs.dir, folder, j, gsub(' ', '_', species[i]))    
      else
        filename = sprintf('%s/test.%s.%d/%s_%s.asc', outputs.dir, folder, j, gsub(' ', '_', species[i]), pd)    
    }

    if (file.exists(filename)) # if the file doesn't exist, don't load it and keep going (no error handling)
    { 
      # load raster then place into relevant output variable,
      # making sure that it retains the same resolution as previously
      # generated global template
      tmp = raster(filename)
      if (!proj)
        DATA[[k]][[i]][[j]] <<- resample(tmp, template, method = 'ngb')
      else
        DATA.PROJ[[k]][[i]][[j]] <<- resample(tmp, template, method = 'ngb')
      
      # if we've chosen the "presence-absence" method
      if (svalue(output.radio, index = TRUE) == 2)
      {
        # work out maxent results filename
        if (cv)
          mrname = sprintf('%s/test.%s/maxentResults.csv', outputs.dir, folder)
        else
          mrname = sprintf('%s/test.%s.%d/maxentResults.csv', outputs.dir, folder, j)
          
        if (file.info(mrname)$size == 0) # has been wiped, use the backup we made
        {
          if (cv)
            mrname = sprintf('%s/test.%s/results.csv', outputs.dir, folder)
          else
            mrname = sprintf('%s/test.%s.%d/results.csv', outputs.dir, folder, j)
        }

        # load maxent results file to extract threshold
        mr = read.csv(mrname)                                                        
        # work out which row is the one we want
        if (cv) 
          q = which(mr$Species == sprintf('%s_%d', gsub(' ', '_', species[i]), j-1))
        else
          q = which(mr$Species == gsub(' ', '_', species[i]))
        # if we find it, extract threshold data and create presence/absence split in output (0-1)
        if (length(q)>0)
        {
          threshold = mr$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold[q]
          if (!proj)
            DATA[[k]][[i]][[j]] <<- (DATA[[k]][[i]][[j]] > threshold)
          else
            DATA.PROJ[[k]][[i]][[j]] <<- (DATA.PROJ[[k]][[i]][[j]] > threshold)
        }
      }
      else # if we've chosen the "habitat suitability" split, set the variable to null
      {
        if (!proj)      
          DATA[[k]][[i]][[j]] = list()
        else
          DATA.PROJ[[k]][[i]][[j]] = list()        
      }
    }

    # stuff for progress bar
    pc.done = round( done + j / (N.species*N.reps) * 100)
#    setTkProgressBar(pb, pc.done, sprintf("Preparing output data for display... (%s) %d%%", methods[k], pc.done), sprintf("%d%% done", pc.done))
  }
  NULL
}

#######################################
# Multiple Maxent runs with uncertainty
#######################################

run.maxents = function()
{
  print("########## in 1")
  # read in required info from GUI objects
  output.dir = svalue(output.dir.label)
  dir.create(output.dir, showWarnings = FALSE)
  print(output.dir)
  env.file = svalue(load.layers.label)
  env.error.file = svalue(load.layer.errors.label)
  proj.file = svalue(proj.dir.label)
  train.file = svalue(load.samples.label)
  
  # these should be zero if checkbox not checked
  obs.error.reps <<- as.integer(svalue(obs.error))
  env.error.reps <<- as.integer(svalue(env.error))
  sb.reps <<- as.integer(svalue(sb.samples))
  var.reps <<- as.integer(svalue(var.samples))
  all.error.reps <<- as.integer(svalue(all.error))
  dl.reps <<- as.integer(svalue(dl.samples))
  tmp.file = paste(dirname(train.file), 'tmp.csv', sep='/')  
  
  # if there are files in the desired directory, ask user if they want to skip
  # running Maxent models, and go straight to loading results instead
  yesno = "NO"
  if (length(list.files(output.dir))>0)
  {
    #yesno = winDialog(type = 'yesno', message = 'Data exists in output folder. Skip straight to loading results?')  
    yesno = tkmessageBox(type = 'yesno', message = 'Data exists in output folder. Skip straight to loading results?')  
    yesno = toupper(yesno) # seems like original author used upper case
  }

  # Maxent options taken from the first tab (as with run.maxent)
  options = NULL
  names = c('linear', 'quadratic', 'product', 'threshold', 'hinge')
  if (svalue(auto.check)) 
    options = paste(options, '-A') 
  else
  {
    for (i in 1:N.features)
      if (svalue(features.checks[[i]])) options = paste(options, names[i]) else options = paste(options, paste('no', names[i], sep=''))
  }
  names = c('responsecurves', 'pictures', 'jackknife')
  for (i in 1:(N.outputs - 1))
    if (svalue(output.checks[[i]])) options = paste(options, names[i]) else options = paste(options, paste('no', names[i], sep=''))

  L = list.files(env.file)
  L = L[extension(L) == '.asc'] # should restrict to only those chosen by maxent settings?
  print(L)
  print("foo")

  pb <<- tkProgressBar("Calculating mask... 0%", "0% done", 0, 100, 0) # draw progress bar

  # for each environmental raster, load and add to mask
  for (i in 1:length(L))
  {
    X = raster(paste(env.file, L[i], sep='/')) # load raster
    if (i == 1) # first time only
    {
      template <<- X # for resampling in output
      mask = is.na(as.matrix(X))
      grid = attributes(X)
    }
    else # subsequent times
      mask = mask | is.na(as.matrix(X))  # cell is masked if ANY of the layers has an NA value there

    # progress bar stuff
    pc.done = round(i / length(L) * 100)
    setTkProgressBar(pb, pc.done, sprintf("Calculating mask... %d%%", pc.done), sprintf("%d%% done", pc.done))
  }

  # rotate and flip mask
  mask = t(mask[nrow(mask):1,])

  # Exclude data points not associated with all climate variables

  train.data = read.csv(train.file) # load observation data

  # calculate which grid value each data point belongs to
  xymin = c(attributes(grid$extent)$xmin, attributes(grid$extent)$ymin)
  xymax = c(attributes(grid$extent)$xmax, attributes(grid$extent)$ymax)
  cellsize = (xymax - xymin)/c(grid$ncols, grid$nrows)
  cc = xymin + cellsize/2 # centre of "bottom-left" cell
  xycoords = t(round((rbind(train.data[,2], train.data[,3]) 
    - cc) / cellsize + 1))
#    - attributes(grid)$cellcentre.offset) / attributes(grid)$cellsize + 1)) # version for read.asciigrid in SDMTools
  ii = which(xycoords[,1] > 0 & xycoords[,1] <= nrow(mask) & xycoords[,2] > 0 & xycoords[,2] <= ncol(mask)) # work out which data are in the bounding rectangle
  ii = ii[which(!mask[xycoords[ii,]])]  # then work out which data are masked
  train.data = train.data[ii,] # crop out those that are not associated with ALL data layers
  
  tmp.file = paste(dirname(train.file), 'tmp.csv', sep='/')   # define a temporary csv filename for writing to

  # work out how many species, add species info to droplist
  species <<- levels(train.data[,1])
  N.species <<- length(species)
  species.droplist[] <<- species

  # populate methods droplist based on GUI inputs
  methods <<- NULL
  if (all.error.reps > 0) methods <<- 'All sources'
  if (obs.error.reps > 0) methods <<- c(methods, 'Observation error')
  if (env.error.reps > 0) methods <<- c(methods, 'Environmental error')
  if (sb.reps > 0) methods <<- c(methods, 'Spatial bias')
  if (var.reps > 0) methods <<- c(methods, 'Model variance')
  if (dl.reps > 0) methods <<- c(methods, 'Data loss')  
  N.methods = length(methods)
  N.reps <<- all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps + dl.reps 
  if (N.methods==0) # throw an error if nothing's been chosen
    stop('No replication methods chosen')
  else
    methods.droplist[] <<- methods
  
  N = nrow(train.data) # number of remaining observations
  
  tmp.data = train.data # create copy to make changes on
  # projects GDA94 lat-long data to MGA Zone 55 (proj4 information from http://spatialreference.org/ref/epsg/28355/)
  # replace "user.projection" with suitable projection for your data in metres
  user.projection = '+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
  Z = project(as.matrix(train.data[,2:3]), user.projection)
  
  if (yesno != "YES") # only run multiple Maxent runs if user hasn't specified to skip
  {
    setTkProgressBar(pb, 0, "Running Maxent models... 0%", "0% done") # draw progress bar
                                                                     
    # -----------------
    # Observation error
    # -----------------
    # Randomly move points, making sure they don't fall into the "ocean" (i.e. climatically undefined points)
    
    if (obs.error.reps > 0) # if observation error has been selected
    {
      dist = as.double(svalue(dist.error)) # extract mean distance info
      for (n in 1:obs.error.reps) # for each model replication
      {
        for (k in 1:N) # for each observation
        {
          okay = FALSE
          while (!okay) # don't continue until we've found a point with climate data
          {
            test.e = Z[k,1] + rnorm(1) * dist * 1000 / chi.mean # mean distance will be "dist", MV normal distributed
            test.n = Z[k,2] + rnorm(1) * dist * 1000 / chi.mean
            test.ll = project(cbind(test.e, test.n), user.projection, inv = TRUE) # convert back to lat-longs
            xycoord = t(round((c(test.ll[,1], test.ll[,2])  # work out coordinate
              - cc) / cellsize + 1))
  #            - attributes(grid)$cellcentre.offset) / attributes(grid)$cellsize + 1) # read.asciigrid / SDMTools version
            if (xycoord[1] > 0 & xycoord[1] <= nrow(mask) & xycoord[2] > 0 & xycoord[2] <= ncol(mask)) # if within bounding rectangle
              okay = !mask[xycoord[1], xycoord[2]] # set as okay if point has data
          }
          tmp.data[k,2] = test.ll[,1]
          tmp.data[k,3] = test.ll[,2]
        }
        tmp.file = paste(dirname(train.file), sprintf('tmp%d.csv', n), sep='/')
        write.csv(tmp.data, tmp.file, row.names=FALSE) # write data to a unique temporary file
    
        multi.output.dir = paste(output.dir, '/test.OE.',n, sep='') # define unique output folder
  
        if (file.exists(paste(multi.output.dir, '/maxentResults.csv', sep='')) & !file.exists(paste(multi.output.dir, '/results.csv', sep=''))) # if the folder already contains a maxentResults file (which will be wiped if model is skipped)
          shell(gsub('/', '\\', paste('copy "', multi.output.dir, '/maxentResults.csv" "', multi.output.dir, '/results.csv"', sep=''), fixed = TRUE))   # create a new maxentResults file that won't be wiped, hopefully
  
        dir.create(multi.output.dir, showWarnings = FALSE) # create output folder
    
        # run Maxent with newly generated data
        if (!svalue(proj.check)) # no projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', env.file, '" -s "', tmp.file,
          '" -o "', multi.output.dir, '" -a ', options, ' nowarnings', sep=''), wait = TRUE)
        else # with projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', env.file, '" -s "', tmp.file,
          '" -o "', multi.output.dir, '" -a ', options, ' -j "', proj.file, '"  nowarnings', sep=''), wait = TRUE)
    
        # progress bar stuff
        pc.done = round(n / N.reps * 100)
        setTkProgressBar(pb, pc.done, sprintf("Running Maxent models... (observation error) %d%%", pc.done), sprintf("%d%% done", pc.done))
      }
    }
  
    # -------------------
    # Environmental error
    # -------------------
    # Use non-spatially-autocorrelated input error rasters
    # Resample using bicubic interpolation (from akima)
  
    if (env.error.reps > 0) # if climate error has been selected
    {
      # calculate vector of cell centres in each direction
      xvec = seq(cc[1], by = cellsize[1], length.out = grid$ncols) 
      yvec = rev(seq(cc[2], by = cellsize[2], length.out = grid$nrows))
      
      for (n in 1:env.error.reps) # for each model replicate
      {
        # create temporary directory to store new generated climate rasters
        tmp.env = paste(output.dir, sprintf('/enverror%d/', n), sep='')
        dir.create(tmp.env, showWarnings = FALSE)
  
        for (i in 1:3) # for each variable (tmin, tmax, rain)
        {
          for (j in 1:12) # for each month
          {
            fn = sprintf('s%02dm%02d.asc', i, j) # generate relevant filename
            # load from folder name containing "sdm" (containing monthly rasters) instead of "bio" (containing original BIOCLIM variable rasters)
            X = raster(paste(gsub('bio', 'sdm', env.file), fn, sep='/'))
            Y = raster(paste(env.error.file, fn, sep='/'))
            Ygrid = attributes(Y)
            Yxymin = c(attributes(Ygrid$extent)$xmin, attributes(Ygrid$extent)$ymin)
            Yxymax = c(attributes(Ygrid$extent)$xmax, attributes(Ygrid$extent)$ymax)
            Ycellsize = (Yxymax - Yxymin)/c(Ygrid$ncols, Ygrid$nrows)
            Ycc = Yxymin + Ycellsize/2 # centre of "bottom-left" cell
            
            Yxvec = seq(Ycc[1], by = Ycellsize[1], length.out = Ygrid$ncols)
            Yyvec = rev(seq(Ycc[2], by = Ycellsize[2], length.out = Ygrid$nrows))
            
            YM = as.matrix(Y)
            Nx = nrow(YM)
            Ny = ncol(YM)
            YM = YM * matrix(rnorm(Nx*Ny), Nx, Ny) # generate errors
            
            ii = which(!is.na(YM))
            q = sample(length(ii)) # hack to avoid points next to each other which breaks interp() - should work, but beware: if it doesn't, it won't return an error (just zeroes)
            I = interp(Yxvec[col(YM)[ii]][q], Yyvec[row(YM)[ii]][q], YM[ii][q], xo = xvec, yo = yvec, extrap = TRUE, linear = FALSE) # bicubic interpolation
            # write interpolation results to raster
            err = raster(t(I$z), xmn = min(I$x) - 0.5*(I$x[2] - I$x[1]), xmx = max(I$x) + 0.5*(I$x[2] - I$x[1]), ymn = min(I$y) + 0.5*(I$y[2] - I$y[1]), ymx = max(I$y) - 0.5*(I$y[2] - I$y[1]))
            writeRaster(X + err, paste(tmp.env, fn, sep=''), overwrite = TRUE) # write to file
          }
        }
        
        tmp = monthly.to.bio6(tmp.env, write = TRUE, overwrite = TRUE) # convert output error surfaces to 6 BIOCLIM variables
        for (i in 1:3) for (j in 1:12) file.remove(sprintf('%s/s%02dm%02d.asc', tmp.env, i, j)) # remove original output files
  
        multi.output.dir = paste(output.dir, '/test.EE.',n, sep='') # define unique output folder
        
        if (file.exists(paste(multi.output.dir, '/maxentResults.csv', sep='')) & !file.exists(paste(multi.output.dir, '/results.csv', sep=''))) # if the folder already contains a maxentResults file (which will be wiped if model is skipped)
          shell(gsub('/', '\\', paste('copy "', multi.output.dir, '/maxentResults.csv" "', multi.output.dir, '/results.csv"', sep=''), fixed = TRUE))   # create a new maxentResults file that won't be wiped, hopefully
                
        dir.create(multi.output.dir, showWarnings = FALSE) # create output folder
    
        # run Maxent with newly generated environmental layers
        if (!svalue(proj.check)) # no projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', tmp.env, '" -s "', train.file,
          '" -o "', multi.output.dir, '" -a ', options, ' nowarnings', sep=''), wait = TRUE)
        else # with projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', tmp.env, '" -s "', train.file,
          '" -o "', multi.output.dir, '" -a ', options, ' -j "', proj.file, '"  nowarnings', sep=''), wait = TRUE)

        # progress bar stuff  
        pc.done = round((obs.error.reps + n) / N.reps * 100)
        setTkProgressBar(pb, pc.done, sprintf("Running Maxent models... (spatial bias) %d%%", pc.done), sprintf("%d%% done", pc.done))
      }
    }
    
    # ------------
    # Spatial bias (TODO: by species, not all together)
    # ------------
    if (sb.reps > 0) # if spatial bias has been selected
    {
      prop = as.double(svalue(sb.prop)) # extract proportion info
    
      # calculate distance matrix (inefficient, could vectorise)
      D = matrix(0, N, N)
      for (i in 2:N){for (j in 1:(i-1)){ D[i,j] = sqrt( diff(Z[c(i,j),1])^2 + diff(Z[c(i,j),2])^2 )}} 
      D = D + t(D) # distance between points is commutative (saves doing each calculation twice)
    
      for (n in 1:sb.reps) # for each model replication
      {
        centre = sample(N, 1) # pick one point as the centre
        threshold = quantile(D[centre,], prop) # calculate threshold that separates prop% of closest points from the remainder
        tmp.data = train.data[which(D[centre,] < threshold), ] # add this subset to temporary file
        tmp.file = paste(dirname(train.file), sprintf('tmp%d.csv', n), sep='/') # generate filename
        write.csv(tmp.data, tmp.file, row.names=FALSE) # write new data to filename
    
        multi.output.dir = paste(output.dir, '/test.SB.',n, sep='') # define unique output folder
        
        if (file.exists(paste(multi.output.dir, '/maxentResults.csv', sep='')) & !file.exists(paste(multi.output.dir, '/results.csv', sep=''))) # if the folder already contains a maxentResults file (which will be wiped if model is skipped)
          shell(gsub('/', '\\', paste('copy "', multi.output.dir, '/maxentResults.csv" "', multi.output.dir, '/results.csv"', sep=''), fixed = TRUE))   # create a new maxentResults file that won't be wiped, hopefully
                
        dir.create(multi.output.dir, showWarnings = FALSE) # create output folder
    
        # run Maxent with newly generated data  
        if (!svalue(proj.check)) # no projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', env.file, '" -s "', tmp.file,
          '" -o "', multi.output.dir, '" -a ', options, ' nowarnings', sep=''), wait = TRUE)
        else # with projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', env.file, '" -s "', tmp.file,
          '" -o "', multi.output.dir, '" -a ', options, ' -j "', proj.file, '"  nowarnings', sep=''), wait = TRUE)

        # progress bar stuff  
        pc.done = round((obs.error.reps + env.error.reps + n) / N.reps * 100)
        setTkProgressBar(pb, pc.done, sprintf("Running Maxent models... (spatial bias) %d%%", pc.done), sprintf("%d%% done", pc.done))
      }
    }
  
    # ------------
    # Model variance
    # ------------
    # Simply run Maxent with cross-validation and use the outputs
  
    if (var.reps > 0) # if model variance has been selected
    {
      multi.output.dir = paste(output.dir, '/test.MV', sep='') # define unique output folder
  
      if (file.exists(paste(multi.output.dir, '/maxentResults.csv', sep='')) & !file.exists(paste(multi.output.dir, '/results.csv', sep=''))) # if the folder already contains a maxentResults file (which will be wiped if model is skipped)
        shell(gsub('/', '\\', paste('copy "', multi.output.dir, '/maxentResults.csv" "', multi.output.dir, '/results.csv"', sep=''), fixed = TRUE))   # create a new maxentResults file that won't be wiped, hopefully

      dir.create(multi.output.dir, showWarnings = FALSE) # create output folder
  
      options.rep = paste(options, paste('replicates=', var.reps, sep='')) # set replication
  
      # run Maxent with cross-validation
      if (!svalue(proj.check)) # no projection
        system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
        '" -e "', env.file, '" -s "', tmp.file,
        '" -o "', multi.output.dir, '" -a ', options.rep, ' nowarnings', sep=''))
      else # with projection
        system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
        '" -e "', env.file, '" -s "', tmp.file,
        '" -o "', multi.output.dir, '" -a ', options.rep, ' -j "', proj.file, '"  nowarnings', sep=''))
    }
  
    # ---------
    # Data loss
    # ---------
    if (dl.reps > 0) # if data loss has been selected
    {
      prop = as.double(svalue(dl.prop)) # extract proportion info
   
      for (n in 1:dl.reps) # for each model replication
      {
        n.subset = prop * N # number of points to include
        n.subset = floor(n.subset) + (runif(1) < (n.subset %% 1)) # deal with fractional number of points by randomly generating an extra point with probability based on fractional part
        points = sample(N, n.subset) # randomly choose this number of rows
        tmp.data = train.data[points, ] # add this subset to temporary file 
        tmp.file = paste(dirname(train.file), sprintf('tmp%d.csv', n), sep='/') # generate filename
        write.csv(tmp.data, tmp.file, row.names=FALSE) # write new data to filename
    
        multi.output.dir = paste(output.dir, '/test.DL.',n, sep='') # define unique output folder
        
        if (file.exists(paste(multi.output.dir, '/maxentResults.csv', sep='')) & !file.exists(paste(multi.output.dir, '/results.csv', sep=''))) # if the folder already contains a maxentResults file (which will be wiped if model is skipped)
          shell(gsub('/', '\\', paste('copy "', multi.output.dir, '/maxentResults.csv" "', multi.output.dir, '/results.csv"', sep=''), fixed = TRUE))   # create a new maxentResults file that won't be wiped, hopefully
                
        dir.create(multi.output.dir, showWarnings = FALSE) # create output folder
    
        # run Maxent with newly generated data      
        if (!svalue(proj.check)) # no projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', env.file, '" -s "', tmp.file,
          '" -o "', multi.output.dir, '" -a ', options, ' nowarnings', sep=''), wait = TRUE)
        else # with projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', env.file, '" -s "', tmp.file,
          '" -o "', multi.output.dir, '" -a ', options, ' -j "', proj.file, '"  nowarnings', sep=''), wait = TRUE)

        # with projection  
        pc.done = round((obs.error.reps + env.error.reps + sb.reps + n) / N.reps * 100)
        setTkProgressBar(pb, pc.done, sprintf("Running Maxent models... (spatial bias) %d%%", pc.done), sprintf("%d%% done", pc.done))
      }
    }
  
    # ------------
    # All sources
    # ------------    
    # All together now!
  
    if (all.error.reps > 0) # if all sources has been selected
    {
      for (n in 1:all.error.reps) # for each model replication
      {
      
        N = nrow(train.data) # number of observations
        tmp.data = train.data # create copy to make changes on
  
        if (env.error.reps > 0) # if climate error has been selected
        {
          # if climate error has been selected
          tmp.env = paste(output.dir, sprintf('/enverror%d/', n), sep='')  
          dir.create(tmp.env, showWarnings = FALSE)
          
          # calculate vector of cell centres in each direction
          xvec = seq(cc[1], by = cellsize[1], length.out = grid$ncols) 
          yvec = rev(seq(cc[2], by = cellsize[2], length.out = grid$nrows))
      
          for (i in 1:3) # for each variable (rain, tmin, tmax)
          {
            for (j in 1:12) # for each month
            {
              fn = sprintf('s%02dm%02d.asc', i, j) # generate relevant filename
              # load from folder name containing "sdm" (containing monthly rasters) instead of "bio" (containing original BIOCLIM variable rasters)
              X = raster(paste(gsub('bio', 'sdm', env.file), fn, sep='/'))
              Y = raster(paste(env.error.file, fn, sep='/'))
              Ygrid = attributes(Y)
              Yxymin = c(attributes(Ygrid$extent)$xmin, attributes(Ygrid$extent)$ymin)
              Yxymax = c(attributes(Ygrid$extent)$xmax, attributes(Ygrid$extent)$ymax)
              Ycellsize = (Yxymax - Yxymin)/c(Ygrid$ncols, Ygrid$nrows)
              Ycc = Yxymin + Ycellsize/2 # centre of "bottom-left" cell
              
              Yxvec = seq(Ycc[1], by = Ycellsize[1], length.out = Ygrid$ncols)
              Yyvec = rev(seq(Ycc[2], by = Ycellsize[2], length.out = Ygrid$nrows))
              
              YM = as.matrix(Y)
              Nx = nrow(YM)
              Ny = ncol(YM)
              YM = YM * matrix(rnorm(Nx*Ny), Nx, Ny) # generate errors
              
              ii = which(!is.na(YM))
              q = sample(length(ii)) # hack to avoid points next to each other which breaks interp() - should work, but beware: if it doesn't, it won't return an error (just zeroes)
              I = interp(Yxvec[col(YM)[ii]][q], Yyvec[row(YM)[ii]][q], YM[ii][q], xo = xvec, yo = yvec, extrap = TRUE, linear = FALSE) # bicubic interpolation
              # write interpolation results to raster              
              err = raster(t(I$z), xmn = min(I$x) - 0.5*(I$x[2] - I$x[1]), xmx = max(I$x) + 0.5*(I$x[2] - I$x[1]), ymn = min(I$y) + 0.5*(I$y[2] - I$y[1]), ymx = max(I$y) - 0.5*(I$y[2] - I$y[1]))
              writeRaster(X + err, paste(tmp.env, fn, sep=''), overwrite = TRUE) # write to file          
            }
          }
          tmp = monthly.to.bio6(tmp.env, write = TRUE, overwrite = TRUE) # convert output error surfaces to 6 BIOCLIM variables
          for (i in 1:3) for (j in 1:12) file.remove(sprintf('%s/s%02dm%02d.asc', tmp.env, i, j)) # remove original output files          
        }
        else
          tmp.env = env.file # use normal env info
  
  
        # Observation error
        if (obs.error.reps > 0) # if observation error has been selected
        {
          dist = as.double(svalue(dist.error)) # extract mean distance info
          for (k in 1:N) # for each observation
          {
            okay = FALSE
            while (!okay) # don't continue until we've found a point with climate data
            {
              test.e = Z[k,1] + rnorm(1) * dist * 1000 / chi.mean # mean distance will be "dist", MV normal distributed
              test.n = Z[k,2] + rnorm(1) * dist * 1000 / chi.mean
              test.ll = project(cbind(test.e, test.n), user.projection, inv = TRUE) # convert back to lat-longs
              xycoord = t(round((c(test.ll[,1], test.ll[,2]) # work out coordinate
                - cc) / cellsize + 1))
              if (xycoord[1] > 0 & xycoord[1] <= nrow(mask) & xycoord[2] > 0 & xycoord[2] <= ncol(mask)) # if within bounding rectangle
                okay = !mask[xycoord[1], xycoord[2]] # set as okay if point has data
            }
            tmp.data[k,2] = test.ll[,1]
            tmp.data[k,3] = test.ll[,2]
          }
        }
        
        N = nrow(tmp.data) # update N (probably not necessary)
        
        # Spatial bias
        if (sb.reps > 0) # if spatial bias has been selected
        {
          prop = as.double(svalue(sb.prop)) # extract proportion info
          # re-project in case obs error used
          Z = project(as.matrix(train.data[,2:3]), '+proj=utm +zone=55 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') 
        
          # calculate distance matrix
          D = matrix(0, N, N)
          for (i in 1:N){for (j in 1:(i-1)){ D[i,j] = sqrt( diff(Z[c(i,j),1])^2 + diff(Z[c(i,j),2])^2 )}} 
          D = D + t(D)  
        
          centre = sample(N, 1) # pick one point as the centre
          threshold = quantile(D[centre,], prop) # calculate threshold that separates prop% of closest points from the remainder
          tmp.data = tmp.data[which(D[centre,] < threshold), ] # apply this subset to temporary file
        }
  
        N = nrow(tmp.data) # update N      
  
        # Data loss
        if (dl.reps > 0) # if data loss has been selected
        {
          n.subset = prop * N # number of points to include
          n.subset = floor(n.subset) + (runif(1) < (n.subset %% 1)) # deal with fractional number of points by randomly generating an extra point with probability based on fractional part
          points = sample(N, n.subset) # randomly choose this number of rows
          tmp.data = tmp.data[points, ]  # apply this subset to temporary file 
        }
  
        tmp.file = paste(dirname(train.file), sprintf('tmp%d.csv', n), sep='/') # generate filename 
        write.csv(tmp.data, tmp.file, row.names=FALSE) # write new data to filename
    
        multi.output.dir = paste(output.dir, '/test.ALL.',n, sep='') # define unique output folder
        
        if (file.exists(paste(multi.output.dir, '/maxentResults.csv', sep='')) & !file.exists(paste(multi.output.dir, '/results.csv', sep=''))) # if the folder already contains a maxentResults file (which will be wiped if model is skipped)
          shell(gsub('/', '\\', paste('copy "', multi.output.dir, '/maxentResults.csv" "', multi.output.dir, '/results.csv"', sep=''), fixed = TRUE))   # create a new maxentResults file that won't be wiped, hopefully
                
        dir.create(multi.output.dir, showWarnings = FALSE) # create output folder
  
        # Apply model variance
        if (var.reps > 0) # if model variance has been selected
        {
          # NOTE: for efficiency, instead of cross-validating for each replicate (each of which has different
          # data potentially with slightly different sizes), we set a training-testing split so that the same
          # *proportion* of data is tested on each time, though they're not in exclusive sets
          # this is closer to a bootstrapping approach than a cross-validation one but should produce similar results
          options.rep = paste(options, paste('randomtestpoints=', round(100*(1/var.reps), 0), sep=''))
        }
        else
          options.rep = options
      
        # run Maxent with all sources accounted for
        if (!svalue(proj.check)) # no projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', tmp.env, '" -s "', tmp.file,
          '" -o "', multi.output.dir, '" -a ', options.rep, ' nowarnings', sep=''), wait = TRUE)
        else # with projection
          system(paste('"', java.dir, 'java" -mx4000m -jar "', maxent,
          '" -e "', tmp.env, '" -s "', tmp.file,
          '" -o "', multi.output.dir, '" -a ', options.rep, ' -j "', proj.file, '"  nowarnings', sep=''), wait = TRUE)
      }
    }
  }

  # ------------------  
  # Prepare for output
  # ------------------  

  # Progress bar stuff
  pc.done = 0
  setTkProgressBar(pb, pc.done, sprintf("Preparing output data for display... %d%%", pc.done), sprintf("%d%% done", pc.done))
  
  # Initialise DATA (and potentially DATA.PROJ) with an element for each method
  DATA <<- vector('list', N.methods)
  if (svalue(proj.check)) DATA.PROJ <<- vector('list', N.methods)  

  # Load data into DATA / DATA.PROJ
  for (k in 1:N.methods) # for each method
  {
    DATA[[k]] <<- vector('list', N.species) # create an element for each species
    if (svalue(proj.check)) DATA.PROJ[[k]] <<- vector('list', N.species)    

    for (i in 1:N.species) # for each species
    {
      if (methods[k] == 'All sources') # if All sources has been selected, prepare output for current method/species + projection if necessary
      {
        done = round( ((i-1) * all.error.reps) / (N.species*N.reps) * 100) # progress bar stuff       
        prep.output(k, i, all.error.reps, 'ALL', done)
        if (svalue(proj.check)) prep.output(k, i, all.error.reps, 'ALL', done, FALSE, TRUE)
      }
      else if (methods[k] == 'Observation error') # if Observation error has been selected, prepare output for current method/species + projection if necessary
      {
        done = round( (N.species*all.error.reps + (i-1) * obs.error.reps) / (N.species*N.reps) * 100) # progress bar stuff
        prep.output(k, i, obs.error.reps, 'OE', done)
        if (svalue(proj.check)) prep.output(k, i, obs.error.reps, 'OE', done, FALSE, TRUE)
      }                                                                                                                     
      else if (methods[k] == 'Environmental error') # if Environmental error has been selected, prepare output for current method/species + projection if necessary
      {
        done = round( (N.species*(all.error.reps + obs.error.reps) + (i-1) * sb.reps) / (N.species*N.reps) * 100) # progress bar stuff       
        prep.output(k, i, env.error.reps, 'EE', done)
        if (svalue(proj.check)) prep.output(k, i, env.error.reps, 'EE', done, FALSE, TRUE)
      }
      else if (methods[k] == 'Spatial bias') # if Spatial bias has been selected, prepare output for current method/species + projection if necessary
      {
        done = round( (N.species*(all.error.reps + obs.error.reps + env.error.reps) + (i-1) * sb.reps) / (N.species*N.reps) * 100) # progress bar stuff
        prep.output(k, i, sb.reps, 'SB', done)
        if (svalue(proj.check)) prep.output(k, i, sb.reps, 'SB', done, FALSE, TRUE)       
      }
      else if (methods[k] == 'Model variance')  # if Model variance has been selected, prepare output for current method/species + projection if necessary
      {
        done = round( (N.species*(all.error.reps + obs.error.reps + env.error.reps + sb.reps) + (i-1) * var.reps) / (N.species*N.reps) * 100) # progress bar stuff
        prep.output(k, i, var.reps, 'MV', done, TRUE)
        if (svalue(proj.check)) prep.output(k, i, var.reps, 'MV', done, TRUE, TRUE)        
      }
      else if (methods[k] == 'Data loss')  # if Data loss has been selected, prepare output for current method/species + projection if necessary     
      {
        done = round( (N.species*(all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps) + (i-1) * dl.reps) / (N.species*N.reps) * 100) # progress bar stuff
        prep.output(k, i, dl.reps, 'DL', done)
        if (svalue(proj.check)) prep.output(k, i, dl.reps, 'DL', done, FALSE, TRUE)        
      }
    }
  }

  # initialise means and standard deviation (and potentially projected version) variables 
  MEANS <<- vector('list', N.methods) # create one element for each method
  if (svalue(proj.check)) MEANS.PROJ <<- vector('list', N.methods)  
  SDS <<- vector('list', N.methods)
  if (svalue(proj.check)) SDS.PROJ <<- vector('list', N.methods)  
  
  for (k in 1:N.methods) # for each method     
  {
    MEANS[[k]] <<- vector('list', N.species) # create an element for each species    
    SDS[[k]] <<- vector('list', N.species)    
    if (svalue(proj.check))
    {
      MEANS.PROJ[[k]] <<- vector('list', N.species)  
      SDS.PROJ[[k]] <<- vector('list', N.species)    
    }
    
    # gather info for calculating means (namely number of replicates)
    # and for progress bar
    if (methods[k] == 'All sources')  
    {
      reps = all.error.reps
      prev = 0
    }
    else if (methods[k] == 'Observation error') 
    {
      reps = obs.error.reps
      prev = all.error.reps
    }
    else if (methods[k] == 'Environmental error') 
    {
      reps = env.error.reps
      prev = all.error.reps + obs.error.reps
    }    
    else if (methods[k] == 'Spatial bias') 
    {
      reps = sb.reps
      prev = all.error.reps + obs.error.reps + env.error.reps
    }
    else if (methods[k] == 'Model variance') 
    {
      reps = var.reps
      prev = all.error.reps + obs.error.reps + env.error.reps + sb.reps
    }
    else if (methods[k] == 'Data loss') 
    {
      reps = dl.reps
      prev = all.error.reps + obs.error.reps + env.error.reps + sb.reps + var.reps
    }
    
    for (i in 1:N.species) # for each species
    {
      n = 0 # count number of valid replicates
      np = 0
      for (j in 1:reps) # for each replicate
      {
        if (length(DATA[[k]][[i]][[j]]) > 0) # if there is data available
        {
          n = n + 1 # add to count
          if (n == 1) # if this is the first valid replicate, create the temporary raster containing the mean data, and initialise it as the first data
          {
            tmp = getValues(DATA[[k]][[i]][[j]]) # strip out a vector of data points from the current data raster
            canvas = DATA[[k]][[i]][[j]] # create a 'canvas' for overwriting with relevant data when creating rasters later
          }
          else # otherwise, just add the current replicate's data
            tmp = tmp + getValues(DATA[[k]][[i]][[j]])
          print(tmp)

          # progress bar stuff
          pc.done = round( (N.species*prev + (i-1) * reps + j/2) / (N.species*N.reps) * 100)
          setTkProgressBar(pb, pc.done, sprintf("Calculating summary data (means) %d%%", pc.done), sprintf("%d%% done", pc.done))
        }
        
        if (svalue(proj.check)) # if a projection exists, repeat the process for it
        {
          if (length(DATA.PROJ[[k]][[i]][[j]]) > 0)
          {
            np = np + 1
            if (np == 1)
            {
              tmpp = getValues(DATA.PROJ[[k]][[i]][[j]])
              canvasp = DATA.PROJ[[k]][[i]][[j]]
            }
            else
              tmpp = tmpp + getValues(DATA.PROJ[[k]][[i]][[j]])
          }        
        }
      }
      if (n > 0) # if there is at least one valid replicate
      {
        MEANS[[k]][[i]] <<- canvas # initialise relevant means info to previously defined canvas
        tmp = tmp/n # divide the data by the number of replicates
        MEANS[[k]][[i]] <<- setValues(MEANS[[k]][[i]], tmp) # re-enter this data into the newly created means variable
      }
      
      if (np > 0) # do the same for projection if replicates exist
      {
        MEANS.PROJ[[k]][[i]] <<- canvasp
        tmpp = tmpp/np
        MEANS.PROJ[[k]][[i]] <<- setValues(MEANS.PROJ[[k]][[i]], tmpp)
      }      

      # undergo a similar process to calculate the standard deviation
      n = 0  
      np = 0
      for (j in 1:reps)
      {
        if (length(DATA[[k]][[i]][[j]]) > 0)
        {
          n = n + 1
          if (n == 1)
          {
            tmp.s = (getValues(DATA[[k]][[i]][[j]]) - tmp)^2  # this time use the just-calculated means info to work out cellwise variance  (sum of squares)
            canvas = DATA[[k]][[i]][[j]]
          }            
          else
            tmp.s = tmp.s + (getValues(DATA[[k]][[i]][[j]]) - tmp)^2

          pc.done = round( (N.species*prev + (i-1) * reps + (reps + j)/2) / (N.species*N.reps) * 100)
          setTkProgressBar(pb, pc.done, sprintf("Calculating summary data (sds) %d%%", pc.done), sprintf("%d%% done", pc.done))
        }
        
        if (svalue(proj.check))        
        {        
          if (length(DATA.PROJ[[k]][[i]][[j]]) > 0)
          {
            np = np + 1
            if (np == 1)
            {
              tmp.sp = (getValues(DATA.PROJ[[k]][[i]][[j]]) - tmpp)^2
              canvasp = DATA.PROJ[[k]][[i]][[j]]
            }            
            else
              tmp.sp = tmp.sp + (getValues(DATA.PROJ[[k]][[i]][[j]]) - tmpp)^2
          }
        }
        
      }
      if (n > 1)
      {
        SDS[[k]][[i]] <<- canvas
        tmp.s = sqrt(tmp.s/(n-1)) # formula for calculating standard deviation for a sample
        SDS[[k]][[i]] <<- setValues(SDS[[k]][[i]], tmp.s)
      }
      if (np > 1)
      {
        SDS.PROJ[[k]][[i]] <<- canvas
        tmp.sp = sqrt(tmp.sp/(np-1))
        SDS.PROJ[[k]][[i]] <<- setValues(SDS.PROJ[[k]][[i]], tmp.sp)
      }
      
    }  
  }
                                                                   
  # close progress bar
  close(pb)
}










# Additional optional routines not incorporated into the GUI

autoplot = FALSE
if (autoplot) # write .pdf files for the means for each method for the first species (for both current and projected)
{
  setwd("D:/output")
  N.methods = length(methods)
  for (i in 1:N.methods) # for each method
  {
    pdf(file = paste(methods[i],".pdf",sep=""))
    plot(MEANS[[i]][[1]], axes = FALSE, main = methods[i], zlim = c(0,1))    
    dev.off()

    pdf(file = paste(methods[i]," proj.pdf",sep=""))
    plot(MEANS.PROJ[[i]][[1]], axes = FALSE, main = methods[i], zlim = c(0,1))
    dev.off()    
  }
}

plotsingle = FALSE
if (plotsingle) # once you've done a single Maxent run, plot the presence-absence as pdfs for species i
{
  i = 1
  
  pd = gsub('\\', '/', svalue(proj.dir.label), fixed = TRUE) # make sure all path symbols facing the right way
  pd = strsplit(pd, '/')[[1]]    
  pd = pd[length(pd)] # extract last bit of pathname  

  current = raster(sprintf('%s/%s.asc', outputs.dir, gsub(' ', '_', species[i])))
  projected = raster(sprintf('%s/%s_%s.asc', outputs.dir, gsub(' ', '_', species[i]), pd))

  mrname = sprintf('%s/maxentResults.csv', outputs.dir)  
  mr = read.csv(mrname)                                                        
  threshold = mr$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold

  pdf(file = paste("No uncertainty.pdf",sep=""))
  plot(current > threshold, axes = FALSE, main = 'No uncertainty')
  dev.off()                 
                            
  pdf(file = paste("No uncertainty proj.pdf",sep=""))
  plot(projected > threshold, axes = FALSE, main = 'No uncertainty')  
  dev.off()                 
}                           
                            
reproject = FALSE           
# once you've run an uncertainty suite with outputs (i.e. populated DATA and DATA.PROJ)
# this code will run the uncertainty suite again with a new projection 
# (avoids redoing the original models: instead, just projects from the ones that already exist)
if (reproject)              
{                           
  i = 1                     
  setwd("D:/output")        
  newproj = "D:/projection folder"  

  svalue(proj.dir.label) = newproj
  pd = gsub('\\', '/', newproj, fixed = TRUE) # make sure all path symbols facing the right way
  pd = strsplit(pd, '/')[[1]]    
  pd = pd[length(pd)] # extract last bit of pathname  
  N.methods = length(methods)
  for (k in 1:N.methods)
  {
    if (methods[k] == 'All sources')
      folder = 'ALL'
    else if (methods[k] == 'Observation error')
      folder = 'OE'                                                                                                         
    else if (methods[k] == 'Environmental error')
      folder = 'EE'
    else if (methods[k] == 'Spatial bias')      
      folder = 'SB'
    else if (methods[k] == 'Model variance')      
      folder = 'MV'
    else if (methods[k] == 'Data loss')      
      folder = 'DL'

    N.runs = length(DATA[[k]][[1]])
    for (j in 1:N.runs)
    {
      if (folder != 'MV')
      {
        lambda = sprintf('%s/test.%s.%d/%s.lambdas', outputs.dir, folder, j, gsub(' ', '_', species[i]))
        outfile = sprintf('%s/test.%s.%d/%s_%s', outputs.dir, folder, j, gsub(' ', '_', species[i]), pd)
      }
      else # from a CV run
      {
        lambda = sprintf('%s/test.%s/%s_%d.lambdas', outputs.dir, folder, gsub(' ', '_', species[i]), j-1)
        outfile = sprintf('%s/test.%s/%s_%d_%s', outputs.dir, folder, gsub(' ', '_', species[i]), j-1, pd)
      }
      options = NULL
      system(paste('"', java.dir, 'java" -mx4000m -cp "', maxent,
      '" density.Project "', lambda, '" "', newproj,
      '" "', outfile, '"', options, sep=''))
    }
    
    prep.output(k, i, N.runs, folder, 0, folder == 'MV', TRUE)
  }
  
  for (k in 1:N.methods) # calculate means
  {
    np = 0
    reps = length(DATA[[k]][[1]])
    for (j in 1:reps)
    {
      if (length(DATA.PROJ[[k]][[i]][[j]]) > 0)
      {
        np = np + 1
        if (np == 1)
        {
          tmpp = getValues(DATA.PROJ[[k]][[i]][[j]])
          canvasp = DATA.PROJ[[k]][[i]][[j]]
        }
        else
          tmpp = tmpp + getValues(DATA.PROJ[[k]][[i]][[j]])
      }        
    }
    
    if (np > 0)
    {
      MEANS.PROJ[[k]][[i]] = canvasp
      tmpp = tmpp/np
      MEANS.PROJ[[k]][[i]] = setValues(MEANS.PROJ[[k]][[i]], tmpp)
    }      
  }

}
