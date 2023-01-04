## ---------------------------
## [script name] 
##
## SCRIPT to modify stimuli for the localizer tasks (block design).
##
## By Shuai Wang, Oct., 2022
## ---------------------------

## clean up
rm(list=ls())
## ---------------------------


## set environment (packages, functions, working path etc.)
library('tuneR')
library('seewave')
# working path
wdir <- '/media/wang/BON/Projects/CP05/experiment_design/'
sdir <- file.path(wdir, 'stimuli')

## Read the stimuli list
fstim <- file.path(sdir, 'stimuli_task-Locs_info.csv')
if (file.exists(fstim)){
  df_stim <- read.csv(file=fstim, stringsAsFactors=FALSE)
} else {
  df_stim <- read.csv(file=file.path(sdir, 'stimuli_task-Locs_list.csv'), stringsAsFactors=FALSE)
  df_stim$stimulus[df_stim$group == 'VisRW'] <- iconv(df_stim$stimulus[df_stim$group == 'VisRW'], to='ASCII//TRANSLIT')
  df_lexq <- read.csv(file=file.path(wdir, 'Lexique383.tsv'), sep='\t', stringsAsFactors=FALSE)
  df_lexq$stimulus <- iconv(df_lexq$ortho, to='ASCII//TRANSLIT')
  df_stim <- merge(df_stim, df_lexq, by='stimulus', all.x=TRUE)
  write.csv(df_stim, file=fstim, row.names=FALSE)
}
## ---------------------------

df_locv <- df_stim[df_stim$group %in% c('VisRW', 'VisCS'),]
write.csv(df_locv, file=file.path(sdir, 'stimuli_task-LocVis.csv'), row.names=FALSE)

## Calculate audio/video duration
# Auditory localizer
tdir <- file.path(sdir, 'task-LocAud')
df_loca <- df_stim[df_stim$group %in% c('AudRW', 'AudPW', 'AudSC'),]
nwav <- dim(df_loca)[1]
for (i in 1:nwav){
  # Load up audio file
  fwav <- sprintf('%s.wav', df_loca$stimulus[i])    # file name in .wav format
  wavo <- readWave(filename=file.path(tdir, fwav))  # wave object
  # Calculate original video duration (in seconds)
  df_loca$duration[i] <- duration(wavo)                           
}
write.csv(df_loca, file=file.path(sdir, 'stimuli_task-LocAud.csv'), row.names=FALSE)
# Lip-movement localizer
tdir <- file.path(sdir, 'task-LocLip')
df_locl <- df_stim[df_stim$group == 'LipRW',]
navi <- dim(df_locl)[1]
for (i in 1:navi){
  # Load up audio file (extracted from the video file)
  fwav <- sprintf('%s.wav', df_locl$stimulus[i])    # file name in .wav format
  wavo <- readWave(filename=file.path(tdir, fwav))  # wave object
  # Calculate original video duration (in seconds)
  df_locl$duration[i] <- duration(wavo)                           
}
write.csv(df_locl, file=file.path(sdir, 'stimuli_task-LocLip.csv'), row.names=FALSE)
## ---------------------------
