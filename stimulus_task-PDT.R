## ---------------------------
## [script name] 
##
## SCRIPT to modify stimuli for the phoneme decision task (event-related design).
## Phoneme decision task (event-related design):
## Stimuli: 30 word orthographics (Ortho), 30 word sound (Audio), 30 word lip-movement (Video), 5 Go trials each condition (OrthoGo, AudioGo, VideoGo)
##
## By Shuai Wang, Oct., 2022
## ---------------------------

## clean up
rm(list=ls())
## ---------------------------


## set environment (packages, functions, working path etc.)
library('tuneR')
library('seewave')
taskname <- 'task-PDT'
# working path
wdir <- '/media/wang/BON/Projects/CP05/experiment_design/'
sdir <- file.path(wdir, 'stimuli')
tdir <- file.path(sdir, taskname)

## Read the stimuli list
fstim <- file.path(sdir, sprintf('stimuli_%s_info.csv', taskname))
if (file.exists(fstim)){
  df_stim <- read.csv(file=fstim, stringsAsFactors=FALSE)
} else {
  df_stim <- read.csv(file=file.path(sdir, sprintf('stimuli_%s_list.csv', taskname)), stringsAsFactors=FALSE)
  df_lexq <- read.csv(file=file.path(wdir, 'Lexique383.tsv'), sep='\t', stringsAsFactors=FALSE)
  df_lexq$stimulus <- iconv(df_lexq$ortho, to='ASCII//TRANSLIT')
  df_stim <- merge(df_stim, df_lexq, by='stimulus')
  write.csv(df_stim, file=fstim, row.names=FALSE)
}
nstim <- dim(df_stim)[1]
## ---------------------------

## Calculate original duration and cut off sound waves
for (i in 1:nstim){
  # Load up audio file
  fwav <- sprintf('%s.wav', df_stim$stimulus[i])    # file name in .wav format
  wavo <- readWave(filename=file.path(tdir, fwav))  # wave object
  # Calculate original video duration (in seconds)
  df_stim$video_duration[i] <- duration(wavo)                           
  cat(sprintf('The original video duration of %s is %.3f seconds.', df_stim$stimulus[i], df_stim$video_duration[i]), '\n')
  # show acoustic onset and offset (in seconds)
  wav_onset <- df_stim$audio_onset[i] / 1000
  wav_offset <- df_stim$audio_offset[i] / 1000
  # cut off pre-onset and post-offset
  wav1 <- cutw(wavo, channel=1, from=wav_onset, to=wav_offset, output='Wave')
  wav2 <- cutw(wavo, channel=2, from=wav_onset, to=wav_offset, output='Wave') 
  wavn <- stereo(wav1, wav2)
  df_stim$audio_duration[i] <- duration(wavn)
  cat(sprintf('The audio duration of %s is %.3f seconds.', df_stim$stimulus[i], df_stim$audio_duration[i]), '\n') 
  writeWave(wavn, filename=file.path(tdir, sprintf('%s_cut.wav', df_stim$stimulus[i])))
  # output sound wave plot
  png(filename=file.path(tdir, sprintf('%s_cut_wave.png', df_stim$stimulus[i])))
    cutw(wavo, from=wav_onset, to=wav_offset, plot=TRUE)
  dev.off()
}
## ---------------------------

write.csv(df_stim, file=file.path(sdir, sprintf('stimuli_%s.csv', taskname)), row.names=FALSE)