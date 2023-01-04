## ---------------------------
## [script name] 
##
## SCRIPT to generate LabView script for the auditory localizer task (block-design fMRI).
## Three localizers: Visual (24 trials per block, 8 blocks), Auditory (12 trials per block, 8 blocks), Lip (6 trials per block, 8 blocks)
## Maximal duration of the word/scrambled audio is: 902ms (from 426 to 902ms)
## Maximal duration of the Lip-movement video is: 1600ms (from 1200 to 1600ms)
## Scanning: TR = 1088ms, multi-band factor = 5, N slice = 70
##
## By Shuai Wang, Oct., 2022
##
## ---------------------------

## clean up
rm(list=ls())
## ---------------------------


## set environment (packages, functions, working path etc.)
taskname <- 'task-LocAud'
# working path
wdir <- '/media/wang/BON/Projects/CP05/experiment_design/'
sdir <- file.path(wdir, 'LabView_scripts')
tdir <- file.path(sdir, taskname)
# scan parameters
trigger_duration <- 0.0777  # trigger_duration in LabView in seconds
trigger_nslice <- 14        # number of slices per trigger, which equals to N(slices) / N(multiband factors)
TR <- trigger_duration * trigger_nslice
# task parameters (in number of triggers)
nblank <- 4 * trigger_nslice  # duration of the blank at the beginning: 4 TRs
conditions <- c('AudRW', 'AudSC')
nblks <- 8   # 8 blocks per condition
ntrls <- 12  # 12 stimuli (audio) per block
stim_duration <- 11                                      # stimulus audio: 12 triggers; 854.7ms
isi_duration <- 3                                        # Inter-stimuli interval (audio): 3 triggers; 233.1ms
rest_duration <- ntrls * (stim_duration + isi_duration)  # baseline block (fixation): 168 triggers; 13053.6ms
catch_duration <- 8                                      # catch trial (object): 621.6ms plus a jitter ranged from 18 to 25 triggers (from 1398.6 to 2020.2ms) 
## ---------------------------

## generate LabView script
# read the stimuli list
df_stim <- read.csv(file=file.path(sdir, sprintf('stimuli_%s.csv', taskname)), stringsAsFactors=FALSE)
# create fMRI experiment sequence
cond_blks <- rep(conditions, nblks)
cond_base <- rep('Rest', nblks*2)
catch_trls <- sample(rep(c('none', 'catch'), nblks))  # randomize catch trials
cond_fseq <- as.vector(rbind(cond_blks, cond_base, catch_trls))  # combine blocks and trials
# create LabView script (.desc) row-wise, time unit is trigger_duration
loc_desc <- data.frame(CONDITION='BlankStart', DURATION=nblank, STIM_TEXT='+', STIM_BITMAP='BLANK', STIM_WAV='', RESPONSE_BUTTON=0)
loc_RW <- df_stim[df_stim$group == 'AudRW', 'stimulus']
loc_RW <- loc_RW[sample(1:length(loc_RW))]  # randomize the list of stimuli
loc_SC <- df_stim[df_stim$group == 'AudSC', 'stimulus']
loc_SC <- loc_SC[sample(1:length(loc_SC))]  # randomize the list of stimuli
for (loc_tag in cond_fseq){
  loc_condition <- rep(loc_tag, ntrls*2)
  loc_duration  <- rep(c(stim_duration, isi_duration), ntrls)
  switch(loc_tag,
    AudRW={
      loc_desc_wav <- as.vector(rbind(loc_RW[1:ntrls], rep('', ntrls)))
      loc_desc_tmp <- data.frame(CONDITION=loc_condition, DURATION=loc_duration, STIM_TEXT=rep('+', ntrls*2),
                                 STIM_BITMAP='BLANK', STIM_WAV=loc_desc_wav, RESPONSE_BUTTON=rep(0, ntrls*2))
      loc_RW <- loc_RW[-c(1:ntrls)]
    },
    AudSC={
      loc_desc_wav <- as.vector(rbind(loc_SC[1:ntrls], rep('', ntrls)))
      loc_desc_tmp <- data.frame(CONDITION=loc_condition, DURATION=loc_duration, STIM_TEXT=rep('+', ntrls*2),
                                 STIM_BITMAP='BLANK', STIM_WAV=loc_desc_wav, RESPONSE_BUTTON=rep(0, ntrls*2))
      loc_SC <- loc_SC[-c(1:ntrls)]
    },   
    Rest={
      loc_desc_tmp <- data.frame(CONDITION=loc_tag, DURATION=rest_duration, STIM_TEXT='+', STIM_BITMAP='BLANK', STIM_WAV='', RESPONSE_BUTTON=0)     
    },
    catch={
      catch_jitter <- sample(18:25)[1]
      loc_desc_tmp <- data.frame(CONDITION=rep(loc_tag, 2), DURATION=c(catch_duration, catch_jitter), STIM_TEXT=c('+', '+'), 
                                 STIM_BITMAP='BLANK', STIM_WAV=c('BEEP', ''), RESPONSE_BUTTON=c(2, 0))
    },
    none={ next }
  )
  loc_desc <- rbind(loc_desc, loc_desc_tmp)
}
# summarize ITI
loc_desc_iti <- loc_desc$DURATION[loc_desc$CONDITION == 'ITI']
# compensate TRs
ntriggers <- sum(as.numeric(loc_desc$DURATION))  # if DURATION column is 0 for audio, then + sum(as.numeric(loc_desc$DURATION.S))
nTR <- ceiling(ntriggers / trigger_nslice)  # compensate the number of TRs as an integer
trigger_add <- nTR * trigger_nslice - ntriggers
loc_desc$DURATION[length(loc_desc$DURATION)] <- as.numeric(loc_desc$DURATION[length(loc_desc$DURATION)]) + trigger_add  # prolong the last ITI
# save R data
save(list=ls(), file=file.path(tdir, sprintf('Stimulation_%s-seq1.Rdata', taskname)))
# output timings
sink(file=file.path(tdir, sprintf('Stimulation_%s-seq1.stat', taskname)), append=FALSE)
  writeLines(sprintf('The number of TRs is %.0f (the initial number is %.2f).', nTR, ntriggers/trigger_nslice))
  writeLines(sprintf('Each TR lasts for %.3f seconds and has %.0f triggers, each trigger lasts for %.3f seconds.\n', TR, trigger_nslice, trigger_duration))
  writeLines(sprintf('The number of blocks is: %.0f, each block lasts for %.0f triggers or %.3f seconds and has %.0f trials.', 
                     nblks, rest_duration, rest_duration*trigger_duration, ntrls))
  cat(cond_fseq, '\n')
  writeLines(sprintf('A single trial contains:'))
  writeLines(sprintf('A stimulus (Talking/Still Lip: %.0f triggers or %.3f seconds)', stim_duration, stim_duration*trigger_duration))
  writeLines(sprintf('An ISI (%.0f triggers or %.3f seconds).\n', isi_duration, isi_duration*trigger_duration))
  writeLines(sprintf('The total scanning time should be %.3f seconds or %.4f minutes.', nTR*TR, nTR*TR/60))
sink()
# output LabView .desc
write.table(loc_desc, file=file.path(tdir, sprintf('Stimulation_%s-seq1.desc', taskname)), sep='\t', row.names=FALSE, quote=FALSE)  # UTF-8
write.table(loc_desc, file=file.path(tdir, sprintf('Stimulation_%s-seqFr1.desc', taskname)), sep='\t', row.names=FALSE, quote=FALSE, fileEncoding='latin1')  # ISO-8859-1 

