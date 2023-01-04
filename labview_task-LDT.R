## ---------------------------
## [script name] 
##
## SCRIPT to generate LabView script for the lexical decision task (event-related design fMRI).
## Lexical decision task (event-related design):
## Stimuli: 15 words (RW), 15 non-learnt pseudowords (PW), 15x3 learnt pseudowords (L1, L2, L3; one list per training method)
## Single trial:
## Design: Fixation --> Red Fixation (100ms) -- > Stim (403-645ms; M=540ms) --> Response window (2500ms) --> ITI
## Script: Red Fixation (100ms) -- > Stim (>645ms) --> ITI (3000~4200ms)
## Scanning: TR = 1088ms, multi-band factor = 5, N slice = 70
##
## By Shuai Wang, Oct., 2022
## ---------------------------

## clean up
rm(list=ls())
## ---------------------------


## set environment (packages, functions, working path etc.)
taskname <- 'task-LDT'
# working path
wdir <- '/media/wang/BON/Projects/CP05/experiment_design/'
sdir <- file.path(wdir, 'LabView_scripts')
tdir <- file.path(sdir, taskname)
# scan parameters
trigger_duration <- 0.0777  # trigger_duration in LabView in seconds
trigger_nslice <- 14       # number of slices per trigger, which equals to N(slices) / N(multiband factors)
TR <- trigger_duration * trigger_nslice
# task parameters
nruns <- 2  # 2 or 3 repeated runs
nTR <- 320  # make each run having the same number of TRs
nblank <- 4 * trigger_nslice  # duration of the blank at the beginning: 4 TRs
conditions <- c('RW', 'PW', 'L1', 'L2', 'L3')
ncond <- length(conditions)
ntrls <- 75  # 75 stimuli per run
pre_duration <- ceiling(0.1 / trigger_duration)     # red fixation: 2 triggers; 155.4ms
stim_duration <- ceiling(0.645 / trigger_duration)  # stimulus (depends on the maximal duration of the sounds): 9 triggers; 699.3ms
resp_duration <- ceiling(2.5 / trigger_duration)    # response window: 33 triggers; 2564.1ms
iti_duration <- 7:21                                # ITI: from 543.9ms to 1631.7ms; from 3108ms to 4195.8ms including the response window
## ---------------------------

## generate LabView script
# read the individual stimuli list
df_stim <- read.csv(file=file.path(sdir, sprintf('stimuli_%s.csv', taskname)), stringsAsFactors=FALSE)
# create fMRI experiment sequence
cond_seqs <- list(NA)
for (irun in 1:nruns){
  cond_reps <- 0
  while (cond_reps != ntrls){
    # randomized but no repeated conditions
    cond_trls <- as.vector(replicate(ntrls/ncond, sample(conditions)))
    cond_reps <- length(rle(cond_trls)$length)
  }
  cond_seqs[[irun]] <- cond_trls
}
# create LabView script (.desc) row-wise for each run, time unit is trigger_duration
for (irun in 1:nruns){
  irun_fseq <- cond_seqs[[irun]]                     # condition sequence for this run
  irun_RW <- df_stim[df_stim$group == 'RW',]  # 15 real words
  irun_PW <- df_stim[df_stim$group == 'PW',]  # 15 non-learnt pseudowords
  irun_L1 <- df_stim[df_stim$group == 'L1',]  # 15 learnt pseudowords as group L1
  irun_L2 <- df_stim[df_stim$group == 'L2',]  # 15 learnt pseudowords as group L2
  irun_L3 <- df_stim[df_stim$group == 'L3',]  # 15 learnt pseudowords as group L3 
  # write script line by line
  irun_desc <- data.frame(CONDITION='BlankStart', DURATION=nblank, STIM_TEXT='+', MISC_TEXTCOLOR='255,255,255', STIM_BITMAP='BLANK', STIM_WAV='', RESPONSE_BUTTON=0)
  for (irun_tag in irun_fseq){
    irun_iti <- sample(iti_duration)[1]
    irun_condition      <- c('Preload', irun_tag, 'RespWindow', 'ITI')
    irun_duration       <- c(pre_duration, stim_duration, resp_duration, irun_iti)
    irun_text           <- c('+', '', '', '+')  # ITI has a normal (white) fixation; Preload may have a red fixation (preserved)
    irun_text_color     <- c('255,255,255', '0,0,0', '0,0,0', '255,255,255')    
    switch(irun_tag,
      RW={
        irun_RW_stim <- sample(1:dim(irun_RW)[1])[1]
        irun_desc_wav <- c('', irun_RW[irun_RW_stim, 'stimulus'], '', '')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP='BLANK', STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 2, 0, 0))  # 5-key keyboard: 2 is index (True word), 3 is middle
        irun_RW <- irun_RW[-irun_RW_stim,]
      },
      PW={
        irun_PW_stim <- sample(1:dim(irun_PW)[1])[1]
        irun_desc_wav <- c('', irun_PW[irun_PW_stim, 'stimulus'], '', '')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP='BLANK', STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 3, 0, 0))       
        irun_PW <- irun_PW[-irun_PW_stim,]
      },
      L1={
        irun_L1_stim <- sample(1:dim(irun_L1)[1])[1]
        irun_desc_wav <- c('', irun_L1[irun_L1_stim, 'stimulus'], '', '')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP='BLANK', STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 2, 0, 0))       
        irun_L1 <- irun_L1[-irun_L1_stim,]
      },          
      L2={
        irun_L2_stim <- sample(1:dim(irun_L2)[1])[1]
        irun_desc_wav <- c('', irun_L2[irun_L2_stim, 'stimulus'], '', '')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP='BLANK', STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 2, 0, 0))       
        irun_L2 <- irun_L2[-irun_L2_stim,]
      },
      L3={
        irun_L3_stim <- sample(1:dim(irun_L3)[1])[1]
        irun_desc_wav <- c('', irun_L3[irun_L3_stim, 'stimulus'], '', '')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP='BLANK', STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 2, 0, 0))       
        irun_L3 <- irun_L3[-irun_L3_stim,]
      }
    )
    irun_desc <- rbind(irun_desc, irun_desc_tmp)
  } 
  # summarize ITI
  irun_dec_iti <- irun_desc$DURATION[irun_desc$CONDITION == 'ITI']
  # compensate TRs
  ntriggers <- sum(as.numeric(irun_desc$DURATION))  # if DURATION column is 0 for audio, then + sum(as.numeric(irun_desc$DURATION.S))
  trigger_add <- nTR * trigger_nslice - ntriggers
  irun_desc$DURATION[length(irun_desc$DURATION)] <- as.numeric(irun_desc$DURATION[length(irun_desc$DURATION)]) + trigger_add  # prolong the last ITI
  # save R data
  save(list=ls(), file=file.path(tdir, sprintf('Stimulation_%s-seq%d.Rdata', taskname, irun)))
  # output timings
  sink(file=file.path(tdir, sprintf('Stimulation_%s-seq%d.stat', taskname, irun)), append=FALSE)
    writeLines(sprintf('For RUN # %s:', irun))     
    writeLines(sprintf('The number of TRs is %.0f (the initial number is %.2f).', nTR, ntriggers/trigger_nslice))
    writeLines(sprintf('Each TR lasts for %.3f seconds and has %.0f triggers, each trigger lasts for %.3f seconds.\n', TR, trigger_nslice, trigger_duration))
    writeLines(sprintf('The number of trials is: %.0f', ntrls))
    cat(irun_fseq, '\n')
    writeLines(sprintf('A single trial contains:'))
    writeLines(sprintf('A pre-load window (%.0f triggers or %.3f seconds)', pre_duration, pre_duration*trigger_duration))
    writeLines(sprintf('A stimulus (%.0f triggers or %.3f seconds)', stim_duration, stim_duration*trigger_duration))
    writeLines(sprintf('A response window (%.0f triggers or %.3f seconds)', resp_duration, resp_duration*trigger_duration)) 
    writeLines(sprintf('An ITI (Mean=%.2f, STD=%.2f, MIN=%.0f, MAX=%.0f triggers).\n', mean(irun_dec_iti), sd(irun_dec_iti), min(irun_dec_iti), max(irun_dec_iti)))
    writeLines(sprintf('The total scanning time should be %.3f seconds or %.4f minutes.', nTR*TR, nTR*TR/60))
  sink()
  # output LabView .desc
  write.table(irun_desc, file=file.path(tdir, sprintf('Stimulation_%s-seq%d.desc', taskname, irun)),
              sep='\t', row.names=FALSE, quote=FALSE)  # UTF-8
  write.table(irun_desc, file=file.path(tdir, sprintf('Stimulation_%s-seqFr%d.desc', taskname, irun)),
              sep='\t', row.names=FALSE, quote=FALSE, fileEncoding='latin1')  # ISO-8859-1 
}
## ---------------------------