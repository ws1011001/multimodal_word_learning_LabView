## ---------------------------
## [script name] 
##
## SCRIPT to generate LabView script for the word-picture matching task (event-related design).
## Word-picture matching task (event-related design):
## Stimuli: 15x3 learnt pseudowords (L1, L2, L3; one list per training method), 45 matching pictures, 45 non-matching pictures
## Single trial:
## Design: Fixation --> Red Fixation (100ms) -- > Word (403-645ms; M=547ms) --> ISI (500ms) --> Picture/Response window (2500ms) --> ITI
## Script: Red Fixation (100ms) -- > Word (>645ms + 500ms) --> Pic./Resp. (2500ms) --> ITI (500~1700ms)
## Scanning: TR = 1088ms, multi-band factor = 5, N slice = 70
##
## By Shuai Wang, Oct., 2022
## ---------------------------

## clean up
rm(list=ls())
## ---------------------------


## set environment (packages, functions, working path etc.)
taskname <- 'task-WPM'
# working path
wdir <- '/media/wang/BON/Projects/CP05/experiment_design/'
sdir <- file.path(wdir, 'LabView_scripts')
tdir <- file.path(sdir, taskname)
# scan parameters
trigger_duration <- 0.0777  # trigger_duration in LabView in seconds
trigger_nslice <- 14       # number of slices per trigger, which equals to N(slices) / N(multiband factors)
TR <- trigger_duration * trigger_nslice
# task parameters
conditions <- c('RWmch', 'RWmis', 'L1mch', 'L2mch', 'L3mch', 'L1mis', 'L2mis', 'L3mis')
ncond <- length(conditions)
nruns <- 2  # 2 or 3 repeated runs
nTR <- 560  # make each run having the same number of TRs
nblank <- 4 * trigger_nslice  # duration of the blank at the beginning: 4 TRs
ntrls <- 15 * ncond  # 15 stimuli per condition
pre_duration <- ceiling(0.1 / trigger_duration)             # red fixation: 2 triggers; 155.4ms
word_duration <- ceiling((0.645 + 0.5) / trigger_duration)  # stimulus (depends on the maximal duration of the sounds) + ISI: 15 triggers; 1165.5ms
resp_duration <- ceiling(2.5 / trigger_duration)            # picture/response window: 33 triggers; 2564.1ms
iti_duration <- 7:21                                        # ITI: from 543.9ms to 1631.7ms; from 3108ms to 4195.8ms including the response window
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
  irun_RWmch <- df_stim[df_stim$group == 'RW',]  # 15 real words
  irun_L1mch <- df_stim[df_stim$group == 'L1',]  # 15 learnt pseudowords as group L1
  irun_L2mch <- df_stim[df_stim$group == 'L2',]  # 15 learnt pseudowords as group L2
  irun_L3mch <- df_stim[df_stim$group == 'L3',]  # 15 learnt pseudowords as group L3 
  irun_RWmis <- df_stim[df_stim$group == 'RW',]  # 15 real words 
  irun_L1mis <- df_stim[df_stim$group == 'L1',]  # 15 learnt pseudowords as group L1
  irun_L2mis <- df_stim[df_stim$group == 'L2',]  # 15 learnt pseudowords as group L2
  irun_L3mis <- df_stim[df_stim$group == 'L3',]  # 15 learnt pseudowords as group L3  
  # assign stimuli to avoid repeating
  isRepeat <- TRUE
  while (isRepeat){
    isRepeat <- FALSE
    irun_RWmch_seq <- as.integer(sample(1:(ntrls / ncond)))
    irun_L1mch_seq <- as.integer(sample(1:(ntrls / ncond)) + ntrls / ncond)  #sample(1:as.integer(ntrls/ncond))
    irun_L2mch_seq <- as.integer(sample(1:(ntrls / ncond)) + ntrls / ncond * 2)  #sample(1:as.integer(ntrls/ncond)) + as.integer(ntrls/ncond)   
    irun_L3mch_seq <- as.integer(sample(1:(ntrls / ncond)) + ntrls / ncond * 3)  #sample(1:as.integer(ntrls/ncond)) + as.integer(ntrls/ncond*2)      
    irun_RWmis_seq <- as.integer(sample(1:(ntrls / ncond)))
    irun_L1mis_seq <- as.integer(sample(1:(ntrls / ncond)) + ntrls / ncond)
    irun_L2mis_seq <- as.integer(sample(1:(ntrls / ncond)) + ntrls / ncond * 2)
    irun_L3mis_seq <- as.integer(sample(1:(ntrls / ncond)) + ntrls / ncond * 3)  
    irun_stim_seq <- rep(0, ntrls)
    irun_stim_seqs <- data.frame(RWmch=irun_RWmch_seq, L1mch=irun_L1mch_seq - as.integer(ntrls/ncond), L2mch=irun_L2mch_seq - as.integer(ntrls/ncond*2), L3mch=irun_L3mch_seq - as.integer(ntrls/ncond*3),
                                 RWmis=irun_RWmis_seq, L1mis=irun_L1mis_seq - as.integer(ntrls/ncond), L2mis=irun_L2mis_seq - as.integer(ntrls/ncond*2), L3mis=irun_L3mis_seq - as.integer(ntrls/ncond*3))
    for (i in 1:ntrls){
      iseq <- irun_fseq[i]
      switch(iseq,
        RWmch={ irun_stim_seq[i] <- irun_RWmch_seq[1]; irun_RWmch_seq <- irun_RWmch_seq[-1] },
        L1mch={ irun_stim_seq[i] <- irun_L1mch_seq[1]; irun_L1mch_seq <- irun_L1mch_seq[-1] },
        L2mch={ irun_stim_seq[i] <- irun_L2mch_seq[1]; irun_L2mch_seq <- irun_L2mch_seq[-1] },
        L3mch={ irun_stim_seq[i] <- irun_L3mch_seq[1]; irun_L3mch_seq <- irun_L3mch_seq[-1] },
        RWmis={ irun_stim_seq[i] <- irun_RWmis_seq[1]; irun_RWmis_seq <- irun_RWmis_seq[-1] },       
        L1mis={ irun_stim_seq[i] <- irun_L1mis_seq[1]; irun_L1mis_seq <- irun_L1mis_seq[-1] },
        L2mis={ irun_stim_seq[i] <- irun_L2mis_seq[1]; irun_L2mis_seq <- irun_L2mis_seq[-1] },
        L3mis={ irun_stim_seq[i] <- irun_L3mis_seq[1]; irun_L3mis_seq <- irun_L3mis_seq[-1] }
      )
    }
    # Check repeats
    cat(sprintf('\nCheck the present seq:\n'), as.character(irun_stim_seq))
    for (i in 1:(ntrls-3)){
      if (length(unique(irun_stim_seq[i:(i+3)])) != 4){ 
        cat(sprintf('\nStop at %d due to the seq:\n', i), as.character(irun_stim_seq[i:(i+3)]))
        isRepeat <- TRUE
        break  
      }
    }
  }
  # Extract the sequences of stimuli
  irun_RWmch_seq <- irun_stim_seqs$RWmch 
  irun_L1mch_seq <- irun_stim_seqs$L1mch
  irun_L2mch_seq <- irun_stim_seqs$L2mch
  irun_L3mch_seq <- irun_stim_seqs$L3mch
  irun_RWmis_seq <- irun_stim_seqs$RWmis 
  irun_L1mis_seq <- irun_stim_seqs$L1mis
  irun_L2mis_seq <- irun_stim_seqs$L2mis
  irun_L3mis_seq <- irun_stim_seqs$L3mis
  # write script line by line
  irun_desc <- data.frame(CONDITION='BlankStart', DURATION=nblank, STIM_TEXT='+', MISC_TEXTCOLOR='255,255,255', STIM_BITMAP='BLANK', STIM_WAV='', RESPONSE_BUTTON=0)
  for (irun_tag in irun_fseq){
    irun_iti <- sample(iti_duration)[1]
    irun_condition      <- c('Preload', irun_tag, 'RespWindow', 'ITI')
    irun_duration       <- c(pre_duration, word_duration, resp_duration, irun_iti)
    irun_text           <- c('+', '', '', '+')  # ITI has a noraml (white) fixation; Preload may have a red fixation (preserved)
    irun_text_color     <- c('255,255,255', '0,0,0', '0,0,0', '255,255,255')
    switch(irun_tag,
       RWmch={
        irun_RWmch_stim <- irun_RWmch_seq[1]  #sample(1:dim(irun_RWmch)[1])[1]
        irun_RWmch_seq <- irun_RWmch_seq[-1]
        irun_desc_wav <- c('', irun_RWmch[irun_RWmch_stim, 'stimulus'], '', '')
        irun_desc_img <- c('BLANK', 'BLANK', irun_RWmch[irun_RWmch_stim, 'stimulus'], 'BLANK')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP=irun_desc_img, STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 0, 2, 0))  # 5-key keyboard: 2 is index (match), 3 is middle
        #irun_RWmch <- irun_RWmch[-irun_RWmch_stim,]
      },          
      L1mch={
        irun_L1mch_stim <- irun_L1mch_seq[1]  #sample(1:dim(irun_L1mch)[1])[1]
        irun_L1mch_seq <- irun_L1mch_seq[-1]
        irun_desc_wav <- c('', irun_L1mch[irun_L1mch_stim, 'stimulus'], '', '')
        irun_desc_img <- c('BLANK', 'BLANK', irun_L1mch[irun_L1mch_stim, 'stimulus'], 'BLANK')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP=irun_desc_img, STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 0, 2, 0))  # 5-key keyboard: 2 is index (match), 3 is middle
        #irun_L1mch <- irun_L1mch[-irun_L1mch_stim,]
      },
      L2mch={
        irun_L2mch_stim <- irun_L2mch_seq[1]  #sample(1:dim(irun_L2mch)[1])[1]
        irun_L2mch_seq <- irun_L2mch_seq[-1]
        irun_desc_wav <- c('', irun_L2mch[irun_L2mch_stim, 'stimulus'], '', '')
        irun_desc_img <- c('BLANK', 'BLANK', irun_L2mch[irun_L2mch_stim, 'stimulus'], 'BLANK')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP=irun_desc_img, STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 0, 2, 0))
        #irun_L2mch <- irun_L2mch[-irun_L2mch_stim,]
      },     
      L3mch={
        irun_L3mch_stim <- irun_L3mch_seq[1]  #sample(1:dim(irun_L3mch)[1])[1]
        irun_L3mch_seq <- irun_L3mch_seq[-1]
        irun_desc_wav <- c('', irun_L3mch[irun_L3mch_stim, 'stimulus'], '', '')
        irun_desc_img <- c('BLANK', 'BLANK', irun_L3mch[irun_L3mch_stim, 'stimulus'], 'BLANK')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP=irun_desc_img, STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 0, 2, 0))
        #irun_L3mch <- irun_L3mch[-irun_L3mch_stim,]
      },     
      RWmis={
        irun_RWmis_stim <- irun_RWmis_seq[1]  #sample(1:dim(irun_RWmis)[1])[1]
        irun_RWmis_seq <- irun_RWmis_seq[-1]
        irun_desc_wav <- c('', irun_RWmis[irun_RWmis_stim, 'stimulus'], '', '')
        irun_desc_img <- c('BLANK', 'BLANK', irun_RWmis[irun_RWmis_stim, 'object_mismatch'], 'BLANK')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color, 
                                    STIM_BITMAP=irun_desc_img, STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 0, 3, 0))  # 5-key keyboard: 2 is index (match), 3 is middle
        #irun_RWmis <- irun_RWmis[-irun_RWmis_stim,]
      },     
      L1mis={
        irun_L1mis_stim <- irun_L1mis_seq[1]  #sample(1:dim(irun_L1mis)[1])[1]
        irun_L1mis_seq <- irun_L1mis_seq[-1]
        irun_desc_wav <- c('', irun_L1mis[irun_L1mis_stim, 'stimulus'], '', '')
        irun_desc_img <- c('BLANK', 'BLANK', irun_L1mis[irun_L1mis_stim, 'object_mismatch'], 'BLANK')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP=irun_desc_img, STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 0, 3, 0))
        #irun_L1mis <- irun_L1mis[-irun_L1mis_stim,]
      }, 
      L2mis={
        irun_L2mis_stim <- irun_L2mis_seq[1]  #sample(1:dim(irun_L2mis)[1])[1]
        irun_L2mis_seq <- irun_L2mis_seq[-1]
        irun_desc_wav <- c('', irun_L2mis[irun_L2mis_stim, 'stimulus'], '', '')
        irun_desc_img <- c('BLANK', 'BLANK', irun_L2mis[irun_L2mis_stim, 'object_mismatch'], 'BLANK')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP=irun_desc_img, STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 0, 3, 0))
        #irun_L2mis <- irun_L2mis[-irun_L2mis_stim,]
      },     
      L3mis={
        irun_L3mis_stim <- irun_L3mis_seq[1]  #sample(1:dim(irun_L3mis)[1])[1]
        irun_L3mis_seq <- irun_L3mis_seq[-1]
        irun_desc_wav <- c('', irun_L3mis[irun_L3mis_stim, 'stimulus'], '', '')
        irun_desc_img <- c('BLANK', 'BLANK', irun_L3mis[irun_L3mis_stim, 'object_mismatch'], 'BLANK')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP=irun_desc_img, STIM_WAV=irun_desc_wav, RESPONSE_BUTTON=c(0, 0, 3, 0))
        #irun_L3mis <- irun_L3mis[-irun_L3mis_stim,]
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
    writeLines(sprintf('A stimulus (%.0f triggers or %.3f seconds)', word_duration, word_duration*trigger_duration))
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