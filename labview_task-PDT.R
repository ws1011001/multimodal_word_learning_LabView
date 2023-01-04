## ---------------------------
## [script name] 
##
## SCRIPT to generate LabView script for the phoneme decision task (event-related design fMRI).
## Phoneme decision task (event-related design):
## Stimuli: 35 word orthographics (Ortho), 35 word sound (Audio), 35 word lip-movement (Video), 8 Go trials each condition (OrthoGo, AudioGo, VideoGo)
## Single trial:
## Design: Fixation --> Red Fixation (100ms) -- > Stim (Ortho=600ms; Audio<663ms; Video<1680ms) --> Response window (2000ms) --> ITI
## Script: Red Fixation (100ms) -- > Stim --> ITI (3000~4200ms)
## Scanning: TR = 1088ms, multi-band factor = 5, N slice = 70 
##
## By Shuai Wang, Oct., 2022
## ---------------------------

## clean up
rm(list=ls())
## ---------------------------


## set environment (packages, functions, working path etc.)
taskname <- 'task-PDT'
# working path
wdir <- '/media/wang/BON/Projects/CP05/experiment_design/'
sdir <- file.path(wdir, 'LabView_scripts')
tdir <- file.path(sdir, taskname)
# scan parameters
trigger_duration <- 0.0777  # trigger_duration in LabView in seconds  
trigger_nslice <- 14       # number of slices per trigger, which equals to N(slices) / N(multiband factors)
TR <- trigger_duration * trigger_nslice
# task parameters
nruns <- 1  # 2 or 3 repeated runs
nTR <- 580  # make each run having the same number of TRs
nblank <- 4 * trigger_nslice  # duration of the blank at the beginning: 4 TRs
conditions <- c('Ortho', 'Audio', 'Video')
ncond <- length(conditions)
ntrls <- 35 * ncond  # 90 NoGo stimuli per run
conditions_go <- c('OrthoGo', 'AudioGo', 'VideoGo')
ncond_go <- length(conditions_go)
ntrls_go <- 8 * ncond
pre_duration <- ceiling(0.1 / trigger_duration)    # red fixation: 2 triggers; 155.4ms
txt_duration <- ceiling(0.6 / trigger_duration)    # stimulus text (600ms): 8 triggers; 621.6ms
wav_duration <- ceiling(0.663 / trigger_duration)  # stimulus audio (663ms): 9 triggers; 699.3ms
avi_duration <- ceiling(1.68 / trigger_duration)   # stimulus video (>1680ms): 22 triggers; 1709.4ms
resp_duration <- ceiling(2.5 / trigger_duration)   # response window: 33 triggers; 2564.1ms
iti_duration <- 7:21                               # ITI: from 543.9ms to 1631.7ms; from 3108ms to 4195.8ms including the response window
## ---------------------------

## generate LabView script
# read the individual stimuli list
df_stim <- read.csv(file=file.path(sdir, sprintf('stimuli_%s.csv', taskname)), stringsAsFactors=FALSE)
# create fMRI experiment sequence
cond_seqs <- list(NA)
for (irun in 1:nruns){
  cond_reps <- 0
  while (cond_reps != ntrls){
    # Randomized and no repeated conditions
    cond_trls <- as.vector(replicate(ntrls/ncond, sample(conditions)))
    cond_reps <- length(rle(cond_trls)$length)
  }
  # Insert Go trials randomly
  cond_trls_go <- as.vector(replicate(ntrls_go/ncond_go, sample(conditions_go)))
  for (i in 1:ntrls_go){
    idx_go <- sample(seq(cond_trls), 1)
    while (any(cond_trls[c(idx_go, idx_go+1)] %in% conditions_go)){
      idx_go <- sample(seq(cond_trls), 1)     
    }
    cond_trls <- c(cond_trls[1:idx_go], cond_trls_go[i], cond_trls[-(1:idx_go)])
  }
  # Output sequence
  cond_seqs[[irun]] <- cond_trls
}
# create LabView script (.desc) row-wise for each run, time unit is trigger_duration
for (irun in 1:nruns){
  irun_fseq <- cond_seqs[[irun]]                   # condition sequence for this run
  irun_ortho <- df_stim[df_stim$group == 'NoGo',]  # 35 word orthographics
  irun_audio <- df_stim[df_stim$group == 'NoGo',]  # 35 word audio
  irun_video <- df_stim[df_stim$group == 'NoGo',]  # 35 word lip-movement
  irun_ortGo <- df_stim[df_stim$group == 'Go',]  # Go trials
  irun_audGo <- df_stim[df_stim$group == 'Go',]
  irun_vidGo <- df_stim[df_stim$group == 'Go',] 
  # assign stimuli to avoid repeating
  isRepeat <- TRUE
  while (isRepeat){
    isRepeat <- FALSE
    irun_ortho_seq <- sample(1:as.integer(ntrls/ncond))
    irun_audio_seq <- sample(1:as.integer(ntrls/ncond))   
    irun_video_seq <- sample(1:as.integer(ntrls/ncond))      
    irun_ortGo_seq <- sample(1:as.integer(ntrls_go/ncond_go)) + as.integer(ntrls/ncond)
    irun_audGo_seq <- sample(1:as.integer(ntrls_go/ncond_go)) + as.integer(ntrls/ncond)
    irun_vidGo_seq <- sample(1:as.integer(ntrls_go/ncond_go)) + as.integer(ntrls/ncond)
    irun_stim_seq <- rep(0, ntrls+ntrls_go)
    irun_nogo_seqs <- data.frame(ortho=irun_ortho_seq, audio=irun_audio_seq, video=irun_video_seq)
    irun_go_seqs <- data.frame(ortGo=irun_ortGo_seq - as.integer(ntrls/ncond), audGo=irun_audGo_seq - as.integer(ntrls/ncond), vidGo=irun_vidGo_seq - as.integer(ntrls/ncond))   
    for (i in 1:(ntrls+ntrls_go)){
      iseq <- irun_fseq[i]
      switch(iseq,
             Ortho={ irun_stim_seq[i] <- irun_ortho_seq[1]; irun_ortho_seq <- irun_ortho_seq[-1] },
             Audio={ irun_stim_seq[i] <- irun_audio_seq[1]; irun_audio_seq <- irun_audio_seq[-1] },
             Video={ irun_stim_seq[i] <- irun_video_seq[1]; irun_video_seq <- irun_video_seq[-1] },
             OrthoGo={ irun_stim_seq[i] <- irun_ortGo_seq[1]; irun_ortGo_seq <- irun_ortGo_seq[-1] },
             AudioGo={ irun_stim_seq[i] <- irun_audGo_seq[1]; irun_audGo_seq <- irun_audGo_seq[-1] },
             VideoGo={ irun_stim_seq[i] <- irun_vidGo_seq[1]; irun_vidGo_seq <- irun_vidGo_seq[-1] }
      )
    }
    # Check repeats
    cat(sprintf('\nCheck the present seq:\n'), as.character(irun_stim_seq))
    for (i in 1:(ntrls+ntrls_go-3)){
      if (length(unique(irun_stim_seq[i:(i+3)])) != 4){ 
        cat(sprintf('\nStop at %d due to the seq:\n', i), as.character(irun_stim_seq[i:(i+3)]))
        isRepeat <- TRUE
        break  
      }
    }
  }
  # Extract the sequences of stimuli
  irun_ortho_seq <- irun_nogo_seqs$ortho
  irun_audio_seq <- irun_nogo_seqs$audio
  irun_video_seq <- irun_nogo_seqs$video
  irun_ortGo_seq <- irun_go_seqs$ortGo
  irun_audGo_seq <- irun_go_seqs$audGo
  irun_vidGo_seq <- irun_go_seqs$vidGo
  # write script line by line
  irun_desc <- data.frame(CONDITION='BlankStart', DURATION=nblank, STIM_TEXT='+', MISC_TEXTCOLOR='255,255,255', 
                          STIM_BITMAP='BLANK', STIM_WAV='', STIM_VIDEO='', RESPONSE_BUTTON=0)
  for (irun_tag in irun_fseq){
    irun_iti <- sample(iti_duration)[1]
    irun_condition      <- c('Preload', irun_tag, 'RespWindow', 'ITI')
    irun_duration       <- c(pre_duration, txt_duration, resp_duration, irun_iti)
    irun_text           <- c('+', '', '', '+')  # ITI has a noraml (white) fixation; Preload may have a red fixation (preserved)
    irun_text_color     <- c('255,255,255', '255,255,255', '0,0,0', '255,255,255')
    irun_resp           <- c(0, 2, 0, 0)  # 5-key keyboard: 2 is index (Go)
    switch(irun_tag,
      Ortho={
        irun_ortho_stim <- irun_ortho_seq[1] #sample(1:dim(irun_ortho)[1])[1]
        irun_ortho_seq <- irun_ortho_seq[-1]
        irun_desc_txt <- c('+', irun_ortho[irun_ortho_stim, 'ortho'], '', '+')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_desc_txt, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP='BLANK', STIM_WAV='', STIM_VIDEO='', RESPONSE_BUTTON=irun_resp)  
        #irun_ortho <- irun_ortho[-irun_ortho_stim,]
      },
      Audio={
        irun_audio_stim <- irun_audio_seq[1]  #sample(1:dim(irun_audio)[1])[1]
        irun_audio_seq <- irun_audio_seq[-1]
        irun_desc_wav <- c('', irun_audio[irun_audio_stim, 'stimulus'], '', '')
        irun_duration[2] <- wav_duration
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP='BLANK', STIM_WAV=irun_desc_wav, STIM_VIDEO='', RESPONSE_BUTTON=irun_resp)       
        #irun_audio <- irun_audio[-irun_audio_stim,]
      },
      Video={
        irun_video_stim <- irun_video_seq[1]  #sample(1:dim(irun_video)[1])[1]
        irun_video_seq <- irun_video_seq[-1]
        irun_desc_avi <- c('', irun_video[irun_video_stim, 'stimulus'], '', '')
        irun_duration[2] <- avi_duration               
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP='BLANK', STIM_WAV='', STIM_VIDEO=irun_desc_avi, RESPONSE_BUTTON=irun_resp)       
        #irun_video <- irun_video[-irun_video_stim,]
      },          
      OrthoGo={
        irun_ortGo_stim <- irun_ortGo_seq[1]  #sample(1:dim(irun_ortGo)[1])[1]
        irun_ortGo_seq <- irun_ortGo_seq[-1]
        irun_desc_txt <- c('+', irun_ortGo[irun_ortGo_stim, 'ortho'], '', '+')
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_desc_txt, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP='BLANK', STIM_WAV='', STIM_VIDEO='', RESPONSE_BUTTON=irun_resp)
        #irun_ortGo <- irun_ortGo[-irun_ortGo_stim,]
      },
      AudioGo={
        irun_audGo_stim <- irun_audGo_seq[1]  #sample(1:dim(irun_audGo)[1])[1]
        irun_audGo_seq <- irun_audGo_seq[-1]
        irun_desc_wav <- c('', irun_audGo[irun_audGo_stim, 'stimulus'], '', '')
        irun_duration[2] <- wav_duration
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP='BLANK', STIM_WAV=irun_desc_wav, STIM_VIDEO='', RESPONSE_BUTTON=irun_resp)       
        #irun_audGo <- irun_audGo[-irun_audGo_stim,]
      },
      VideoGo={
        irun_vidGo_stim <- irun_vidGo_seq[1]  #sample(1:dim(irun_vidGo)[1])[1]
        irun_vidGo_seq <- irun_vidGo_seq[-1]
        irun_desc_avi <- c('', irun_vidGo[irun_vidGo_stim, 'stimulus'], '', '')
        irun_duration[2] <- avi_duration                      
        irun_desc_tmp <- data.frame(CONDITION=irun_condition, DURATION=irun_duration, STIM_TEXT=irun_text, MISC_TEXTCOLOR=irun_text_color,
                                    STIM_BITMAP='BLANK', STIM_WAV='', STIM_VIDEO=irun_desc_avi, RESPONSE_BUTTON=irun_resp)       
        #irun_vidGo <- irun_vidGo[-irun_vidGo_stim,]
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
    writeLines(sprintf('The number of trials is: %.0f', ntrls+ntrls_go))
    cat(irun_fseq, '\n')
    writeLines(sprintf('A single trial contains:'))
    writeLines(sprintf('A pre-load window (%.0f triggers or %.3f seconds)', pre_duration, pre_duration*trigger_duration))
    writeLines(sprintf('A stimulus (TXT: %.0f triggers or %.3f seconds; WAV: %.0f triggers or %.3f seconds; AVI: %.0f triggers or %.3f seconds)', 
                       txt_duration, txt_duration*trigger_duration, wav_duration, wav_duration*trigger_duration, avi_duration, avi_duration*trigger_duration))
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