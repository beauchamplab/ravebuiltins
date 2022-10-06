if(FALSE) {
    dim(power$data)
    dim(volt)
    
    vsub <- volt$subset(Trial = Trial %in% GROUPS[[1]]$Trial_num)
    tind <- vsub$dimnames$Time[round(seq(1, length(vsub$dimnames$Time),
                                         length.out = length(power$dimnames$Time)))] %>% round(4)
    
    # index and take the same number of time points that we have for power?
    g1 <- vsub$subset(Time = round(Time,4) %in% tind)
    # .r <- range(volt$dimnames$Time) %>% diff %>% divide_by(dim(volt)[2L]) %>% raise_to_power(-1)
    
    plot.clean(volt$dimnames$Time, c(-1,1)*100)
    abline(h=mean(g1$data), col='lightgray')
    ebar_polygon(x=g1$dimnames$Time,
                 colMeans(g1$data[,,1]),
                 .fast_column_se(g1$data[,,1]),
                 col=get_color(1))
    abline(v=analysis_window)
    rave_axis(1,at=pretty(volt$dimnames$Time))
    
    .v <- volt$subset(Trial = Trial %in% GROUPS[[2]]$Trial_num, Time = round(Time,4) %in% tind)
    # lines(.v$dimnames$Time, .m, col='orange')
    ebar_polygon(.v$dimnames$Time, colMeans(.v$data[,,1]), .fast_column_se(.v$data[,,1]),
                 col='dodgerblue3')
    
    dim(volt$data)
    par(mfrow=1:2)
    .pw1 <- rave::pwelch(colMeans(volt$data[,1:1e3,1]), fs=1375,
                         nfft = 256, noverlap = 8, window = 128, log='xy')
    .pw <- rave::pwelch(colMeans(volt$data[,1e3:2e3,1]), fs=1375,
                        nfft = 256, noverlap = 8, window = 128, plot = F, log='xy')
    lines(log10(.pw$freq), 10*log10(.pw$spec), col='orange')
    ind <- .pw$freq %within% c(2,200)
    plot(.pw$freq[ind], (10 * (log10(.pw$spec)-log10(.pw1$spec)))[ind], type='l')
}



## Voltage data
# vsub <- volt$subset(Trial = Trial %in% GROUPS[[ii]]$Trial_num, data_only = TRUE, drop = TRUE)
# add_data(voltage_data[[ii]]) <-  t(vapply(seq_len(dim(vsub)[2L]),function(i) .fast_mse(vsub[,i]), rep(0,2)))

# add_data(voltage_data[[ii]]) <-  t(vapply(seq_len(dim(vsub)[2L]),function(i) .fast_mse(vsub[,i]), rep(0,2)))
# for the welch periodogram, grab the power only within the analysis range
# voltage_welch[[ii]]$welch <- pwelch(vsub[,voltage.time_ind], module_tools$get_sample_rate(original = TRUE), plot=FALSE)
