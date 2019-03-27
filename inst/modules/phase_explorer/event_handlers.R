observeEvent(input$FREQUENCY, {
        FREQUENCY = input$FREQUENCY
        frequencies %?<-% NULL
        if(length(FREQUENCY) && length(frequencies) && ! (FREQUENCY %in% frequencies)) {
            new_frequency <- frequencies[..get_nearest(FREQUENCY, frequencies)]
            
            showNotification(p('Chosen frequency: ', strong(FREQUENCY),
                               "doesn't exist. Switching to: ", strong(new_frequency)), type = 'warning', id='BAD_FREQ')
            FREQUENCY <- new_frequency
            
            updateSliderInput(session, 'FREQUENCY', value = FREQUENCY)
        }
    }, priority = 1000)
