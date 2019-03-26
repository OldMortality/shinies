library(profvis)
profvis({ shiny::runApp('~/Documents/shiny/app6') }  
        , prof_output = '~/Documents/profvis')
profvis(prof_input = '~/Documents/profvis/filea097be80525.Rprof')
