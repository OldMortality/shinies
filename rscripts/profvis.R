library(profvis)
profvis({ runApp('~/Documents/shiny/app5') }  
        , prof_output = '~/Documents/profvis')
profvis(prof_input = '~/Documents/profvis/file3f72364a4ed8.Rprof')
