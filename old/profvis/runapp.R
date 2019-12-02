library(profvis)
profvis({ runApp('/Users/micheldelange/Documents/shiny/app6/') }  
        , prof_output = '/Users/micheldelange/Documents/')


profvis({ runApp('~/shinies/app6/') }  
        , prof_output = '~/shinies/old/')



