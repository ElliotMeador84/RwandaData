files <- list('data', 'scripts', 'png', 'pdf')

lapply(files, dir.create)


script_file <- grep('.R$',list.files(recursive = T), value = T)


file.copy(script_file, paste0('scripts/',script_file))


