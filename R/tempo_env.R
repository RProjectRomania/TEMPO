
tempoEnv <- new.env()
tempoEnv$language <- "ro"
tempoEnv$matrices <- "http://statistici.insse.ro:8077/tempo-ins/matrix/matrices?lang="
tempoEnv$matrix   <- "http://statistici.insse.ro:8077/tempo-ins/matrix/"
tempoEnv$dataCsv  <- "http://statistici.insse.ro:8077/tempo-ins/pivot"
tempoEnv$dataJson <- "http://statistici.insse.ro:8077/tempo-ins/matrix/dataSet/"
tempoEnv$config   <- "http://statistici.insse.ro:8077/tempo-online/assets/data/tempo-config.json"

# TODO some form of logger