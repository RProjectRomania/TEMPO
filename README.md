# TEMPO

`TEMPO` lists and downloads data from the Romanian National Institute of
Statistics' TEMPO Online database.

```r
install.packages("TEMPO")

tables <- tempo_toc(language = "en")
tempo_bulk(
  codes = "ACC101B",
  language = "en",
  directory = file.path(tempdir(), "tempo-data")
)
```

Both functions require access to the TEMPO Online service. `tempo_bulk()`
writes CSV files only to the directory supplied by the caller.
