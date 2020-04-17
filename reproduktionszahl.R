# Autor: Initiative Grundrechte, https://twitter.com/IGrundrechte

library(EpiEstim)
library(jsonlite)
library(dplyr)

# Amtlicher Gemeindeschlüssel für das Bundesland (Sachsen = 14)
idbundesland = "14"

# Abfrage der Datenbank RKI COVID19
# Übersicht: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
# eigene Abfrage zusammenstellen unter: https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?outFields=*&where=1%3D1
# Erläuterung der Abfrage:
# neuerfall = -1 ausschließen (Falschmeldung vom Vortag)
# aggregieren über Meldedatum summieren von AnzahlFall zu AnzahlFallSumme
data <- fromJSON("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?where=%28neuerfall%3D0+or+neuerfall%3D1%29+and+idbundesland%3D" + idbundesland + "&objectIds=&time=&resultType=none&outFields=Meldedatum%2CAnzahlFallSumme&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=Meldedatum&groupByFieldsForStatistics=Meldedatum&outStatistics=%5B%7B%0D%0A++++++++%22statisticType%22%3A+%22sum%22%2C+%0D%0A++++++++%22onStatisticField%22%3A+%22AnzahlFall%22%2C+%0D%0A++++++++%22outStatisticFieldName%22%3A+%22AnzahlFallSumme%22%0D%0A++++++%7D%5D&having=&resultOffset=&resultRecordCount=&sqlFormat=standard&f=pjson&token=")

# Daten aufbereiten nach den Anforderungen von EpiEstim
# dates: Datum, I: Anzahl Fälle zum Datum
dd <- flatten(data$features)
dd <- transmute(dd, dates = as.Date(as.POSIXct(`attributes.Meldedatum` / 1000, origin="1970-01-01")), I = `attributes.AnzahlFallSumme`)

# Tage ohne Fälle mit I = 0 hinzufügen
all_dates = seq(head(dd$dates,1), tail(dd$dates,1), 1)
for (j in 1:length(all_dates)) 
{
  filterdate = all_dates[j];
  if (length(which(dd$dates == filterdate)) == 0)
  {
    dd <- rbind(dd,list(filterdate, 0))
  }
}
dd <- na.omit(dd[order(dd),])

# letzte 2 Tage löschen (zur Vermeidung von Ungenauigkeiten wegen des Meldeverzugs)
dd <- head(dd, -2)

# Verarbeiten der Daten in EpiEstim
# Verwendung eines Seriellen Intervalls mit Mittelwert 5,0 und Standardabweichung 1,9 (Ferretti u.a. 2020),
# analog zu: https://www.genstat.imise.uni-leipzig.de/sites/www.genstat.imise.uni-leipzig.de/files/files/uploads/Bulletin_covid19_sachsen_leipzigPlus_2020_04_11_v5.pdf
res_parametric_si <- estimate_R(dd, method="parametric_si", config = make_config(list(mean_si = 5.0, std_si = 1.9)))

# Ausgabe des Plots im interaktiven Modus
plot(res_parametric_si, legend = FALSE)

res_parametric_si
