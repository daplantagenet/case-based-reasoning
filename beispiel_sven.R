# 1) Excelsheet als "RFSC.csv" abspeichern
# 2) unten angegebenen Code in ein R-Workspace einf¸gen und ausf¸hren lassen
# Wichtig: alles in ein Verzeichnis und auch den Workspace auf dieses Verzeichnis einstellen
# 3) erg.csv anschauen
# 
# Interpretation: Die Knoten werden nach einer Tiefensuche durchnummeriert, also in der Reihenfolge wie auf dem Excelsheet.
# Wir betrachten eine Zeile des Ergebnisfiles:
#   Die Struktur ist: a;b;c
# a und b sind die Indize der Terminalnodes in der oben angegebenen Nummerierung. c ihr Abstand.
# 
# Ist nur ein Brute-Force-Algorithmus, aber besser als nichts.
# Und besser wird es wohl auch nicht (jedenfalls nicht viel).
# 
# Da "Jeder Terminalknoten mit jedem Terminalknoten" betrachtet wird, hat man schon mal O(n^2).
# Und wenn man die maximale Tiefe des Baumes (wie in dem vorliegenden Code) auf 50 deckelt, bleibt jede Rechnung in O(1).
# Und besser geht es nicht.
# Theoretisch hat der Algorithmus bei nicht zu entarteten B‰umen eine Laufzeit von O((n^2)*log n).
# Und das log n macht auch nicht mehr viel aus.
# n ist dabei die Anzahl der (Terminal-)Knoten. Es sollte bei nicht entarteten B‰umen keinen wesentlichen Unterschied machen, ob man von Der Gesamtknotenanzahl oder von der Anzahl aller Terminalknoten spricht.
# 
# Warnung: Wenn die Baumtiefe zu groﬂ ist (bei mit grˆﬂer als 48 "Links"-Schritte in einem Pfad von der Wurzel zu einem Terminalknoten), versagt der Algorithmus. In C++ kann man das Problem geschickt umgehen, wenn man die Matrix in eine neue doppelt so groﬂe umkopiert, so bald der Platz nicht mehr ausreicht. Dann kann man auch Arrays verwenden und hat eine kurze Zugriffszeit.
# 
# P.S.: Hab es nicht wirklich getestet, bitte selber machen.
# P.P.S.: Wollte auch keinen Test machen, da mir nicht ganz klar war, ob die Ergebnisse so gew¸nscht sind.
# 

library(XLConnect)
source("getDist.R")
mydata <- readWorksheetFromFile("RFSC.xlsx", "Alter")

myv <- is.na(mydata$contPT)
mynv <- which(myv)
nodeID <- mydata$nodeID[mynv]
system.time(getDist(mynv, myv, nodeID)) # 1000 trees ~ 2 min.
