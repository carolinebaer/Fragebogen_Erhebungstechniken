## Auswertung unseres Fragebogens für Erhebungstechniken ##

library(readr)
umfrage <- read_csv("results-survey397474.csv")

umf <- umfrage
umf <- data.frame( umf$`Antwort ID`, umf$`Statistik: Angesehene Wissenschaft oder lästiges Übel? Was ist deine Meinung dazu?`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Traue keiner Statistik, die du nicht selbst gefälscht hast.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik ist eine der zukunftsorientiertesten Wissenschaften des 21. Jahrhunderts.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik ist sehr trocken.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistiker*innen haben gute Berufsaussichten.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Viele Studierende haben Angst vor dem Fach Statistik.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistiken sind eine wichtige Informationsquelle.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik studieren nur Männer.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik fördert Kreativität.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistiken beweisen gar nichts.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik hat vielfältige Anwendungsgebiete.]`,
                   umf$`Was glaubst du wie relevant ist Statistik für... [...deinen zukünftigen Beruf?]`,
                   umf$`Was glaubst du wie relevant ist Statistik für... [...dein jetziges Studium?]`,
                   umf$`Was glaubst du wie relevant ist Statistik für... [...deinen Alltag?]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [verständlich]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [cool]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [innovativ]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [langweilig]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [rechenintensiv]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [kompliziert]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [abwechlungsreich]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [reine Zeitverschwendung]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [anstrengend]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:   Statistik ist...  [spannend]`,
                   umf$`[Wie sind deine Eindrücke/Assoziationen zum Thema Statistik?]`,
                   umf$`[Wie glaubst du wird Statistik im Allgemeinen wahrgenommen?]`,
                   umf$`Was stellst du dir unter der Arbeit von Statistiker*innen vor?`,
                   umf$`Ist Statistik ein Bestandteil in deinem Studium?`,
                   umf$`[Wie schätzt du deine Leistung in Statistik ein?]`,
                   umf$`Alter (in Jahren):`,
                   umf$`Geschlecht:`, umf$`Geschlecht: [Sonstiges]`,
                   umf$`Studienfach:`, umf$`Studienfach: [Sonstiges]`,
                   umf$`Nächster angestrebter Abschluss:`, 
                   umf$`Nächster angestrebter Abschluss: [Sonstiges]`,
                   umf$`Fachsemester:`, umf$`Hast du noch Anmerkungen oder Fragen an uns?` )

names(umf)
names(umf) <- c("Antwort_ID", "Einstiegsfrage", "Traue_keiner_Statistik", 
                "zukunftsorientiert", "trocken", "gute_Berufsaussichten", "Angst",
                "wichtige_Informationsquelle", "nur_Maenner", "Kreativitaet",
                "beweisen_nichts", "vielfaeltige_Anwendungsgebiete",
                "Relevanz_Beruf", "Relevanz_Studium", "Relevanz_Alltag",
                "verstaendlich", "cool", "innovativ", "langweilig", "rechenintensiv",
                "kompliziert", "abwechslungsreich", "reine_Zeitverschwendung",
                "anstrengend", "spannend", "Assoziationen", "allgemeine_Wahrnehmung",
                "Vorstellung_Arbeit_Statistiker", "Bestandteil", "Leistung", "Alter",
                "Geschlecht", "Geshlecht_sonstiges", "Studienfach", 
                "Studienfach_sonstiges", "Abschluss", "Abschluss_sonstiges",
                "Fachsemester", "Anmerkungen" )


#sehr nützlicher Link zur Farbwahl:
#https://bjoernwalther.com/farben-in-r-der-col-befehl/

thesen <- umf[3:12]

thesen_stapel_barplot <- function(){
  thesen_plot <- matrix(0, nrow = 10, ncol = 4)
  for(i in 1:10){
    setz <- table(thesen[i])
    thesen_plot[i,1] <- setz[3]
    thesen_plot[i,2] <- setz[1]
    thesen_plot[i,3] <- setz[2]
    thesen_plot[i,4] <- setz[4]
  }
  return(thesen_plot)
}

matrix_plot <- thesen_stapel_barplot()
matrix_plot[7,4] <- 0  #da, "stimme voll zu" von niemanden angekreuzt wurde 
barplot(t(matrix_plot), horiz = TRUE, col = c("indianred4", "goldenrod", 
        "darkseagreen3", "darkslategrey"), main = "gestapeltes Balkendiagramm",
        names.arg = c("traue_keiner", "zukunftsorientiert", "trocken",
                      "Berufsaussichten", "Angst", "Infoquelle",
                      "nur_Maenner", "Kreativitaet", "beweisen_nichts",
                      "vielf_Anwendung"), las = 2, axes = FALSE,
        legend.text = c("Stimme gar nicht zu", "Stimme eher nicht zu",
                        "Stimme eher zu", "Stimme voll zu"))
#C: Text der Balken noch abgeschnitten -> Raender vermtulich noch aendern
#C: die Legende muss außerhalb des Diagramms
#C: eine Sortierung nach rot oder dunkelgruen waere ausserdem noch schoen



eigenschaften <- umf[16:25]

angekreuz_eig <- eigenschaften == "Ja"
angekreuz_eig
colSums(angekreuz_eig)
#     verstaendlich                       cool               innovativ 
#                33                         20                      13 
#        langweilig             rechenintensiv             kompliziert 
#                14                         43                      39 
# abwechslungsreich    reine_Zeitverschwendung             anstrengend 
#                27                          3                      34 
#          spannend 
#                42 

angekreuz_eig <- colSums(angekreuz_eig)
barplot(angekreuz_eig, col = "darkcyan", names.arg = names(angekreuz_eig), las = 2,
        main = "Haeufigkeit der angekreuzten Eigenschaften")
barplot(sort(angekreuz_eig), col = "darkcyan", names.arg = names(sort(angekreuz_eig)),
        las = 2, main = "Haeufigkeit der angekreuzten Eigenschaften")

#positiv <- c(1:3, 7, 10)   #positive Eigenschaften (vorm sortieren)
#negativ <- c(4:6, 8:9)
barplot(sort(angekreuz_eig), col = c(rep("darkcyan", 3), rep("brown4", 3), 
                                     "darkcyan", rep("brown4", 2), "darkcyan"), 
        main = "Haeufigkeit der angekreuzten Eigenschaften", 
        names.arg = names(sort(angekreuz_eig)), las = 2)
legend(0,40,c("positive Eigenschaften", "negative Eigenschaften"),
       fill = c("darkcyan", "brown4"))
#C: man koennte das legend() auch reinsetzen erzeugt aber eine Warnmeldung
#C: die Eigenschafts-Namen sind leider noch abgeschnitten



relevanz <- umf[13:15]
table(relevanz)

plot(relevanz)


#mit Kodierung: "absolut irrelevant" = 1,...,"ziemlich relevant" = 6
table(relevanz$Relevanz_Beruf)
kod_rel_beruf <- c( rep(1,3), rep(6, 4), rep(3,14), rep(4,27), rep(2,11), rep(5,12))
mean(kod_rel_beruf)
#[1] 3.647887
median(kod_rel_beruf)
#[1] 4
#dh. der Durchschnitt haelt Statistik fuer den Beruf fuer "eher irrelevant" bis
# "eher relevant"

table(relevanz$Relevanz_Studium)
kod_rel_studium <- c(rep(1,5), rep(6,23), rep(3,8), rep(4,12), rep(2,8), rep(5,15))
mean(kod_rel_studium)
#[1] 4.309859
median(kod_rel_studium)
#[1] 5
#dh. der Durchschnitt haelt Statistik fuers Studium fuer "eher relevant" 
# bis "ziemlich relevant"

table(relevanz$Relevanz_Alltag)
kod_rel_alltag <- c(rep(1,3), rep(6,4), rep(3,14), rep(4,27), rep(2,11), rep(5,12))
mean(kod_rel_alltag)
#[1] 3.647887
median(kod_rel_alltag)
#[1] 4
#dh. der Durchschnitt haelt Statistik fuer den Beruf fuer "eher irrelevant" bis
# "eher relevant"

#C: auffaellig sind die so aehnlichen Ergebnisse bei Beruf und Alltag
sum(na.omit(relevanz$Relevanz_Beruf) == na.omit(relevanz$Relevanz_Alltag)) #21
sum(na.omit(relevanz$Relevanz_Beruf) != na.omit(relevanz$Relevanz_Alltag)) #50
#C: trotz gleich mediane und mittelwerte, zeigt sich nicht, dass die befragten bei
#  beruf und alltag gleich angekreuzt haben


all(which(is.na(relevanz$Relevanz_Beruf)) == which(is.na(relevanz$Relevanz_Alltag)))
#entweder alle drei oder keine beantwortet worden

boxplot(kod_rel_beruf)
boxplot(kod_rel_studium)
boxplot(kod_rel_alltag)
#boxplots von Beurf und Alltag identisch
table(relevanz$Relevanz_Beruf)
table(relevanz$Relevanz_Alltag)
#Anzahlen der einzelnen Ausprägungen aber definitv nicht gleich


studi_fach_ohne_na <- umf[is.na(umf$Studienfach) == FALSE,]
statis <- studi_fach_ohne_na[studi_fach_ohne_na$Studienfach %in% c("Statistik",
                                                                   "Data Science"),]
#der extra-Datensatz mit den Antworten von Statistik und Data Science -Studenten

statis$Antwort_ID
statis$Einstiegsfrage
#alle vorhanden Antworten positiverer Art
statis$Leistung
#Leistungseinschaetzung "mittelmaessig" oder besser

nicht_statis <- studi_fach_ohne_na[(studi_fach_ohne_na$Studienfach %in% 
                                   c("Statistik", "Data Science")) == FALSE ,]

par(mfrow = c(1,2))
barplot(table(nicht_statis$Relevanz_Beruf))
barplot(table(statis$Relevanz_Beruf))

barplot(table(nicht_statis$Traue_keiner_Statistik))
barplot(table(statis$Traue_keiner_Statistik))

barplot(table(nicht_statis$zukunftsorientiert))
barplot(table(statis$zukunftsorientiert))

table(nicht_statis$Geschlecht)
table(statis$Geschlecht)

#write.csv(umf, "Umfrage_Tabelle.csv")

fac_statis <- sapply(statis, factor)
str(fac_statis)
