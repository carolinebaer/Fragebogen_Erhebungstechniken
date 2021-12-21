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

statis[ ,c(3:15,26,27,30)] <- lapply( statis[ ,c(3:15,26,27,30)], factor) 
# the "[]" keeps the dataframe structure
str(statis)

#factor einfuegen ______________________________________________________________
#install.packages("forcats")
library("forcats")

#fuer Beruf:
fac_umf_rel_beruf <- as.factor(umf$Relevanz_Beruf)
levels(fac_umf_rel_beruf)
table(fac_umf_rel_beruf)
fct_rev(fac_umf_rel_beruf)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Absolut irrelevant", after = 0)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Ziemlich irrelevant", after = 1)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Eher irrelevant", after = 2)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Eher relevant", after = 3)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Ziemlich relevant", after = 4)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Absolut relevant", after = 5)
table(fac_umf_rel_beruf)
umf$Relevanz_Beruf <- fac_umf_rel_beruf

#fuer Alltag (Kopie von beruf)
fac_umf_rel_beruf <- as.factor(umf$Relevanz_Alltag)
levels(fac_umf_rel_beruf)
table(fac_umf_rel_beruf)
fct_rev(fac_umf_rel_beruf)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Absolut irrelevant", after = 0)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Ziemlich irrelevant", after = 1)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Eher irrelevant", after = 2)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Eher relevant", after = 3)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Ziemlich relevant", after = 4)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Absolut relevant", after = 5)
table(fac_umf_rel_beruf)
umf$Relevanz_Alltag <- fac_umf_rel_beruf

#fuer Studium (Kopie von beruf)
fac_umf_rel_beruf <- as.factor(umf$Relevanz_Studium)
levels(fac_umf_rel_beruf)
table(fac_umf_rel_beruf)
fct_rev(fac_umf_rel_beruf)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Absolut irrelevant", after = 0)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Ziemlich irrelevant", after = 1)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Eher irrelevant", after = 2)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Eher relevant", after = 3)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Ziemlich relevant", after = 4)
fac_umf_rel_beruf <- fct_relevel(fac_umf_rel_beruf, "Absolut relevant", after = 5)
table(fac_umf_rel_beruf)
umf$Relevanz_Studium <- fac_umf_rel_beruf


levels(umf$Traue_keiner_Statistik)
umf$Traue_keiner_Statistik <- fct_relevel(umf$Traue_keiner_Statistik, "Stimme gar nicht zu", after = 0 )
levels(umf$Traue_keiner_Statistik)

levels(umf$zukunftsorientiert)
umf$zukunftsorientiert <- fct_relevel(umf$zukunftsorientiert, "Stimme gar nicht zu", after = 0 )
levels(umf$zukunftsorientiert)

levels(umf$trocken)
umf$trocken <- fct_relevel(umf$trocken, "Stimme gar nicht zu", after = 0 )
levels(umf$trocken)

levels(umf$gute_Berufsaussichten)
umf$gute_Berufsaussichten <- fct_relevel(umf$gute_Berufsaussichten, "Stimme gar nicht zu", after = 0 )
levels(umf$gute_Berufsaussichten)

levels(umf$Angst)
umf$Angst <- fct_relevel(umf$Angst, "Stimme gar nicht zu", after = 0 )
levels(umf$Angst)

#teil2
fac_umf12 <- as.factor(umf[ ,12])
factor(fac_umf12)
umf[ ,12] <- fct_relevel(fac_umf12, "Stimme gar nicht zu", after = 0)
factor(umf[ ,12])

fac_umf11 <- as.factor(umf[ ,11])
factor(fac_umf11)
umf[ ,11] <- fct_relevel(fac_umf11, "Stimme gar nicht zu", after = 0)
factor(umf[ ,11])

fac_umf10 <- as.factor(umf[ ,10])
factor(fac_umf10)
umf[ ,10] <- fct_relevel(fac_umf10, "Stimme gar nicht zu", after = 0)
factor(umf[ ,10])

fac_umf9 <- as.factor(umf[ ,9])
factor(fac_umf9)
levels(fac_umf9) <- c("Stimme eher nicht zu", "Stimme eher zu", "Stimme gar nicht zu", "Stimme voll zu")
umf[ ,9] <- fct_relevel(fac_umf9, "Stimme gar nicht zu", after = 0)
factor(umf[ ,9])

fac_umf8 <- as.factor(umf[ ,8])
factor(fac_umf8)
umf[ ,8] <- fct_relevel(fac_umf8, "Stimme gar nicht zu", after = 0)
factor(umf[ ,8])


#
fac_umf26 <- as.factor(umf[ ,26])
fac_umf26 <- fct_relevel(fac_umf26, "negativ", after = 0)
umf[, 26] <- fct_relevel(fac_umf26, "neutral", after = 2)
levels(umf[, 26])

fac_umf27 <- as.factor(umf[ ,27])
fac_umf27 <- fct_relevel(fac_umf27, "negativ", after = 0)
umf[, 27] <- fct_relevel(fac_umf27, "neutral", after = 2)
levels(umf[, 27])

levels(umf[, 27]) <- c(levels(umf[, 27]), "positiv")
levels(umf[, 27])

fac_umf30 <- as.factor(umf[ ,30])
levels(fac_umf30)
umf[,30]  <- fct_rev(fac_umf30)
levels(umf[,30])



umf_ohne_freifeld <- umf[-c(2,28)] #____________________________________________

#sehr nützlicher Link zur Farbwahl:
#https://bjoernwalther.com/farben-in-r-der-col-befehl/

thesen <- umf[3:12] #___________________________________________________________

par(mfrow = c(1,1))

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


#C: sortiertes Balkendiagramm (nach "stimme gar nicht zu")
sort(matrix_plot[,1])
# [1]  1  1  2  2  3  5  6 20 27 42
matrix_plot_sort <- matrix(c(matrix_plot[10,], matrix_plot[6,], matrix_plot[5,],
                             matrix_plot[3,], matrix_plot[4,], matrix_plot[2,],
                             matrix_plot[1,], matrix_plot[8,], matrix_plot[9,],
                             matrix_plot[7,]), nrow = 10, byrow = TRUE)
matrix_plot_sort

#Raender: c(unten, links, oben, rechts)
#opar <- par(no.readonly = TRUE)  ##sollte eig urspungszustand wiederherstellen

par(mar = c(4,7.5,3,1)) #Raender einstellen, damit nichts abgeschnitten ist
barplot(t(matrix_plot_sort), horiz = TRUE, col = c("indianred4", "goldenrod", 
                                "darkseagreen3", "darkslategrey"), 
        main = "gestapeltes Balkendiagramm",
        names.arg = c("vielf_Anwendung", "Infoquelle", "Angst", "trocken", 
                      "Berufsaussichten", "zukunftsorientiert", "traue_keiner", 
                      "Kreativitaet", "beweisen_nichts", "nur_Maenner"), 
        las = 2, axes = FALSE)
legend(x = "bottom", inset = c(0, -0.2), 
       c("Stimme gar nicht zu", "Stimme eher nicht zu", "Stimme eher zu", 
         "Stimme voll zu"), xpd = TRUE, ncol = 2, 
       fill = c("indianred4", "goldenrod", "darkseagreen3", "darkslategrey"), 
       cex = 1)
#on.exit(par(opar))



eigenschaften <- umf[16:25] #___________________________________________________

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
par(mar = c(9,3,3,1))
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



relevanz <- umf[13:15] #________________________________________________________
# fac_relevanz <- list(c(as.factor(relevanz[1]), as.factor(relevanz[2]),
#                                  as.factor(relevanz[3])))
# fac_relevanz <- factor(relevanz$Relevanz_Beruf, labels = c("Absolut irrelevant",
#                        "Ziemlich irrelevant", "Eher irrelevant", "Eher relevant",
#                        "Ziemlich relevant", "Absolut relevant"))
# str(fac_relevanz)

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

par(mfrow = c(1,2))
boxplot(kod_rel_beruf, main = "Beruf-Relevanz")
boxplot(kod_rel_alltag, main = "Alltag-Relevanz")
boxplot(kod_rel_studium, main = "Studium-Relevanz")
#boxplots von Beruf und Alltag identisch
table(relevanz$Relevanz_Beruf)
table(relevanz$Relevanz_Alltag)
#Anzahlen der einzelnen Ausprägungen aber definitv nicht gleich


studi_fach_ohne_na <- umf[is.na(umf$Studienfach) == FALSE,] #____________________
statis <- studi_fach_ohne_na[studi_fach_ohne_na$Studienfach %in% c("Statistik",
                                                                   "Data Science"),]
#der extra-Datensatz mit den Antworten von Statistik und Data Science -Studenten

statis$Antwort_ID
statis$Einstiegsfrage
#alle vorhanden Antworten positiverer Art
statis$Leistung
#Leistungseinschaetzung "mittelmaessig" oder besser

nicht_statis <- studi_fach_ohne_na[(studi_fach_ohne_na$Studienfach %in% 
                                   c("Statistik", "Data Science")) == FALSE ,] #___

par(mfrow = c(1,2))
barplot(table(nicht_statis$Relevanz_Beruf), 
        main = "Berufs-Relevanz (Nicht-Statistiker)")
barplot(table(statis$Relevanz_Beruf), 
        main = "Berufs-Relevanz (Statistiker)")

barplot(table(nicht_statis$Traue_keiner_Statistik))
barplot(table(statis$Traue_keiner_Statistik))

barplot(table(nicht_statis$zukunftsorientiert))
barplot(table(statis$zukunftsorientiert))

table(nicht_statis$Geschlecht)
table(statis$Geschlecht)

#write.csv(umf, "Umfrage_Tabelle.csv")

