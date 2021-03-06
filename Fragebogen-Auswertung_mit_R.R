## Auswertung unseres Fragebogens f�r Erhebungstechniken ##

library(readr)
umfrage <- read_csv("results-survey397474.csv")

umf <- umfrage
umf <- data.frame( umf$`Antwort ID`, umf$`Statistik: Angesehene Wissenschaft oder l�stiges �bel? Was ist deine Meinung dazu?`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Traue keiner Statistik, die du nicht selbst gef�lscht hast.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik ist eine der zukunftsorientiertesten Wissenschaften des 21. Jahrhunderts.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik ist sehr trocken.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistiker*innen haben gute Berufsaussichten.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Viele Studierende haben Angst vor dem Fach Statistik.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistiken sind eine wichtige Informationsquelle.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik studieren nur M�nner.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik f�rdert Kreativit�t.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistiken beweisen gar nichts.]`,
                   umf$`Beurteile wie sehr du den folgenden Thesen zustimmst: [Statistik hat vielf�ltige Anwendungsgebiete.]`,
                   umf$`Was glaubst du wie relevant ist Statistik f�r... [...deinen zuk�nftigen Beruf?]`,
                   umf$`Was glaubst du wie relevant ist Statistik f�r... [...dein jetziges Studium?]`,
                   umf$`Was glaubst du wie relevant ist Statistik f�r... [...deinen Alltag?]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [verst�ndlich]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [cool]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [innovativ]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [langweilig]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [rechenintensiv]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [kompliziert]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [abwechlungsreich]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [reine Zeitverschwendung]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [anstrengend]`,
                   umf$`Kreuze alle Eigenschaften an, welche deiner Meinung nach auf Statistik zutreffen:�  Statistik ist...  [spannend]`,
                   umf$`[Wie sind deine Eindr�cke/Assoziationen zum Thema Statistik?]`,
                   umf$`[Wie glaubst du wird Statistik im Allgemeinen wahrgenommen?]`,
                   umf$`Was stellst du dir unter der Arbeit von Statistiker*innen vor?`,
                   umf$`Ist Statistik ein Bestandteil in deinem Studium?`,
                   umf$`[Wie sch�tzt du deine Leistung in Statistik ein?]`,
                   umf$`Alter (in Jahren):`,
                   umf$`Geschlecht:`, umf$`Geschlecht: [Sonstiges]`,
                   umf$`Studienfach:`, umf$`Studienfach: [Sonstiges]`,
                   umf$`N�chster angestrebter Abschluss:`, 
                   umf$`N�chster angestrebter Abschluss: [Sonstiges]`,
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

#sehr n�tzlicher Link zur Farbwahl:
#https://bjoernwalther.com/farben-in-r-der-col-befehl/

thesen <- umf[3:12] #___________________________________________________________

#Raender: c(unten, links, oben, rechts)


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
kod_rel_beruf <- c( rep(1,3), rep(6, 15), rep(3,8), rep(4,22), rep(2,5), rep(5,18))
mean(kod_rel_beruf)
#[1] 4.295775
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
#Anzahlen der einzelnen Auspr�gungen aber definitv nicht gleich


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

#_______________________________________________________________________________
#Grafiken
#_______________________________________________________________________________
##-EIGENSCHAFTEN-##
#Eigenschaften-Abb1
par(mar = c(9,4,4,4))
#8 Statis und 50 nicht-statis (laut workspace), daher 68 insgesamt
barplot(angekreuz_eig/68, col = "darkcyan", 
    names.arg = c(names(angekreuz_eig)[1:7],"Zeitverschwendung",names(angekreuz_eig)[9:10]), 
        las = 2, main = "Haeufigkeit der angekreuzten Eigenschaften", ylim = c(0,1))

#Eigenschaften-Abb2
barplot(sort(angekreuz_eig/68), col = "darkcyan", 
        names.arg = c("Zeitverschwendung",names(sort(angekreuz_eig))[-1]),
        las = 2, main = "(sortierte) Haeufigkeit der angekreuzten Eigenschaften", ylim = c(0,1))

#Eigenschaften-Abb3
barplot(sort(angekreuz_eig/68), col = c("brown4", rep("darkcyan", 1), 
                    rep("brown4", 1), rep("darkcyan",3), rep("brown4", 2), 
                    "darkcyan", "brown4"), 
        main = "(sortierte) Haeufigkeit der angekreuzten Eigenschaften", 
        names.arg = c("Zeitverschwendung",names(sort(angekreuz_eig))[-1]), las = 2, ylim = c(0,1))
legend(x="topleft",c("positive Eigenschaften", "negative Eigenschaften"),
       fill = c("darkcyan", "brown4"), bty = "n")

#Eigenschaften-Abb1(Statistik vs. Nicht-Statistik)
eigensch_statis <- statis[16:25]
angekreuz_eig_statis <- eigensch_statis == "Ja"
angekreuz_eig_statis
colSums(angekreuz_eig_statis)
angekreuz_eig_statis <- colSums(angekreuz_eig_statis)

eigensch_nicht_statis <- nicht_statis[16:25]
angekreuz_eig_nicht_statis <- eigensch_nicht_statis == "Ja"
angekreuz_eig_nicht_statis
colSums(angekreuz_eig_nicht_statis)
angekreuz_eig_nicht_statis <- colSums(angekreuz_eig_nicht_statis)

matr <- matrix(c(angekreuz_eig_statis/18, angekreuz_eig_nicht_statis/50), ncol = 2)
matr

par(mar = c(9,4,4,1))
barplot(t(matr), beside = TRUE, col = c("cadetblue4", "coral3"), ylim = c(0,1), 
        main = "Haeufigkeit der angekreuzten Eigenschaften", las = 2,
        names.arg = c(names(angekreuz_eig_statis)[1:7], "Zeitverschwendung",
                      names(angekreuz_eig_statis)[9:10]))
legend(x="topleft", c("Statistiker", "Nicht-Statistiker"), bty = "n",
       fill = c("cadetblue4", "coral3"))

#Eigenschaften-Abb2(Statistik vs. Nicht-Statistik)
par(mfrow = c(1,2))
barplot(sort(angekreuz_eig_statis)/18, col = "darkcyan", 
        names.arg = c(names(sort(angekreuz_eig_statis))[1], 
                      "Zeitverschwendung",names(sort(angekreuz_eig_statis))[3:10]),
        las = 2, main = "Haeufigkeit der angekreuzten Eigenschaften-Statis", 
        ylim = c(0, 1))

barplot(sort(angekreuz_eig_nicht_statis)/50, col = "darkcyan", 
        names.arg = c("Zeitverschwendung",names(sort(angekreuz_eig_nicht_statis))[2:10]),
        las = 2, main = "Haeufigkeit der angekreuzten Eigenschaften-Nicht-Statis", 
        ylim = c(0, 1))

#Eigenschaften-Abb3(Statistik vs. Nicht-Statistik)
par(mfrow = c(1,2))
par(mar = c(9, 4,4,4))
barplot(sort(angekreuz_eig_statis)/18, col = c(rep("brown4",2) ,rep("darkcyan", 1), 
                rep("brown4", 1), "darkcyan", rep("brown4", 2), rep("darkcyan",3)), 
        main = "Haeufigkeit der angekreuzten Eigenschaften-Statis", 
        names.arg = c(names(sort(angekreuz_eig_statis))[1] ,"Zeitverschwendung",names(sort(angekreuz_eig_statis))[3:10]), 
        las = 2, ylim = c(0,1))
legend(x="topleft",c("positive Eigenschaften", "negative Eigenschaften"),
       fill = c("darkcyan", "brown4"), cex = 0.55, bty = "n")

barplot(sort(angekreuz_eig_nicht_statis)/50, col = c(rep("brown4", 1), 
               rep("darkcyan", 3), rep("brown4", 1), rep("darkcyan",2), rep("brown4", 3)), 
        main = "Haeufigkeit der angekreuzten Eigenschaften-Nicht-Statis", 
        names.arg = c("Zeitverschwendung",names(sort(angekreuz_eig_nicht_statis))[-1]), 
        las = 2, ylim = c(0,1))
legend(x="topleft",c("positive Eigenschaften", "negative Eigenschaften"),
       fill = c("darkcyan", "brown4"), cex = 0.55, bty = "n")

dev.off()  #zum Zurueckstellen von allen Raendern, etc.
#-------------------------------------------------------------------------------
##-RELEVANZ-##
par(mfrow = c(1,3))
par(mar = c(9,3,9,3))
boxplot(na.omit(umf$Relevanz_Beruf), main = "Berufs-Relevanz", col = "lightblue")
boxplot(na.omit(umf$Relevanz_Studium), main = "Studiums-Relevanz", col = "lightblue")
boxplot(na.omit(umf$Relevanz_Alltag), main = "Alltag-Relevanz", col = "lightblue")

par(mfrow = c(3,2))
#Legende dazu was 1 bis 6 bedeutet fehlt noch
boxplot(nicht_statis$Relevanz_Beruf,
            main = "Berufs-Relevanz (Nicht-Statistiker)", col = "lightblue")

boxplot(statis$Relevanz_Beruf, ylim = c(1, 6),
                main = "Berufs-Relevanz (Statistiker)", col = "lightblue")

boxplot(nicht_statis$Relevanz_Alltag,
                main = "Alltags-Relevanz (Nicht-Statistiker)", col = "lightblue")

boxplot(statis$Relevanz_Alltag, ylim = c(1, 6),
                main = "Alltag-Relevanz (Statistiker)", col = "lightblue")

boxplot(nicht_statis$Relevanz_Studium,
                main = "Studiums-Relevanz (Nicht-Statistiker)", col = "lightblue")

boxplot(statis$Relevanz_Studium,  ylim = c(1, 6),
                main = "Studiums-Relevanz (Statistiker)", col = "lightblue")
#-------------------------------------------------------------------------------
##-THESEN-##

# Hier habe ich es auch nochmal mit den Werten von prop.table verglichen:

# Wichtig: prop.table berechnet die Prozentwerte in dem es durch die Laenge der Spalte
# OHNE NAs teilt. So muesste man das dann auch im plot tun.

# table durch die Leange teilen(mit Nas):
table(umf$nur_Maenner)/length(umf$nur_Maenner)
# table durch die Laenge teilen (ohne NAs):
table(umf$nur_Maenner)/71
# wird auch so bei prop.table gemacht:
prop.table(table(umf$nur_Maenner))

length(umf$nur_Maenner)
#[1] 124
length(na.omit(umf$nur_Maenner))
#[1] 71
na.omit(umf$nur_Maenner)
umf$nur_Maenner

# Es gibt bei allen Thesen 53 NAs:
summary(thesen)

# Habe versucht nochmal einen gestapelten barplot zu machen. Habe das aber etwas 
# anders gemacht.

# Zuerst neuen data.frame aufstellen mit etwas anderer Struktur:
These <- c( rep("Traue_keiner_Statistik",124), rep("zukunftsorientiert",124),rep("trocken",124),rep("gute_Berufaussichten",124),
            rep("Angst",124), rep("wichtige_Informationsquelle",124),rep("nur_Maenner",124), rep("Kreativitaet",124),
            rep("beweisen_nichts",124), rep("vielfaeltige_Anwendungsgebiete",124) )
gplot <- data.frame(These)
Bewertung <- c(thesen$Traue_keiner_Statistik,thesen$zukunftsorientiert,thesen$trocken,thesen$gute_Berufsaussichten,
               thesen$Angst, thesen$wichtige_Informationsquelle, thesen$nur_Maenner,thesen$Kreativitaet,
               thesen$beweisen_nichts, thesen$vielfaeltige_Anwendungsgebiete)
gplot$Bewertung <- Bewertung
gplot    #df mit Bewertung und These "nebeneinander"

# Dann kann man einen gemeinsamen table machen:
table(gplot$Bewertung, gplot$These)

# Barplot absolute Werte und unsortiert:
barplot( table(gplot$Bewertung, gplot$These), horiz = TRUE,  col = c("indianred4", "goldenrod","darkseagreen3", "darkslategrey"),
         names.arg = c("Angst","beweisen_nichts","Berufsaussichten","Kreativitaet","nur_Meanner","traue_keiner","trocken",
                       "vielf_Anwendung","Infoquelle","zukonftsorientiert"),las=2 )

# table durch Laenge der NA bereinigten Thesen teilen: 71
# vergleiche mit summary: Es gibt immer 53 NAs 124-53 = 71

# Barplot mit relativen Werten unsortiert:
barplot( table(gplot$Bewertung, gplot$These)/71, horiz = TRUE,  col = c("indianred4", "goldenrod","darkseagreen3", "darkslategrey"),
         names.arg = c("Angst","beweisen_nichts","Berufsaussichten","Kreativitaet","nur_Meanner","traue_keiner","trocken",
                       "vielf_Anwendung","Infoquelle","zukonftsorientiert"),las=2 )

# Sortierung machen:
t <- table(gplot$Bewertung, gplot$These)/71

# Gewuenschte Reihenfolge (mit den Tabellen abgelesen) als Index:
x <- c("vielfaeltige_Anwendungsgebiete","wichtige_Informationsquelle","gute_Berufaussichten","Angst","zukunftsorientiert",
       "Traue_keiner_Statistik","trocken","beweisen_nichts","Kreativitaet","nur_Maenner")
x <- rev(x)         # leider habe ich oben alles falsch herrum abgetippt, also nochmal umdrehen
sortiert <- t[,x]   
sortiert

# Barplot sortiert und mit relativen Werten.

par(mar = c(12,8,4,2))
barplot( sortiert , horiz = TRUE,  col = c("indianred4", "goldenrod","darkseagreen3", "darkslategrey"),
         names.arg = c("nur_Maenner", "Kreativit�t", "beweisen_nichts","trocken","traue_keiner",
                       "zukunftsorientiert","Angst","Berufsaussichten","Infoquelle","vielf_Anwendung"), las =2,
         main = "Zustimmung der einzelnen Thesen")
legend(x = "bottom", inset = c(0, -0.45), 
       c("Stimme gar nicht zu", "Stimme eher nicht zu", "Stimme eher zu", 
         "Stimme voll zu"), xpd = TRUE, ncol = 2, 
       fill = c("indianred4", "goldenrod", "darkseagreen3", "darkslategrey"), 
       cex = 1, bty = "n")


#-------------------------------------------------------------------------------
##-ASSOZIATIONEN & WAHRNEHMUNG-##
Coloursceme <- rev(c("darkgreen", "chartreuse", "yellow", "orange", "red"))

barplot(table(studi_fach_ohne_na[26])/sum(table(studi_fach_ohne_na[26])), 
        col = Coloursceme, main = "Eigene Assoziation des Faches Statistik", 
        ylim = c(0,0.7))
barplot(table(studi_fach_ohne_na[27])/sum(table(studi_fach_ohne_na[27])), 
        col = Coloursceme, main = "Eigene Annahme zur allgemeinen Wahrnehmung des Faches Statistik", 
        ylim = c(0,0.7))

assoziation <- c(table(statis[26])/sum(table(statis[26])), table(nicht_statis[26])/sum(table(nicht_statis[26])))
dim(assoziation) <- c(5,2)
eigeneWahr <- c(table(statis[27])/sum(table(statis[27])), table(nicht_statis[27])/sum(table(nicht_statis[27])))
dim(eigeneWahr) <- c(5,2)

par(mar = c(8,3,3,3))
barplot(t(assoziation), col = c("#1b98e0", "#353436"), beside = TRUE, main = "Eigene Assoziation des Faches Statistik", names.arg = c("negativ", "eher negativ", "neutral", "eher positiv", "positiv"), las = 2, ylim = c(0,0.7))
legend("topleft", legend = c("Statistiker", "Nicht-Statistiker"), 
       fill = c("#1b98e0", "#353436"), bty = "n")

par(mar = c(8,3,3,3))
barplot(t(eigeneWahr), col = c("#1b98e0", "#353436"), beside = TRUE, main = "Allgemeine Wahrnehmung des Faches Statistik", names.arg = c("negativ", "eher negativ", "neutral", "eher positiv", "positiv"), las = 2, ylim = c(0,0.7))
legend("topright", legend = c("Statistiker", "Nicht-Statistiker"), 
       fill = c("#1b98e0", "#353436"), bty = "n")
#-------------------------------------------------------------------------------
##-KORRELATIONSPLOT-##
numeric_thesen <- sapply(thesen, as.numeric)
numeric_thesen
# Namen erstellen:
M <- cor(na.omit(numeric_thesen))
colnames(M) <- c("Tr_kS","Zuknftor","trocken","Berufaus","Angst","Infoquel","Maenner","Kreativ","Beweis","Anwendgeb")
rownames(M) <- c("Tr_kS","Zuknftor","trocken","Berufaus","Angst","Infoquel","Maenner","Kreativ","Beweis","Anwendgeb")

# Paket installieren:
# install.packages("corrplot")
library("corrplot")
dev.off()
# Verschiedene Methoden:
corrplot( M , method="color", type="lower" , main = "Korrelationen der Zustimmung mehrerer Thesen", mar = c(1, 1, 4, 1),tl.cex = 1.5, cl.cex = 1.2 )
# corrplot( M , method="color", type="full" )
# corrplot( M , method="circle", type="full")
# corrplot( M , method="pie", type="full")
# corrplot( M , method="circle", type="lower")
?corrplot

dev.off()


## Tabellen ergaenzend, um konkrte Werte schnell ablesen zu koennen:
## erstellt mit prob.table (wie auf Uebungsblatt 4 vorgeschlagen)


# Datensaetze fuer Gesamt (d.h. Statstiker und Nicht Statistiker zusammmen )

# Anteilswerte fuer die Thesen:
sapply(thesen, function(x){ prop.table(table(x)) })

# Anteilswerte Relevanz:
sapply(relevanz,function(x){ prop.table(table(x)) })

# Anteilswerte Eigenschaften:
sapply(eigenschaften,function(x){ prop.table(table(x)) }) # Mit fehlenden Werten

eigenschaften[ eigenschaften == "N/A"] <- NA  # "N/A" in Na umkodieren um na.omit verwenden zu k�nnen

sapply(eigenschaften,function(x){ prop.table(table( na.omit(x) ) ) }) # ohne fehlende Werte

# Anteilswerte Wahrnehmung/Asso:
sapply(umf[ ,26:27],function(x){ prop.table(na.omit( table(x)) )}) # ohne Fehlende Werte

#---------------------------------------------------------------------------------------------------

# Datensaetze fuer Statistiker:

# Anteilswerte Eigenschften Statistiker (ohne fehlende Werte!)
do.call( rbind, sapply(eigensch_statis, function(x){ prop.table(table(x)) }) )

# Anteilswerte Wahrnehmung/Assoziationen Statistiker:
asso_statis <- statis[  ,26:27] #Teildatensatz 
sapply(asso_statis, function(x){ prop.table(table(x)) })

#Anteilswerte Relevanz Statistiker:
relevanz_statis <- statis[ ,13:15]
sapply(relevanz_statis, function(x){ prop.table(table(x)) })

#---------------------------------------------------------------------------------------------------

# Datensaetze fuer  Nicht-Statistiker:

# Anteilswerte Eigenschaften Nicht-Statistiker (ohne fehlende Werte!)
do.call( rbind, lapply(eigensch_nicht_statis, function(x){ prop.table(table(x)) }) )

# Kleiner Einschub ###################################################################
# Mir kamen die glatten Werte so komisch vor, aber sie scheinen wirklich so zu sein:

# table(eigensch_nicht_statis$verstaendlich)
# prop.table( table(eigensch_nicht_statis$verstaendlich) )

# prop.table stimmt ueberein mit relativen W'keiten:
# 24/50
# 26/50
######################################################################################


# Anteilswerte Wahrnehmung/Assoziationen Nicht-Statistiker:
asso_nicht_statis <- nicht_statis[  ,26:27] #Teildatensatz 
sapply(asso_nicht_statis, function(x){ prop.table(table(x)) }) 

#Anteilswerte Relevanz Nicht-Statistiker:
relevanz_nicht_statis <- nicht_statis[ ,13:15]
sapply(relevanz_nicht_statis, function(x){ prop.table(table(x)) })

#----------------------------------------------------------------------------------------------------
#zusammenhang von Leistung und eigene Assoziation

table(umf[c(26, 30)])
#                Leistung
# Assoziationen  schlecht nicht so gut mittelm��ig ganz gut bestens
# negativ             1            1           0        0       0
# eher negativ        0            3           3        2       0
# neutral             1            1           4        7       2
# eher positiv        0            1           7       11       0
# positiv             0            0           2        5       4

plot(umf[c(26, 30)])
# corrplot(umf[c(26, 30)])  Problem: nicht numerisch

prop.table(table(umf[c(26, 30)]))
#               Leistung
# Assoziationen   schlecht nicht so gut mittelm��ig   ganz gut    bestens
# negativ       0.01818182   0.01818182  0.00000000 0.00000000 0.00000000
# eher negativ  0.00000000   0.05454545  0.05454545 0.03636364 0.00000000
# neutral       0.01818182   0.01818182  0.07272727 0.12727273 0.03636364
# eher positiv  0.00000000   0.01818182  0.12727273 0.20000000 0.00000000
# positiv       0.00000000   0.00000000  0.03636364 0.09090909 0.07272727



#### Wie viele NA und Stichprobenumfang ########################################################

# N= 128  des umf Datensatz mit NA Zeilen

?complete.cases
# Datensatz wo TrUE bei kein NA und FALSE bei NA
index <- sapply( umf, complete.cases)

str(index)
index <- as.data.frame(index)
index <- index[ ,-1 ] # Antwort_ID rausnehmen
index <- index[ , -c(15:24)] # Alle Eigenschaften raus nehmen, weil dort auch ein TRUE steht
index

# Da ueberall FALSE steht wo NA steht, sind die Zeilensummen wo nur NAs stehen Null
which( rowSums(index) == 0 )

# Datensatz ohne die NA Zeilen (d.h. in allen Spalten steht bei der Person ein NA)
# N = 76 wenn man NA Zeilen rausnimmt . d.h. 124-76 = 48 NA Zeilen
umf_ohne_NA <- umf[ -(which( rowSums(index) == 0 )), ]
umf_ohne_NA

# N_Statis = 18 , N_Nicht_Statis = 50 , N_Statis+N_Nicht_Statis = 68
# Das heisst in Studienfach muss es auch NAs geben (dann w�rden die Zeilen ja auch nicht in des stati Datensaetzen
# auftauchen) und zwar 8 Stueck, was auch der Fall ist:
sum(is.na(umf_ohne_NA$Studienfach))


#statis demografische Daten:

summary( statis[ ,30:38] )
sapply( statis[ ,30:38], table)

#nicht statis demografische Daten
summary( nicht_statis[ ,30:38] )
# 1998 entfernen
which(nicht_statis$Alter == 1998)
nicht_statis <- nicht_statis[-26 ,  ]
summary( nicht_statis[ ,30:38] )
sapply( nicht_statis[ ,30:38], table)

#Gesamt Stichprobe demg. Daten:
# Falsche ALter rausschmeissen
which(umf_ohne_NA$Alter == 1998)
umf_ohne_NA <- umf_ohne_NA[-32 ,  ]

summary( umf_ohne_NA[ ,30:38] )
sapply( umf_ohne_NA[ ,30:38], table)







