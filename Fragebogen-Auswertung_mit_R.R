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
                   umf$`Was stellst du dir unter der Arbeit von Statistiker*innen vor?`,
                   umf$`Ist Statistik ein Bestandteil in deinem Studium?`,
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
                "anstrengend", "spannend", "Vorstellung_Arbeit_Statistiker",
                "Bestandteil", "Alter", "Geschlecht", "Geshlecht_sonstiges",
                "Studienfach", "Studienfach_sonstiges", "Abschluss", "Abschluss_sonstiges",
                "Fachsemester", "Anmerkungen" )


#sehr nützlicher Link zur Farbwahl:
#https://bjoernwalther.com/farben-in-r-der-col-befehl/

thesen <- umf[3:12]

thesen_stapel_barplot <- function(){
  thesen_plot <- matrix(0, nrow = 10, ncol = 4)
  for(i in 1:10){
    setz <- table(thesen[i])
    for(j in 1:4){
      thesen_plot[i,j] <- setz[j]
    }
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

