# ============
# Datenimport
# ============

required_packages <- c("openxlsx")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# =====================
# Excel-Datei einlesen
# =====================

library(readxl)
data <- read_excel("Customer_Churn_Dataset_Soylu.xlsx")
getwd()

# ===============
# Datenüberblick
# ===============

str(data)
head(data$signup_date)
names(data)
dim(data)

# INTERPRETATION:
# Der Datensatz umfasst 50.000 Beobachtungen und 18 Variablen. Die Strukturprüfung zeigt numerische, 
# kategoriale sowie eine Datumsvariable. Die Variablennamen und ersten Werte wurden kontrolliert. 
# Die Variable avg_listening_hours_per_week wurde dagegen zunächst als Zeichenvaraible eingelesen. 
# Somit ist eine numerische Konverteierung von avg_listening_hours_per_week erforderlich. Die 
# Identifikationsvaraible user_id wird nicht als Prädiktor in die modellierung aufgenommen.

# ======================
# Datentypen überprüfen
# ======================

sapply(data, class)

# ======================
# Datenbereinigung
# ======================

# avg_listening_hours_per_week -> numeric konvertieren
data$avg_listening_hours_per_week <- as.numeric(data$avg_listening_hours_per_week)
sapply(data, class)

# INTERPRETATION:
# avg_listeing_hours_per_week wurde von einer kategorialne Variable zu einer numerischen 
# Variable umkodiert.

# Fehlende Werte prüfen
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Anzahl fehlender Werte je Spalte:")
print(missing_values)

# Duplikate prüfen
duplicates <- sum(duplicated(data))
print("Anzahl der Duplikate:")
print(duplicates)

# INTERPRETATION:
# Im Datensatz wurden keine Dublikate oder fehlenden Werte gefunden.

# ======================
# Plausibilitätsprüfung
# ======================

# Numerische Variablen prüfen
summary(data)

# Kategoriale Variablen prüfen
unique(data$subscription_status)
unique(data$subscription_type)
unique(data$ad_interaction)
unique(data$ad_conversion_to_subscription)
unique(data$country)
unique(data$favorite_genre)
unique(data$most_liked_feature)
unique(data$desired_future_feature)
unique(data$primary_device)

# Datum prüfen
summary(data$signup_date)
range(data$signup_date, na.rm = TRUE)

# INTERPRETATION
# Es wurden keine unplausiblen Werte bzw. unerwarteten Kategorien festgestellt.

# ===============================
# Umkodierung der Churn-Variable
# ===============================

library(dplyr)
data <- data %>%
  rename(churn = subscription_status)
colnames(data)
data$churn [data$churn == "Active"] <- "No"
data$churn[data$churn == "Inactive" ] <- "Yes"

# Churn als Faktor setzen
data$churn <- factor(data$churn)
colnames(data)
table(data$churn)
unique(data$churn)

# INTERPRETATION:
# Die Zielvariable subscription_status wurde in Churn unbenannt. Die Ausprägungen Active 
# und Inactive wurden zur eindeutigen Kennzeichnung des Churn-Status in No und Yes umcodiert.
# Anschließend wurde die Varaiable als Faktor definiert.

# =====================
# Variablen definieren
# =====================

# Numerische Variablen definieren
numerische_variablen <- c( 
  "age", 
  "avg_skips_per_day", 
  "avg_listening_hours_per_week", 
  "playlists_created", 
  "music_suggestion_rating_1_to_5",
  "inactive_3_months_flag",
  "months_inactive"
)

# Datentypkontrolle
sapply(data[numerische_variablen], class)

# Kategoriale Variablen definieren
kategoriale_variablen <- c(
  "churn",
  "subscription_type",
  "ad_interaction",
  "ad_conversion_to_subscription",
  "favorite_genre", 
  "most_liked_feature",
  "desired_future_feature",
  "primary_device",
  "country"
)

# Datentypkontrolle
sapply(data[kategoriale_variablen], class)

# ======================
# Deskriptive Statistik 
# ======================

summary(data[numerische_variablen])

sapply(data[numerische_variablen], mean, na.rm = TRUE)
sapply(data[numerische_variablen], median, na.rm = TRUE)
sapply(data[numerische_variablen], sd, na.rm = TRUE)

summary(data[kategoriale_variablen])

lapply(data[kategoriale_variablen], table)
lapply(data[kategoriale_variablen], function(x){
  prop.table(table(x)) * 100
})

# INTERPRETATION:
# Die deskriptive Analyse der kategorialen Variablen zeigen, dass 84,2 % der Nutzer der nicht-Churn und 
# 15,8 % DER Churn-Klasse zugeordnet sind. Der Free-Tarif stellt mit rund 45 % die größte Abonnemnentgruppe
# dar, gefolgt von Premium Individual mit etwa 28 %. Rund 35 % der Nutzer weisen eine Werbe-
# interaktion auf, während lediglich etwa 8,7 % eine Conversion zu einem Abonnement zeigen.Die Kategorien 
# der bevorzugten Genres, Plattformfunktionen, gewünschten zukünftigen Funktionen sowie der primär
# verwendeten Endgeräte sind hingegen weitgehend gleichmäßig verteilt. 

# =====================================================
# Ausreißerprüfung (IQR-Methode)
# =====================================================

ausreisser_variablen <- c(
  "age",
  "avg_skips_per_day",
  "avg_listening_hours_per_week",
  "playlists_created",
  "months_inactive"
)

for (spalte in ausreisser_variablen) {
  print(paste("Analyse der Variable:", spalte))
  daten <- (data[[spalte]]) 
  
# Quartile berechnen
  Q1 <- quantile(daten, 0.25, na.rm = TRUE)
  Q3 <- quantile(daten, 0.75, na.rm = TRUE)
  
# Interquartilsabstand
  IQR_wert <- Q3 - Q1
  
# Untere und obere Grenze 
  untere_grenze <- Q1 - 1.5 * IQR_wert
  obere_grenze <- Q3 + 1.5 * IQR_wert 
  
# Ausreißer bestimmen
  ausreisser <- daten[daten < untere_grenze | daten > obere_grenze]
  
# Ergebnisse ausgeben
  print(paste("Anzahl der Ausreißer:", length(ausreisser)))
  print("------------------")
}

# INTERPRETATION:
# Bei der IQR-Methode wurden statistische Ausreißer in den numerischen Variablen identifiziert. 
# Besonders bei months_inactive treten aufgrund der rechtsschiefen Verteilung vergelcihsweise 
# viele auffällige Werte auf. Da die identifizierten Werte inhaltlich plausibel sind und
# für die Churn-Prognose relevante Informationen enthalten können, werden sie nicht aus dem 
# Datensatz entfernt.

# ================
# Viselle Prüfung 
# ================

# Boxplot: Durchschnittliche Skips pro Tag
library(ggplot2)
data%>%
  ggplot(aes(y =  avg_skips_per_day)) +  
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.size = 2) +  
  labs (title = "Verteilung der durchschnittlichen Skips pro Tag") +
  xlab (NULL) +
  ylab ("Durchschnittliche Anzahl an Skips pro Tag") + 
  theme_minimal()

summary(data$avg_skips_per_day)

# INTERPRETATION:
# Die durchschnittliche Anzahl der Skips liegt bei 10 Skips pro Tag. (M = 10,03; Median = 10)
# Einzelne höhere und niedrigere Werte werden als potenzielle Ausreißer identifiziert, jedoch aufgrund 
# ihrer PLausibilität in der Analyse berücksichtigt. 

# Boxplot: Durchschnittliche Hörstunden pro Woche
data %>%
  ggplot(aes(y =  avg_listening_hours_per_week)) +  
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.size = 2) +  
  labs (title = "Verteilung der durchschnittlichen Hörzeit pro Woche") + 
  xlab (NULL) + 
  ylab ("Durchschnittliche Hörzeit pro Woche (Stunden)") +
  theme_minimal()

summary(data$avg_listening_hours_per_week)

# INTERPRETATION: 
# Die durchschnittliche wöchentliche Hörzeit beträgt rund 10 Stunden (M = 9,99; Median = 9,98). 
# Die mittleren 50 % der Nutzer weisen eine Hörzeit zwischen etwa 7,3 und 12,7 Stunden 
# pro Woche auf. Einzelne höhere Werte werden als potenzielle Ausreißer identifiziert, liegen
# jedoch in einem plausiblen Wertebereich und werden daher beibehalten.

# Boxplot: Anzahl erstellter Playlists
data %>%
  ggplot(aes(y =  playlists_created)) +  
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.size = 2) +  
  labs (title = "Verteilung der Anzahl erstellter PLaylists") +
  xlab (NULL) +
  ylab ("Anzahl erstellter Playlists") + 
  theme_minimal()

summary(data$playlists_created)

# INTERPRETATION:
# Die mittleren 50 % der Nutzer haben zwischen 6 und 10 Playlists erstellt. Der Median beträgt 
# 8 PLalists. Oberhalb von 16 Playlists treten 185 potenzielle Ausreißer auf, mit Werten bis maximal
# 23 Playlists. Da diese Werten inhaltlich plausibel sind, werden sie im Datensatz beibehalten. 

# Boxplot: Age
data %>%
  ggplot(aes(y = age)) +  
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.size = 2) +  
  labs (title = "Verteilung des Alters") +
  xlab (NULL) +
  ylab ("Alter") + 
  theme_minimal()  

summary(data$age)

# INTERPRETATION:
# Das Alter der Nutzer liegt zwischen 16 und 60 Jahren. Der Median beträgt 38 Jahre,
# wobei die mittleren 50 % der Nutzer zwischen 27 und 49 Jahren liegen. 
# Es sind keine potenziellen Ausreißer erkennbar, was auch mit der IQR-Prüfung übereinstimmt. 

# Boxplot: inaktive Monate
data%>%
  ggplot(aes(y = months_inactive)) +  
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.size = 2) +  
  labs (title = "Verteilung der Anzahl inaktiver Monate") +
  xlab (NULL) +
  ylab ("Anzahl inaktiver Monate") + 
  theme_minimal()   

summary(data$months_inactive)

# INTERPRETATION:
# Die Inaktivitätsdauer beträgt im Median 1 Monat. Die mittleren 50 % der Nutzer weisen 
# Werte zwischen 0 und 2 Monaten auf. Nach der IQR-Methode werden Inaktivitätszeiten ab 6 Monaten 
# als ppotenzielle Ausreißer klassifiziert. Da diese Werte trotz ihrer statistischen Auffälligkeit 
# inhaltlich plausibel sind, werden sie nicht entfernt und in der weiteren Analyse berücksichtigt. 

# ===============================
# Explorative Datenanalyse (EDA)
# ===============================

# Datenüberblick - Churn-Rate
mean(data$churn == "Yes") * 100

# Absolute und relative Häufigkeiten des Churn-Status
table(data$churn)
prop.table(table(data$churn)) * 100

# Balkendiagramm
data%>%
  ggplot(aes(x = churn)) +
  geom_bar(aes(y = after_stat(prop), group = 1),
           fill = "steelblue", alpha = 0.7,colour = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Verteilung des Churn-Status") + 
  xlab("Churn-Status") +
  ylab("Anteil der Nutzer")

# INTERPRETATION: 
# Von insgesamt 50.000 Nutzern weisen 7.891 einen Churn-Status und 42.109 keinen Churn-Status auf.
# Damit beträgt die Churn-Rate rund 15,8 %, während etwa 84,2 % der Nutzer nicht abgewandert sind. 
# Die Zielvariable ist somit deutlich ungleich verteilt, wobei Nicht-Churn-Fälle überwiegen.

# ============
# Age - Alter
# ============

# Übersicht
summary(data$age)
sd(data$age)

# Häufigkeiten
table(data$age, data$churn)
prop.table(table(data$age, data$churn), margin = 1) * 100

# Altersgruppen erstellen
data$age_group <- cut(
  data$age, 
  breaks = c(15, 25, 35, 45, 55, Inf),
  labels = c("16-25", "26-35", "36-45", "46-55", "56+"),
  right = TRUE
)

# Altersgruppen prüfen
prop.table(table(data$age_group, data$churn), margin = 1) * 100

# INTERPRETATION:
# Das durchschnittliche Alter der Nutzer beträgt rund 38 Jahre (Median = 38; SD = 12, 98). 
# Die Churn-Rate unterscheidet sich zwischen den Altersgruppen nur geringfühig. Sie liegt
# zwischen etwa 15,5 % und 16,3 %. Die höchste Churn-Rate weist die Altersgruppe der 
# 36- bis 45-Jährigen mit rund 16,25 % auf, die niedrigste die Gruppe ab 56 Jahren 
# mit rund 15,48 %. Insgesamt sind damit keine deutlichen altersabhängigen Unterschiede 
# im Churn-Verhalten erkennbar. 

# Altersgruppe x Abonnementtyp
table(data$age_group, data$subscription_type)
prop.table(table(data$age_group, data$subscription_type), margin = 1) * 100

# INTERPRETATION:
# Die Verteilung der Abonnementtypen ist in allen Altersgruppen sehr ähnlich. 
# Das Free-Abo ist mit rund 45 % am häufigsten, gefolgt von Premium Individual mit 
# 28 %. Premium Duo, Premium Family und Student werden jeweils von rund 9 % der Nutzer 
# gewählt. Deutliche Unterschiede zwischen den Altersgruppen sind nicht erkennbar. 

# Altersgruppen x Inaktive Monate
aggregate(months_inactive ~ age_group, data = data, mean)

# Boxplot Altergruppen nach inaktiven Monaten
data%>%
  ggplot(aes(x = age_group, y = months_inactive)) +  
  geom_boxplot(fill = "steelblue", outlier.color = "red", outlier.size = 2) +  
  labs (title = "Verteilung der Anzahl inaktiver Monate nach Altersgruppe") +
  xlab ("Altersgruppe") +
  ylab ("Anzahl inaktiver Monate")

# INTERPRETATION:
# Die durchschnittliche Inaktivitätsdauer unterscheidet sich zwischen den Altersgruppen
# nur geringfügig und liegt in allen Gruppen bei etwa 1,5 Monaten. Die höchste Werte zeigen
# die 36- bis 45-Jährigen mit rund 1,57 Monaten, die niedrigsten die Nutzer ab 56 Jahren mit 
# rund 1,5 Monaten. Deskriptiv ist somit kein deutlicher Zusammenhang zwischen Alter und 
# Inaktivitätsdauer erkennbar. 

# Altersgruppe x Hörstunden
aggregate(avg_listening_hours_per_week ~ age_group, data = data, mean)

# INTERPRETATION:
# Die durchschnittliche wöchentliche Hördauer ist in allen Altersgruppen nahezu gleich. 
# Sie liegt in allen Altersgruppen bei etwa 10 Stunden.  

# Histogramm
library(magrittr)
data%>%
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 3,fill = "steelblue", alpha = 0.7, colour = "black", ) +
  labs(title = "Altersverteilung der Nutzer") +  
  xlab ("Alter in Jahren") +
  ylab ("Anzahl der Nutzer")

# INTERPRETATION:
# Die Altersverteilung der Nutzer zeigt eine annährend gleichmäßige Verteilung 
# der Alterswerte. Auffälligen Häufungen oder Schiefe sind nicht erkennbar.
# Die Altersgruppen sind insgesamt ausgewogen vertreten. 

# Density: Alter
data%>%
  ggplot(aes(x = age)) +
  geom_density(fill = "steelblue", alpha = 0.7,colour = "black") +
  geom_vline(xintercept = mean(data$age),
             linetype = "dashed",colour = "red") +
  labs(title = "Dichteverteilung des Alters") + 
  xlab("Alter") +
  ylab("Dichte") +
  scale_x_continuous(breaks = c(16, 20, 30, 40, 50, 60))

# INTERPRETATION:
# Die Altersverteilung ist annährend gleichmäßig. Auffällige Häufungen oder eine deutliche
# Schiefe sind nicht erkennnbar.

# Histogramm: Altersgruppe x Churn
data%>%
  group_by (age_group) %>%
  summarise(
    Anzahl = n(),
    Churn_Anzahl = sum(churn == "Yes"),
    Churn_Rate = mean(churn == "Yes") * 100 
    )

# INTERPRETATION:
# Die Churn-Raten liegen in allen Altersgruppen sehr nah beieinander, ungefähr zwischen 
# 15,5 % und 16,3 %.
# Die gruppen 36-45 Jahre weist mit etwa 16,3 % zwar die höchste Churn-Rate auf, der Unterschied zu
# den anderen Altersgruppen beträgt aber weniger als 1 %. 

# Alter nach Churn agreggieren 
aggregate(age ~ churn, data = data, median)
aggregate(age ~ churn, data = data, sd)
aggregate(age ~ churn, data = data, mean)

# Statistischer Vergleich
t.test(age ~ churn, data = data)

# INTERPRETATION:
# Das durchschnittsalter unterscheidet sich zwischen Kunden mit und ohne CHurn nicht statistisch signifikant.
# Der Welch-t-Test zeigt keinen statistisch signifikanten Unterschied (p = 0,835). Das Alter 
# weist somit keinen erkennbaren Zusammenhang mit dem Churn-Status auf. 

# ===============
# Country - Land
# ===============

# Häufigkeiten
table(data$country)
prop.table(table(data$country)) * 100

# Land x Churn 
table(data$country, data$churn)
prop.table(table(data$country, data$churn), margin = 1) * 100

# Statistischer Zusammenhang Land x Churn
chisq.test(table(data$country, data$churn))

# INTERPRETATION:
# Die Nutzer sind über die betrachteten Länder annährend gleichmäßig verteilt. 
# Der Chi-Quadrat-Test zeigt keinen statistisch signifikanten Zusammenhang
# zwischen dem Herkunftsland und dem Churn-Status.
# (ꭓ² = 10.203, df = 11, p = 0.5122)
# Die Nullhypothese kann daher nicht verworfen werden.

# ==================================
# Signup Date - Registrierungsdatum
# ==================================

# Übersicht
summary(data$signup_date)
str(data$signup_date)
class(data$signup_date)
head(data$signup_date)

# Registrierungsdatum x Churn
library(lubridate)
data$signup_year <- year (data$signup_date)
head(data$signup_year)

# Balkendiagramm
data%>%
  ggplot(aes(x = factor(signup_year))) + 
  geom_bar(fill = "steelblue", alpha = 0.7, colour = "black", ) +
  labs(title = "Verteilung der Nutzer nach Registrierungsjahr") + 
  xlab ("Registrierungsjahr") +
  ylab ("Anzahl der Nutzer")

# Häufigkeiten
table(data$signup_year, data$churn)
prop.table(table(data$signup_year, data$churn), margin = 1) * 100

# Statistischer Zusammenhang
chisq.test(table(data$signup_year, data$churn))

# INTERPRETATION:
# Die Registrierungsdaten reichen vom 01.01.2018 bis zum 01.03.2026. Der Median liegt am 31.01.2022
# Zwischen 2018 und 2025 verteilen sich die Registrierungen relativ Gleichmäßig auf die 
# einzelnen Jahre. Die deutlich geringere Anzahl im Jahr 2026 ist darauf zurückzuführen, dass
# dieses Jahr nur bis März erfasst wurde und stellt daher keinen tatsächlichen Rückgang 
# der Registrierungen dar. Die Churn-Raten liegen über die Registrierungsjahre hinweg auf einem ähnlichen Niveau.
# Der Chi-Quadrat-Test zeigt keinen statistisch signifikanteen Zusammenhang zwischen Registrierungs-
# jahr und Churn-Status (ꭓ² = 6,20; df = 8; p = 0,623). Somit liefert das Registrierungsjahr keinen deutlichen
# Hinweis auf ein unterschiedliches Churn-Verhalten. 

# =======================
# months inactive - Monate der Inaktivität 
# =======================

# Übersicht
summary(data$months_inactive)

# Land x Monate Inaktiv
aggregate(months_inactive ~ country, data = data, mean)

# Boxplot Inaktive Monate x Land
data%>%
  ggplot(aes(x = country, y = months_inactive)) +  
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.size = 2) +  
  labs (title = "Inaktive Monate nach Land") +
  xlab ("Land") +
  ylab ("Inaktive Monate")

# Balkendiagramm
data%>%
  ggplot(aes(x = months_inactive)) +
  geom_bar(width = 1, fill = "steelblue", alpha = 0.7,colour = "black") +
  theme(legend.position = "none") +
  labs(title = "Häufigkeitsverteilung der Anzahl inaktiver Monate") + 
  xlab ("Anzahl der inaktiven Monate") +
  ylab ("Anzahl der Nutzer")

# INTERPRETATION:
# Die Inaktivitätsdauer beträgt im Median 1 Monat und ist deutlich rechtsschief verteilt. 
# Die meisten Nutzer weisen nur kurze Inaktivitätszeiten auf. Zwischen den Ländern 
# bestehen geringe Unterschiede. 

# Monate der Inaktivität x Churn
# Aggregieren
aggregate(months_inactive ~ churn, data, mean)
aggregate(months_inactive ~ churn, data, median)
aggregate(months_inactive ~ churn, data, sd)

# Balkendiagramm 
data%>%
  ggplot(aes(x = months_inactive)) +
  geom_bar(width = 1, fill = "steelblue", alpha = 0.7, colour = "black") +
  facet_wrap(~ churn) + 
  theme(legend.position = "none") +
  labs(title = "Häufigkeitsverteilung der Anzahl inaktiver Monate") + 
  xlab ("Anzahl der inaktiven Monate") +
  ylab ("Anzahl der Nutzer")

# Boxplot Inaktive Monate x Churn
data%>%
  ggplot(aes(x = churn, y = months_inactive)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, colour = "black", outlier.colour = "red") +
  labs(title = "Monate inkativ nach Churn-Status") + 
  xlab ("Churn-Status") +
  ylab ("Anzahl der inaktiven Monate") +
  theme_minimal()

# Statistischer Vergleich
t.test(months_inactive ~ churn, data = data)

# INTERPRETATION:
# Kunden mit Churn weisen deutlich längere Inaktivitätszeiten auf als Kunden ohne Churn.
# Während Nicht-Abwanderer durchschnittlich rund 0,98 Monate innaktiv sind, beträgt der Mittelwert 
# bei abgewanderten Kunden etwa 4,49 Monate. Auch die Mediane unterscheiden sich deutlich 
# mit 1 gegenüber 4 Monanten.
# Der Welch-t-Test zeigt einen statistisch hoch signifikanten Unterschied der durchschnittlich
# Inaktivitätsdauer zwischen Churn- und nicht-Churn- Kunden (t = -157,3; p < 2,2 × 10^-16). 
# Die Ergebnisse zeigen einen deutlichen und statistischen signifikanten Unterschied der 
# Inaktivitätsdauer zwischen Churn- und Nicht-Churn-Kunden.

# Abonnementtyp x Inaktive Monate
aggregate(months_inactive ~ subscription_type, data, mean)

# Boxplot inaktive Monate x Churn
data%>%
  ggplot(aes(x = churn, y = months_inactive)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, colour = "black", outlier.colour = "red") +
  labs(title = "Anzahl inkativer Monate nach Churn-Status") + 
  xlab ("Churn-Status") +
  ylab ("Anzahl inaktiver Monate") +
  theme_minimal()

# INTERPRETATION:
# Nutzer mit Churn weisen deutlich mehr inaktive Monate auf als Nutzer ohne Churn. 
# Der Boxplot zeigt eine klare Verschiebung der Verteilung zu höheren Inaktivitätswerten bei 
# der Churn-Gruppe. Dies deutet auf einen starken Zusammenhang zwischen längerer INaktivitäts-
# dauer und dem Churn-Status hin. Insgesamt beträgt die duschschnittliche INaktivitätsdauer rund
# 1,5 Monate. 

# Korrelation: Inaktive Monate x 3-Moants-Inaktivitätsstatus
cor(data$months_inactive, data$inactive_3_months_flag)

# Numerische Churn-Codierung / Kontrolle der num. Churn-Codierung
data$churn_num <- ifelse(data$churn == "Yes", 1, 0)
table(data$churn, data$churn_num)

# Korrelation:  Inaktive Monate x Churn
cor(data$months_inactive, data$churn_num)

# Korrelation: 3-Monats-Inaktivitätsstatus x Churn
cor(data$inactive_3_months_flag, data$churn_num)

# Boxplot - 3-Monats-Inaktivitätsstatus x Inaktive Monate
data%>%
  ggplot(aes(x = factor(inactive_3_months_flag), y = months_inactive)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, colour = "black", outlier.colour = "red") +
  labs(title = "Monate inkativ nach 3 Monats-Inaktivitätsstatus") + 
  xlab ("3 Monate inaktiv (0 = Nein, 1 = Ja)") +
  ylab ("Anzahl der inaktiven Monate") +
  theme_minimal()

# INTERPRETATION:
# Die Korrelationsanalyse zeigt einen starken positiven Zusammenhang zwischen Inaktivität
# und Churn. Insbesondere weist der 3-Monats-Inaktivitätsindikator eine hohe Korrelation mit dem Churn-Status auf
# (r = 0,81). Auch die Anzahl der inaktiven Monate steht deutlich mit Churn in Zusammenhang (r = 0,66).
# Die Ergebnisse unterstützen damit die Annahme, dass eine erhöhte Inaktivität mit einem häufigeren 
# Auftreten von Churn verbunden ist.

# =====================================================
# Inactive 3 Months flag - 3-Monats-Inaktivitätsstatus
# =====================================================

# Übersicht
summary(data$inactive_3_months_flag)
table(data$inactive_3_months_flag)
prop.table(table(data$inactive_3_months_flag)) * 100

# Balkendiagramm
data%>%
  ggplot(aes(x = factor(inactive_3_months_flag))) +
  geom_bar(fill = "steelblue", alpha = 0.7,colour = "black") +
  labs(title = "Häufigkeitsverteilung des 3-Monats-Inaktivitätsstatus") +
  xlab ("Mindestens 3 Monate inaktiv (0 = Nein, 1 = Ja)") +
  ylab ("Anzahl der Nutzer") +
  theme_minimal()

# INTERPRETATION: 
# Von den 50.000 Nutzern waren 11.123 Personen (22,25 %) mindestens drei Monate inaktiv, während 
# 38.877 Nutzer (77,75 %) dieses Kriterium nicht erfüllen. Der überwiegende Teil der Nutzer
# weist keine Inaktivitätsdauer von mindestens drei Monaten auf.

# Balkendiagramm inaktive 3 Monate x Churn
data%>%
  ggplot(aes(x = factor(inactive_3_months_flag))) +
  geom_bar(fill = "steelblue", alpha = 0.7,colour = "black") +
  facet_wrap(~ churn) + 
  labs(title = "Häufigkeitsverteilung des 3-Monats-Inaktivitätsstatus nach Churn-Status") +
  xlab ("Mindestens 3 Monate inaktiv (0 = Nein, 1 = Ja)") +
  ylab ("Anzahl der Nutzer") +
  theme_minimal()

# Häufigkeit
table(data$inactive_3_months_flag, data$churn)
prop.table(table(data$churn, data$inactive_3_months_flag), margin = 1) * 100

# INTERPRETATION:
# Zwischen dem 3-Monats-Inaktivitätsstatus und dem Churn-Status zeigt sich ein sehr 
# deutlicher Unterschied. Von den Nutzern mit mindestens 3 Monaten Inaktivität sind
# 7.891 abgewandert, was rund 70,9 % dieser Gruppe entspricht. Bei Nutzern ohne 
# mindestens dreimonatige Inaktivität tritt im Datensatz dagegen kein Churn-Fall auf.

# Statistischer Zusammenhang
chisq.test(table(data$inactive_3_months_flag, data$churn))

# INTERPRETATION:
# Der Chi-Quadrat-Test bestätigt einen ststistisch hoch signifikanten Zusammenhang zwischen
# dem 3-Monats-Inaktivitätsstatus und Churn (ꭓ² = 32.744; df = 1; p < 2,2 x 10^-16).
# (Prüfen, ob direkt oder indirekt aus Churn erzeugt wurde, wegen Data Leakage)

# ===================================
# Ad interactions - Werbeinteraktion
# ===================================

# Häufigkeit der Werbeinteraktion
table(data$ad_interaction)

# Balkendiagramm
data%>%
  ggplot(aes(x = factor(ad_interaction))) + 
  geom_bar(width = 0.5,fill = "steelblue", alpha = 0.7, colour = "black", ) +
  labs(title = "Verteilung der Werbeinteraktionen") + 
  xlab ("Interaktion mit Werbung") +
  ylab ("Anzahl der Nutzer")

# INTERPRETATION:
# 17.486 (ca. 35 %) der Nutzer interagieren mit Werbung, während 32.514 (ca. 65 %)
# keine Werbeinteraktion aufweisen. Werbeinteraktionen treten damit bei etwa einem 
# Drittel der nutzer auf. 

# Balkendiagramm: Werbeinteraktion x Churn
data%>%
  ggplot(aes(x = factor(ad_interaction))) + 
  geom_bar(width = 0.5, fill = "steelblue", alpha = 0.7, colour = "black", ) +
  facet_wrap(~ churn) + 
  labs(title = "Verteilung der Werbeinteraktion nach Churn-Status") + 
  xlab ("Werbeinteraktion") +
  ylab ("Anzahl der Nutzer")

# Absolute Häufigkeit
table(data$ad_interaction, data$churn)

# Relative Häufigkeit
prop.table(table(data$ad_interaction, data$churn), margin = 1) * 100

# INTERPRETATION:
# Die Churn-Rate unterscheidet sich zwischen Nutzern mit und ohne Werbeinteraktion 
# nur geringfügig. Ohne Werbeinteraktion beträgt sie etwa 15,65 %, bei Nutzern mit 
# Werbeinteraktion etwa 16,02 %. Deskriptiv ist kein deutlicher Zusammenhang zwischen 
# Werbeinteraktion und Churn erkennbar. 

# Häufigkeiten: Werbeinteraktion x Abonnementtyp
table(data$ad_interaction, data$subscription_type)
prop.table(table(data$ad_interaction, data$subscription_type), margin = 2) * 100

# Statischer Zusammenhang: Abonnementtyp x Churn
chisq.test(table(data$subscription_type, data$churn))

# Statischer Zusammenhang: Werbeinteraktion x Abonnementtyp
chisq.test(table(data$ad_interaction, data$subscription_type))

# INTERPRETATION:
# Der Anteil der Nutzer mit Werbeinteraktion (Subscription Type) liegt bei allen Abonnementtypen 
# auf einem ähnlichen Niveau von etwa 34-35 %.
# Der Chi-Quadrat-Test bestätigt, dass kein statistisch signifikanter Zusammenhang zwischen
# Werbeinteraktion und Abonnementtyp besteht (ꭓ² = 4,96; df = 4; p = 0,291). 

# ==========================================================
# Ad conversion to subscription - Conversion zum Abonnemnet
# ==========================================================

table(data$ad_conversion_to_subscription)

prop.table(table(data$ad_conversion_to_subscription))

# Balkendiagramm
data%>%
  ggplot(aes(x = factor(
    ad_conversion_to_subscription, 
    levels = c("No", "Yes"),
    labels = c("Keine Conversion", "Conversion")))) +
  geom_bar(fill = "steelblue", alpha = 0.7,colour = "black") +
  labs (title = "Verteilung der Werbeconversion zum Abonnement") +
  xlab ("Conversion zum Abonnement") +
  ylab ("Anzahl der Nutzer")

# INTERPRETATION:
# 4.352 von 50.000 Nutzern (8,70 %) weisen eine Conversion zum Abonnement auf, während bei 45.648 
# Nutzern (91,30 %) keine Conversion erfolgt. Eine erfolgreiche Conversion tritt damit
# nur bei einem vergeleichweise kleinen Anteil der Nutzer auf.

# Balkendiagramm: Werbeconversion x Churn
data%>%
  ggplot(aes(x = factor(
   ad_conversion_to_subscription, 
   levels = c("No", "Yes"),
   labels = c("Keine Conversion", "Conversion")))) +
  geom_bar(fill = "steelblue", alpha = 0.7,colour = "black") +
  facet_wrap(~ churn) + 
  labs (title = "Werbeconversion zum Abonnement nach Churn-Status") +
  xlab ("Conversion zum Abonnement") +
  ylab ("Anzahl der Nutzer")

# INTERPRETATION:
# Die grafische Gegnüberstellung zeigt keine deutlichen Unterschiede hinsichtlich der 
# Werbeconversionzwischen Churn- und Nicht-Churn-Nutzern.

# Zusammenhang
chisq.test(table(data$ad_conversion_to_subscription, data$churn))

# INTERPRETATION:
# Der Chi-Quadrat-Test zeigt keinen statistisch signifikanten Zusammenhang
# zwischen der Conversion zum Abonnement und dem Churn-Status  (ꭓ² = 0,0002; df = 1; 
# P = 0.989). Die Variable liefert keinen Hinweis auf einen Zusammenhang mit der 
# Kundenabwanderung.

# ===========================================================================
# Music suggestion rating 1 to 5 - Bewertung der Musikvorschläge von 1 bis 5
# ===========================================================================

summary(data$music_suggestion_rating_1_to_5)

# Häufigkeiten
table(data$music_suggestion_rating_1_to_5, data$churn)
prop.table(table(data$music_suggestion_rating_1_to_5, data$churn), margin = 1)

# Balkendiagramm
data%>%
  ggplot(aes(x = music_suggestion_rating_1_to_5)) +
  geom_bar(fill = "steelblue", alpha = 0.7,colour = "black") +
  labs(title = "Bewertung der Musikvorschläge (1-5)") +
  xlab ("Bewertung (1 = sehr schlecht, 5 = sehr gut)") +
  ylab ("Anzahl der Nutzer")

# INTERPRETATION:
# Die Bewertung der Musikvorschläge liegt im Mittel bei 3,64 Punkten und im Median bei
# 4 Punkten. Am häufigsten wurde die Bewertung 4 vergeben, gefolgt von den Bewertungen 3 und 5.
# Insgesamt werden die Musikvorschläge damit überwiegend positiv bewertet.

# Balkendiagramm: Music suggestion rating 1 to 5 x Churn
data%>%
  ggplot(aes(x = music_suggestion_rating_1_to_5)) +
  geom_bar(fill = "steelblue", alpha = 0.7,colour = "black") +
  facet_wrap(~ churn) +
  labs(title = "Bewertung der Musikvorschläge (1-5)") +
  xlab ("Bewertung (1 = sehr schlecht, 5 = sehr gut)") +
  ylab ("Anzahl der Nutzer")

# INTERPRETATION:
# Die Verteilung der Bewertungen ist bei Churn- und Nicht-Churn-Nutzern sehr ähnlich.
# Auch die Churn-Raten unterscheiden sich zwischen den einzelnen Bewertungsstufen nur geringfügig
# und liegen ungefähr zwischen 15,4 % und 16,4 %. Deskriptiv ist daher kein deutlicher Zusammenhang
# zwischen der Bewertung der Musikvorschläge und Churn erkennbar.

# Statistischer Vergleich 
chisq.test(table(data$music_suggestion_rating_1_to_5, data$churn))

# INTERPRETATION:
# Der Chi-Quadrat-Test zeigt keinen statistisch signifikanten Zusammenhang zwischen der 
# Bewertung der Musikvorschläge und dem Churn-Status (ꭓ² = 4,45; df = 4; p = 0,349).
# Die Bewertung der Musikvorschläge liefert keinen deutlichen Hinweis auf unterschiedliche
# Churn-Verhalten.

# =======================================================================
# Avg listening hours per week - durchschnittliches Musikhören pro Woche
# =======================================================================

# Übersicht
summary(data$avg_listening_hours_per_week)
sd(data$avg_listening_hours_per_week)

# Histogramm
data%>%
  ggplot(aes(x = avg_listening_hours_per_week)) +
  geom_histogram(binwidth = 2, fill = "steelblue", alpha = 0.7,colour = "black") +
  labs(title = "Verteilung der durchschnittlichen Hörzeit pro Woche") +
  xlab("Hörstunden pro Woche") +
  ylab("Anzahl der Nutzer")

# INTERPRETATION:
# Die Nutzer hören im Durchschnitt rund 9,99 Stunden Musik pro Woche; der Median liegt 
# mit 9,98 Stunden nahezu identisch. Die mittleren 50 % der Nutzer weisen eine wöchentliche 
# Hörzeit zwischen 7,28 und 12,68 Stunden auf. Insgesamt reicht die Hörzeit von 0 bis 26,25 Stunden. 
# Die Verteilung ist annähernd symmetrisch und konzentriert sich um etwa 10 Stunden pro Woche.

# Histogramm: durchschnittliches Musikhören pro Woche x Churn
data%>%
  ggplot(aes(x = avg_listening_hours_per_week)) +
  geom_histogram(binwidth = 2, fill = "steelblue", alpha = 0.7,colour = "black") +
  facet_wrap(~ churn) +
  labs(title = "Durchschnittliche Hörzeit pro Woche nach Churn-Status") +
  xlab("Hörstunden pro Woche") +
  ylab("Anzahl der Nutzer")

# Density: durchschnittliches Musikhören pro Woche
data%>%
  ggplot(aes(x = avg_listening_hours_per_week)) +
  geom_density(fill = "steelblue", alpha = 0.7,colour = "black") +
  geom_vline(xintercept = mean(data$avg_listening_hours_per_week),
             linetype = "dashed",colour = "red") +
  labs(title = "Dichteverteilung der Hörstunden pro Woche") + 
  xlab("Hörstunden pro Woche") +
  ylab("Dichte")

# INTERPRETATION:
# Die Verteilung der wöchentlichen Hörzeit ist bei Churn- und Nicht-Churn-Nutzern sehr 
# ähnlich. Visuell sind keine deutlichen Unterschiede in der Nutzungsintensität zwischen 
# den beiden Gruppen erkennbar.

# Statistischer Vergleich
t.test(avg_listening_hours_per_week ~ churn, data = data)

# INTERPRETATION:
# Der Welch-t-Test zeigt keinen statistisch signifikanten Unterschied der durchschnittlichen 
# Hörzeit zwischen Churn- und Nicht-Churn-Nutzern (t = −0,25; p = 0,801). Die wöchentliche Hörzeit 
# steht somit in dieser Analyse nicht erkennbar mit dem Churn-Status in Zusammenhang.

# ========================================
# favorite Genre - Bevorzugte Musikgenres
# ========================================

table(data$favorite_genre)

prop.table(table(data$favorite_genre)) * 100

class(data$favorite_genre)

# Balkendiagramm 
data%>%
  ggplot(aes(x = favorite_genre)) +
  geom_bar(width = 0.5, fill = "steelblue", alpha = 0.7,colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Verteilung der bevorzugten Musikgenres") +
  xlab("Musikgenre") +
  ylab("Anzahl der Nutzer")

# INTERPRETATION:
# Die bevorzugten Musikgenres sind im Datensatz nahezu gleichmäßig verteilt. Jedes Genre 
# umfasst ungefähr 4.100 bis 4.300 Nutzer, sodass kein einzelnes Genre deutlich dominiert.

# Balkendiagramm: Bevorzugte Musikgenres x Churn
data%>%
  ggplot(aes(x = favorite_genre)) +
  geom_bar(width = 0.5, fill = "steelblue", alpha = 0.7,colour = "black") +
  facet_wrap( ~ churn) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Verteilung der bevorzugten Musikgenres nach Churn-Status") +
  xlab("Musikgenre") +
  ylab("Anzahl der Nutzer")

# Häufigkeit bevorzugtes Genre x Churn
table(data$favorite_genre, data$churn)
prop.table(table(data$favorite_genre, data$churn), margin = 1) * 100

# INTERPRETATION:
# Auch innerhalb der Churn- und Nicht-Churn-Gruppe zeigt sich eine sehr ähnliche Verteilung 
# der bevorzugten Genres. Deskriptiv sind keine auffälligen Unterschiede zwischen den Genres 
# hinsichtlich des Churn-Status erkennbar.

# Statistischer Zusammenhang
chisq.test(table(data$favorite_genre, data$churn))

# INTERPRETATION:
# Der Chi-Quadrat-Test zeigt keinen statistisch signifikanten Zusammenhang zwischen dem 
# bevorzugten Musikgenre und dem Churn-Status (χ² = 7,05; df = 11; p = 0,795). 
# Ein Zusammenhang zwischen dem bevorzugten Musikgenre und der Kundenabwanderung konnte 
# nicht nachgewiesen werden. 

# ======================================================
# Most liked feature - Favorisierte Plattformfunktionen
# ======================================================

# Häufigkeit
table(data$most_liked_feature)

# Häufigkeiten x Churn
table(data$most_liked_feature, data$churn)
prop.table(table(data$most_liked_feature, data$churn), margin = 1) * 100

# Balkendiagramm
data%>%
  ggplot(aes(x = most_liked_feature)) +
  geom_bar(width = 0.5, fill = "steelblue", alpha = 0.7,colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Favorisierte Features") +
  xlab("Features") +
  ylab("Anzahl der Nutzer")

# INTERPRETATION:
# Die favorisierten Plattformfunktionen sind nahezu gleichmäßig verteilt. Jede Funktion 
# wird von etwa 6.200 Nutzern als bevorzugtes Feature angegeben. Mit 6.358 Nennungen wird 
# „Lyrics“ am häufigsten gewählt, während „AI DJ“ mit 6.199 Nennungen am seltensten genannt 
# wird. Die Unterschiede sind insgesamt jedoch sehr gering.

# Balkendiagramm: Favorisierte Plattformfunktionen x Churn
data%>%
  ggplot(aes(x = most_liked_feature)) +
  geom_bar(width = 0.5, fill = "steelblue", alpha = 0.7,colour = "black") +
  facet_wrap(~ churn) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Favorisierte Features") +
  xlab ("Features") +
  ylab ("Anzahl der Nutzer")

# INTERPRETATION:
# Auch zwischen Churn- und Nicht-Churn-Nutzern zeigt sich eine sehr ähnliche Verteilung 
# der favorisierten Funktionen. Deskriptiv sind keine deutlichen Unterschiede hinsichtlich 
# des bevorzugten Features und des Churn-Status erkennbar.

# Statistischer Zusammenhang
chisq.test(data$most_liked_feature, data$churn)

# INTERPRETATION:
# Der Chi-Quadrat-Test zeigt keinen statistisch signifikanten Zusammenhang zwischen dem 
# favorisierten Plattform-Feature und dem Churn-Status (χ² = 3,59; df = 7; p = 0,825). 
# Das bevorzugte Feature steht somit in diesem Datensatz nicht erkennbar mit der Kundenabwanderung 
# in Zusammenhang.

# ===========================================================
# Desired future features - Gewünschte zukünftige Funktionen
# ===========================================================

# Häufigkeiten
table(data$desired_future_feature)
prop.table(table(data$desired_future_feature)) * 100


# Balkendiagramm
data%>%
  ggplot(aes(x = desired_future_feature)) +
  geom_bar(fill = "steelblue", alpha = 0.7, colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(title = "Verteilung der gewünschten Plattformfunktionen") +    
  xlab("Plattformfunktion") +
  ylab("Anzahl der Nutzer") 

# INTERPRETATION:
# Die gewünschten zukünftigen Funktionen sind nahezu gleichmäßig verteilt. Jede Funktion 
# wird von etwa 8.200 bis 8.400 Nutzern genannt. „Mood-based Auto Playlists“ wird mit 8.419 Nennungen 
# am häufigsten gewünscht, während „HiFi Audio“ mit 8.231 Nennungen am seltensten genannt wird. 
# Die Unterschiede sind insgesamt gering.

# Balkendiagramm: Gewünschte zukünftige Funktionen x Churn
data%>%
  ggplot(aes(x = desired_future_feature)) +
  geom_bar(fill = "steelblue", alpha = 0.7, colour = "black") +
  facet_wrap(~ churn) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  labs(title = "Gewünschte Plattformfunktionen nach Churn-Status") +    
  xlab("Plattformfunktion") +
  ylab("Anzahl der Nutzer") 

# Häufigkeiten: gewünschte Funktionen x Churn
table(data$desired_future_feature, data$churn)
prop.table(table(data$desired_future_feature, data$churn), margin = 1) * 100

# INTERPRETATION: 
# Die Churn-Raten unterscheiden sich zwischen den gewünschten zukünftigen Funktionen nur geringfügig
# und liegen zwischen etwa 15,3 % und 16,3 %. Die höchste Churn-Rate zeigt „Social Listening“ mit rund
# 16,3 %, die niedrigste „Better AI Recommendations“ mit rund 15,3 %. Deskriptiv ist kein deutlicher 
# Zusammenhang mit Churn erkennbar.

# Statistischer Zusammenhang
chisq.test(data$desired_future_feature, data$churn)

# INTERPRETATION:
# Der Chi-Quadrat-Test bestätigt, dass kein statistisch signifikanter Zusammenhang zwischen der 
# gewünschten zukünftigen Funktion und dem Churn-Status besteht (χ² = 3,89; df = 5; p = 0,566). 
# Die gewünschte zukünftige Funktion liefert somit keinen deutlichen Hinweis auf unterschiedliches 
# Churn-Verhalten.

# ======================================
# Primary device - verwendetes Endgerät
# ======================================

# Häufigkeiten
table(data$primary_device)
prop.table(table(data$primary_device)) * 100

# Balkendiagramm 
data %>%
  ggplot(aes(x = primary_device)) +
  geom_bar(fill = "steelblue", alpha = 0.7,, colour = "black") +
  labs(title = "Verteilung der verwendeten Endgeräte") +
  xlab("Endgeräte") +
  ylab("Anzahl der Nutzer")

# INTERPRETATION: 
# Die verwendeten Endgeräte sind im Datensatz nahezu gleichmäßig verteilt. Jede Gerätekategorie
# umfasst ungefähr 10.000 Nutzer. Ein bestimmtes Endgerät dominiert somit nicht deutlich.

# Balkendiagramm: Verwendetes Endgerät x Churn
data %>%
  ggplot(aes(x = primary_device)) +
  geom_bar(fill = "steelblue", alpha = 0.7,colour = "black") +
  facet_wrap(~ churn) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Verteilung der verwendeten Endgeräte nach Churn-Status") +
  xlab("Endgeräte") +
  ylab("Anzahl der Nutzer")

# Häufigkeiten: Verwendetes Endgerät x Churn
table(data$primary_device, data$churn)
prop.table(table(data$primary_device, data$churn), margin = 1) * 100

# INTERPRETATION:
# Die Churn-Raten unterscheiden sich zwischen den Endgeräten nur geringfügig. Sie liegen zwischen
# etwa 15,4 % bei Smart-Speaker-Nutzern und 16,4 % bei Mobile-Nutzern. Deskriptiv ist daher kein
# deutlicher Zusammenhang zwischen dem verwendeten Endgerät und Churn erkennbar.

# Statistischer Zusammenhang
chisq.test(data$primary_device, data$churn)

# INTERPRETATION:
# Der Chi-Quadrat-Test zeigt keinen statistisch signifikanten Zusammenhang zwischen dem primär
# verwendeten Endgerät und dem Churn-Status (χ² = 4,16; df = 4; p = 0,385). Das verwendete Endgerät
# liefert somit in diesem Datensatz keinen relevanten Hinweis auf Kundenabwanderung.

# ========================================
# Playlists created - erstellte Playlists
# ========================================

# Übersicht
summary(data$playlists_created)
sd(data$playlists_created)  

aggregate(playlists_created ~ churn, data = data, mean)

# Histogramm
data %>%
  ggplot(aes(x = playlists_created)) +
  geom_histogram(binwidth = 2, fill = "steelblue", alpha = 0.7,colour = "black") +
  labs(title = "Verteilung der erstellten Playlists") +
  xlab("Anzahl der erstellten Playlists") +
  ylab("Anzahl der Nutzer")

# INTERPRETATION:
# Die Nutzer erstellen im Durchschnitt etwa 8 Playlists (M = 8,00; Median = 8). Die mittleren 50 %
# der Werte liegen zwischen 6 und 10 Playlists. Insgesamt reicht die Anzahl von 0 bis 23 erstellten
# Playlists. Die Verteilung weist eine leichte Rechtsschiefe auf.

# Histogramm: erstellte Playlists x Churn
data %>%
  ggplot(aes(x = playlists_created)) +
  geom_histogram(binwidth = 2, fill = "steelblue", alpha = 0.7,colour = "black") +
  facet_wrap(~ churn) +
  labs(title = "Verteilung der erstellten Playlists nach Churn-Status") +
  xlab("Anzahl der erstellten Playlists") +
  ylab("Anzahl der Nutzer") 

# INTERPRETATION:
# Im oberen Wertebereich treten einzelne potenzielle Ausreißer auf. Da Werte bis 23 erstellte Playlists
# inhaltlich plausibel sind, können diese Beobachtungen im Datensatz belassen werden.

# Density
data %>%
  ggplot(aes(x = playlists_created)) +
  geom_density(fill = "steelblue", alpha = 0.2, colour = "steelblue", adjust = 2) +
  geom_vline(xintercept = mean(data$playlists_created),
             linetype = "dashed",colour = "red") +
  labs(title = "Verteilung der erstellten Playlists") +
  xlab("Anzahl der Playlists") +
  ylab("Dichte") +
  theme_minimal()

# Boxplot
data%>%
  ggplot(aes(x = factor(churn), y = playlists_created)) +
  geom_boxplot(fill = "steelblue", colour = "black", outlier.colour = "red") +
  labs(title = "Erstellten Playlists nach Churn-Status") +
  xlab("Churn-Status") +
  ylab("Anzahl der erstellten Playlists") +
  theme_minimal()

# Statistischer Vergleich
t.test(playlists_created ~ churn, data = data)

# INTERPRETATION:
# Die durchschnittliche Anzahl erstellter Playlists unterscheidet sich zwischen Kunden mit
# und ohne Churn nur geringfügig. Kunden ohne Churn erstellen durchschnittlich 7,99 Playlists,
# Kunden mit Churn 8,05 Playlists. Der Welch-t-Test zeigt, dass dieser Unterschied statistisch
# nicht signifikant ist (t = −1,74; p = 0,082). Somit lässt sich anhand der Anzahl erstellter 
# Playlists kein eindeutiger Zusammenhang mit dem Churn-Status feststellen.

# ====================================================
# Avg skips per day - durchschnittliche Skips pro Tag
# ====================================================

# Übersicht
summary(data$avg_skips_per_day)
sd(data$avg_skips_per_day)

# Histogramm
data %>%
  ggplot(aes(x = avg_skips_per_day)) +
  geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7, colour = "black") +
  labs(title = "Verteilung der durchschnittlichen Skips pro Tag") +
  xlab("Durchschnittliche Anzahl der Skips pro Tag") +
  ylab("Anzahl der Nutzer")

# INTERPRETATION:
# Die Nutzer überspringen im Durchschnitt etwa 10 Songs pro Tag.
# Der Median liegt ebenfalls bei 10 Skips, während die mittleren 50 % der Werte zwischen 
# 8 und 12 Skips pro Tag liegen. Die Verteilung ist annähernd unimodal mit einer leichten 
# Rechtsschiefe. Einzelne Nutzer weisen mit bis zu 25 Skips pro Tag vergleichsweise hohe Werte auf.

# Histogramm: durchschnittliche Skips pro Tag x Churn
data %>%
  ggplot(aes(x = avg_skips_per_day)) +
  geom_histogram(binwidth = 2, fill = "steelblue", alpha = 0.7, colour = "black") +
  facet_wrap(~ churn) +
  labs(title = "Verteilung der durchschnittlichen Skips pro Tag nach Churn-Status") +
  xlab("Durchschnittliche Anzahl der Skips pro Tag") +
  ylab("Anzahl der Nutzer")

# Density
data %>%
  ggplot(aes(x = avg_skips_per_day)) +
  geom_density(fill = "steelblue", alpha = 0.7, colour = "steelblue", adjust = 3) +
  geom_vline(xintercept = mean(data$avg_skips_per_day),
             linetype = "dashed",colour = "red") +
  labs(title = "Verteilung der durchschnittlichen Skips pro Tag") +
  xlab("Durchschnittliche Skips pro Tag") +
  ylab("Dichte") +
  theme_minimal()

# Boxplot
data%>%
  ggplot(aes(x = factor(churn), y = avg_skips_per_day)) +
  geom_boxplot(fill = "steelblue", colour = "black", outlier.colour = "red") +
  labs(title = "Durchschnittliche Anzahl der Skips pro Tag nach Churn") +
  xlab ("Churn") +
  ylab("Durchschnittliche Skips pro Tag")

# INTERPRETATION:
# Die durchschnittliche Anzahl der Skips pro Tag unterscheidet sich zwischen Kunden mit
# und ohne Churn nur minimal. Kunden ohne Churn weisen durchschnittlich etwa 10,02 Skips 
# pro Tag auf, während es bei Kunden mit Churn etwa 10,05 Skips sind. Auch die grafischen
# Verteilungen zeigen keine deutlichen Unterschiede zwischen den beiden Gruppen.

# Scatterplot
data %>%
  ggplot(aes(x = avg_skips_per_day, y = months_inactive)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Zusammenhang zwischen Skips und inaktiven Monaten") +
  xlab("Durchschnittliche Skips pro Tag") +
  ylab("Durchschnittliche Anzahl der inaktiven Monate")

# INTERPRETATION:
# Zwischen der durchschnittlichen Anzahl der Skips pro Tag und der Anzahl inaktiver Monate
# besteht praktisch kein linearer Zusammenhang (r = 0,007; p = 0,123). Der Zusammenhang ist
# statistisch nicht signifikant. Eine höhere Anzahl an Skips gibt es in diesem Datensatz somit
# keinen Hinweis darauf, dass es mit einer höheren Inaktivitätsdauer zusammenhängt.

# Aggregieren
aggregate(data$avg_skips_per_day ~ churn, data = data, mean)

# Zusammenhang
cor.test(data$months_inactive, data$avg_skips_per_day, method = "pearson")

# Statistischer Vergleich
t.test(avg_skips_per_day ~ churn, data = data)

# INTERPRETATION:
# Die durchschnittliche Anzahl der Skips pro Tag unterscheidet sich zwischen Kunden mit
# und ohne Churn nur geringfügig. Kunden ohne Churn überspringen durchschnittlich etwa 10,02 Songs pro Tag,
# während Kunden mit Churn durchschnittlich etwa 10,05 Songs überspringen.
#
# Der Welch-t-Test zeigt keinen statistisch signifikanten Unterschied zwischen den beiden Gruppen 
# (t = -0,63; p = 0,529). Somit besteht kein Hinweis darauf, dass sich die durchschnittliche Anzahl
# der täglichen Skips zwischen Churn- und Nicht-Churn-Kunden systematisch unterscheidet.

# ===========================================
# Korrelationsmatrix 
# ===========================================

round(cor(data[, c(
  "age",
  "avg_listening_hours_per_week",
  "avg_skips_per_day",
  "playlists_created",
  "months_inactive",
  "churn_num")]), 2)

install.packages("GGally")
library(GGally)

data%>%
  select(age, 
         avg_listening_hours_per_week, 
         avg_skips_per_day, playlists_created, 
         months_inactive,
         churn_num
  ) %>%
  ggpairs()

install.packages("corrplot")
library(corrplot)

cor_matrix <- cor(data[, c(
  "age",
  "avg_listening_hours_per_week",
  "avg_skips_per_day",
  "playlists_created",
  "months_inactive",
  "churn_num")])

# INTERPRETATION:
# Die paarweise explorative Analyse zwischen den ausgewählten Variablen überwiegend nur sehr 
# schwache Zusammenhänge. Die Korrelationskoeffizienten liegen größtenteils
# nahe bei null. Auch zwischen Churn- und nicht-churn-Nutzern zeigen sich bei Alter, Hörstunden,
# Skips und erstellten Playlists nur geringe Unterschiede. 
# deutlich auffälliger ist hingegen die Variable months_inactive. Nutzer mit Churn weisen tendenziell
# eine höhere Anzahl inaktiver Monate auf. Deskriptiv deutet dies darauf hin, dass insbesondere 
# die Inaktivitätsdauer mit dem Churn-Status zusammenhängt.

kurze_namen <- c("Alter", "Hörstunden", "Skips", "Playlists", "Inaktive Monate", "Churn")

colnames(cor_matrix) <- kurze_namen
rownames(cor_matrix) <- kurze_namen


corrplot(
  cor_matrix,
  method = "circle",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.0,
  cl.cex = 1.0,
  number.cex = 0.7,
  mar = c(1, 1, 1, 0),
  col = colorRampPalette(c("black", "blue", "firebrick2"))(500))

corrplot(
  cor_matrix,
  method = "circle",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.0,
  cl.cex = 1.0,
  addCoef.col = "black",
  number.cex = 0.8,
  mar = c(1, 1, 1, 1),
  col = colorRampPalette(c("black", "blue", "firebrick2"))(500))

# INTERPRETATION
# Die Korrelationsmatrix zeigt überwiegend keine bzw. nur sehr schwache lineare Zusammenhänge 
# zwischen den betrachteten Variablen. Eine deutliche Ausnahme bildet die Anzahl inaktiver
# Monate, die positiv mit dem Churn-Status zusammenhängt (r ≈ 0.66). Für Alter,
# Hörstunden, Skips, erstellte PLaylists und die Bewertung der Musikempfehlungen sind 
# dagegen keine relevanten Zusammenhänge mit Churn erkennbar.

# =============
# Modellierung 
# =============

# ==============================================
# Zielvariable als Faktor definieren und Prüfen
# ==============================================

data$churn <- factor(data$churn, levels = c("No", "Yes"))

# Prüfen - Churn als Faktor
class(data$churn)
is.factor(data$churn)

# Prüfen - Ausprägungen
levels(data$churn)
table(data$churn)

# Fehlende Werte prüfen
sum(is.na(data$churn))

# Kandidaten für das Churn-Modell
modell_variablen <- c(
  "churn",
  "age",
  "avg_listening_hours_per_week",
  "avg_skips_per_day",
  "playlists_created",
  "months_inactive",
  "music_suggestion_rating_1_to_5",
  "subscription_type",
  "ad_interaction",
  "ad_conversion_to_subscription",
  "favorite_genre"
)

# Prüfen modell_variablen
str(data[, modell_variablen])

# Fehlende Werte prüfen
colSums(is.na(data[, modell_variablen]))

# Numerische Variablen in Faktor Variablen umwandeln
data$subscription_type <- factor(data$subscription_type)
data$ad_interaction <- factor(data$ad_interaction)
data$ad_conversion_to_subscription <- factor(data$ad_conversion_to_subscription)
data$favorite_genre <- factor(data$favorite_genre)

# Faktor Variablen prüfen
str(data[, modell_variablen])

# Numerische Variable mit Likert-Skala in Faktor Variable umwandeln
data$music_suggestion_rating_1_to_5 <- factor(data$music_suggestion_rating_1_to_5, levels =c(1, 2, 3, 4, 5))

# Variable prüfen 
class(data$music_suggestion_rating_1_to_5)
levels(data$music_suggestion_rating_1_to_5)

# =================
# Train-Test-Split
# =================

# Modellierung
modell_data <- data[, modell_variablen]

install.packages("rsample")
library(rsample)


# Split
set.seed(123)

split <- initial_split(
  modell_data,
  prop = 0.8,
  strata = churn
)

# Trainings- und Testdaten erstellen
train_data <- training(split)
test_data <- testing(split)

dim(train_data)
dim(test_data)

# Absolute Häufigkeit der Zielvariable überprüfen
table(train_data$churn)
table(test_data$churn)

# Relative Häufigkeit der Zielvariable
prop.table(table(train_data$churn)) * 100
prop.table(table(test_data$churn)) * 100

# INTERPRETATION:
# Durch die stratifizierte Aufteilung bleibt die Verteilung der Zielvariable in Trainings- 
# und Testdaten nahezu identisch.
# In beiden Datensätzen beträgt der Anteil der Churn-Fällerund 15,8 %.

# =======================
# Logistische Regression
# =======================

# =================
# Modell erstellen
# =================
modell <- glm(
  churn ~ age +
    avg_skips_per_day +
    avg_listening_hours_per_week + 
    playlists_created + 
    music_suggestion_rating_1_to_5 +
    months_inactive +
    subscription_type +
    ad_interaction +
    ad_conversion_to_subscription +
    favorite_genre,
  data = train_data, 
  family = binomial(link = "logit")
)

# Zusammenfassung
summary (modell)


# INTERPRETATION:
# Das logistische Regressionsmodell zeigt, dass insbesondere die Anzahl inaktiver Monate
# mit dem Churn-Status zusammenhängt.
# Der Koeffizient von months_inactive ist positiv und hochsignitfikant (β ≈ 1,10; p < 0,001).
# Für Alter, Hörstunden, tägliche Skips, erstellte Playlists, Bewertung der musikvorschläge, 
# Abonnementtyp, sowie Werbeinteraktionen zeigen sich im multivariaten Modell keine statistische 
# signifikanten Effekte.
# Für das Musikgenre K-Pop zeigt sich im Vergleich zum Intercept ein statistisch signifikanter 
# negativer Zusammenhang mit dem Churn-Status (β ≈ -0,27; p < 0,004). K-Pop-Nutzer weisen somit
# im Modell tendenziell eine geringere Churn-Wahrscheinlichkeit als nutzer des Intercepts auf.
# Der Gesamteffekt der Variable favorite_genre sollte zusätzlich im weiteren Verlauf geprüft werden. 

# ============
# Odds Ratios 
# ============

# Odds Ratios berechnen
odds_ratios <- exp(coef(modell))
round(odds_ratios, 3)

# Odds Ratios mit 95 % Konfidenzintervalle und p-Werte
OR_Tabelle <- cbind(
  OR = exp(coef(modell)),
  Untergrenze_95 = exp(confint.default(modell)[, 1]),
  Obergrenze_95 = exp(confint.default(modell)[, 2]),
  p_Wert = coef(summary(modell))[, 4]
)

round(OR_Tabelle, 3)

# Prüfen: Referenzkategorie von favorite_genre
levels(train_data$favorite_genre)

# Prüfen: Gesamteffekt von favorite_genre
drop1(modell, test = "Chisq")

# INTERPRETATION:
# Für Alter, Hörstunden, Skips, erstellte Playlists, die Bewertung der Musikempfehlungen, 
# Subscrption Type sowie die Werbevariablen zeigen sich keine statistisch signifikanten 
# Effekte. Die jeweiligen 95 % Konfidenzintervalle schließen den Wert 1 ein.

# Für months_inactive beträgt die Odds Ratio 2,990 (95 % Konfidenzintervall: 2,917-3,066;
# P < 0,001).
# Unter Konstanthaltung der übrigen Variablen sind die Churn-Odds mit jedem zusätzlichen
# inaktiven Monat somit auf das 2,99-Fache erhöht.

# Der Likelihood Ratio Test zeigt für favorite_genre als Gesamtvariable keinen statistisch
# signifikanten Beitrag zum Modell (LRT = 11,4; df = 11; p = 0,412).
# Obwohl der einzelne Vergleich zwischen K-Pop und der Referenzkategorie Bollywood zuvor 
# statistisch signifikant war, ergibt sich für das bevorzugte Musikgenre insgesamt kein
# signifikanter Zusammenhang mit Churn. 

# ===================================
# Modelldiagnostik
# ===================================

install.packages("car")
library(car)

vif(modell)

# INTERPRETATION:
# Die GVIF-Werte liegen für alle Prädiktoren nahe bei 1 (zwischen 1,00 und 1,10). 
# Es bestehen somit keine Hinweise auf problematische Multikollinearität zwischen
# den im Modell enthaltenen Variablen.

# ===========================
# Linearität im Logit prüfen
# ===========================

modell_logit <- glm(
  churn ~ age + age:log(age + 1) +
    avg_skips_per_day + avg_skips_per_day:log(avg_skips_per_day + 1) +
    avg_listening_hours_per_week + 
    avg_listening_hours_per_week:log(avg_listening_hours_per_week + 1) +
    playlists_created +  playlists_created:log( playlists_created + 1 ) +
    months_inactive + months_inactive:log(months_inactive + 1 ) +
    music_suggestion_rating_1_to_5 + 
    subscription_type + 
    ad_interaction +
    ad_conversion_to_subscription + 
    favorite_genre,
  data = train_data,
  family = binomial(link = "logit")
)

summary(modell_logit)

# INTERPRETATION:
# Für Alter, Skips, Hörstunden und erstellte PLaylists ergeben sich keine Hinweise
# auf eine Verletzung der Linearitätsannahme im Logit (p > 0,05).

# Für months_inactive ist der zusätzliche Transformationsterm dagegen hichsignifikant
# (p < 0,001). Dies deutetdarauf hin, dass der Zusammenhang zwischen der Anzahl 
# inactiver Monate und den Log-Odds von Churn nicht ausreichend linear beschrieben wird. 

# ===================================
# Nichtlinearität von months_inactive 
# ===================================

# Spline Modellierung
modell_spline <- glm(
  churn ~ age +
    avg_skips_per_day +
    avg_listening_hours_per_week +
    playlists_created +
    music_suggestion_rating_1_to_5 +
    splines::ns(months_inactive, df = 3) +
    subscription_type +
    ad_interaction +
    ad_conversion_to_subscription +
    favorite_genre,
  data = train_data,
  family = binomial(link = "logit")
)

# Verteilung der inaktiven Monate 
table(train_data$months_inactive)

# Churn nach Anzahl inaktiver Monate
table(train_data$months_inactive, train_data$churn)

# INTERPRETATION:
# Für months_inactive zeigt sich kein linearer Zusammenhang mit Churn. Bei 0 bis 2 
# Monaten trifft im Trainingsdatensatz kein Churn auf. Ab 3 Monaten steigt der Churn-Anteil 
# sprunghaftauf etwa 71 % und bleibt anschließend auf einem hohen Niveau. Dies spricht 
# für einen Schwellenwert- bzw. nichtlinearen Zusammenhang.

# Zusammenhang zwischen months_inactive und inactive_3_months_flag prüfen
table(
  data$inactive_3_months_flag,
  data$months_inactive >= 3,
  useNA = "ifany"
)

sum(
  data$churn == "Yes" &
    data$months_inactive < 3
)

sum(
  data$churn == "No" &
    data$months_inactive >= 3
)

table(
  data$months_inactive >= 3,
  data$churn
)

# INTERPRETATION:
# Im Datensatz tritt kein Churn bei weniger als drei inaktiven Monaten auf.
# Ab drei inaktiven Monaten sind jedoch sowohl Churn- als auch Nicht-Churn-Fälle 
# vorhanden. Damit scheint eine Inaktivität von mindestens drei Monaten eine 
# notwendige Bedingung im vorliegenden Datensatz zu sein. 

# INTERPRETATION:
# Die Analyse zeigte einen außergewöhnlich starken Zusammenhang zwischen
# months_inactive und Churn. Die Zielvariable Churn entspricht dem ursprünglichen
# Subscription Status (Active/Inactive), während months_inactive angibt, wie lange
# ein Nutzer bereits inaktiv ist.
# Damit kann months_inactive bereits Informationen über den Zustand enthalten,
# der mit Churn vorhergesagt werden soll. Für die Prognose zukünftigen Churns
# besteht daher das Risiko von Target Leakage, da das Modell Informationen
# verwenden könnte, die erst mit bzw. nach Eintritt des Zielzustands bekannt sind.

# months_inactive und der daraus abgeleitete inactive_3_months_flag werden
# deshalb aus dem finalen Prognosemodell ausgeschlossen.

# ============================================
# Finales Modellierungsdatensatz ohne Leakage 
# ============================================
modell_variablen_final <- c(
  "churn",
  "age",
  "avg_listening_hours_per_week",
  "avg_skips_per_day",
  "playlists_created",
  "music_suggestion_rating_1_to_5",
  "subscription_type",
  "ad_interaction",
  "ad_conversion_to_subscription",
  "favorite_genre"
)

modell_data_final <- data[, modell_variablen_final]

str(modell_data_final)

# Fehlende Werte prüfen
colSums(is.na(modell_data_final))

# ============================================================
# Train-Test-Split
# ============================================================

set.seed(123)

split_final <- initial_split(
  modell_data_final,
  prop = 0.8,
  strata = churn
)

train_data_final <- training(split_final)
test_data_final  <- testing(split_final)

dim(train_data_final)
dim(test_data_final)

table(train_data_final$churn)
table(test_data_final$churn)

prop.table(table(train_data_final$churn)) * 100
prop.table(table(test_data_final$churn)) * 100

# =======================================
# Finales logistisches Regressionsmodell
# =======================================
modell_final <- glm(
  churn ~ age +
    avg_listening_hours_per_week +
    avg_skips_per_day +
    playlists_created +
    music_suggestion_rating_1_to_5 +
    subscription_type +
    ad_interaction +
    ad_conversion_to_subscription +
    favorite_genre,
  data = train_data_final,
  family = binomial(link = "logit")
)

summary(modell_final)

# INTERPRETATION:
# Nach Ausschluss der potenziellen Leakage-Variablen zeigt sich für keinen einzelnen 
# Modellkoeffizienten ein statistisch signifikanter Zusammenhang mit Churn auf dem 
# 5-%-Signifikanzniveau. Die geringe Reduktion der Deviance deutet zudem auf einen 
# begrenzten zusätzlichen Erklärungsbeitrag der verbleibenden Prädiktoren hin.
# Mehrstufige kategoriale Variablen werden anschließend zusätzlich über globale 
# Likelihood-Ratio-Tests geprüft.

# Odds Ratios
odds_ratios_final <- exp(coef(modell_final))
round(odds_ratios_final, 3)

# Odds Ratios mit 95%-Konfidenzintervallen und p-Werten
OR_Tabelle_final <- cbind(
  OR = exp(coef(modell_final)),
  Untergrenze_95 = exp(confint.default(modell_final)[, 1]),
  Obergrenze_95 = exp(confint.default(modell_final)[, 2]),
  p_Wert = coef(summary(modell_final))[, 4]
)

round(OR_Tabelle_final, 3)

# INTERPRETATION:
# Die Odds Ratios der Prädiktoren liegen überwiegend nahe bei 1. Zudem schließen 
# die 95%-Konfidenzintervalle bei allen Prädiktoren den Wert 1 ein. Somit ergeben sich 
# keine statistisch signifikanten Effekte auf die Churn-Odds.

# Globale Signifikanz der Prädiktoren
drop1(modell_final, test = "Chisq")

# INTERPRETATION:
# Die globalen Likelihood-Ratio-Tests zeigen für keinen der Prädiktoren einen statistisch
# signifikanten zusätzlichen Beitrag zum Modell (alle p-Werte > 0,05).
# Dies gilt auch für die mehrstufigen kategorialen Variablen music_suggestion_rating_1_to_5, 
# subscription_type und favorite_genre.

# Multikollinearität prüfen
vif(modell_final)

# INTERPRETATION:
# Die GVIF-Werte liegen für alle Prädiktoren nahe bei 1. Es bestehen keine 
# Hinweise auf problematische Multikollinearität zwischen den unabhängigen Variablen.

# Linearität im Logit prüfen
modell_logit_final <- glm(
  churn ~
    age + age:log(age + 1) +
    avg_listening_hours_per_week +
    avg_listening_hours_per_week:log(avg_listening_hours_per_week + 1) +
    avg_skips_per_day +
    avg_skips_per_day:log(avg_skips_per_day + 1) +
    playlists_created +
    playlists_created:log(playlists_created + 1) +
    music_suggestion_rating_1_to_5 +
    subscription_type +
    ad_interaction +
    ad_conversion_to_subscription +
    favorite_genre,
  data = train_data_final,
  family = binomial(link = "logit")
)

summary(modell_logit_final)

# INTERPRETATION:
# Für age, avg_listening_hours_per_week und avg_skips_per_day ergeben sich keine 
# Hinweise auf eine Verletzung der Linearitätsannahme im Logit (p > 0,05).
# Für playlists_created zeigt der zusätzliche Transformationsterm dagegen einen 
# signifikanten Effekt (p = 0,036), was auf einen möglichen nichtlinearen Zusammenhang
# mit den Churn-Log-Odds hindeutet.

# Zusammenhang zwischen playlists_created und Churn prüfen

library(dplyr)

playlist_check <- train_data_final %>%
  group_by(playlists_created) %>%
  summarise(
    Anzahl = n(),
    Churn_Rate = mean(churn == "Yes"),
    .groups = "drop"
  )

playlist_check

# Alle Werte anzeigen
print(playlist_check, n = Inf)

# INTERPRETATION:
# Die Churn-Rate bleibt über den überwiegend gut besetzten Bereich von playlists_created 
# weitgehend konstant bei etwa 15 bis 17 %. Größere Abweichungen treten hauptsächlich 
# bei sehr seltenen Randwerten auf, die nur wenige Beobachtungen enthalten. Daher 
# zeigt sich trotz des signifikanten Transformationsterms kein klarer stabiler 
# nichtlinearer Zusammenhang.

modell_playlist_spline <- glm(
  churn ~ age +
    avg_listening_hours_per_week +
    avg_skips_per_day +
    splines::ns(playlists_created, df = 3) +
    music_suggestion_rating_1_to_5 +
    subscription_type +
    ad_interaction +
    ad_conversion_to_subscription +
    favorite_genre,
  data = train_data_final,
  family = binomial(link = "logit")
)

AIC(modell_final, modell_playlist_spline)

# INTERPRETATION:
# Das Spline-Modell weist mit 34911,08 nur einen geringfügig niedrigeren AIC auf als 
# das lineare Modell mit 34911,64. Die Differenz von 0,56 zeigt keinen relevanten Vorteil 
# der komplexeren nichtlinearen Modellierung. Aus Gründen der Parsimonie wird playlists_created 
# daher weiterhin linear im finalen Modell berücksichtigt.

anova(
  modell_final,
  modell_playlist_spline,
  test = "Chisq"
)

# INTERPRETATION:
# Der Likelihood-Ratio-Test zeigt keine statistisch signifikante Verbesserung des Modells 
# durch die nichtlineare Spline-Modellierung von playlists_created (LRT = 4,559; df = 2; 
# p = 0,102).
# Daher wird playlists_created weiterhin linear im finalen Modell berücksichtigt.

# =================
# Kreuzvalidierung
# =================
set.seed(123)

cv_folds <- vfold_cv(
  train_data_final,
  v = 10,
  strata = churn
)

cv_folds

library(purrr)
install.packages("yardstick")
library(yardstick)

# ROC-AUC der 10 Cross-Validation-Folds

cv_auc <- map_dbl(cv_folds$splits, function(split) {
  
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  modell_fold <- glm(
    churn ~ age +
      avg_listening_hours_per_week +
      avg_skips_per_day +
      playlists_created +
      music_suggestion_rating_1_to_5 +
      subscription_type +
      ad_interaction +
      ad_conversion_to_subscription +
      favorite_genre,
    data = train_fold,
    family = binomial(link = "logit")
  )
  
  prob <- predict(
    modell_fold,
    newdata = valid_fold,
    type = "response"
  )
  
  roc_auc_vec(
    truth = valid_fold$churn,
    estimate = prob,
    event_level = "second"
  )
})

cv_auc

# Mittlere ROC-AUC der 10 Folds
mean(cv_auc)

# Standardabweichung der ROC-AUC
sd(cv_auc)

# INTERPRETATION:
# Die mittlere ROC-AUC der 10-fachen Kreuzvalidierung beträgt 0,496 bei einer 
# Standardabweichung von 0,015. Da eine ROC-AUC von etwa 0,50 dem Zufallsniveau entspricht,
# besitzt das Modell keine ausreichende Trennschärfe zwischen Churn- und Nicht-Churn-Fällen.
# Die geringe Streuung zeigt, dass dieses Ergebnis über die zehn Validierungsfolds relativ 
# stabil ist.

# PR-AUC der 10 Cross-Validation-Folds

cv_pr_auc <- map_dbl(cv_folds$splits, function(split) {
  
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  modell_fold <- glm(
    churn ~ age +
      avg_listening_hours_per_week +
      avg_skips_per_day +
      playlists_created +
      music_suggestion_rating_1_to_5 +
      subscription_type +
      ad_interaction +
      ad_conversion_to_subscription +
      favorite_genre,
    data = train_fold,
    family = binomial(link = "logit")
  )
  
  prob <- predict(
    modell_fold,
    newdata = valid_fold,
    type = "response"
  )
  
  pr_auc_vec(
    truth = valid_fold$churn,
    estimate = prob,
    event_level = "second"
  )
})

cv_pr_auc

# Mittlere PR-AUC
mean(cv_pr_auc)

# Standardabweichung
sd(cv_pr_auc)

# INTERPRETATION:
# Die mittlere PR-AUC der 10-fachen Kreuzvalidierung beträgt 0,156 bei einer 
# Standardabweichung von 0,006. Dieser Wert liegt ungefähr auf dem Niveau der 
# Churn-Prävalenz von 15,8 %. Das Modell erzielt damit auch hinsichtlich der Erkennung 
# der Churn-Fälle keine relevante Verbesserung gegenüber einem Modell ohne Trennschärfe.
# Die geringe Standardabweichung zeigt, dass dieses Ergebnis über die zehn Validierungsfolds
# relativ stabil ist.

# =================================
# Vorhersage auf dem Testdatensatz
# =================================


test_prob <- predict(
  modell_final,
  newdata = test_data_final,
  type = "response"
)

head(test_prob)

# =================
# Modellevaluation
# =================

# ROC-AUC auf dem Testdatensatz
test_roc_auc <- roc_auc_vec(
  truth = test_data_final$churn,
  estimate = test_prob,
  event_level = "second"
)

test_roc_auc

# PR-AUC auf dem Testdatensatz
test_pr_auc <- pr_auc_vec(
  truth = test_data_final$churn,
  estimate = test_prob,
  event_level = "second"
)

test_pr_auc

# INTERPRETATION:
# Auf dem Testdatensatz beträgt die ROC-AUC 0,493 und die PR-AUC 0,157.
# Die Ergebnisse stimmen weitgehend mit den Werten der Kreuzvalidierung überein. Damit 
# zeigt sich auch auf bisher ungesehenen Daten keine ausreichende Trennschärfe zwischen
# Churn- und Nicht-Churn-Fällen. Die geringe Abweichung zwischen Kreuzvalidierung und
# Testdatensatz spricht gegen ein relevantes Overfitting des Modells.

# Klassifikation bei Schwellenwert 0,5
test_class <- ifelse(
  test_prob >= 0.5,
  "Yes",
  "No"
)

test_class <- factor(
  test_class,
  levels = c("No", "Yes")
)

# Confusion Matrix
table(
  Vorhersage = test_class,
  Tatsächlich = test_data_final$churn
)

# INTERPRETATION:
# Bei einem Schwellenwert von 0,5 klassifiziert das Modell alle Beobachtungen als 
# No Churn. Dadurch werden zwar 8.422 Nicht-Churn-Fälle korrekt erkannt, jedoch kein
# einziger der 1.579 tatsächlichen Churn-Fälle. Die dadurch entstehende hohe Accuracy
# ist aufgrund der ungleichen Klassenverteilung nicht als gute Modellleistung zu 
# interpretieren.

# Accuracy
accuracy_vec(
  truth = test_data_final$churn,
  estimate = test_class
)

# Sensitivität / Recall für Churn
sens_vec(
  truth = test_data_final$churn,
  estimate = test_class,
  event_level = "second"
)

# Spezifität
spec_vec(
  truth = test_data_final$churn,
  estimate = test_class,
  event_level = "second"
)

# Precision
precision_vec(
  truth = test_data_final$churn,
  estimate = test_class,
  event_level = "second"
)

# F1-Score
f_meas_vec(
  truth = test_data_final$churn,
  estimate = test_class,
  event_level = "second"
)

# INTERPRETATION:
# Bei einem Schwellenwert von 0,5 klassifiziert das Modell sämtliche Beobachtungen
# als No Churn. Die Accuracy beträgt dadurch 84,21 %, was im Wesentlichen dem Anteil
# der Mehrheitsklasse entspricht.
#
# Die Sensitivität beträgt 0 %, da kein einziger tatsächlicher Churn-Fall erkannt wird. 
# Die Spezifität beträgt dagegen 100 %, da alle Nicht-Churn-Fälle korrekt als No Churn
# klassifiziert werden.
#
# Precision und F1-Score sind nicht definiert, da das Modell keine positiven Churn
# Vorhersagen erzeugt. Die hohe Accuracy ist daher nicht als gute Modellleistung zu
# interpretieren.

# Balanced Accuracy
bal_accuracy_vec(
  truth = test_data_final$churn,
  estimate = test_class,
  event_level = "second"
)

# INTERPRETATION:
# Die Balanced Accuracy beträgt 0,50.
# Da sie Sensitivität und Spezifität gleichermaßen berücksichtigt, zeigt dieser Wert,
# dass das Modell insgesamt keine bessere Klassifikationsleistung als auf Zufallsniveau
# erreicht. Die hohe Accuracy von 84,21 % entsteht lediglich dadurch, dass sämtliche
# Fälle der Mehrheitsklasse No Churn zugeordnet werden.

# ===================================
# Interpretation der modellergebnisse
# ===================================

# Nach Ausschluss der potenziellen Leakage-Variablen months_inactive und 
# inactive_3_months_flag zeigt das finale logistische Regressionsmodell keine 
# statistisch signifikanten Prädiktoren auf dem 5-%-Niveau. Auch die Odds Ratios
# liegen überwiegend nahe bei 1 und die zugehörigen 95% Konfidenzintervalle schließen
# jeweils den Wert 1 ein.
#
# Die 10-fache Kreuzvalidierung ergibt eine mittlere ROC-AUC von 0,496 sowie eine
# mittlere PR-AUC von 0,156. Beide Werte liegen ungefähr auf dem Niveau eines Modells
# ohne relevante Trennschärfe.
#
# Dieses Ergebnis bestätigt sich auf dem unabhängigen Testdatensatz:
# Die ROC-AUC beträgt 0,493 und die PR-AUC 0,157.
#
# Bei einem Klassifikationsschwellenwert von 0,5 sagt das Modell sämtliche Beobachtungen
# als No Churn voraus. Dadurch beträgt die Accuracy zwar 84,21 %, die Sensitivität
# für Churn jedoch 0 % und die Spezifität 100 %.
# Die Balanced Accuracy von 0,50 zeigt, dass die hohe Accuracy ausschließlich
# durch die Mehrheitsklasse zustande kommt.
#
# Insgesamt besitzt das finale Modell daher keine ausreichende prognostische Fähigkeit
# zur zuverlässigen Identifikation von Churn-Fällen.

install.packages("ranger")
library(ranger)

set.seed(123)

cv_folds <- vfold_cv(
  train_data_final,
  v = 10,
  strata = churn
)

cv_folds

# ============================================================
# Random Forest – 10-fache Kreuzvalidierung
# ============================================================

cv_rf_auc <- map_dbl(cv_folds$splits, function(split) {
  
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  rf_fold <- ranger(
    churn ~ .,
    data = train_fold,
    probability = TRUE,
    num.trees = 500,
    seed = 123
  )
  
  prob_rf <- predict(
    rf_fold,
    data = valid_fold
  )$predictions[, "Yes"]
  
  roc_auc_vec(
    truth = valid_fold$churn,
    estimate = prob_rf,
    event_level = "second"
  )
})

cv_rf_auc 

# Mittlere ROC-AUC des Random Forest
mean(cv_rf_auc)

# Standardabweichung
sd(cv_rf_auc)

# INTERPRETATION:
# Der Random Forest erreicht in der 10-fachen Kreuzvalidierun eine mittlere ROC-AUC
# von 0,502 bei einer Standardabweichung von 0,010. Damit liegt die Trennschärfe des
# Modells praktisch auf Zufallsniveau. Gegenüber der logistischen Regression mit einer
# mittleren ROC-AUC von 0,496 zeigt sich keine relevante Verbesserung.

# PR-AUC des Random Forest in der 10-fachen Kreuzvalidierung
cv_rf_pr_auc <- map_dbl(cv_folds$splits, function(split) {
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  rf_fold <- ranger(
    churn ~ .,
    data = train_fold,
    probability = TRUE,
    num.trees = 500, 
    seed = 123
  )
  
  prob_rf <- predict(
    rf_fold,
    data = valid_fold
  )$predictions[, "Yes"]
  
  pr_auc_vec(
    truth = valid_fold$churn,
    estimate = prob_rf,
    event_level = "second"
  )
})

# Mittlere PR-AUC
mean(cv_rf_pr_auc)

# Standardabweichung
sd(cv_rf_pr_auc)

# INTERPRETATION:
# Der Random Forest erreicht in der 10-fachen Kreuzvalidierung eine mittlere PR-AUC
# von 0,159 bei einer Standardabweichung von 0,006. Der Wert liegt damit ungefähr
# auf Höhe der Churn-Prävalenz von 15,8 %. Der Random Forest zeigt somit auch hinsichtlich
# der Erkennung von Churn-Fällen keine relevante prognostische Verbesserung.

install.packages("rpart")
library(rpart)

# ============================================================
# Decision Tree – 10-fache Kreuzvalidierung
# ============================================================
set.seed(123)

cv_tree_auc <- map_dbl(cv_folds$splits, function(split) {
  
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  tree_fold <- rpart(
    churn ~ .,
    data = train_fold,
    method = "class"
  )
  
  prob_tree <- predict(
    tree_fold,
    newdata = valid_fold,
    type = "prob"
  )[, "Yes"]
  
  roc_auc_vec(
    truth = valid_fold$churn,
    estimate = prob_tree,
    event_level = "second"
  )
})

cv_tree_auc

# Mittlere ROC-AUC
mean(cv_tree_auc)

# Standardabweichung
sd(cv_tree_auc)

# Decision Tree auf gesamten Trainingsdaten
tree_final <- rpart(
  churn ~ .,
  data = train_data_final,
  method = "class"
)

tree_final

printcp(tree_final)

# INTERPRETATION:
# Der Decision Tree erzeugt mit den Standardeinstellungen keine Aufteilungen und
# besteht ausschließlich aus dem Wurzelknoten. Es werden keine Prädiktoren für die
# Baumkonstruktion verwendet (Variables actually used: character(0)).
# Aufgrund der Mehrheitsklasse wird für alle Beobachtungen No Churn vorhergesagt.
# Dies erklärt die ROC-AUC von 0,50 in allen Cross-Validation-Folds. 

# ============================================================
# Decision Tree – Tuning des Complexity Parameters
# ============================================================

cp_werte <- c(0, 0.0001, 0.0005, 0.001, 0.005, 0.01)

tree_tuning <- map_dfr(cp_werte, function(cp_wert) {
  
  auc_folds <- map_dbl(cv_folds$splits, function(split) {
    
    train_fold <- analysis(split)
    valid_fold <- assessment(split)
    
    tree_fold <- rpart(
      churn ~ .,
      data = train_fold,
      method = "class",
      control = rpart.control(
        cp = cp_wert
      )
    )
    
    prob_tree <- predict(
      tree_fold,
      newdata = valid_fold,
      type = "prob"
    )[, "Yes"]
    
    roc_auc_vec(
      truth = valid_fold$churn,
      estimate = prob_tree,
      event_level = "second"
    )
  })
  
  data.frame(
    cp = cp_wert,
    ROC_AUC_Mittel = mean(auc_folds),
    ROC_AUC_SD = sd(auc_folds)
  )
})

tree_tuning

# =================================================
# Decision Tree (cp = 0) – PR-AUC Kreuzvalidierung
# =================================================
set.seed(123)

cv_tree_pr_auc <- map_dbl(cv_folds$splits, function(split) {
  
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  tree_fold <- rpart(
    churn ~ .,
    data = train_fold,
    method = "class",
    control = rpart.control(
      cp = 0
    )
  )
  
  prob_tree <- predict(
    tree_fold,
    newdata = valid_fold,
    type = "prob"
  )[, "Yes"]
  
  pr_auc_vec(
    truth = valid_fold$churn,
    estimate = prob_tree,
    event_level = "second"
  )
})

cv_tree_pr_auc

# Mittlere PR-AUC
mean(cv_tree_pr_auc)

# Standardabweichung
sd(cv_tree_pr_auc)

# INTERPRETATION:
# Der getunte Decision Tree erreicht in der 10-fachen Kreuzvalidierung eine mittlere
# PR-AUC von 0,160 bei einer Standardabweichung von 0,005. Der Wert liegt nur geringfügig
# über der Churn-Prävalenz von etwa 15,8 %. Zusammen mit der mittleren ROC-AUC von 0,504
# zeigt sich somit auch für den Decision Tree keine ausreichende prognostische Trennschärfe.

modellvergleich_cv <- data.frame(
  Modell = c(
    "Logistische Regression",
    "Random Forest",
    "Decision Tree"
  ),
  ROC_AUC = c(
    mean(cv_auc),
    mean(cv_rf_auc),
    max(tree_tuning$ROC_AUC_Mittel)
  ),
  PR_AUC = c(
    mean(cv_pr_auc),
    mean(cv_rf_pr_auc),
    mean(cv_tree_pr_auc)
  )
)

round(modellvergleich_cv[, -1], 3)
modellvergleich_cv

# INTERPRETATION:
# Im Vergleich der drei Modelle erzielt der Decision Tree numerisch die höchste
# mittlere ROC-AUC (0,504) und PR-AUC (0,160). Die Unterschiede zur logistischen
# Regression und zum Random Forest sind jedoch sehr gering.
# Insgesamt liegen die ROC-AUC-Werte aller Modelle nahe bei 0,50 und die PR-AUC-Werte
# ungefähr auf Höhe der Churn-Prävalenz. Somit zeigt keines der drei Modelle eine 
# ausreichende prognostische Trennschärfe.

set.seed(123)

rf_final <- ranger(
  churn ~ .,
  data = train_data_final,
  probability = TRUE,
  num.trees = 500,
  seed = 123
)

rf_test_prob <- predict(
  rf_final,
  data = test_data_final
)$predictions[, "Yes"]

rf_test_roc <- roc_auc_vec(
  truth = test_data_final$churn,
  estimate = rf_test_prob,
  event_level = "second"
)

rf_test_pr <- pr_auc_vec(
  truth = test_data_final$churn,
  estimate = rf_test_prob,
  event_level = "second"
)

rf_test_roc
rf_test_pr

# INTERPRETATION:
# Der Random Forest erreicht auf dem Testdatensatz eine ROC-AUC von 0,504 und eine
# PR-AUC von 0,159. Die Werte stimmen weitgehend mit den Ergebnissen der Kreuzvalidierung
# überein. Damit zeigt sich kein relevantes Overfitting, jedoch auch keine ausreichende
# prognostische Trennschärfe für Churn.

# Finalen Decision Tree auf den gesamten Trainingsdaten trainieren
tree_final_tuned <- rpart(
  churn ~ .,
  data = train_data_final,
  method = "class",
  control = rpart.control(
    cp = 0
  )
)

# Churn-Wahrscheinlichkeiten auf Testdaten
tree_test_prob <- predict(
  tree_final_tuned,
  newdata = test_data_final,
  type = "prob"
)[, "Yes"]

# ROC-AUC
tree_test_roc <- roc_auc_vec(
  truth = test_data_final$churn,
  estimate = tree_test_prob,
  event_level = "second"
)

# PR-AUC
tree_test_pr <- pr_auc_vec(
  truth = test_data_final$churn,
  estimate = tree_test_prob,
  event_level = "second"
)

tree_test_roc
tree_test_pr

# ========================
# Finaler Modellvergleich
# ========================
modellvergleich_final <- data.frame(
  Modell = c(
    "Logistische Regression",
    "Random Forest",
    "Decision Tree"
  ),
  CV_ROC_AUC = c(
    mean(cv_auc),
    mean(cv_rf_auc),
    max(tree_tuning$ROC_AUC_Mittel)
  ),
  CV_PR_AUC = c(
    mean(cv_pr_auc),
    mean(cv_rf_pr_auc),
    mean(cv_tree_pr_auc)
  ),
  Test_ROC_AUC = c(
    test_roc_auc,
    rf_test_roc,
    tree_test_roc
  ),
  Test_PR_AUC = c(
    test_pr_auc,
    rf_test_pr,
    tree_test_pr
  )
)

modellvergleich_final


install.packages("xgboost")
library(xgboost)

# ============================================================
# XGBoost – Daten vorbereiten
# ============================================================

x_train_xgb <- model.matrix(
  ~ . - 1,
  data = train_data_final[, setdiff(names(train_data_final), "churn")]
)

y_train_xgb <- ifelse(
  train_data_final$churn == "Yes",
  1,
  0
)

# ============================================================
# XGBoost – 10-fache Kreuzvalidierung
# ============================================================

set.seed(123)

cv_xgb_metrics <- map_dfr(cv_folds$splits, function(split) {
  
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  x_train_fold <- model.matrix(
    ~ . - 1,
    data = train_fold[, setdiff(names(train_fold), "churn")]
  )
  
  x_valid_fold <- model.matrix(
    ~ . - 1,
    data = valid_fold[, setdiff(names(valid_fold), "churn")]
  )
  
  y_train_fold <- ifelse(
    train_fold$churn == "Yes",
    1,
    0
  )
  
  dtrain <- xgb.DMatrix(
    data = x_train_fold,
    label = y_train_fold
  )
  
  xgb_fold <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = 3,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    data = dtrain,
    nrounds = 200,
    verbose = 0
  )
  
  prob_xgb <- predict(
    xgb_fold,
    x_valid_fold
  )
  
tibble(
  roc_auc = roc_auc_vec (
    truth = valid_fold$churn,
    estimate = prob_xgb,
    event_level = "second"
  ), 
  pr_auc = pr_auc_vec(
    truth = valid_fold$churn,
    estimate = prob_xgb,
    event_level = "second"
  )
)
})

cv_xgb_auc <- cv_xgb_metrics$roc_auc
cv_xgb_pr_auc <- cv_xgb_metrics$pr_auc

mean(cv_xgb_auc)
sd(cv_xgb_auc)

mean(cv_xgb_pr_auc)
sd(cv_xgb_pr_auc)

# INTERPRETATION:
# XGBoost erreicht in der 10-fachen Kreuzvalidierung eine mittlere ROC-AUC von 
# 0,492 bei einer Standardabweichung von 0,012. Die ROC-AUC liegt damit praktisch
# auf Zufallsniveau. Auch das Gradient-Boosting-Modell zeigt somit keine ausreichende
# Trennschärfe zur Vorhersage von Churn. Für PR-AUC 0,156 und SD PR-AUC 0,006.

# ============================================================
# XGBoost – Klassenungleichgewicht berücksichtigen
# ============================================================

set.seed(123)

cv_xgb_weighted <- map_dfr(cv_folds$splits, function(split) {
  
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  # Prädiktoren in numerische Matrizen umwandeln
  x_train_fold <- model.matrix(
    ~ . - 1,
    data = train_fold[, setdiff(names(train_fold), "churn")]
  )
  
  x_valid_fold <- model.matrix(
    ~ . - 1,
    data = valid_fold[, setdiff(names(valid_fold), "churn")]
  )
  
  # Zielvariable: No = 0, Yes = 1
  y_train_fold <- ifelse(
    train_fold$churn == "Yes",
    1,
    0
  )
  
  # Gewicht für die Minderheitsklasse Churn
  gewicht_churn <- sum(y_train_fold == 0) / sum(y_train_fold == 1)
  
  # XGBoost-Datenformat
  dtrain <- xgb.DMatrix(
    data = x_train_fold,
    label = y_train_fold
  )
  
  # Gewichtetes XGBoost-Modell
  xgb_fold <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = 3,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8,
      scale_pos_weight = gewicht_churn
    ),
    data = dtrain,
    nrounds = 200,
    verbose = 0
  )
  
  # Vorhersage
  prob_xgb <- predict(
    xgb_fold,
    x_valid_fold
  )
  
  data.frame(
    ROC_AUC = roc_auc_vec(
      truth = valid_fold$churn,
      estimate = prob_xgb,
      event_level = "second"
    ),
    PR_AUC = pr_auc_vec(
      truth = valid_fold$churn,
      estimate = prob_xgb,
      event_level = "second"
    )
  )
})

cv_xgb_weighted

# Mittelwerte
colMeans(cv_xgb_weighted)

# Standardabweichungen
sapply(cv_xgb_weighted, sd)

# INTERPRETATION:
# Die Gewichtung der Churn-Klasse führt zu keiner relevanten Verbesserung der Modellleistung.
# Die mittlere ROC-AUC beträgt etwa 0,491 und die mittlere PR-AUC etwa 0,155.
# Damit liegen beide Kennzahlen weiterhin ungefähr auf Zufalls- bzw. Basisniveau.
# Das Klassenungleichgewicht ist somit nicht die Hauptursache für die geringe prognostische
# Trennschärfe des XGBoost-Modells.

install.packages("themis")

library(recipes)
library(themis)

# Ersten Cross-Validation-Fold auswählen
smote_train <- analysis(cv_folds$splits[[1]])
smote_valid <- assessment(cv_folds$splits[[1]])

# SMOTE-Rezept
smote_recipe <- recipe(churn ~ ., data = smote_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(churn, over_ratio = 1)

# Rezept anhand der Trainingsdaten vorbereiten
smote_prep <- prep(
  smote_recipe,
  training = smote_train
)

# SMOTE nur auf Trainingsdaten anwenden
train_smote <- bake(
  smote_prep,
  new_data = NULL
)

# Validierungsdaten lediglich transformieren – kein SMOTE
valid_smote <- bake(
  smote_prep,
  new_data = smote_valid
)

# Kontrolle
table(train_smote$churn)
table(valid_smote$churn)

# ============================================================
# Random Forest + SMOTE – 10-fache Kreuzvalidierung
# ============================================================

set.seed(123)

cv_rf_smote <- map_dfr(cv_folds$splits, function(split) {
  
  train_fold <- analysis(split)
  valid_fold <- assessment(split)
  
  # SMOTE-Rezept nur auf Trainingsfold
  smote_recipe <- recipe(churn ~ ., data = train_fold) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_smote(churn, over_ratio = 1)
  
  smote_prep <- prep(
    smote_recipe,
    training = train_fold
  )
  
  # Trainingsdaten mit SMOTE
  train_smote <- bake(
    smote_prep,
    new_data = NULL
  )
  
  # Validierungsdaten ohne SMOTE
  valid_smote <- bake(
    smote_prep,
    new_data = valid_fold
  )
  
  # Random Forest
  rf_smote <- ranger(
    churn ~ .,
    data = train_smote,
    probability = TRUE,
    num.trees = 500,
    seed = 123
  )
  
  # Churn-Wahrscheinlichkeiten
  prob_rf_smote <- predict(
    rf_smote,
    data = valid_smote
  )$predictions[, "Yes"]
  
  # Metriken
  data.frame(
    ROC_AUC = roc_auc_vec(
      truth = valid_smote$churn,
      estimate = prob_rf_smote,
      event_level = "second"
    ),
    
    PR_AUC = pr_auc_vec(
      truth = valid_smote$churn,
      estimate = prob_rf_smote,
      event_level = "second"
    )
  )
})

cv_rf_smote

# INTERPRETATION:
# Der Random Forest mit SMOTE erreicht in der 10-fachen
# Kreuzvalidierung eine mittlere ROC-AUC von etwa 0,495
# und eine mittlere PR-AUC von etwa 0,157.
#
# SMOTE führt damit zu keiner relevanten Verbesserung
# gegenüber dem Random Forest ohne Oversampling.
# Dies spricht ebenfalls dafür, dass das Klassenungleichgewicht
# nicht die Hauptursache für die geringe Prognoseleistung ist.
