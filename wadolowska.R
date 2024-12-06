install.packages("ggplot2")
install.packages("fitdistrplus")


library(ggplot2)
library(fitdistrplus)

# Ustawienie lokalizacij konsoli
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
getwd()



#-----------------------------------------
# Zad 1
# Ceny akcji spółki na rok 2022
#-----------------------------------------


cln <- read.csv("cln_d.csv")

kurs_zamkniecia <- cln$Zamkniecie
kurs_data <- as.Date(cln$Data)

clnf <- data.frame(data = kurs_data, zamkniecie = kurs_zamkniecia)

#-----------------------------------------
# Wykres zamknięcia
#-----------------------------------------

wykres_kursu <- ggplot(clnf, aes(x = data, y = zamkniecie, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "Cena zamknięcia (zł)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(clnf$data), max(clnf$data))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_kursu

ggsave(
  "img/Wykres_cen_akcji_cln.png",
  plot = wykres_kursu,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Histogram kursów zamknięcia
#-----------------------------------------

histogram_kursu_ggplot <- ggplot(
  clnf,
  aes(x = zamkniecie, y = after_stat(density))
) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(title = NULL, x = "Cena zamknięcia (zł)", y = "Gęstość")

hist(kurs_zamkniecia, prob = TRUE, xlab = "Zamknięcie", ylab = "Gęstość")

histogram_kursu_ggplot

ggsave(
  "img/historgram_cln.png",
  plot = histogram_kursu_ggplot,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Log zwroty (stopy zwrotu)
#-----------------------------------------


log_zwroty_cln <- diff(log(kurs_zamkniecia))

# Przycięcie pierwszego elementu kurs_data
kurs_data_cut <- kurs_data[-1]

log_zwroty_clndf <- data.frame(data = kurs_data_cut, log_zwroty= log_zwroty_cln)

?hist


histogram_zwrotów_ggplot <- ggplot(
  log_zwroty_clndf,
  aes(x = log_zwroty, y = after_stat(density))
) +
  geom_histogram(fill = "grey", color = "black") +
  labs(title = NULL, x = "log-zwroty (%)", y = "Gęstość")

histogram_zwrotów_ggplot

wykres_zwrotów <- ggplot(log_zwroty_clndf, aes(x = data, y = log_zwroty, group = 1)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "log-zwroty (%)") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(min(kurs_data_cut), max(kurs_data_cut))
  ) +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1))

wykres_zwrotów

ggsave(
  "img/historgram_zwrotów_cln.png",
  plot = histogram_zwrotów_ggplot,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

ggsave(
  "img/wykres_zwrotów_cln.png",
  plot = wykres_zwrotów,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

#-----------------------------------------
# Estymacja parametrów rozkładu normalnego i t-studenta
#-----------------------------------------

dist_norm <- fitdist(log_zwroty_cln, "norm")
dist_t <- fitdist(log_zwroty_cln, "t", start=list(df=12))

curve(dt(x,dist_t$estimate), xlim=c(-4,4), col=2,lwd=2)


dist_norm
dist_t
#-----------------------------------------
# Wykresy diagnostyczne
#-----------------------------------------

par(mfrow=c(1,1))
curve(dnorm(x,dist_norm$estimate[1],dist_norm$estimate[2]), xlim=c(-4,4),lwd=2)
curve(dt(x,dist_t$estimate), add=T, col=2,lwd=2)

key <- c("norm", "t-student")
png(
  "img/wykresy_diagnostyczne_cln.png",
  width = 18,
  height = 18,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(2, 2))
denscomp(list(dist_norm, dist_t), legendtext = key)
qqcomp(list(dist_norm, dist_t), legendtext = key)
cdfcomp(list(dist_norm, dist_t), legendtext = key)
ppcomp(list(dist_norm, dist_t), legendtext = key)

dev.off()


#-----------------------------------------
# Analiza wartości statystyk
#-----------------------------------------

gofstat(
  list(dist_norm, dist_t),
  fitnames = key
)


#-----------------------------------------
# Zad 5
# Test hipotezy o równości rozkładów
#-----------------------------------------

iterations <- 10000
n <- length(log_zwroty_cln)
n

D <- c()

for (i in 1:iterations) {
  y_ln <- rnorm(n, dist_norm$estimate[1], dist_norm$estimate[2])
  D[i] <- ks.test(
    y_ln,
    pnorm,
    dist_norm$estimate[1],
    dist_norm$estimate[2],
    exact = TRUE
  )$statistic
}

# Obliczamy dn_ln, czyli wartosc statystyki D,
# dla danych kurs_zamkniecia i rozkładu F0 wybranego w punkcie
dn_n <- ks.test(
  log_zwroty_cln,
  pnorm,
  dist_norm$estimate[1],
  dist_norm$estimate[2],
  exact = TRUE
)$statistic

dn_n

png(
  "img/hipoteza_o_rownosci_cln.png",
  width = 9,
  height = 12,
  pointsize = 9,
  units = "cm",
  res = 480
)
par(mfrow = c(1, 1))
hist(D, prob = TRUE, xlab = "")
points(dn_n, 0, pch = 19, col = "red")
dev.off()


# Odleglosc dystrybuanty empirycznej dla kurs_zamkniecia,
# oraz dystrybuanty F0 jest istotnie większa od odleglosci obserwowanych
# dla probek tej samej licznosci z rozkladu F0.

p_value_n <- length(D[D > dn_n]) / iterations
p_value_n


alfa <- c(0.05)
p_value_n <= alfa
# Wartosc p-value jest mniejsza od przyjetego poziomu istotnosci, zatem
# hipoteze o rownosci dystrybuant (F = F0, gdzie F poszukiwany rozklad) odrzucam.
