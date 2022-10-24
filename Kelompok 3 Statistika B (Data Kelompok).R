library(readxl)
Data_exel <- read_excel("D:/Statistika/Data exel.xlsx")
View(Data_exel)

# Input Frekuensi(fi)
Frekuensi <- c(5,17,46,38,26,29,23,12,4)
print(Frekuensi)

# Input Titik Tengah (xi)
xi <- c(147.5,151.5,155.5,159.5,163.5,167.5,171.5,175.5,179.5)
print(xi)

# Mean (rata-rata)
MEAN <- sum(Frekuensi*xi) / sum(Frekuensi)
print(MEAN)
mean = round(MEAN)
print(mean)

# Median
# Letak Median
MEDIAN <- 1/2 * sum(Frekuensi)
MEDIAN = round(MEDIAN)
print(MEDIAN)

# Tb (batas bawah kelas median)
Tb <- 158 - 0.5
print(Tb)

#fkk (frekuensi komulatif kurang dari kelas median)
fkk = 46

#f (frekuensi kelas median) & P (panjang kelas)
f <- 38
P <- 4
median <- Tb + ((1/2 * sum(Frekuensi)- fkk) /f) * P
print(median)

# MODUS
# b (batas bawah kelas interval dengan frekuensi terbanyak)
b <- 154 - 0.5
print(b)
# d1 (selisih frekuensi kelas modus dengan kelas modus sebelumnya)
d1 <- 46 - 17
print(d1)
# d2 (selisih frekuensi kelas modus dengan kelas modus setelahnya)
d2 <- 46 - 38
print(d2)

MODUS <- b + (d1 / (d1+d2)) * P
print(MODUS)
modus = round(MODUS)
print(modus)

# Mean deviasi (simpangan rata-rata)
SR <- sum(Frekuensi*abs(xi - mean)) / sum(Frekuensi)
print(SR)

# Standar deviasi (simpangan baku)
# mencari ragam/ varian dahulu, karena SD merupakan akarkuadrad dari ragam/varian
S2 <- sum(Frekuensi*(xi - mean)^2) / sum(Frekuensi)
print(S2)
SD <- sqrt (S2)
print(SD)


