
# Credit risk adalah resiko yang harus ditanggung oleh seorang individu atau 
# lembaga ketika memberikan pinjaman (biasanya dalam bentuk uang) ke individu
# atau pihak lain.

# Resiko ini berupa tidak bisa dibayarkannya pokok dan bunga pinjaman, sehingga
# mengakibatkan kerugian berikut:
# - Gangguan aliran kas (cash flow) sehingga modal kerja terganggu.
# - Meningkatkan biaya operasional untuk mengejar pembayaran tersebut (collection).

# Untuk memperkecil resiko kredit ini, biasanya dilakukan proses yang disebut
# dengan credit scoring dan credit rating terhadap pihak peminjam. Output 
# proses ini akan menjadi basis untuk menentukan apakah aplikasi pengajuan
# pinjaman baru diterima atau ditolak.

#Import library
library(readxl)    #untuk membaca data excel
library(tidyverse) #untuk manipulasi data
library(C50)       #untuk menggunakan algoritma C5.0
library(reshape2)  #untuk menggunakan fungsi prediksi dcast()


#Import dataset
datakredit = read_excel(
  "DATA SCIENCE/Portofolio/Credit Risk Analysys/Credit Score.xlsx") %>% as.tibble
datakredit
View(datakredit)

# Mengubah label Class Variable (Optional)
datakredit$risk_rating[datakredit$risk_rating == "1"]  <-  "1 (Sangat Rendah)"
datakredit$risk_rating[datakredit$risk_rating == "2"]  <-  "2 (Rendah)"
datakredit$risk_rating[datakredit$risk_rating == "3"]  <-  "3 (Cukup)"
datakredit$risk_rating[datakredit$risk_rating == "4"]  <-  "4 (Tinggi"
datakredit$risk_rating[datakredit$risk_rating == "5"]  <-  "5 (Sangat Tinggi)"


#Ubah Class Variable menjadi factor
datakredit$risk_rating = datakredit$risk_rating%>%as.factor
datakredit

#Mempersiapkan class dan input variables 
class=datakredit$risk_rating
input=datakredit%>%select(jumlah_tanggungan,durasi_pinjaman_bulan)
class
input


#Mempersiapkan training dan testing set

# Membuat indeks pengacakan
set.seed(100) #untuk menyeragamkan hasil random antar tiap komputer
indeks_training = sample(900, 800) #ambil 800 sampel yg terdiri dari angka 1-900
indeks_training

indeks_testing = c(1:900)
indeks_testing = indeks_testing[-indeks_training]
indeks_testing

indeks_testing %in% indeks_training

# Membuat Training set
input_training = input[indeks_training,]
input_training

class_training = class[indeks_training]
class_training

# Membuat Testing set
input_testing = input[indeks_testing,]
input_testing

class_testing = class[indeks_testing]
class_testing

# Menghasilkan dan menampilkan summary model
model <- C5.0(input_training, class_training,
              control = C5.0Control(label="Risk Rating"))
summary(model)


#Menampilkan plot Decision Tree
plot(model)


# Prediksi class dengan input testing set
hasil_prediksi=predict(model, input_testing)
hasil_prediksi

# Predict vs Actual
compare=data.frame(input_testing,class_testing,hasil_prediksi,
                   label=(class_testing==hasil_prediksi))%>%as.tibble
compare  
summary(compare$label)


#Confusion Matrix Testing model
dcast(hasil_prediksi ~ class_testing, data=compare)%>%View




