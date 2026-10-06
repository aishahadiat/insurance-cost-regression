# Biaya Asuransi Kesehatan: Perokok dan Obesitas

Proyek regresi di R untuk melihat faktor apa yang paling memengaruhi biaya asuransi kesehatan, dan model mana yang paling akurat memprediksinya. Data dari Kaggle: [Medical Cost Personal Datasets](https://www.kaggle.com/datasets/mirichoi0218/insurance), isinya 1.338 baris.

Analisis lengkap (kode, grafik, dan hasil) ada di [insurance_analysis.md](insurance_analysis.md).

## Yang ketemu

Awalnya saya kira distribusi biaya yang miring ke kanan butuh model Gamma. Setelah saya uji (data dibagi 80:20 secara acak sebanyak 30 kali), ternyata regresi linear biasa lebih baik dari Gamma (rata-rata RMSE 6.172 vs 7.964). Yang paling berpengaruh ke akurasi justru interaksi antara merokok dan BMI (RMSE turun ke 4.964), dan kalau BMI diganti penanda obesitas (BMI 30 ke atas), errornya turun lagi ke 4.601.

Modelnya menjelaskan sekitar 86% variasi biaya. Efek yang paling menarik ada di kombinasi merokok dan obesitas:

| | Tidak obesitas | Obesitas |
|---|---|---|
| Bukan perokok | 7.977 | 8.843 |
| Perokok | 21.363 | 41.558 |

Rata-rata biaya aktual per kelompok. Obesitas hampir tidak mengubah biaya kalau tidak merokok, tapi untuk perokok biayanya hampir dua kali lipat.

## Catatan

Model ini saya pilih karena paling akurat di data uji, bukan karena semua asumsi regresi linear terpenuhi. Grafik residualnya masih menunjukkan pola yang belum tertangkap. Batas obesitas 30 juga saya pilih setelah melihat grafik dari seluruh data, jadi perbaikan errornya sedikit lebih optimis dari kenyataan. Datanya data observasi dari dataset contoh, jadi yang terlihat adalah hubungan, bukan sebab-akibat.

## File

- `insurance_analysis.Rmd`: kode dan narasi analisis
- `insurance_analysis.md`: hasil knit yang bisa dibaca langsung di GitHub
- `insurance.csv`: datanya

Tools: R (ggplot2), R Markdown
