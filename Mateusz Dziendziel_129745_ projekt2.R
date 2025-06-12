# MATEUSZ DZIENDZIEL NR. INDEKSU : 129745 , PROJEKT NR.2 Z PRZEDMIOTU STATYSTYKA W BIZNESIE

# założenia: SKU - nazwa produktu, PSD - ID klienta, wartości w kolumnach to ilość wydanych zł 
# na produkty w naszej firmie 
#########################################################################################################
#########################################################################################################
#########################################################################################################



# ETAP 1: stworzenie pętli, która będzie instalować pakiety tylko jeśli jeszcze
# nie są zainstalowane + wczytanie danych + czyszczenie danych



#########################################################################################################
#########################################################################################################
#########################################################################################################

# 1.1) Instalacja potrzebnych pakietów
packages <- c("tidyverse", "qcc", "factoextra", "car", "gtools")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


# 1.2) Wczytanie danych
data <- read.csv("C:/Users/PC/Desktop/Statystyka w biznesie/projekt_2.csv", header = TRUE, sep = ";")


# 1.3) Sprawdzenie danych i unikalnych wartości

summary(data)
length(unique(data$PSD)) 
# Mamy 9795 obserwacji i tyle samo unikalnych wartości w kolumnie "PSD", 
# co oznacza, że każdy klient występuje tylko raz i jest unikalny. 
# Funkcja summary pokazuje, że kolumny SKU3 i SKU6 zawierają wyłącznie zera, 
# więc można je usunąć, ponieważ są nieprzydatne. 
# Kolumnę PSD zamienię na ID – będzie bardziej czytelna.

data <- data %>%
  select(-SKU3, -SKU6) %>%
  rename(client_id = PSD)

colSums(data[-1]) %>%
  sort(decreasing = TRUE)
# Tutaj sumuję wartości dla każdego produktu. Widać, że SKU4, SKU7 i SKU10 mają znacznie niższe sumy
# niż pozostałe, co sprawia, że ich udział w analizie będzie minimalny. 
# Dlatego decyduję się usunąć te kolumny i skupić analizę na bardziej istotnych produktach.

data <- data  %>%
  select(-SKU4, - SKU7, -SKU10)

#########################################################################################################
#########################################################################################################
#########################################################################################################



# ETAP 2: Podstawowa analiza i odpowiedzi na kluczowe pytania:

# 1) Ile łącznie wydali nasi klienci?
# 2.1) Ile średnio wydawał jeden klient?
# 2.2) Jaka była mediana wydatków klientów w naszej firmie?
# 2.3) Jaka była maksymalna wartość wydatków pojedynczego klienta?
# 2.4) W ilu produktach średnio są zainteresowani nasi klienci? Ile minimalnie, a ile maksymalnie?
# 3) Analiza PARETO klientów
# 4) Ilu było nabywców każdego produktu? Który produkt był najczęściej kupowany? Który najbardziej dochodowy?
# 5) Jak wyglądał rozkład sprzedaży każdego produktu? Średnia, mediana, outlinery
# 6) Analiza PARETO produktów – przeprowadzenie i wnioski

# Mała legenda na później, aby łatwiej się orientować:

# 1) list2$ID_percentage – procentowy udział danego produktu w całkowitych zakupach klienta
#    (czyli jaki procent całości zakupów klienta stanowi dany produkt)

# 2) list2$SKU_percentage – udział klienta w całkowitej sprzedaży danego produktu
#    (czyli jaki procent sprzedaży produktu przypada na tego klienta)

# Stworzenie listy na potrzeby drugiego etapu, aby nie było bałaganu w środowisku pracy.

list2<- list()



#########################################################################################################
#########################################################################################################
#########################################################################################################


list2$ID_percentage <- data %>%
  mutate(total_client_value = rowSums(select(., 2:ncol(.)))) %>% #żeby nie liczyć do całkowitej sumy nr. klienta
  group_by(client_id) %>% 
  mutate(SKU1 = (SKU1/ total_client_value) * 100, SKU2 = (SKU2/ total_client_value) * 100, 
         SKU5 = (SKU5/ total_client_value) * 100, SKU8 = (SKU8/ total_client_value) * 100,
         SKU9 = (SKU9/ total_client_value) * 100, SKU11 = (SKU11/ total_client_value) * 100, 
         SKU12 = (SKU12/ total_client_value) * 100, SKU13 = (SKU13/ total_client_value) * 100, 
         SKU14 = (SKU14/ total_client_value) * 100, SKU15 = (SKU15/ total_client_value) * 100) %>%
  ungroup() %>%
  mutate(total_client_value_percentage = total_client_value / sum(total_client_value) * 100) %>%  
  arrange(desc(total_client_value_percentage)) %>%
  mutate(total_value_percentage_cumulated = cumsum(total_client_value_percentage))


list2$SKU_percentage <- data  %>%
  mutate(SKU1 = (SKU1/ sum(SKU1)) * 100, SKU2 = (SKU2/ sum(SKU2)) * 100, 
         SKU5 = (SKU5/ sum(SKU5)) * 100, SKU8 = (SKU8/ sum(SKU8)) * 100,
         SKU9 = (SKU9/ sum(SKU9)) * 100, SKU11 = (SKU11/ sum(SKU11)) * 100, 
         SKU12 = (SKU12/ sum(SKU12)) * 100, SKU13 = (SKU13/ sum(SKU13)) * 100, 
         SKU14 = (SKU14/ sum(SKU14)) * 100, SKU15 = (SKU15/ sum(SKU15)) * 100)


# 1) Ile łącznie wydali nasi klienci?
list2$całkowity_dochód <- sum(list2$ID_percentage$total_client_value)
print(list2$całkowity_dochód) 
# Łączna wartość wydatków klientów wyniosła 1 086 851 909 zł


summary(list2$ID_percentage)
# 2.1) Ile średnio wydawał jeden klient?
# Średnio jeden klient wydaje w naszej firmie około 111 tys. zł


# 2.2) Jaka była mediana wydatków klientów w naszej firmie?
# Mediana wydatków wynosi 75,860 tys. zł


# 2.3) Jaka była maksymalna wartość wydatków pojedynczego klienta?
list2$najwiecej_wydane <- list2$ID_percentage %>%
  slice_max(total_client_value, n = 1)
print(list2$najwiecej_wydane)
# Pojedynczy klient wydał najwięcej – 2.1 mln zł, jego ID to 820


# 2.4) W ilu produktach średnio są zainteresowani nasi klienci? Ile minimalnie, a ile maksymalnie?
list2$Kupione_Produkty <- data.frame(
  client_id = data[[1]],
  liczba_produktow = rowSums(data[, -1] > 0)
)

summary(list2$Kupione_Produkty)
# Klienci średnio kupują 9.67 produktów. Minimalnie byli zainteresowani 7 produktami, maksymalnie 10.


# 3) Analiza PARETO klientów
list2$pareto_klienci <- list2$ID_percentage %>%
  select(client_id, total_client_value, total_client_value_percentage, total_value_percentage_cumulated) %>%
  mutate(rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, total_client_value, total_client_value_percentage, total_value_percentage_cumulated)

ggplot(list2$pareto_klienci, aes(x = rank)) +
  geom_line(aes(y = total_value_percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = total_value_percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w obrotach firmy"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


# 4) Ilu było nabywców każdego produktu? Który produkt był najczęściej kupowany? Który najbardziej dochodowy?

list2$ilosc_nabywcow_produktow <- colSums(data[, -1] > 0)
list2$ilosc_nabywcow_produktow <- data.frame(
  SKU = c("SKU1", "SKU2", "SKU5", "SKU8", "SKU9", "SKU11", "SKU12", "SKU13", "SKU14", "SKU15"),
  x = c(9795, 9758, 9494, 7644, 9313, 9461, 9795, 9793, 9795, 9795)) %>%
  arrange(desc(x)) %>%                      
  mutate(SKU = factor(SKU, levels = SKU))

ggplot(list2$ilosc_nabywcow_produktow, aes(x = SKU, y = x, fill = SKU)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = x), vjust = -0.5) +                    
  labs(title = "Liczba klientów kupujących dany produkt",
       y = "Liczba klientów", x = NULL) +                      
  scale_y_continuous(limits = c(0, 15000), expand = c(0,0)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),    
    axis.title.y = element_text(margin = margin(r = 10)),      
    legend.position = "none"                                   
  )



list2$zsumowana_wartosc_produktow <- round(colSums(data[, -1])) %>%
list2$zsumowana_wartosc_produktow <- data.frame(
    SKU = c("SKU11", "SKU9", "SKU1", "SKU14", "SKU15", "SKU13", "SKU12", "SKU2", "SKU5", "SKU8"),
    x = c(298518884, 183646190, 143208387, 138182212, 106270110, 102581635, 89904206, 15304644, 7128729, 2106913)
  ) %>%
  mutate(
    percent = x / sum(x) * 100              
  ) %>%
  arrange(desc(percent)) %>%
  mutate(SKU = factor(SKU, levels = SKU))  

ggplot(list2$zsumowana_wartosc_produktow, aes(x = SKU, y = percent, fill = SKU)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5) +  
  labs(title = "% Udział produktu w całości obrotów ",
       y = "% wartości produktu", x = NULL) + 
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "none"
  )


# Najbardziej dochodowym produktem okazał się SKU11 – jego łączna wartość sprzedaży to aż 27.5% całości obrotu


# 5) Jak wyglądał rozkład sprzedaży każdego produktu? Średnia, mediana, wartości odstające (outlinery)

# Najpierw lekko przekształcimy dane – aby nie tworzyć wielu osobnych wykresów,
# użyjemy `facet_wrap` z pakietu ggplot2. W tym celu potrzebujemy danych w formacie długim (long format),
# czyli przekształconych do postaci: klient | produkt | wartość


list2$data_for_boxplot_SKU_percentage <- list2$SKU_percentage %>%
  pivot_longer(
    cols = starts_with("SKU"),    
    names_to = "SKU",             
    values_to = "value"          
  )
list2$data_for_boxplot_SKU_percentage1 <- list2$data_for_boxplot_SKU_percentage %>%
  filter(SKU == c("SKU1", "SKU2"))

list2$data_for_boxplot_SKU_percentage2 <- list2$data_for_boxplot_SKU_percentage %>%
  filter(SKU == c("SKU5", "SKU8"))

list2$data_for_boxplot_SKU_percentage3 <- list2$data_for_boxplot_SKU_percentage %>%
  filter(SKU == c("SKU9", "SKU11"))

list2$data_for_boxplot_SKU_percentage4 <- list2$data_for_boxplot_SKU_percentage %>%
  filter(SKU == c("SKU12", "SKU13"))
 
list2$data_for_boxplot_SKU_percentage5 <- list2$data_for_boxplot_SKU_percentage %>%
  filter(SKU == c("SKU14", "SKU15"))

# Teraz możemy stworzyć nasze wykresy – ze względu na ich dużą liczbę i dla lepszej czytelności,
# lepiej podzielić je na kilka mniejszych grup (paczek).


#Produkty 1 i 2
ggplot(list2$data_for_boxplot_SKU_percentage1, aes(x = "", y = value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", outlier.size = 2, outlier.alpha = 1) +  
  facet_wrap(~ SKU, scales = "free_y", ncol = 4) +  
  labs(
    title = "Rozkład wartości sprzedaży według SKU",
    y = "Wartość sprzedaży (%)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    strip.text = element_text(face = "bold", size = 11),  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()  
  )


#Produkty 5 i 8
ggplot(list2$data_for_boxplot_SKU_percentage2, aes(x = "", y = value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", outlier.size = 2, outlier.alpha = 1) +  
  facet_wrap(~ SKU, scales = "free_y", ncol = 4) +  
  labs(
    title = "Rozkład wartości sprzedaży według SKU",
    y = "Wartość sprzedaży (%)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    strip.text = element_text(face = "bold", size = 11),  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()  
  )

#Produkty 9 i 11
ggplot(list2$data_for_boxplot_SKU_percentage3, aes(x = "", y = value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", outlier.size = 2, outlier.alpha = 1) +  
  facet_wrap(~ SKU, scales = "free_y", ncol = 4) +  
  labs(
    title = "Rozkład wartości sprzedaży według SKU",
    y = "Wartość sprzedaży (%)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    strip.text = element_text(face = "bold", size = 11),  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()  
  )

#Produkty 12 i 13
ggplot(list2$data_for_boxplot_SKU_percentage4, aes(x = "", y = value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", outlier.size = 2, outlier.alpha = 1) +  
  facet_wrap(~ SKU, scales = "free_y", ncol = 4) +  
  labs(
    title = "Rozkład wartości sprzedaży według SKU",
    y = "Wartość sprzedaży (%)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    strip.text = element_text(face = "bold", size = 11),  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()  
  )

#Produkty 14 i 15
ggplot(list2$data_for_boxplot_SKU_percentage5, aes(x = "", y = value)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red", outlier.size = 2, outlier.alpha = 1) +  
  facet_wrap(~ SKU, scales = "free_y", ncol = 4) +  
  labs(
    title = "Rozkład wartości sprzedaży według SKU",
    y = "Wartość sprzedaży (%)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
    strip.text = element_text(face = "bold", size = 11),  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()  
  )


# 6) Analiza PARETO produktów – przeprowadzenie i wnioski

# SKU1
list2$SKU1 <- data %>%
  select(client_id, SKU1) %>%
  arrange(desc(SKU1)) %>%
  mutate(percentage = (SKU1/ sum(SKU1) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU1, percentage, percentage_cumulated)
  
ggplot(list2$SKU1, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU1)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())
  

# SKU 2
list2$SKU2 <- data %>%
  select(client_id, SKU2) %>%
  arrange(desc(SKU2)) %>%
  mutate(percentage = (SKU2/ sum(SKU2) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU2, percentage, percentage_cumulated)

ggplot(list2$SKU2, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU2)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#SKU 5
list2$SKU5 <- data %>%
  select(client_id, SKU5) %>%
  arrange(desc(SKU5)) %>%
  mutate(percentage = (SKU5/ sum(SKU5) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU5, percentage, percentage_cumulated)

ggplot(list2$SKU5, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU5)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#SKU 8
list2$SKU8 <- data %>%
  select(client_id, SKU8) %>%
  arrange(desc(SKU8)) %>%
  mutate(percentage = (SKU8/ sum(SKU8) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU8, percentage, percentage_cumulated)

ggplot(list2$SKU8, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU8)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#SKU 9
list2$SKU9 <- data %>%
  select(client_id, SKU9) %>%
  arrange(desc(SKU9)) %>%
  mutate(percentage = (SKU9/ sum(SKU9) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU9, percentage, percentage_cumulated)

ggplot(list2$SKU9, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU9)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#SKU11
list2$SKU11 <- data %>%
  select(client_id, SKU11) %>%
  arrange(desc(SKU11)) %>%
  mutate(percentage = (SKU11/ sum(SKU11) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU11, percentage, percentage_cumulated)

ggplot(list2$SKU11, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU11)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#SKU12
list2$SKU12 <- data %>%
  select(client_id, SKU12) %>%
  arrange(desc(SKU12)) %>%
  mutate(percentage = (SKU12/ sum(SKU12) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU12, percentage, percentage_cumulated)

ggplot(list2$SKU12, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU12)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#SKU13
list2$SKU13 <- data %>%
  select(client_id, SKU13) %>%
  arrange(desc(SKU13)) %>%
  mutate(percentage = (SKU13/ sum(SKU13) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU13, percentage, percentage_cumulated)

ggplot(list2$SKU13, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU13)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#SKU14
list2$SKU14 <- data %>%
  select(client_id, SKU14) %>%
  arrange(desc(SKU14)) %>%
  mutate(percentage = (SKU14/ sum(SKU14) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU14, percentage, percentage_cumulated)

ggplot(list2$SKU14, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU14)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#SKU15
list2$SKU15 <- data %>%
  select(client_id, SKU15) %>%
  arrange(desc(SKU15)) %>%
  mutate(percentage = (SKU15/ sum(SKU15) * 100),
         percentage_cumulated = cumsum(percentage),
         rank = row_number()) %>%
  mutate(rank = (rank / 9795) * 100) %>%
  select(client_id, rank, SKU15, percentage, percentage_cumulated)

ggplot(list2$SKU15, aes(x = rank)) +
  geom_line(aes(y = percentage_cumulated), color = "red", size = 0.7) +
  geom_point(aes(y = percentage_cumulated), color = "red", size = 1.3) +
  geom_hline(yintercept = seq(0, 100, by = 10), linetype = "dashed", color = "gray80") +
  scale_y_continuous(
    name = NULL,
    breaks = NULL,
    labels = NULL,
    sec.axis = sec_axis(~ ., name = "Skumulowany udział (%)", breaks = seq(0, 100, by = 10))
  ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
    labels = paste0(seq(0, 100, by = 10), "%"),
    name = "% klientów"
  ) +
  labs(
    title = "Pareto Chart – Udział klientów w sprzedaży produktu (SKU15)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())


#########################################################################################################
#########################################################################################################
#########################################################################################################



# etap 3 – określenie optymalnej liczby klastrów oraz segmentacja klientów

# na początku, aby ustalić odpowiednią liczbę klastrów, zastosujemy tzw. "metodę łokcia"
# z wykorzystaniem pakietu "factoextra", który wizualnie wskaże najlepszy punkt podziału.

# kolejnym krokiem będzie segmentacja klientów na podstawie danych z ramki "list2$ID_percentage",
# zawierającej procentowy udział poszczególnych produktów w zakupach każdego klienta.

# analiza pozwoli nam zidentyfikować grupy klientów o zbliżonych wzorcach zakupowych,
# co umożliwi lepsze dopasowanie strategii – np. promocji – do specyfiki każdego segmentu,
# zwiększając tym samym skuteczność działań marketingowych i maksymalizując zysk.

#Stworzenie listy w celu zchowania porządku
list3 <- list()



#########################################################################################################
#########################################################################################################
#########################################################################################################



# 3.1) dobór odpowiedniej liczby klastrów:
# przygotowanie danych do analizy – wybieramy tylko kolumny zawierające informacje o produktach (SKU)
list3$segmentacja <- list2$ID_percentage %>%
  select(starts_with("SKU"))


# 3.2) wyznaczenie optymalnej liczby klastrów
fviz_nbclust(list3$segmentacja, kmeans, method = "wss")

# optymalna liczba klastrów to 4 – widać to na wykresie, ponieważ po 4. klastrze linia traci stromy spadek 
# i zaczyna się wypłaszczać, co sugeruje, że dalsze zwiększanie liczby klastrów nie przynosi istotnej poprawy


# 3.3) klastrowanie
set.seed(1)  # ustawienie ziarna losowości, aby wyniki były powtarzalne przy każdym uruchomieniu
list3$klastrowanie_rezultat <- kmeans(list3$segmentacja, centers = 4, nstart = 25)

# przypisanie numeru klastra do każdego klienta
list3$segmentacja$klaster <- list3$klastrowanie_rezultat$cluster


# 3.4) Analiza klastrów

# Przygotowanie danych: dodanie wartości całkowitych wydatków w naszej firmie i ID klienta oraz konwersja klastrów na faktor
list3$segmentacja$total_client_value <- list2$ID_percentage$total_client_value
list3$segmentacja$ID <- list2$ID_percentage$client_id
list3$segmentacja <- list3$segmentacja %>%
  mutate(klaster = factor(klaster))


# 1) Liczebność klientów w poszczególnych klastrach + wykres
list3$ilosc_klientow_w_klastrze <- list3$segmentacja %>%
  count(klaster)

ggplot(list3$ilosc_klientow_w_klastrze, aes(x = klaster, y = n, fill = klaster)) +
  geom_col() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Liczebność", limits = c(0, 5000)) +
  ggtitle("Liczba klientów w poszczególnych klastrach") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.text.x = element_blank()  
  ) +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_manual(values = c("red", "blue", "green", "orange")) +
  guides(fill = guide_legend(title = "Klaster"))


# 2) Porównanie średnich całkowitych wydatków w zł między klastrami
list3$srednie_calkowite_wydatki_na_klaster <- list3$segmentacja %>%
  group_by(klaster) %>%
  summarize(mean_total_spendings = round(mean(total_client_value) / 1000))

ggplot(list3$srednie_calkowite_wydatki_na_klaster, aes(x = klaster, y = mean_total_spendings, fill = klaster)) +
  geom_col() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Średnie wydatki klientów (w tys.)", limits = c(0, 200)) +
  ggtitle("Średnie całkowite wydatki w firmie w podziale na klastry (w tys.)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.text.x = element_blank()
  ) +
  geom_text(aes(label = mean_total_spendings), vjust = -0.5) +
  scale_fill_manual(values = c("red", "blue", "green", "orange")) +
  guides(fill = guide_legend(title = "Klaster"))

# Sprawdzenie istotności różnic między średnimi wydatkami

# Warunki do testu ANOVA:
# 1) Normalność rozkładu - spełniona ze względu na dużą liczbę obserwacji (>2000 w każdej grupie)
# 2) Homogeniczność wariancji - sprawdzamy testem Levene’a
leveneTest(total_client_value ~ factor(klaster), data = list3$segmentacja)
# p-value > 0.05 -> brak podstaw do odrzucenia H0, wariancje są podobne

# Test ANOVA:
list3$anova_result <- aov(total_client_value ~ factor(klaster), data = list3$segmentacja)
summary(list3$anova_result)
# p-value > 0.05 -> brak istotnych statystycznie różnic między średnimi wydatkami
  
  
# 4) Heatmapa udziałów produktów w klastrach

list3$heatmapa <- list3$segmentacja %>%
  group_by(klaster) %>%
  summarize(across(starts_with("SKU"), mean)) %>%
  pivot_longer(
    cols = starts_with("SKU"),
    names_to = "SKU",
    values_to = "mean"
  ) %>%
  mutate(SKU = factor(SKU))

# Naturalne sortowanie nazw SKU
list3$heatmapa$SKU <- factor(list3$heatmapa$SKU, levels = mixedsort(levels(list3$heatmapa$SKU)))

ggplot(list3$heatmapa, aes(x = SKU, y = factor(klaster), fill = mean)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    x = NULL,
    y = "Klaster",
    fill = "Średni udział",
    title = "Średni udział danego produktu w określonym klastrze "
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  
  