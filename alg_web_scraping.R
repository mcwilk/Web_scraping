# Cel:
# Poniższy skrypt pobiera dane z serwisu Allegro.pl z ogłoszeń dotyczących mieszkań sprzedawanych \
# w województwie lubelskim i zapisuje je do pliku CSV.

# 1. Ustawiamy katalog roboczy
# setwd()

# 2. Pobieramy sterowniki dla naszej wersji Chrome'a

# 3. Kopiujemy sterowniki oraz Selenium-server do katalogu roboczego

# 4. Ładujemy niezbędne biblioteki:
library(seleniumPipes)
library(dplyr)
library(stringr)

# 5. Uruchamiamy Selenium-server poleceniem w terminalu:
# java -jar selenium-server-standalone-3.0.1.jar -port 4444

# 6. Za pomocą Selenium-server pobieramy linki do ogłoszeń z 10 kolejnych stron:
remDr <- remoteDr(remoteServerAddr="http://localhost", port=4444L, browserName="chrome", newSession=TRUE, javascript=TRUE)
newUrl <- "https://allegro.pl/kategoria/mieszkania-na-sprzedaz-lubelskie-116146?bmatch=baseline-product-eyesa2-engag-dict43-rea-1-1-0513&p="

wektorLinkow <- c()       # wektor, w którym zostaną zapisane linki do ogłoszeń

N = 3       # liczba stron z ogłoszeniami
for(i in 1:N) {     # przeglądamy strony z ogłoszeniami i pobieramy linki do ogłoszeń
  elems <- remDr %>% go( paste0 ( newUrl,i ) ) %>% findElements(using="class name", "_9c44d_LUA1k")
  for(j in 1:length(elems)) {
    e <- findElementsFromElement( elems[[j]], using="tag name", "a" )
    if(length(e)>0) {
      link <- e[[1]] %>% getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)    # każdy link zostaje zapisany w wektorze 'wektorLinkow'
    }
  }
}

length(wektorLinkow)    #180
wektorLinkowU <- unique(wektorLinkow)   # usuwamy ewentualne duplikaty linków
length(wektorLinkowU)   #179

# 7. Pobieramy dane z poszczególnych linków:
listaOgloszen <- list()     # lista wektorow elementów dla kolejnych ogłoszeń
lw <- 1   # licznik

for(w in lw:20) {    #Pierwszych 20 ogłodzeń; dla wszystkich zamienić '20' na: length(wektorLinkowU)
  Sys.sleep(1)
  singlePageURL <- wektorLinkowU[w]
  remDr %>% go(singlePageURL)
  stringl <- remDr %>% getCurrentUrl()
  if ( !str_detect(stringl, pattern = "404")) {
    
    # a) Szukamy ceny:
    elems <- remDr %>% findElements(using = "css selector", "._1svub._lf05o._9a071_2MEB_")
    e <- findElementsFromElement( elems[[1]], using="tag name", "span" )
    c1 <- e[[1]]
    cena <- c1 %>% getElementText()
    listaOgloszen[[w]] <- c(cena)

    # b) Szukamy lokalizacji:
    elems <- remDr %>% findElements(using="css selector", "._9a071_1DiQK._9a071_25Xk5")
    lokalizacja <- elems[[1]] %>% getElementText()
    listaOgloszen[[w]] <- c(listaOgloszen[[w]], lokalizacja)
    
    # c) Szukamy szczegolow:
    # Nazwa szczegółu oraz jego zawartość są umieszczone w osobnych klasach, dlatego pobieramy \ 
    # je osobno, a następnie łączymy za pomocą funkcji 'paste0'.
    
    # Nazwy szczegółów:
    elemsTitle <- remDr %>% findElements(using = "css selector", "._17qy1._1vryf._f8818_1X1F-")
    elemsTitleWektor <- c()
    for(i in 1:length(elemsTitle)){
      eT <- elemsTitle[[i]] %>% getElementAttribute("innerText")
      #print(eT)
      elemsTitleWektor <- c(elemsTitleWektor, eT)
    }
    
    # Zawartość szczegółów:
    elemsContent <- remDr %>% findElements(using = "css selector", "._17qy1._f8818_DQKcc")
    elemsContentWektor <- c()
    for(i in 1:length(elemsContent)){
      eC <- elemsContent[[i]] %>% getElementAttribute("innerText")
      #print(eC)
      elemsContentWektor <- c(elemsContentWektor, eC)
    }
    
    wektorSzczegolow <- c()
    for(i in 1:length(elemsTitleWektor)){
      detail <- paste0(elemsTitleWektor[[i]], elemsContentWektor[[i]])
      wektorSzczegolow <- c(wektorSzczegolow, detail)
    }
    listaOgloszen[[w]] <- c(listaOgloszen[[w]], wektorSzczegolow)
    
    listaOgloszen[[w]] <- c(listaOgloszen[[w]], singlePageURL)
  
  } else {
    listaOgloszen[[w]] <- c(NA)     # wstawiamy NA jeśli ogłoszenie nieaktualne (błąd 404)
  }
}

# 8. Zapisujemy do pliku listę ogłoszeń ze szczegółami, linkiem, lokalizacją etc.
save(listaOgloszen, file="listaBackup.rd")

# 9. Przerabiamy listę 'listaOgloszen' na DF:
unikalneOgloszenia <- listaOgloszen %>% unique()

# Tworzymy tabelę i zapisujemy w pliku CSV:
for(i in 1:length(unikalneOgloszenia)) {
  
  if(!is.na ( unikalneOgloszenia[[i]][1]) ) {
    
    # Póki nie załadowaliśmy żadnych danych z tych pobranych, wszędzie wstawiamy NA
    cena <- NA;
    lokalizacja <- NA;
    wojewodztwo <- NA;
    faktura <- NA;
    powierzchnia <- NA;
    cenaZAm2 <- NA;
    liczbaPokoi <- NA;
    pietro <- NA;
    liczbaPieter <- NA;
    rynek <- NA;
    typBudynku <- NA;
    materialBudowlany <- NA;
    formaWlasnosci <- NA;
    rokBudowy <- NA;
    lazienkaWC <- NA;
    ogrzewanie <- NA;
    media <- NA;
    infoDodatkowe <- NA;
    dodatkowaPowierzchnia <- NA;
    link<-NA;
    
    # Ładujemy pobrane dane
    cena <- as.numeric(str_replace_all(unikalneOgloszenia[[i]][1], "[^\\d]",""))
    
    lokList <- unikalneOgloszenia[[i]][2] %>% strsplit(",")
    lokalizacja <- unlist(lokList)[1]
    
    wojewodztwo <- unlist(unlist(lokList)[2] %>% strsplit(' '))[3]
    
    for( j in 3:( length(unikalneOgloszenia[[i]])-1 ) ){
      
      if ( grepl("Faktura", unikalneOgloszenia[[i]][j] ) ){
        faktura <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Powierzchnia", unikalneOgloszenia[[i]][j] ) ){
        powierzchnia <- as.numeric( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] %>% 
                                      str_replace_all("[^\\d]+[.]*[^\\d]", ""))
        next;
      }
      
      if ( grepl("Cena za m", unikalneOgloszenia[[i]][j] ) ){
        cenaZAm2 <- as.numeric( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] %>% 
                                  str_replace_all("[^\\d]+[.]*[^\\d]", ""))
        next;
      }
      
      if ( grepl("Liczba pokoi", unikalneOgloszenia[[i]][j] ) ){
        liczbaPokoi <- as.numeric( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Piętro", unikalneOgloszenia[[i]][j] ) ){
        pietro <- as.numeric( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Liczba pięter", unikalneOgloszenia[[i]][j] ) ){
        liczbaPieter <- as.numeric( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Rynek", unikalneOgloszenia[[i]][j] ) ){
        rynek <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Typ budynku", unikalneOgloszenia[[i]][j] ) ){
        typBudynku <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Materiał budowlany", unikalneOgloszenia[[i]][j] ) ){
        materialBudowlany <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Forma własności", unikalneOgloszenia[[i]][j] ) ){
        formaWlasnosci <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Rok budowy", unikalneOgloszenia[[i]][j] ) ){
        rokBudowy <- as.numeric( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Łazienka i WC", unikalneOgloszenia[[i]][j] ) ){
        lazienkaWC <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Ogrzewanie", unikalneOgloszenia[[i]][j] ) ){
        ogrzewanie <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Media", unikalneOgloszenia[[i]][j] ) ){
        media <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Informacje", unikalneOgloszenia[[i]][j] ) ){
        infoDodatkowe <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
      
      if ( grepl("Dodatkowa powierzchnia", unikalneOgloszenia[[i]][j] ) ){
        dodatkowaPowierzchnia <- ( unlist(unikalneOgloszenia[[i]][j] %>% strsplit(":"))[2] )
        next;
      }
    }
    
    link <- unikalneOgloszenia[[i]][ length(unikalneOgloszenia[[i]]) ]
    
    wektorDanych<-c(cena, lokalizacja, wojewodztwo, faktura, powierzchnia, cenaZAm2, liczbaPokoi, 
                    pietro, liczbaPieter, rynek, typBudynku, materialBudowlany, formaWlasnosci,
                    rokBudowy, lazienkaWC, ogrzewanie, media, infoDodatkowe, dodatkowaPowierzchnia, link)
    #print(wektorDanych)
    
    write.table(rbind(wektorDanych), file="Allegro_scraping", row.names=FALSE, col.names=FALSE, sep="\t", 
                append=TRUE, na="NA")
  }
}

# Wczytujemy tabelę z utworzonego pliku CSV
mieszkaniaZcsv <- read.table(file="Allegro_scraping", sep="\t", header=FALSE, na.strings="NA")

# Nadajemy kolumnom nazwy
names(mieszkaniaZcsv) <- c("cena", "lokalizacja", "wojewodztwo", "faktura", "powierzchnia", "cenaZAm2", 
                           "liczbaPokoi", "pietro", "liczbaPieter", "rynek", "typBudynku", "materialBudowlany", 
                           "formaWlasności", "rokBudowy", "lazienkaWC", "ogrzewanie", "media", "infoDodatkowe", 
                           "dodatkowaPowierzchnia", "link")
View(mieszkaniaZcsv)

# Podsumowanie
summary(mieszkaniaZcsv) 




