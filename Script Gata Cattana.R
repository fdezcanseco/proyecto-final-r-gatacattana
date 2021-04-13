# Comenzamos nuestro proyecto creando el nombre del mismo y su ruta, para asegurarnos de que
# estamos en la ruta correcta lanzamos la siguiente función:
getwd()

# Ahora vamos a crear una carpeta dentro del proyecto para alojar los diferentes scripts que manejaremos en orden de tener nuestro 
# código lo más limpio posible:
dir.create("scripts")
list.files("scripts")
list.files("datos-gatacattana")

# Comprobamos los archivos presentes en cada subcarpeta correspondiente a cada disco de la artista:
list.files("datos-gatacattana/Banzai")
list.files("datos-gatacattana/Anclas")
list.files("datos-gatacattana/Los siete contra tebas")

# Cargamos las librerías necesarias y actualizamos un paquete:

library(tidyverse)
library(tidytext)
install.packages("stopwords")

# Este primer análisis pretende analizar el último disco de Gata: Banzai; así pues serán los datos del mismo los que carguemos:
list.files(path = "datos-gatacattana/Banzai",
           pattern = "*.txt")

# Lo guardamos dentro de una variable llamada disc.banzai:

disc.banzai <- list.files(path = "datos-gatacattana/Banzai",
                           pattern = "*.txt")

# Ahora, mediante una expresión regular, vamos a limpiar el nombre de las canciones elimando, de momento, solo .txt.
# Quizás más adelante nos quedemos solo con el nombre de las canciones o con el número + nombre:

tracks.banzai <- gsub("\\.txt",
                     "",
                     disc.banzai,
                     perl = TRUE)

# Creamos nuestro primer dataset para manejar los datos de las canciones: 

songs.banzai <- tibble(name = character(),
                       paragraph = numeric (),
                       lyrics = character())

# Ahora que tenemos creada la tabla, tan solo queda rellenarla, name y lyrics espera characters, y paragraph valor númerico.
# comezamos el bucle: se repetirá tantas veces como elementos tenga el disco banzai: disc.banzai.

for (i in 1: length(disc.banzai)){
# vamos a alojar en discourse todas las letras de las canciones
  discourse <- readLines(paste("datos-gatacattana/Banzai",
                               disc.banzai[i],
                               sep = "/"))
# temporal es una tabla que se crea de puente para cada repetición, aquí definimos el valor que recoge la tabla inicial que creamos
# llamada songs.banzai
  temporal <- tibble(name = tracks.banzai[i],
                     paragraph = seq_along(discourse),
                     lyrics = discourse)
# En bind_rows alojamos definitivamente los valores del dataset de temporañ
  songs.banzai <- bind_rows(songs.banzai, temporal)
}
# Para tener un espacio de trabajo limpio, vamos a eliminar la tabla temporal y el punto de control:
#rm(temporal,i)

#comprobamos y ejecutamos la tabla:
songs.banzai

# Ahora vamos a dividirlo en tokens para nuestro posterior análisis:
# Vamos a crear un nuevo objeto donde alojaremos esa división, le damos el nombre: tokens.songs.banzai para que sea escalable:
tokens.songs.banzai <- songs.banzai %>%
  unnest_tokens(token,lyrics)

# Comprobamos nuestra nueva tabla:
tokens.songs.banzai

# vamos a calcular el número de tokens medio (sin contar los estribillos repetidos) de las canciones:
nrow(tokens.songs.banzai) / length(disc.banzai)

# Veamos ahora con la siguiente función el número de palabras tipo y las veces que se dan
view(tokens.songs.banzai %>% 
  count(token, sort = TRUE))

# Para ser más precisos, vamos a calcular la frencuencia relativa de las palabras tipo. Esto lo conseguimos dividiendo las veces
# que aparece la palabra tipo por el total de las palabras tokens. Además para incluir esta nueva columna, usaremos 
# la función mutate. Además se guardará en un objeto llamo rf (relative frecuency)

rf.tokens.songs.banzai<-tokens.songs.banzai %>%
  count(token, sort = T) %>%
  mutate(rf = n / sum(n))


#comprobamos los datos:
view(rf.tokens.songs.banzai)

tokens.songs.banzai %>%
  group_by(name) %>%
  count() %>%
  ggplot() + 
  geom_bar(aes(name,
               n),
           stat = 'identity')

# Vamos ahora a limpiar las canciones de ruido o stop words. Para comenzar, cargamos la lista de stopwords:
stopWords <- get_stopwords("es")

stopWords <- stopWords %>%
  rename(token = word)

# Vamos a guardar las canciones sin stopwords en la siguiente variable:

cleaned.noise.songs.banzai <- tokens.songs.banzai %>%
  anti_join(stopWords)

# A continuación, vamos a usar una nueva lista de stop words más completa, para no machacar la variable anterior 
# y poder contrastar una lista con otra, la guardamos en otra variable:

withoutNoise <- read_csv("https://tinyurl.com/7PartidasVacias",
                         locale = default_locale())

withoutNoise <-withoutNoise %>%
  rename(token = palabra)

cleaned.noise.songs.banzai.V2 <- tokens.songs.banzai %>%
  anti_join(withoutNoise)

view(cleaned.noise.songs.banzai.V2 %>%
  count(token, sort = T))


# Ahora que tenemos limpio nuestros datos, vamos a intentar una gráfica:

cleaned.noise.songs.banzai.V2 %>%
  count(token, sort = T) %>%
  filter(n > 5) %>%
  ggplot(aes(x = token, y =n, fill = token)) + 
  geom_bar(stat = "identity") + 
  theme_minimal () +
  theme(legend.position = "none") +
  ylab("Ocurrency") + 
  xlab(NULL) +
  ggtitle("BANZAI, GATA CATTANA") +
  coord_flip()

# Una segunda gráfica:

cleaned.noise.songs.banzai.V2 %>%
  count(token, sort = T) %>%
  top_n(5) %>%
  mutate(token = reorder(token, n)) %>%
  ggplot(aes(x = token, y = n, fill = token)) + 
  geom_bar(stat="identity") + 
  theme_minimal() +
  theme(legend.position ="none") + 
    labs(title = "Banzai",
       subtitle = "Gata Cattana",
       x = NULL,
       Y = "Ocurrency") +
  coord_flip()
  

# Una tercera: 

cleaned.noise.songs.banzai %>%
  count(token, sort = T) %>%
  filter(n > 5) %>%
  ggplot(aes(x = token, y =n, fill = token)) + 
  geom_bar(stat = "identity") + 
  theme_minimal () +
  theme(legend.position = "none") +
  ylab("Ocurrency") + 
  xlab(NULL) +
  ggtitle("BANZAI, GATA CATTANA") +
  coord_flip()


# A continuación, vamos a agrupar las palabras más frecuentes de cada canción:

groupBySong <- cleaned.noise.songs.banzai %>%
  group_by(name) %>%
  count(token, sort =T) %>%
  ungroup()

# Y para facilitar el visionado de estos datos, lo plasmamos en la siguiente gráfica:

groupBySong %>%
  filter(n > 7) %>%
  ggplot(aes(x = token,
             y = n)) +
  geom_col(fill = "aquamarine") +
  coord_flip() +
  facet_wrap ( ~ name,
               ncol = 3,
               scales = "free_y")


# Vamos a dividirlo en bigramas ahora:

songs.banzai.bigrams <- songs.banzai %>%
  unnest_tokens(bigram,
                lyrics,
                token = "ngrams",
                n = 2)

# A continuación, dividimos los bigramas por orden de aparición:

songs.banzai.bigrams %>%
  count(bigram, sort = T)

# Vamos a dividir en dos segmentos los bigramas (cada segmento en una tabla):

songs.banzai.separated.bigrams <- songs.banzai.bigrams %>%
separate(bigram,
         c("segment1", "segment2"),
         sep = " ")

# Ahora vamos a vaciar las dos tablas de stop words. Vamos a usar otra ténica coon el operador lógico %in%

songs.banzai.filtered.bigrams <- songs.banzai.separated.bigrams %>%
  filter(!segment1 %in% withoutNoise$token,
         !segment2 %in% withoutNoise$token)

# Comprobamos los 10 bigramas más usados:
songs.banzai.filtered.bigrams %>%
  count(segment1, segment2, sort =T)

# A continuación, vamos a borrar los bigramas que dan error o aquellos que no interesan como "lara lara":

songs.banzai.filtered.bigrams <- songs.banzai.filtered.bigrams %>%
  filter(!segment1 %in% c("NA", "lara"),
         !segment2 %in% c("NA", "lara"))

united.bigrams.banzai <- songs.banzai.filtered.bigrams %>%
  unite(bigram, segment1, segment2, sep = " ")

view(united.bigrams.banzai %>%
  count(bigram, sort =T))

# Por último, vamos a representar en una tabla los bigramas:

united.bigrams.banzai %>%
  count(bigram, sort = T) %>%
  top_n(15) %>%
  ggplot() +
  geom_col(aes(y = n, x = reorder (bigram,n)),
           fill = "maroon") +
  coord_flip() +
  theme_linedraw() +
  labs(x = "Bigramas", Y = "Frecuencia") + 
  ggtitle("Bigramas más frecuentes en Banzai", subtitle = "Gata Cattana")

# A pesar de ser un brevísimo corpus, vamos a intentar representar una nube de palabra:
install.packages(c("wordcloud", "RColorBrewer"))

library(tidyverse)
library(tidytext)
library(wordcloud)

cleaned.noise.songs.banzai.V2 %>%
  count(token, sor = T) %>%
  with(wordcloud(token,
                 n,
                 max.words = 100,
                 color = brewer.pal(8, "Dark2")))
