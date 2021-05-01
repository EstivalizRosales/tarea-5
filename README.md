Tarea 5
================

## Estivaliz Rosales

## Actividad 5:

Realizar análisis de agrupamiento (K-means, incluye preprocesamiento de
los datos) e índices de evaluación para el archivo “sandwiches.csv”
tomando las columnas de nota y precio. Hacer análisis para diferentes K
y / o medidas de distancia para que vean cómo se comporta el clustering
(En caso de tener algún problema con ese csv, pueden utilizar el csv de
Pokémon también para la actividad)

## Cargar datos y librerias

Como bien sabemos, lo primero que debemos hacer es cargar los datos a
nuestro R, en conjunto con las librerias.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v stringr 1.4.0
    ## v tidyr   1.1.3     v forcats 0.5.1
    ## v readr   1.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(readr)
library(stringr)
library(readr)
library(datasets)
library(cluster)
library(factoextra)
```

    ## Warning: package 'factoextra' was built under R version 4.0.5

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
setwd("C:/Users/JuanRosales/OneDrive/Mineria de datos/tarea 5")
datos  <- read.csv("sanguchez.csv",sep=";")
head(datos)
```

    ##                                                               url
    ## 1          https://365sanguchez.com/abocado-cantina-buenos-aires/
    ## 2                   https://365sanguchez.com/alba-hotel-matanzas/
    ## 3   https://365sanguchez.com/albedrio-restaurant-santiago-centro/
    ## 4 https://365sanguchez.com/albedrio-restaurant-santiago-centro-2/
    ## 5              https://365sanguchez.com/aldea-nativa-providencia/
    ## 6            https://365sanguchez.com/aleman-experto-providencia/
    ##                 Local                                         Direccion  Precio
    ## 1     Abocado Cantina   C1125AAE, French 2316, C1125AAF CABA, Argentina $5.210.
    ## 2          Alba Hotel   Carlos Ibañez del Campo s/n – Matanzas, Navidad  $7.000
    ## 3 Albedrio Restaurant     Huérfanos 640, Santiago, Región Metropolitana  $7.290
    ## 4 Albedrío Restaurant Pasaje Huerfanos 640 edificio B local 5, Santiago  $8.690
    ## 5        Aldea Nativa  Tobalaba 1799, Providencia, Región Metropolitana  $4.900
    ## 6      Alemán Experto Av. Pedro de Valdivia 1683, Providencia, Santiago  $6.500
    ##                                                                                                                  Ingredientes
    ## 1                                               Suprema de pollo dulce, espinaca, crema ácida, repollo avinagrado y guacamole
    ## 2                     Carne mechada en reducción de vino tinto, champiñones salteados, cebolla caramelizada y queso derretido
    ## 3                          Mayonesa al olivo, champiñones salteados, jalapeños, queso Mozzarella, papas hilo y cebolla morada
    ## 4                          Queso Mozzarella, Rúcula, Champiñon portobello relleno de cheddar y luego apanado en panko y frito
    ## 5 Tofu asado no transgénico, palta, tomate, champiñones, mix de hojas verdes orgánicas,  mayonesa de zanahoria vegana casera,
    ## 6                           Hamburguesa, queso Cheddar, cebolla caramelizada, berros, pepinillos y salsa Jack Daniel’s Honey.
    ##   nota
    ## 1    3
    ## 2    3
    ## 3    4
    ## 4    4
    ## 5    4
    ## 6    3
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               texto
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Ojo acá! En la sanguchería “Abocado” (@AbocadoCantina) de Recoleta, más que un sándwich exquisito (que igual estaba bueno), descubrí una maravilla para copiar: acá el apanado, el frito del pollo, era dulce. Y bien crocante. Exquisito. Les juro que es el mejor apanado que he probado en mi vida. A esta suprema de pollo dulce, la acompañaban con espinaca (yo la hubiese puesto a la crema), crema ácida, repollo avinagrado y guacamole. Lamentablemente, la palta acá en Argentina no es como la chilena. Es más aguachenta. Y el pan, nuevamente sigue la línea que me ha tocado en este país, que no logra ser del nivel que tenemos en Chile. Pero insisto: ese batido hay que exportarlo. Estaba exquisito. Y sigo pensando en él.
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Aprovechando que me escapé a Matanzas con @catabarra_ a canjear mi regalo de cumpleaños (clases de surf), quise probar algunos sanguchitos de la zona. Y como hace un año me quedé a alojar en @albahotelboutique y tuve una muy buena experiencia, hoy quise darle una oportunidad a su carta de comida. Y a pesar de que en general nos fue bastante mal (3 de los platos andaban muuuy bajos), mi sanguchito salvó muy bien. Y es que la mezcla de carne mechada en reducción de vino tinto, champiñones salteados, cebolla caramelizada en marraqueta (y le sumé queso derretido), es demasiado buena. No falla. Así que de 1 a 5, este se lleva 3 narices de chancho. Es decir, es un buen sándwich. Vaya a probarlo con confianza. Una marrquetita crujiente y de poca miga, una mechada muy suave y harto queso son sus puntas de lanzas. Sí, hay cosas por mejorar. Por ejemplo, las “mechas” de la carne como que se pegaban unas a otras, entonces a veces de un mordisco te llevabas casi toda la carne. O el caldo lo haría más intenso. Porque lo que chorreaba aquí eran los champiñones más que la carne. Pero apaña harto, además que estás comiendo EN la playa misma.
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Sin duda, uno de los lugares que me ENCANTA visitar. Lejos la mejor hamburguesa que tienen es la Portobello (con un champiñón frito relleno de Cheddar y todo), pero como no estamos en temporada de hongos, no había ahora. Esa, sin duda, se lleva cinco narices. Hoy vine a @RestoAlbedrio con@MaxKbzon y nos dimos la torta comiendo. Él fue por un sándwich de prieta con manzana verde, papas hilo y mayo de ají verde. Yo, una burger “Picante”, con mayonesa al olivo, champiñones salteados, jalapeños, queso Mozzarella, papas hilo y cebolla morada. Solo les adelanto una cosa: tienen una salsa de reducción de cerveza con jugo de piña y azúcar rubia, que debiesen venderla en bidones!! Es EXQUISITA!
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Con @nitanzorron fuimos a probar esta maravilla de @albedrio_resto. Anoten: hamburguesa casera, queso mozzarella, rúcula y champiñon portobello relleno de cheddar y luego apanado en panko y frito . Una maravilla! Es que los champiñones rellenos ya son atómicos… Pero ahora que vienen fritos, tienes un sabor estratosférico. La mejor idea del mundo. Es una verdura muy “carnosa” y rica, realzada por el queso y el apanado. El toque amargo de la rúcula viene bien, y la hamburguesa en sí, creo que es la más jugosa que he probado. Me recordó a la de Ciudad Vieja. Anda perfecta. El pan Brioche, bien dulce, y de miga consistente. No tan aireada. Mi único punto a mejorar es que sentí que era muy “aguado” (los champiñones tienen alto porcentaje de agua), por lo que me faltó malicia. Un picante, o una salsa de ajo… No sé. Algo que te vuele la cabeza. De hecho, Albedrío tiene dos salsas que creo que pondrían a esta hamburguesa en el top chileno: la de la casa, que es una reducción de cerveza, pulpa de piña y azúcar rubia, y una mayonesa con cilantro y ajo que es perfecta. Con @nitanzorron conversamos que agregando esa salsa, el sandwich sube de nivel a SS3. Muy buena. Vayan a ver nuestra visita a mi canal de YouTube (link en mi perfil) para todos los detalles y comenten si les tinca porque encuentro que es mega creativa y muuuuy rica.
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Ojo los vegetarianos!! Porque gracias a@genoveva_tenaillon (síganla si quieren ver unas recetas exquisitas pero saludables al mismo tiempo) que me pasó el dato, encontré el templo de los sándwiches vegetarianos y jugos naturales wenos wenos wenos. Es Aldea Nativa, en Tobalaba, y a pesar de que es 99% más probable que prefiera un sándwich carnívoro, creo que los que probé acá son de los mejorcitos que me han tocado (hasta ahora, en La Tegualda están los mejores). El Barros Luco de la Geno estaba bien bueno (con carne de libre pastoreo, sin hormonas ni antibióticos… Y no, claramente este no era veggie jaja), pero me gustó más el mío: tofu asado no transgénico, palta, tomate, champiñones, mix de hojas verdes orgánicas, y le sumé una mayonesa de zanahoria vegana casera, que viene con todos los sándwiches (échensela entera). A ver. Era rico, pero la nota se la lleva principalmente porque es el mejor tofu que he probado en Chile. En general lo cocinan muy fome, pero este estaba marinado en soya y asado a la plancha, así que tenía un gustito distinto al típico “quesillo sin sabor” jajaj. Además, venía como con un cevichito de champiñones que también se lo puse adentro  y agarró una jugosidad que el pan agradeció. Con estos dos ingredientes que le puse, las verduras agarraron un aliño exquisito. De los vegetarianos que he probado, es de los más ricos. Pero si no te gusta el Tofu, también puedes probar alguna de las hamburguesas vegetarianas que tienen. Me gustó harto el lugar, además de que también es un mercado donde venden miles de productos orgánicos, vegetarianos y de esa onda.
    ## 6 Salsa de bourbon: checkAlemán ExpertoCómo llegué a Alemán ExpertoYa había venido un par de veces al Alemán Experto. Tanto al local de Santa Magdalena, como a este de Pedro de Valdivia. En todas las visitas tuve suerte dispar. En algunos me gustó harto, otras no tanto.La cosa es que hoy tuve que hacer trámites cerca, y como tenía poco tiempo para buscar una sanguchería, preferí ir al Alemán Experto, que aún no lo sumaba a 365 Sánguchez.Fotos tomadas con mi celular Sony Xperia XRestaurante y sanguchería Alemán ExpertoAlemán Experto es una sanguchería que cuenta con dos locales. El primero está en Santa Magdalena, y el otro en Pedro de Valdivia, en la esquina con Francisco Bilbao. Ojo, que también están abriendo uno en La Dehesa.Este restaurante es, tal como lo dice su nombre, bien alemán. Es decir, abundan los sánguches grandes y la cerveza.Hablando del local de Pedro de Valdivia, siento que hicieron un gran trabajo con su fachada exterior. Si no me creen, miren la foto de más arriba. Y es que la terraza que sacaron está increíble. Además, por su ubicación, siempre hay gente, por lo que me tinca que se arma buen ambiente para los after office.Les dejo su pagina web. Carta de sándwiches Alemán ExpertoLa carta de sándwiches del Alemán Experto es amplia, tanto en sus bases, como también en sus combinaciones gourmet y clásicas.Por el lado más jugado, la sanguchería Alemán Experto cuenta con hamburguesas y mechadas BBQ. De las primeras, destacan una que tiene camarones y queso azul ($6.400), y la que pedí yo. Se llama Jack Daniel’s Honey, y tiene una salsa basada en ese licor, además de queso Cheddar, berros, cebolla caramelizada y pepinillos.En las mechadas BBQ, hay dos opciones. una con tocino crispy, y la otra con queso Azul y aros de cebolla.Luego de esta sección más “gourmet”, Alemán Experto también cuenta con hamburguesas, churrascos, lomitos, aves, salchichas y mechadas para poder armarlas como italianos, lucos y chacareros.Para terminar, hay una sección de sándwiches vegetarianos. Son hamburguesas de quinoa, y tiene cuatro combinaciones distintas. Hamburguesa Jack Daniel’s Honey en Alemán ExpertoA pesar de no ser un fanático del bourbon, admito que sí me gusta esta variante con toques de miel. Y en una salsa, mejor aún.Tengo que decir que es un sándwich correcto. La salsa no se roba el protagonismo, y aporta un toque justo de dulzor y también de “malicia”.La cebolla caramelizada estaba suave, y los berros perfectos para contrastar el frescor con lo dulce de la cebolla y la salsa.Lo que no me gustó tanto, es que la hamburguesa estaba un poco seca. Tuvo suerte, eso sí, de que venía acompañada con harta salsa, por lo que lograba pasar piola. Pero si nos quedamos en la carne, le falta.Y el otro punto negativo, y esto ya parece que es una maldición que me persigue, fue el queso Cheddar. Primero, porque no estaba derretido. Cueck. Y segundo, porque su sabor era demasiado plástico. Les prometo que tengo ganas de hacer una cata de quesos Cheddar, quizás con Daniel Greve, para poderles recomendar cuáles son buenos. Pero este, no.Maridaje con Cerveza Austral LagerEn resumen: Alemán Experto puede ser experto en otras cosas, pero no en hamburguesasRecién ahora, que estoy escribiendo estas líneas, me doy cuenta que quizás hice una movida tonta. Si voy a un lugar que se llama Alemán Experto, lo normal sería haber pedido un lomito. Con chucrut, con pepinillos… algo por ahí.Se supone que los alemanes también le pegan a las fricandelas, pero este no fue el caso. De hecho, la carne no era tan especiada como suele serlo en ese país. Pero aún así, me tinca que el lomito aquí puede ser un gran acierto.Quedé con ganas de volver. Volver y probar otra proteína, como el lomito o la mechada. Así que nos vemos pronto, Alemán Experto.

## Borrar datos

Como solo utilizaremos dos columnas, borraremos todas las demas para
trabajar con mas comodidad. Tambien borraremos todos los datos nulos
para disminuir la BBDD

``` r
datos <- datos[,! (colnames(datos) %in% c("url","Local","Direccion","Ingredientes","texto"))]

na.omit(datos)
```

    ##                                           Precio nota
    ## 1                                        $5.210.    3
    ## 2                                         $7.000    3
    ## 3                                         $7.290    4
    ## 4                                         $8.690    4
    ## 5                                         $4.900    4
    ## 6                                         $6.500    3
    ## 7                                         $5.500    3
    ## 8                                         $8.000    3
    ## 9                                         $8.900    3
    ## 10                                        $5.900    3
    ## 11                                        $8.900    4
    ## 12                                        $6.340    3
    ## 13                                        $5.500    3
    ## 14                                        12 NZD    2
    ## 15                                        $4.490    4
    ## 16                                        $4.490    2
    ## 17                                        $8.900    4
    ## 18                                        $6.800    3
    ## 19                                        $7.500    3
    ## 20                                        12 USD    5
    ## 21                                        $5.900    4
    ## 22                                        17 AUD    5
    ## 23                                        $6.800    3
    ## 24                                        $7.900    3
    ## 25                                        $5.800    3
    ## 26                                        $9.000    4
    ## 27                                        $6.900    3
    ## 28                                        $5.900    2
    ## 29                                        $5.800    2
    ## 30                                        $4.500    1
    ## 31                                        $6.250    3
    ## 32                                        $8.000    3
    ## 33                                        $4.600    4
    ## 34                                        $5.600    5
    ## 35                                        $7.500    4
    ## 36                                        $6.500    5
    ## 37                                        $7.600    4
    ## 38                                        $5.800    3
    ## 39                                        $6.300    3
    ## 40                                        $7.500    4
    ## 41                                        11 USD    2
    ## 42                                        15 AUD    5
    ## 43                                        $6.500    4
    ## 44                                        $6.450    3
    ## 45                                        $5.900    1
    ## 46                                        $5.100    3
    ## 47                                        $3.500    3
    ## 48                                        $6.900    3
    ## 49                                        $6.400    3
    ## 50                                        $6.800    4
    ## 51                                        $3.850    3
    ## 52                                        $5.500    5
    ## 53                                        18 USD    2
    ## 54                                        $6.600    3
    ## 55                                        $5.700    3
    ## 57                                        $4.990    2
    ## 58                                        $7.350    3
    ## 59                                        $8.900    5
    ## 60                                        $6.350    3
    ## 61                                        $6.100    3
    ## 62                                        $4.800    1
    ## 63                                        $8.200    3
    ## 64                                        $5.900    4
    ## 65                                        $6.990    2
    ## 66                                        $6.800    3
    ## 67                                        $6.900    5
    ## 68                                        $5.900    5
    ## 69                                       $12.900    3
    ## 70                                       $10.700    4
    ## 71                                        $6.880    3
    ## 72                                        $5.500    3
    ## 73                                        $7.800    1
    ## 74                                        $4.000    5
    ## 75                                        $5.500    3
    ## 76                                        $7.900    3
    ## 77                                        $6.100    4
    ## 78                                        $5.500    5
    ## 80                                        $6.800    4
    ## 81                                        $2.490    4
    ## 82                                        $4.390    3
    ## 83                                        $7.600    3
    ## 84                                        $5.500    5
    ## 85                                 $4.200 aprox.    5
    ## 86                                        $3.500    3
    ## 87                                       $14.500    3
    ## 88                                        $2.600    4
    ## 89                                        $6.000    4
    ## 90                                        $5.800    3
    ## 91                                        $8.500    2
    ## 92                                        $4.000    3
    ## 93                                        $5.800    4
    ## 94                                        $8.450    3
    ## 95                                        22 USD    3
    ## 96                                        $7.590    5
    ## 97                                        $7.600    4
    ## 98                                        $5.450    3
    ## 99                                        $6.900    3
    ## 100                                        8 USD    3
    ## 101                                       $5.800    4
    ## 102                                       $8.700    4
    ## 103                                       $9.200    3
    ## 104                                       $4.500    3
    ## 105                                       $6.990    3
    ## 106                                       $4.490    2
    ## 107                                       $2.900    5
    ## 108                                       $7.900    2
    ## 109                                       $1.650    2
    ## 110                                       $6.300    3
    ## 111                                       $5.000    5
    ## 112                                       $4.900    3
    ## 113                                       $3.290    1
    ## 114                                       $5.800    4
    ## 115                                      $10.500    4
    ## 116                                       $6.500    3
    ## 117                                       $4.300    2
    ## 118                                       $7.100    3
    ## 119                                       $4.950    3
    ## 120                                       $7.500    4
    ## 121                                       $7.200    1
    ## 122                                       $3.800    2
    ## 123                                       $5.700    3
    ## 124                        $2.000 (2 por $3.600)    1
    ## 125                                       $5.590    3
    ## 126                                       $6.600    4
    ## 127                                       $6.900    5
    ## 128                                       $4.900    4
    ## 129                                       $5.900    2
    ## 130                                       $4.800    3
    ## 131                                       18 USD    1
    ## 132                                       $4.850    3
    ## 133                                       $4.200    2
    ## 134                                       $8.900    1
    ## 135                      18 soles (Aprox $3.600)    4
    ## 136                                       $4.000    3
    ## 137                                       $4.500    2
    ## 138                                       $4.600    4
    ## 139                                       $3.750    2
    ## 140                                       $6.100    3
    ## 141                                       $7.500    2
    ## 142                                       $7.900    1
    ## 143                                       $3.500    3
    ## 144                                       $7.990    4
    ## 145                                       $4.500    2
    ## 146                $3500 sandwich y combo $5.500    3
    ## 147                                       $3.700    3
    ## 148                                       $5.800    1
    ## 149                                     15.5 AUD    2
    ## 151                                       $6.800    2
    ## 152                                        9 USD    3
    ## 153                                      9.5 USD    3
    ## 154                                       23 NZD    4
    ## 155                                       $2.300    3
    ## 156                                       $4.000    3
    ## 157                                       $5.000    5
    ## 158                                       $7.400    5
    ## 159                                       $6.500    3
    ## 160                                       $5.400    5
    ## 161                                       $1.750    3
    ## 162                                       $5.000    5
    ## 163                                       $5.400    4
    ## 164                                       $3.000    1
    ## 165                                       $7.100    3
    ## 166                                       $8.000    3
    ## 167                                       $4.500    1
    ## 168                                       $8.600    3
    ## 169                                       $4.900    3
    ## 171                                       $5.900    3
    ## 172                                       $6.500    1
    ## 173                                       $3.400    2
    ## 174                                       $5.900    5
    ## 175                                       15 AUD    5
    ## 176                                       $9.890    2
    ## 177                                       $7.950    4
    ## 178                                       $3.900    2
    ## 179                                       $5.990    4
    ## 180                                       $3.500    4
    ## 181                                       $5.990    5
    ## 182                                       $5.490    3
    ## 183                                       $6.500    1
    ## 184                                       $8.400    3
    ## 185                                       $8.900    2
    ## 186                                       $7.200    5
    ## 187                                       $7.200    3
    ## 188                                       $6.900    5
    ## 189                                       $4.800    3
    ## 190                                       28 USD    5
    ## 191                                       $4.900    4
    ## 192                                       $7.900    2
    ## 193                                       $4.950    3
    ## 194                                       $3.500    3
    ## 195                                                 5
    ## 196                                       $7.260    2
    ## 197                                       19 AUD    5
    ## 198                                       $5.690    2
    ## 199                                       $5.700    3
    ## 200                                       $5.900    5
    ## 201                                       $5.000    4
    ## 202                                       20 USD    5
    ## 203                                       $6.300    3
    ## 204                                         $600    4
    ## 205                                       $5.930    2
    ## 206                                       $4.900    4
    ## 207                                       $6.700    3
    ## 208                                       $5.500    2
    ## 209                                       $5.900    4
    ## 210                                       $8.500    5
    ## 211                                       $3.600    3
    ## 212                               $14.600 aprox.    1
    ## 213                                       $6.000    4
    ## 214                                       $8.400    5
    ## 215                                       $6.600    5
    ## 216                                       $6.900    5
    ## 217                                       $6.990    2
    ## 218                                       $4.500    4
    ## 219                                       $7.500    4
    ## 220                                       $4.890    2
    ## 221                                       $6.400    2
    ## 222                                       $7.200    4
    ## 223                                       $7.000    1
    ## 224                                       $5.200    3
    ## 225                                       $5.800    4
    ## 226                                       $6.490    4
    ## 227                   14.90 soles (aprox $3.000)    2
    ## 228                                       $7.800    5
    ## 229                                       $8.500    3
    ## 230                                       $4.500    4
    ## 231                                       $6.300    3
    ## 232                                       $5.400    5
    ## 233                                       $4.500    3
    ## 234                                       $8.790    3
    ## 235                                       $8.100    2
    ## 236                                       $4.990    2
    ## 237                                       $4.000    2
    ## 238                                       $3.000    5
    ## 239                                       $2.000    4
    ## 240                                      $10.800    3
    ## 241                                       $5.500    5
    ## 242                                       $4.500    5
    ## 243                                       $7.900    3
    ## 244                                       $7.100    4
    ## 245                                       $7.990    3
    ## 246                                       $6.090    2
    ## 247                                       $5.990    5
    ## 248                                       $6.900    3
    ## 249                                       $4.300    2
    ## 250                                       $6.900    4
    ## 251                                       $6.200    3
    ## 252                                       $3.990    4
    ## 253                                       $5.200    4
    ## 254                                       $6.000    3
    ## 255                                       $7.900    4
    ## 256                                       $7.900    5
    ## 257                                       $5.800    4
    ## 258                                       $6.500    4
    ## 259                                       $6.900    3
    ## 260                                       $4.000    3
    ## 261                                       $4.430    3
    ## 262                                       $5.800    3
    ## 263                                       $6.200    3
    ## 264                                       $6.500    5
    ## 265                                       $6.990    4
    ## 266                                       $5.800    5
    ## 267                                       $7.200    3
    ## 268                                       $3.800    3
    ## 269                                       $6.400    3
    ## 270                                       $7.800    3
    ## 271                                       $8.900    3
    ## 272                                       $6.800    4
    ## 273                                       $5.100    3
    ## 274                                       $4.000    4
    ## 275                                       $6.500    3
    ## 276                                       $8.900    3
    ## 277                                       $5.800    3
    ## 278                                       $8.990    2
    ## 279                                       $5.000    4
    ## 280                                       $3.500    3
    ## 281                                       $3.600    2
    ## 282                                       $7.500    3
    ## 283                                       $4.100    2
    ## 284                                       $7.890    5
    ## 285                                       $6.000    3
    ## 286                                       $9.900    3
    ## 287                                       $4.000    1
    ## 288                                       $6.000    3
    ## 289                                       $5.100    1
    ## 290                                       $6.900    3
    ## 291                                       $7.400    3
    ## 292                                       $5.400    5
    ## 293                                        7 USD    3
    ## 294                                     15 AUD      4
    ## 295                                       $6.400    1
    ## 296                                       $7.500    1
    ## 297                                       $5.700    2
    ## 298                                       $3.400    4
    ## 299                                       $3.400    3
    ## 300                                       $9.000    1
    ## 301                                       $4.000    3
    ## 302                                       $5.500    3
    ## 303                                    14.00 NZD    3
    ## 304                                       $6.400    3
    ## 305                                       $6.990    3
    ## 306 16.9 AUD en pan normal, y 66.9 en pan de oro    4
    ## 307                                       16 USD    4
    ## 308                                       $1.700    1
    ## 309                                       $5.900    3
    ## 310                                       $6.250    3
    ## 311                                       $5.600    2
    ## 312                            $4.200 con bebida    3
    ## 313                                       $1.550    4
    ## 314                                                 1
    ## 315                                       $8.200    4
    ## 316                                       $2.360    3
    ## 317                                       $4.150    3
    ## 318                                       $6.090    3
    ## 319                                       $6.100    2
    ## 320                                       $3.500    4
    ## 321                                       $6.900    5
    ## 322                                       $5.000    1
    ## 323                                       $8.600    1
    ## 324                                       $9.000    2
    ## 325                                       $4.500    4
    ## 326                                       $4.500    3
    ## 327                                       $4.200    3
    ## 328                                       $4.200    1
    ## 329                                       $5.600    5
    ## 330                                       $6.900    2
    ## 331                                       $8.990    2
    ## 332                                       $7.500    4
    ## 333                                      $12.900    4
    ## 334                                       16 USD    4
    ## 335                                       $4.700    3
    ## 336                                       $7.950    1
    ## 337                                       $4.000    1
    ## 338                                       $5.100    1
    ## 339                                       $6.300    3
    ## 340                                       $5.800    4
    ## 341                                       $5.000    3
    ## 342                                       $4.590    3
    ## 343                                       $6.100    4
    ## 344                                       $5.790    3
    ## 345                                       $6.000    4
    ## 346                                       $6.300    5
    ## 347                                       $7.400    5
    ## 348                                       $2.800    3
    ## 349                                       $6.900    3
    ## 350                                       $2.500    2
    ## 351                                       $5.590    3
    ## 352                                       $6.700    4
    ## 353                                       $6.900    2
    ## 354                                       $6.950    3
    ## 355                                       $6.900    1
    ## 356                                        9 USD    4
    ## 357                                       $5.500    1
    ## 358                                     13.5 AUD    5
    ## 359                                       $8.900    3
    ## 360                                       $7.100    1
    ## 361                                       26 USD    3
    ## 362                                       $6.900    5
    ## 363                                       $4.400    2
    ## 364                                       $7.800    4
    ## 366                                      9.5 USD    3
    ## 367                                       $5.990    2
    ## 368                                       $8.100    5
    ## 369                                       $5.400    2
    ## 370                                       $5.900    4
    ## 371         $140 pesos argentinos (Aprox $3.150)    2
    ## 372                                       $5.900    3
    ## 373                                       $7.500    1
    ## 374                                       $3.990    3
    ## 375                                       $5.900    5
    ## 376                                       $6.800    1
    ## 377                                       13 USD    2
    ## 378                                       14 AUD    4
    ## 379                                       $5.500    4
    ## 380                                       $4.500    3
    ## 381                                       25 USD    3
    ## 382                                       17 USD    3
    ## 383                                       $7.900    5
    ## 384                                       22 NZD    1
    ## 385                                       $4.300    3
    ## 386                                       $2.500    2
    ## 387                                       14 NZD    4
    ## 388                                       $4.050    2
    ## 389                                       $8.900    3
    ## 390                                       $9.800    3
    ## 391                                       $2.700    3
    ## 392                                       $7.490    2
    ## 394                                       $5.800    2
    ## 395                                       $3.000    3
    ## 396                                       $8.800    3
    ## 397                                       $8.600    3
    ## 398                                       $6.800    4
    ## 399                                       $3.900    5
    ## 400                                       18 AUD    3
    ## 402                                       $4.500    4
    ## 403                                       $5.500    4
    ## 404                                       $4.200    4
    ## 405                                       $5.500    3
    ## 406                                       $4.890    3
    ## 408                                       $3.900    3
    ## 409                                       $6.900    4
    ## 410                                       $4.900    2

``` r
datos <- na.omit(datos)
head(datos)
```

    ##    Precio nota
    ## 1 $5.210.    3
    ## 2  $7.000    3
    ## 3  $7.290    4
    ## 4  $8.690    4
    ## 5  $4.900    4
    ## 6  $6.500    3

## Misma clase de datos

Para poder gacer una correcta comparacion, necesitamos que en la base de
datos solo exista un tipo de datos, y el precio es un caracter ya que
posee “$”, puntos y comas. Por lo que procederemos a borrar dichos
elementos para convertir nuestra variable a un numero.

``` r
datos$Precio <- as.numeric(gsub('[$.]', '', datos$Precio))
```

    ## Warning: NAs introducidos por coerción

``` r
head(datos)
```

    ##   Precio nota
    ## 1   5210    3
    ## 2   7000    3
    ## 3   7290    4
    ## 4   8690    4
    ## 5   4900    4
    ## 6   6500    3

``` r
datos <- datos[complete.cases(datos), ]
summary(datos)
```

    ##      Precio           nota      
    ##  Min.   :  600   Min.   :1.000  
    ##  1st Qu.: 4892   1st Qu.:3.000  
    ##  Median : 5990   Median :3.000  
    ##  Mean   : 6061   Mean   :3.154  
    ##  3rd Qu.: 7100   3rd Qu.:4.000  
    ##  Max.   :14500   Max.   :5.000

## Boxplot´s

Usaremos estas imagenes como primera aproximacion para realizar el
analisis.

``` r
pre=boxplot(datos$Precio, horizontal =TRUE)
```

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
stats_pre = boxplot.stats(datos$precio)
pre
```

    ## $stats
    ##      [,1]
    ## [1,] 1650
    ## [2,] 4890
    ## [3,] 5990
    ## [4,] 7100
    ## [5,] 9900
    ## 
    ## $n
    ## [1] 358
    ## 
    ## $conf
    ##          [,1]
    ## [1,] 5805.453
    ## [2,] 6174.547
    ## 
    ## $out
    ## [1] 12900 10700 14500 10500   600 10800  1550 12900
    ## 
    ## $group
    ## [1] 1 1 1 1 1 1 1 1
    ## 
    ## $names
    ## [1] "1"

``` r
stats_pre
```

    ## $stats
    ## [1] NA NA NA NA NA
    ## 
    ## $n
    ## [1] 0
    ## 
    ## $conf
    ## [1] NA NA
    ## 
    ## $out
    ## NULL

``` r
datos<-filter(datos, Precio <10500)
boxplot(datos$Precio, horizontal = TRUE)
```

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
not=boxplot(datos$nota, horizontal =TRUE)
```

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
stats_not = boxplot.stats(datos$nota)
not
```

    ## $stats
    ##      [,1]
    ## [1,]    2
    ## [2,]    3
    ## [3,]    3
    ## [4,]    4
    ## [5,]    5
    ## attr(,"class")
    ##         1 
    ## "integer" 
    ## 
    ## $n
    ## [1] 352
    ## 
    ## $conf
    ##          [,1]
    ## [1,] 2.915786
    ## [2,] 3.084214
    ## 
    ## $out
    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## 
    ## $group
    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ## 
    ## $names
    ## [1] "1"

``` r
stats_not
```

    ## $stats
    ## [1] 2 3 3 4 5
    ## 
    ## $n
    ## [1] 352
    ## 
    ## $conf
    ## [1] 2.915786 3.084214
    ## 
    ## $out
    ##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

``` r
datos<-filter(datos, nota>1)
boxplot(datos$nota, horizontal = TRUE)
```

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Escalar datos

``` r
escala_d  = scale(datos)%>%as_tibble ()
escala_d %>% summary()
```

    ##      Precio              nota        
    ##  Min.   :-3.18270   Min.   :-1.4658  
    ##  1st Qu.:-0.63283   1st Qu.:-0.3841  
    ##  Median :-0.03252   Median :-0.3841  
    ##  Mean   : 0.00000   Mean   : 0.0000  
    ##  3rd Qu.: 0.61535   3rd Qu.: 0.6975  
    ##  Max.   : 2.34498   Max.   : 1.7792

## Cluster k=10

``` r
modelo_kmeans <- kmeans(escala_d, centers = 10)
modelo_kmeans2 <- kmeans(datos, centers = 10)

escala_d$clus <- modelo_kmeans$cluster %>% as.factor()
datos$clus <- modelo_kmeans2$cluster %>% as.factor()

ggplot(escala_d, aes(Precio, nota, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
\#\# Evolución suma de cuadrados intra-cluster en la medida que
aumentamos el numero de k

``` r
SSinterior <- numeric(30)

for(k in 1:30){
  modelo <- kmeans(escala_d, centers = k)
  SSinterior[k] <- modelo$tot.withinss
}

plot(SSinterior)
```

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
\#\#EVALUACIÓN

\#\#Inspeccion visual

``` r
escala_d$clus <- as.numeric(escala_d$clus)
datos$clus <- as.numeric(datos$clus)

# uso distancia euclidiana
tempDist <- dist(escala_d) %>% as.matrix()

#reordeno filas y columnas en base al cluster obtenido
index <- sort(modelo_kmeans$cluster, index.return=TRUE)
tempDist <- tempDist[index$ix,index$ix]
rownames(tempDist) <- c(1:nrow(escala_d))
colnames(tempDist) <- c(1:nrow(escala_d))

image(tempDist)
```

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Coeficiente de silueta

``` r
coefSil <- silhouette(modelo_kmeans$cluster,dist(escala_d))
summary(coefSil)
```

    ## Silhouette of 321 units in 10 clusters from silhouette.default(x = modelo_kmeans$cluster, dist = dist(escala_d)) :
    ##  Cluster sizes and average silhouette widths:
    ##        41        20        28        28        55         8        30        48 
    ## 0.7815651 0.8016876 0.6011713 0.6239775 0.8480067 0.7856281 0.6839626 0.6132247 
    ##        43        20 
    ## 0.7711831 0.7477706 
    ## Individual silhouette widths:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2713  0.6710  0.7458  0.7270  0.8114  0.8853

``` r
fviz_silhouette(coefSil)+coord_flip()
```

    ##    cluster size ave.sil.width
    ## 1        1   41          0.78
    ## 2        2   20          0.80
    ## 3        3   28          0.60
    ## 4        4   28          0.62
    ## 5        5   55          0.85
    ## 6        6    8          0.79
    ## 7        7   30          0.68
    ## 8        8   48          0.61
    ## 9        9   43          0.77
    ## 10      10   20          0.75

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
\#\# Utilizamos el coeficiente de silueta para encontrar el mejor valor
de K

``` r
coefSil=numeric(30)
for (k in 2:30){
  modelo <- kmeans(escala_d, centers = k)
  temp <- silhouette(modelo$cluster,dist(escala_d))
  coefSil[k] <- mean(temp[,3])
}
tempDF=data.frame(CS=coefSil,K=c(1:30))

ggplot(tempDF, aes(x=K, y=CS)) + 
  geom_line() +
  scale_x_continuous(breaks=c(1:30))
```

![](tarea5_ayudantia_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

\#\#esto era lo que no me funcionaba porsia \#\`\`\`{r} \#datos2 &lt;-
datos\[, colnames(datos) %in% c(“Type.1”,“HP”, “Attack”, “Defense”,
“Speed”, “Generation”, “Legendary”)\] \#escala\_d2 = scale(datos2)
%&gt;% as\_tibble()

\#modelo\_kmean &lt;- kmeans(escala\_d2, centers = 3) \#modelo\_kmean2
&lt;- kmeans(datos2, centers = 3)

\#escala\_d2*c**l**u**s*2 &lt;  − *m**o**d**e**l**o*<sub>*k*</sub>*m**e**a**n*cluster
%&gt;% as.factor()
\#datos2*c**l**u**s*2 &lt;  − *m**o**d**e**l**o*<sub>*k*</sub>*m**e**a**n*2cluster
%&gt;% as.factor()

\#ggplot(escala\_d2, aes(Speed, Attack, color=clus2)) + \#
geom\_point(alpha=0.5, show.legend = T) + \# theme\_bw() \`\`\`
