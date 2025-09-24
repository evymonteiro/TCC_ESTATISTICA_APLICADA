
rm(list = ls())

vars_select = c("N_AIH", "ANO_CMPT", "MES_CMPT", "UF_ZI", "MUNIC_RES", "NASC", "IDADE", "SEXO", 
                "DIAG_PRINC", "QT_DIARIAS", "DT_INTER", "DT_SAIDA",  "VAL_SP", "VAL_TOT")


# Doenças respiratórias que podem ser ocasionadas por queimadas:

# J40 - Bronquite não especificada como aguda ou crônica
# J41 - Bronquite crônica simples e mucopurulenta
# J42 - Bronquite crônica não especificada
# J43 - Enfisema pulmonar
# J44 - Doença Pulmonar Obstrutiva Crônica (DPOC)
# J45 - Asma
# J46 - Estado de mal asmático
# J18 - Pneumonia não especificada
# J00-J06 - Infecções agudas das vias aéreas superiores (resfriados comuns, rinite aguda, sinusite)
# J20-J22 - Outras infecções agudas das vias aéreas inferiores (bronquiolites, bronquite aguda, infecção respiratória inferior aguda não especificada)

# DEFININDO AS CID DE INTERESSE
cids_select <- c("J40", "J41", "J42", "J43", "J44", "J45", "J46", "J18",
                 "J00", "J01", "J02", "J03", "J04", "J05", "J06",
                 "J20", "J21", "J22")

### PERÍODO DE ANÁLISE 2015 A 2018
### MENSAL


##Bibliotecas: 

library(scales)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(purrr)
library(geobr)
library(RColorBrewer)
library(colorspace)
library(tmap)
library(leaflet)
library(leaflet.extras2)
library(leafem)
library(ggspatial)
library(vioplot)
library(sf)
library(sp)
library(raster)
library(lattice)
library(spatstat)
library(spdep)
library(spatialreg)
library(spgwr)
library(gstat)
library(automap)
library(arrow)


#######################################
#### CARREGANDO BANCO DE DADOS #####
#######################################


#read_parquet("dados.parquet")

# Lendo os dados extraidos do TerraBrasilis

### LOAD BANCO DE DADOS DATASUS ###

#load("2018.RData")
load("dados_sih.RData")
load("dados_inpe.RData")


#dados <- c(X2015, X2016, X2017, X2018)
#dados <- data.frame(valor = dados)


library(dplyr)
library(tidyr)

#lista_de_dados <- list(X2015, X2016, X2017, X2018)

#dados <- tibble(valor = lista_de_dados) %>%
 # unnest(cols = c(valor))

#dados_sih <- bind_rows(sih_df_MS, sih_df_MT)

# Verifique o resultado
#head(dados)

#save(dados_sih, file = "dados_sih.RData")

#################################################
######### FORMATAÇÃO REALIZADA NO TERRABRASILIS ##########
###########################################

# Formatando as datas do banco e retirando aqueles registros sem data
#dados_inpe <- dados %>%
 # mutate(Ano = year(ymd_hms(DataHora))) |>  # extrai o ano da coluna DataHora
#  mutate(across(where(is.numeric), ~na_if(., -999)))

head(dados_inpe)

unique(dados_inpe$Ano)

##################################################
## Agregando a Freqencia de focos de calor por mes e Estado
##################################################

# Supondo que dadosBR$DataHora esteja no formato POSIXct ou Date

#dados_inpe <- dados_inpe %>%
 # mutate(Mes = month(as.Date(DataHora))) %>%
  #count(Estado, Municipio, Mes, Ano, name = "Focos_Mes")


### count() é uma função do pacote dplyr que é, na prática, um atalho para um group_by() + summarize(n = n()).

##Transformando os nomes das colunas Estado para as UFs

#dados_inpe <- dados_inpe %>%
 # mutate(Estado = recode(Estado, !!!UFs))

#dados_inpe <- dados_inpe %>%
 # filter(Estado %in% c("MT", "MS"))


####### Padronizando as datas de dos dados_inpe e dados_sih
####### Selecionando o período de interesse do dados_sih



glimpse(dados_sih)
table(dados_sih$uf)

summary(dados_sih)

sih_mes <- dados_sih %>%
  count(Estado, UF_ZI, MUNIC_RES, DIAG_PRINC, MES_CMPT, ANO_CMPT, name = "Internações")

save(sih_mes, file = "sih_mes.RData")


##########################################################
# ANÁLISE EXPLORATÓRIA DOS DADOS 
##########################################################

summary(sih_mes)
summary(dados_inpe)

par(mfrow = c(1, 2))
hist(sih_mes$Internações)
hist(dados_inpe$Focos_Mes)

par(mfrow = c(1, 2))
boxplot(dados_inpe$Focos_Mes)
boxplot(sih_mes$Internações)

par(mfrow = c(1, 1))

boxplot(Focos_Mes ~ Mes, 
        data = dados_inpe,
        main = "Distribuição de Focos por Mês",
        xlab = "Mês",
        ylab = "Qtd de Focos",
        col = "skyblue")

boxplot(Focos_Mes ~ Ano, 
        data = dados_inpe,
        main = "Distribuição de Focos por Ano",
        xlab = "Ano",
        ylab = "Qtd de Focos",
        col = "skyblue")

boxplot(Internações ~ MES_CMPT, 
        data = sih_mes,
        main = "Distribuição de Internações por Mes",
        xlab = "Mês",
        ylab = "Qtd de Focos",
        col = "skyblue")

boxplot(Internações ~ ANO_CMPT, 
        data = sih_mes,
        main = "Distribuição de Internações por Ano",
        xlab = "Ano",
        ylab = "Qtd de Focos",
        col = "skyblue")


##########################################################
# PROPORÇÃO
##########################################################

# Certifique-se de que a biblioteca dplyr está carregada
library(dplyr)

cids <- sih_mes %>%
  # Agrupa os dados por Ano, Mes e munic_res
  group_by(ANO_CMPT, MES_CMPT, MUNIC_RES) %>%
  # Calcula o total de internações para os CIDs de interesse
  summarise(
    internacoes_interesse = sum(DIAG_PRINC %in% cids_select)
  ) %>%
  # FILTRA: Remove todas as linhas onde a contagem é zero
  filter(internacoes_interesse > 0) %>%
  # Desagrupa os dados
  ungroup()

# Exibir as primeiras linhas do novo data frame
print(head(cids))

# Exibir as primeiras linhas do novo data frame com as proporções
print(head(cids))

##Proporção = 0

print(sum(proporcao$proporcao_cids == 0))


###############################################################
######## AGREGANDO DADOS ESPACIAIS #######################
############################################################

### Shapefile biomas disponível em 
# https://www.ibge.gov.br/geociencias/informacoes-ambientais/vegetacao/15842-biomas.html

## Também pode ser baixado por meio do geo_br
#Shapes:

municipios <- read_municipality()


st_crs(municipios)

save(estados, file = "shape_estados.RData")

library(wesanderson)

ggplot(municipios) +
  geom_sf(aes(fill = abbrev_state), color = "gray40") +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 2)) +
  labs(
    title = "MUNICÍPIOS MT E MS",
    fill = "Estado",
    caption = "Sistema de Coordenadas Geográficas, SIRGAS 2000. Fonte: IBGE"
  ) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true") +
  ggspatial::annotation_scale(location = "br") +
  coord_sf(datum = st_crs(4326)) +
  theme_classic() +
  theme(
    # Centraliza o título principal do gráfico
    plot.title = element_text(hjust = 0.5),
    # Centraliza a legenda de cores na parte inferior do gráfico
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center"
  )

library(dplyr)
library(sf)
names(municipios)

municipios <- municipios %>%
  filter(abbrev_state %in% c("MS", "MT"))

# 2. Salvar o novo objeto em um arquivo shapefile
# O arquivo será salvo na sua pasta de trabalho
st_write(
  municipios,
  "municipios.shp",
  driver = "ESRI Shapefile" # Driver para o formato .shp
)


# Mapa
mapa_focos_bioma <- ggplot() +
  # Biomas coloridos por tipo
  geom_sf(data = biomas, aes(fill = name_biome), color = NA, alpha = 0.5) +
  
  # Estados
  geom_sf(data = estados, fill = NA, color = "black", size = 0.4) +
  
  # Rótulo dos estados
  geom_text(data = estados_centroides,
            aes(x = X, y = Y, label = abbrev_state),
            size = 3, fontface = "bold", color = "black") +
  
  # Pontos proporcionais aos focos
  geom_point(data = biomas_centroides,
             aes(x = X, y = Y, size = QtdFocos),
             color = "red", alpha = 0.7) +
  
  # Escala de cor discreta para os biomas
  scale_fill_brewer(palette = "Dark2", name = "Biomas") +
  scale_size_continuous(
    name = "Qtd. de Focos",
    range = c(6, 40),
    breaks = c(2000, 25000, 50000, 100000, 200000, 350000),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  
  # Legenda, título e anotações
  labs(title = "Focos de Calor por Bioma no Brasil",
       caption = "Fonte: dados_bioma + IBGE") +
  
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  
  annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
  
  coord_sf() +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    plot.caption = element_text(hjust = 0.5)
  ) +
  
  annotation_custom(
    grob = grid::textGrob("CRS: SIRGAS 2000",
                          x = unit(0.95, "npc"),
                          y = unit(0.1, "npc"),
                          just = c("right", "bottom"),
                          gp = grid::gpar(fontsize = 9, fontface = "italic"))
  )



####### PLOTANDO MAPA DE FOCOS POR ESTADO ####

### Adiciona o total de foco de incêndios de cada Estado a geometria dos polígonos
## de cada Estado. 

focos_por_estado <- dadosBR_mes %>%
  group_by(Estado) %>%
  summarise(QtdFocos = sum(QtdFocos, na.rm = TRUE)) %>%
  ungroup() %>%
  { left_join(estados, ., by = c("abbrev_state" = "Estado")) }

save(focos_por_estado, file = "focos_por_estado.RData")


library(ggplot2)
library(ggspatial)
library(grid)

#biomas <- biomas %>% 
#  filter(name_biome != "Sistema Costeiro")

mapa_focos_por_estado <- ggplot() +
  # Preenchimento dos estados pela quantidade de focos
  geom_sf(data = focos_por_estado, aes(fill = QtdFocos), color = NA) +
  # Rótulo dos estados
  geom_text(data = estados_centroides,
            aes(x = X, y = Y, label = abbrev_state),
            size = 3.5, fontface = "bold", color = "black") +
  scale_fill_gradient(
    low = "#fde0dd",  # rosa bem claro
    high = "#c51b8a", # rosa escuro
    name = "Qtd. de Focos\n(2022–2024)",
    labels = comma_format()
  ) +
  labs(
    title = "Total de Focos por Estado (2022–2024)",
    caption = "Fonte: INPE + IBGE"
  ) +
  
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  
  annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
  
  coord_sf() +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    plot.caption = element_text(hjust = 0.5)
  ) +
  
  annotation_custom(
    grob = grid::textGrob("CRS: SIRGAS 2000",
                          x = unit(0.95, "npc"),
                          y = unit(0.1, "npc"),
                          just = c("right", "bottom"),
                          gp = grid::gpar(fontsize = 9, fontface = "italic"))
  )




############## MAPA DE INTERNAÇÕES POR ESTADO  ######
## media_dias_por_internacao
## internação /população 

internacoes_por_estado <- sih_mes %>%
  group_by(uf) %>%
  summarise(Internações = sum(Internações, na.rm = TRUE)) %>%
  ungroup() %>%
  { left_join(estados, ., by = c("abbrev_state" = "uf"))}


save(internacoes_por_estado, file = "internacoes_por_estado.RData")



mapa_internacoes <-  ggplot() +
  geom_sf(data = internacoes_por_estado, aes(fill = Internações), color = NA) +
  # Rótulo dos estados
  geom_text(data = estados_centroides,
            aes(x = X, y = Y, label = abbrev_state),
            size = 3.5, fontface = "bold", color = "black") +
  scale_fill_gradient(
    low = "lightblue",  
    high = "blue", 
    name = "Internações por CID Respiratória \n(2022–2024)",
    labels = comma_format()
  ) +
  labs(
    title = "Internações por CID Respiratória (2022–2024)",
    caption = "Fonte: Datasus + IBGE"
  ) +
  
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  
  annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
  
  coord_sf() +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    plot.caption = element_text(hjust = 0.5)
  ) +
  
  annotation_custom(
    grob = grid::textGrob("CRS: SIRGAS 2000",
                          x = unit(0.95, "npc"),
                          y = unit(0.1, "npc"),
                          just = c("right", "bottom"),
                          gp = grid::gpar(fontsize = 9, fontface = "italic"))
  )

################## MAPAS INTERNAÇÕES E FOCOS ###############
#### FOCO / INTERNAÇÃO. 
library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(scales)
library(grid)

# Centroides dos focos de calor por ESTADO (para pontos proporcionais)

focos_centroides <- focos_por_estado %>%
  mutate(centroide = st_centroid(geom)) %>%
  transmute(
    X = st_coordinates(centroide)[,1],
    Y = st_coordinates(centroide)[,2],
    QtdFocos
  )

save(focos_centroides, file = "focos_centroides.RData")

# Mapa
mapa_internacoes_por_estado <- ggplot() +
  # Estados preenchidos proporcionalmente às internações
  geom_sf(data = internacoes_por_estado, aes(fill = Internações), color = "gray30", size = 0.3) +
  
  # Bordas dos estados
  geom_sf(data = estados, fill = NA, color = "black", size = 0.3) +
  
  # Rótulos dos estados
  geom_text(data = estados_centroides,
            aes(x = X, y = Y, label = name_state),
            size = 2.8, fontface = "bold", color = "black") +
  
  # Pontos proporcionais aos focos de calor
  geom_point(data = focos_centroides,
             aes(x = X, y = Y, size = QtdFocos),
             color = "red", alpha = 0.7) +
  
  # Escala de preenchimento para internações
  scale_fill_gradient(low = "lightyellow", high = "darkred", name = "Internações") +
  
  # Escala de tamanho para focos
  scale_size_continuous(
    name = "Qtd. de Focos",
    range = c(6, 40),
    breaks = c(500, 5000, 15000, 50000, 100000, 150000),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  
  # Título e anotações
  labs(title = "Internações por Estado e Focos de Calor no Brasil",
       caption = "Fonte: dados do SUS e INPE") +
  
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  
  annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
  
  coord_sf() +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    plot.caption = element_text(hjust = 0.5)
  ) +
  
  annotation_custom(
    grob = grid::textGrob("CRS: SIRGAS 2000",
                          x = unit(0.95, "npc"),
                          y = unit(0.1, "npc"),
                          just = c("right", "bottom"),
                          gp = grid::gpar(fontsize = 9, fontface = "italic"))
  )



######## ESTATÍSTICA ESPACIAL - DADOS DE ÁREA ##################

#Contagem delimitada por polígonos. 

library(sf)
library(spdep)
library(tmap)
library(tidyverse)

dadosBR %>% filter(is.na(Latitude) | is.na(Longitude))
dadosBR <- dadosBR %>%
  mutate(Estado = recode(Estado, !!!UFs))
save(pontos_sf, file = "pontos_sf.RData")

pontos_sf <- st_as_sf(dadosBR, coords = c("Longitude", "Latitude"), crs = 4674)


library(ggplot2)
library(sf)
library(ggspatial)
library(grid)

########################################################
###### MORAN GLOBAL, LOCAL E LISAMAP FOCOS DE INCENDIO ############
########################################################


library(sf)
library(spdep)
library(ggplot2)

## ===============================
## MATRIZ DE VIZINHANÇA - ESTADOS
## ===============================
library(sf)
library(spdep)
library(ggplot2)

#Transformar para CRS projetado métrico 
focos_proj <- st_transform(focos_por_estado, 31983)
save(focos_proj, file = "focos_proj.RData")


#Extrair centroides no CRS projetado
coord_estados_proj <- as.matrix(st_coordinates(st_centroid(focos_proj)))

#Criar vizinhança por contiguidade 
nb_focos_estados <- poly2nb(focos_proj)

#Criar pesos espaciais normalizados 
lw_focos_estados <- nb2listw(nb_focos_estados, style = "W")

#Criar linhas de vizinhança para visualização
nb_focos_estados.sf <- as(nb2lines(nb_focos_estados, coords = coord_estados_proj), "sf")
nb_focos_estados.sf <- st_set_crs(nb_focos_estados.sf, st_crs(focos_proj))

#Plot
ggplot() +
  geom_sf(data = focos_proj, fill = "lightpink", color = "white") +
  geom_sf(data = nb_focos_estados.sf, color = "red", size = 1.2) +
  labs(title = "Vizinhança por Contiguidade - Estados (projetado)") +
  theme_minimal()

#### MORAN GLOBAL 

## Hipótese Nula: Não há autocorrelação espacial vs H1: Há autocorrelação. 

moran.test(focos_proj$QtdFocos, lw_focos_estados)
moran.test(focos_bioma_proj$QtdFocos, lw_bioma)

###Por Estado H0 é rejeitada. 
###Por Bioma H0 não é rejeitada

### MORAN LOCAL 

lisa_focos_estados <- localmoran(focos_proj$QtdFocos, lw_focos_estados)
lisa_bioma <- localmoran(focos_bioma_proj$QtdFocos, lw_bioma)


# LISA para Estados
focos_proj$Ii <- lisa_focos_estados[, "Ii"]
focos_proj$pvalue <- lisa_focos_estados[, "Pr(z != E(Ii))"]

save(internacoes_proj, file = "internacoes_proj.RData")

# Correlograma (Estados)
correl_estado <- sp.correlogram(nb_focos_estados, focos_proj$QtdFocos, order = 4, method = "I")
print(correl_estado)
plot(correl_estado)

### Mapeando os polígonos que tiveram os p-valores mais significativos no Moran Local
library(tmap)
tm_shape(focos_bioma_proj) +
  tm_polygons(
    fill = "pvalue",
    fill.scale = tm_scale_intervals(
      breaks = c(0, 0.01, 0.05, 0.1, 1),
      values = RColorBrewer::brewer.pal(5, "Blues")
    ),
    fill.legend = tm_legend(title = "p-valores"),
    col = "grey"
  ) +
  tm_layout(
    title = "Significância do Moran Local",
    title.position = c("center", "top"),  
    legend.outside = TRUE
  ) +
  tm_compass(
    position = c("left", "top"),
    type = "arrow",
    size = 2,
    show.labels = TRUE
  ) +
  tm_scale_bar(
    position = c("left", "bottom")
  ) +
  tm_credits(
    text = paste0("CRS: SIRGAS 2000 UTM ZONE 23 EPSG 31893 "),
    position = c("right", "bottom"),
    size = 0.7,
    fontface = "italic"
  )

## ===============================
## LISAMAP
## ===============================

# CLASSIFICAÇÃO DE CLUSTERS LISA
classificar_clusters <- function(obj, variavel, moran, pvalor, nome_saida = "cluster_type") {
  media <- mean(obj[[variavel]], na.rm = TRUE)
  obj[[nome_saida]] <- "Não significativo"
  obj[[nome_saida]][obj[[variavel]] >= media & obj[[moran]] >= 0 & obj[[pvalor]] <= 0.05] <- "Alto-Alto"
  obj[[nome_saida]][obj[[variavel]] <  media & obj[[moran]] >= 0 & obj[[pvalor]] <= 0.05] <- "Baixo-Baixo"
  obj[[nome_saida]][obj[[variavel]] <  media & obj[[moran]] <  0 & obj[[pvalor]] <= 0.05] <- "Baixo-Alto"
  obj[[nome_saida]][obj[[variavel]] >= media & obj[[moran]] <  0 & obj[[pvalor]] <= 0.05] <- "Alto-Baixo"
  obj[[nome_saida]] <- factor(obj[[nome_saida]], levels = c(
    "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo", "Não significativo"
  ))
  return(obj)
}

## ===============================
## LISAMAP FOCOS POR ESTADO: 
## ===============================

focos_proj <- classificar_clusters(focos_proj, variavel = "QtdFocos", moran = "Ii", pvalor = "pvalue")
save(focos_proj, file = "focos_proj.RData")

tm_shape(focos_proj) +
  tm_fill(
    col = "cluster_type",
    palette = c(
      "Alto-Alto"         = "#E60000",
      "Baixo-Baixo"       = "#0033CC",
      "Baixo-Alto"        = "#9999FF",
      "Alto-Baixo"        = "#FF9999",
      "Não significativo" = "#FFFFFF"
    ),
    title = "Cluster Local (LISA)",
    legend.is.portrait = TRUE
  ) +
  tm_borders(col = "gray40", lwd = 0.4) +
  tm_layout(
    main.title = "Mapa LISA - Clusters Locais de Focos por Estado",
    main.title.size = 1.2,
    main.title.position = "center",
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "white"
  ) +
  tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_credits("CRS: SIRGAS 2000 / UTM Zone 23S", position = c("RIGHT", "BOTTOM"), size = 0.6)


########################################################
###### MORAN GLOBAL, LOCAL E LISAMAP INTERNAÇÕES ############
########################################################

# Carregar pacotes
library(sf)
library(spdep)
library(dplyr)
library(tmap)

# Garantir que a coluna Pop está numérica
POPULACAO <- POPULACAO %>%
  mutate(Pop = as.numeric(gsub("\\.", "", Pop)))  # Remove pontos de milhar se houver

#save(lw_bioma, file = "lw_bioma.RData")

# Projetar os dados espaciais
internacoes_proj <- st_transform(internacoes_por_estado, 31983)

# Juntar com os dados de população
internacoes_proj <- internacoes_proj %>%
  left_join(POPULACAO, by = c("abbrev_state" = "UF")) %>%
  mutate(
    taxa_incidencia = (Internações / Pop) * 100000
  )

# Coordenadas dos centroides 
coord_internacoes <- as.matrix(st_coordinates(st_centroid(internacoes_proj)))

# Matriz de vizinhança e pesos
nb_internacoes <- poly2nb(internacoes_proj)
lw_internacoes <- nb2listw(nb_internacoes, style = "W") ##PESO

# Moran Global
moran_global <- moran.test(internacoes_proj$taxa_incidencia, lw_internacoes)
print(moran_global)

# Moran Local (LISA)
lisa_internacoes <- localmoran(internacoes_proj$taxa_incidencia, lw_internacoes)

# Adicionar resultados ao objeto sf
internacoes_proj$Ii <- lisa_internacoes[, "Ii"]
internacoes_proj$pvalue <- lisa_internacoes[, "Pr(z != E(Ii))"]
save(internacoes_proj, file = "internacoes_proj.RData")

# Correlograma
correlograma <- sp.correlogram(nb_internacoes, internacoes_proj$taxa_incidencia, order = 4, method = "I")
plot(correlograma)

# Classificação dos clusters locais
internacoes_proj <- classificar_clusters(
  internacoes_proj,
  variavel = "taxa_incidencia",
  moran = "Ii",
  pvalor = "pvalue"
)
# Mapa LISA
tmap_mode("plot")
tmap_style("white")

tm_shape(internacoes_proj) +
  tm_fill(
    col = "cluster_type",
    palette = c(
      "Alto-Alto"         = "#E60000",  # Vermelho forte
      "Baixo-Baixo"       = "#0033CC",  # Azul escuro
      "Baixo-Alto"        = "#9999FF",  # Azul claro
      "Alto-Baixo"        = "#FF9999",  # Rosa claro
      "Não significativo" = "#FFFFFF"   # Branco
    ),
    title = "Cluster Local (LISA)",
    legend.is.portrait = TRUE
  ) +
  tm_borders(col = "gray40", lwd = 0.4) +
  tm_layout(
    main.title = "Mapa LISA - Internações por Estado (Taxa/100 mil hab.)",
    main.title.size = 1.2,
    main.title.position = "center",
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "white"
  ) +
  tm_compass(type = "8star", position = c("right", "top"), size = 2) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_credits("CRS: SIRGAS 2000 / UTM Zone 23S", position = c("RIGHT", "BOTTOM"), size = 0.6)


###########################################
## MODELOS GWR ##########
######################################

library(sf)
library(spdep)
library(spatialreg)   
library(spgwr)        
library(tidyverse)
library(dplyr)

#Adicionar quantidade de focos ao shapefile de internações. 

internacoes_proj <- internacoes_proj %>%
  left_join(
    focos_por_estado %>%
      sf::st_drop_geometry() %>%
      dplyr::select(abbrev_state, QtdFocos),
    by = "abbrev_state"
  )

# ------------------------
# Modelo GWR
# ------------------------


# Carregar biblioteca
library(spgwr)


# Calcular banda ideal
gwr_banda <- gwr.sel(taxa_incidencia_z ~ QtdFocos_z,
                     data = internacoes_proj,
                     coords = coord_internacoes,
                     adapt = TRUE)

# Ajustar o modelo GWR
gwr_modelo <- gwr(taxa_incidencia_z~ QtdFocos_z,
                  data = internacoes_proj,
                  coords = coord_internacoes,
                  adapt = gwr_banda,
                  hatmatrix = TRUE,
                  se.fit = TRUE)

gwr_modelo
summary(gwr_modelo)
results <- as.data.frame(gwr_modelo$SDF)
head(results)

internacoes_proj$coef.focos <- results$QtdFocos
internacoes_proj$localR2 <- results$localR2
internacoes_proj$pred.gwr <- results$pred
internacoes_proj$pred.car <- car_modelo$fitted.values

#### Mapa coeficientes:
library(viridis)

map.internacoes <- ggplot(internacoes_proj) +
  geom_sf(aes(fill = coef.focos), color = "black", size = 0.1) +
  viridis::scale_fill_viridis(
    option = "C",         
    name = "Coef. QtdFocos",
    direction = 1,        
    limits = c(0, max(internacoes_proj$coef.focos))
  ) +
  ggtitle("Coeficientes locais da variável QtdFocos") +
  theme_void()


summary(internacoes_proj$coef.focos)
summary(internacoes_proj$pred.car)


# Média e desvio padrão originais
mu <- mean(internacoes_proj$taxa_incidencia, na.rm = TRUE)
sigma <- sd(internacoes_proj$taxa_incidencia, na.rm = TRUE)

# Despadronizando
internacoes_proj$pred.gwr <- results$pred* sigma + mu
internacoes_proj$pred.car <- car_modelo$fitted.values* sigma + mu


# R2 e AIC do GWR
R2_gwr <- modelo_gwr$GW.diagnostic$R2
AIC_gwr <- modelo_gwr$GW.diagnostic$AICc

####Mapas de valores preditos e observados

library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# ----------------------------------------- Taxa Bruta Observada

breaks <- c(0, 40, 70, 100, 200, Inf)
labels <- c("0-40", "40-70", "70-100", "100-200", ">200")

internacoes_proj$brks <- cut(internacoes_proj$taxa_incidencia,
                             breaks = breaks,
                             include.lowest = TRUE,
                             right = TRUE,
                             labels = labels)

tx.bruta <- ggplot(internacoes_proj) +
  geom_sf(aes(fill = brks), color = "black", size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd", name = "Taxa") +
  ggtitle("Taxa Bruta Observada") +
  theme_void()


# ----------------------------------------- Predito - GWR
internacoes_proj$brks.gwr <- cut(internacoes_proj$pred.gwr,
                                 breaks = breaks,
                                 include.lowest = TRUE,
                                 right = TRUE,
                                 labels = labels)

pred.gwr <- ggplot(internacoes_proj) +
  geom_sf(aes(fill = brks.gwr), color = "black", size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd", name = "Taxa") +
  ggtitle("Taxa Predita - GWR") +
  theme_void()
