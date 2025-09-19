
rm(list = ls())


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



#######################################
#### CARREGANDO BANCO DE DADOS #####
#######################################


load("dados.Rdata")

# Lendo os dados extraidos do TerraBrasilis

### LOAD BANCO DE DADOS DATASUS ###

load("dados_sih.RData")

#################################################
######### FORMATAÇÃO REALIZADA NO TERRABRASILIS ##########
###########################################

# Formatando as datas do banco e retirando aqueles registros sem data
dadosBR <- dadosBR %>%
  mutate(Ano = year(ymd_hms(DataHora))) |>  # extrai o ano da coluna DataHora
  mutate(across(where(is.numeric), ~na_if(., -999)))

head(dados_inpe)

##################################################
## Agregando a Freqencia de focos de calor por mes e Estado
##################################################

# Supondo que dadosBR$DataHora esteja no formato POSIXct ou Date

dadosBR_mes <- dadosBR %>%
  mutate(Mes = month(as.Date(DataHora))) %>%
  count(Estado, Municipio, Mes, Ano, name = "Focos_Mes")


### count() é uma função do pacote dplyr que é, na prática, um atalho para um group_by() + summarize(n = n()).

##Transformando os nomes das colunas Estado para as UFs


UFs <- c("ACRE" = "AC",
         "ALAGOAS" = "AL",
         "AMAPÁ" = "AP",
         "AMAZONAS" = "AM",
         "BAHIA" = "BA",
         "CEARÁ" = "CE",
         "DISTRITO FEDERAL" = "DF",
         "ESPÍRITO SANTO" = "ES",
         "GOIÁS" = "GO",
         "MARANHÃO" = "MA",
         "MATO GROSSO" = "MT",
         "MATO GROSSO DO SUL" = "MS",
         "MINAS GERAIS" = "MG",
         "PARÁ" = "PA",
         "PARAÍBA" = "PB",
         "PARANÁ" = "PR",
         "PERNAMBUCO" = "PE",
         "PIAUÍ" = "PI",
         "RIO DE JANEIRO" = "RJ",
         "RIO GRANDE DO NORTE" = "RN",
         "RIO GRANDE DO SUL" = "RS",
         "RONDÔNIA" = "RO",
         "RORAIMA" = "RR",
         "SANTA CATARINA" = "SC",
         "SÃO PAULO" = "SP",
         "SERGIPE" = "SE",
         "TOCANTINS" = "TO")

dadosBR_mes <- dadosBR_mes %>%
  mutate(Estado = recode(Estado, !!!UFs))


####### Padronizando as datas de acordo com o banco de dados do datasus. 

class(dados_sih$MES_CMPT)
class(dadosBR_mes$MES_CMPT)

dadosBR_mes <- dadosBR_mes %>%
  mutate(
    MES_CMPT = as.character(sprintf("%02d", month(Mes))), 
    ANO_CMPT = as.character(year(Mes))
  ) %>%
  dplyr::select(-Mes)

##Analisando os biomas: 

biomas_por_estado <- dadosBR %>%
  group_by(Estado) %>%
  summarise(Biomas = paste(unique(Bioma), collapse = ", ")) %>%
  arrange(Estado)

biomas_por_estado <- biomas_por_estado %>%
  mutate(Estado = recode(Estado, !!!UFs))

dadosBR_mes <- dadosBR_mes %>%
  left_join(biomas_por_estado, by = "Estado")

############################ Salvo até aqui dadosBR_mes ######################

save(dadosBR_mes, file = "dadosBR_mes.RData")

######### INCENDIOS POR MES/ANO, BIOMA E ESTADO: 
library(dplyr)
library(lubridate)

dados_biomas <- dadosBR %>%
  mutate(
    data = as.Date(DataHora),
    MES_CMPT = sprintf("%02d", month(data)),   # "01" a "12" como character
    ANO_CMPT = as.character(year(data))        # ano como character
  ) %>%
  count(Estado, Bioma, MES_CMPT, ANO_CMPT, name = "QtdFocos")


dados_biomas <- dados_biomas %>%
  mutate(Estado = recode(Estado, !!!UFs))


save(dados_biomas, file = "dados_biomas.RData")


#########################################
###### BANCO DE DADOS DATASUS ########## 
###########################################


# DEFININDO UM VETOR DE VARIÁVEIS
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

ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
         "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
         "RO", "RR", "RS", "SC", "SE", "SP", "TO")


### DEVIDO AO ALTO CUSTO DE PROCESSAMENTO PARA BAIXAR TODOS OS DADOS
### UTILIZAR DE 2022 A 2024. 

#########################################################
############ FUNÇÃO DOWNLOAD DATASUS #####################
########################################################

#RR <- fetch_datasus(year_start = 2022, year_end = 2024, 
# month_start = 1, month_end = 12, 
# uf = "RR", vars = vars_select,
#  information_system = "SIH-RD") %>%
# filter(DIAG_PRINC %in% cids_select)

#save(RR, file = "RR.RData")



############################################################
########### LOOPING PARA DOWNLOAD DE DADOS DO DATASUS ####
#######################################################

################
##A função map_dfr pertencente a biblioteca purr é utilizada para aplicar
## uma função a cada elemento da lista ou vetor, e posteriormente unir tudo
## em um dataframe. 

#dados_sih <- map_dfr(ufs, function(uf_sigla) {
# message("Baixando UF: ", uf_sigla)
#tryCatch({
# fetch_datasus(
#  year_start = 2022, year_end = 2024,
# month_start = 1, month_end = 12,
#uf = uf_sigla,
#vars = vars_select,
#information_system = "SIH-RD"
#) %>%
#filter(DIAG_PRINC %in% cids_select) %>%
#mutate(UF = uf_sigla)  # Adiciona coluna com UF
# }, error = function(e) {
# message("Erro ao baixar dados da UF ", uf_sigla, ": ", e$message)
# NULL
# })
#})



##Lendo todos os arquivos R.data para engloba-los no arquivo 
## unificado de dados SIH

#for (uf in ufs) {
#arquivo <- paste0(uf, ".RData")
#if (file.exists(arquivo)) {
load(arquivo)
message("Carregado: ", arquivo)
} else {
  warning("Arquivo não encontrado: ", arquivo)
}
#}


#save(dados_sih, file = "dados_sih.RData")

load("dados_sih.RData")

glimpse(dados_sih)
table(dados_sih$uf)

summary(dados_sih)


##########################################################

## Agora temos o banco dedados dados_sih que contêm as colunas
## "uf", "MES_CMPT" e "ANO_CMPT" em que cada linha representa uma
## internação (com seu número de dias internados também tabelado).
## Da mesma forma que contamos os focos por mes, também iremos contar
## os casos de internação por mes associado a cada uf. 


sih_mes <- dados_sih %>%
  count(uf, MES_CMPT, ANO_CMPT, name = "Internações")


#### ADICIONANDO TAMBÉM A QUANTIDADE DE DIAS DE INTERNAÇÃO
#### E O VALOR TOTAL GASTO: 

somatorio_sih <- dados_sih %>%
  group_by(uf, MES_CMPT, ANO_CMPT) %>%
  summarise(
    QT_DIARIAS = sum(QT_DIARIAS, na.rm = TRUE),
    VAL_TOT = sum(VAL_TOT, na.rm = TRUE),
    .groups = "drop"
  )

sih_mes <- left_join(sih_mes, somatorio_sih, by = c("uf", "MES_CMPT", "ANO_CMPT"))

#### A partir daqui os bancos de dados do datasus e terrabrasilis 
#### estão prontos para serem utilizados juntos no trabalho. 

#save(sih_mes, file = "sih_mes.RData")

##############################################################
## Após carregar os R.Data do ínicio, rodar a partir daqui ###
###############################################################

##########################################################
# DEFININDO AMOSTRAGEM PARA TODO BD: 01-01-2022 A 31/12/2024 # 
##########################################################

sih_mes <- sih_mes %>%
  filter(
    ANO_CMPT >= 2022,
    ANO_CMPT <= 2024
  )

dadosBR_mes <- dadosBR_mes %>%
  filter(
    ANO_CMPT >= 2022,
    ANO_CMPT <= 2024
  )

dados_biomas <- dados_biomas %>%
  filter(
    ANO_CMPT >= 2022,
    ANO_CMPT <= 2024
  )

dadosBR <- dadosBR %>%
  mutate(
    DataHora = ymd_hms(DataHora),              
    Ano = year(DataHora),                      
    Mes = month(DataHora),                     
    Dia = day(DataHora),                       
    Data = as_date(DataHora)                   
  )

dadosBR <- dadosBR %>%
  filter(
    Ano >= 2022,
    Ano <= 2024
  )


save(sih_mes, file = "sih_mes.RData")

##########################################################
# ANÁLISE EXPLORATÓRIA DOS DADOS 
##########################################################

summary(sih_mes)
summary(dadosBR_mes)
par(mfrow = c(1, 2))
hist(sih_mes$Internações)
hist(dadosBR_mes$QtdFocos)

par(mfrow = c(1, 2))
boxplot(dadosBR_mes$QtdFocos)
boxplot(sih_mes$Internações)

par(mfrow = c(1, 1))

boxplot(QtdFocos ~ MES_CMPT, 
        data = dadosBR_mes,
        main = "Distribuição de Focos por Mês",
        xlab = "Mês",
        ylab = "Qtd de Focos",
        col = "skyblue")

boxplot(QtdFocos ~ ANO_CMPT, 
        data = dadosBR_mes,
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


###############################################################
######## AGREGANDO DADOS ESPACIAIS #######################
############################################################

### Shapefile biomas disponível em 
# https://www.ibge.gov.br/geociencias/informacoes-ambientais/vegetacao/15842-biomas.html

## Também pode ser baixado por meio do geo_br
#Shapes:
biomas <- read_biomes()
estados <- read_state()
municipios <- read_municipality()
st_crs(biomas)
st_crs(estados)
st_crs(municipios)

save(estados, file = "shape_estados.RData")


ggplot(municipios) +
  geom_sf(fill = "lightgreen", color = "gray40") +
  labs(title = "Estados do Brasil - IBGE")
ggplot(biomas) +
  geom_sf(aes(fill = name_biome)) +
  labs(title = "Biomas do Brasil (IBGE via geobr)")

########### DADOS SIH ASSOCIADOS AOS BIOMAS - ERRO ##############
##Tirar ultimo digito codigo municipal (ler e-book datasus)

###Tentando atribuir coordenada entre as contagens, para associa-las a um
###bioma ---- APRESENTOU ERROS DE GEOMETRIA. 

#sih_muni <- dados_sih %>%
# filter(ANO_CMPT >= 2022, ANO_CMPT <= 2024) %>%
#group_by(uf, MUNIC_RES, MES_CMPT, ANO_CMPT) %>%
#summarise(
# Internacoes = n(),
# .groups = "drop"
#)

##Centroides municipios

#library(dplyr)

#municipios <- municipios %>%
#st_make_valid() 

#centroide_mun <- municipios %>%
#mutate(geom = st_centroid(geom)) %>%
# dplyr::select(code_muni, geom)

#mun_coords <- centroide_mun %>%
#mutate(
# lon = st_coordinates(geom)[, 1],
#lat = st_coordinates(geom)[, 2]
#  ) %>%
#st_drop_geometry()

## Extraindo o último dígito do código de múnicipio
## de acordo com o e-book do datasus

#mun_coords <- mun_coords %>%
# mutate(code_muni = substr(code_muni, 1, nchar(code_muni) - 1))

### Juntando:

#sih_muni <- sih_muni %>%
# left_join(
#  dplyr::select(mun_coords, code_muni, lat, lon), 
# by = c("MUNIC_RES" = "code_muni")
# )




### MAPA MOSTRANDO OS BIOMAS EM CADA ESTADO #######

# CENTRÓIDE PARA ADIÇÃO DE RÓTULOS: 
estados_centroides <- estados %>%
  mutate(centro = st_point_on_surface(geom)) %>%
  cbind(st_coordinates(.$centro))

save(estados_centroides, file = "estados_centroides.RData")


# Mapa com rótulos
library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(grid) 
#install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)
wes_palette("BottleRocket1")   

##### CRIANDO UM MAPA TEMÁTICO DE FOCOS POR BIOMA: #### 

library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(viridis)

# Calcular os centroides dos biomas para criar simbolos proporcionais. 

focos_bioma <- dados_biomas %>%
  group_by(Bioma) %>%
  summarise(QtdFocos = sum(QtdFocos, na.rm = TRUE)) %>%
  ungroup() %>%
  { left_join(biomas, ., by = c("name_biome" = "Bioma")) }

#focos_bioma <- biomas %>%
# left_join(dados_biomas, by = c("name_biome" = "Bioma"))

biomas_centroides <- focos_bioma %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(focos_bioma %>% st_drop_geometry() %>% dplyr::select(name_biome, QtdFocos))


save(biomas_centroides, file = "biomas_centroides.RData")

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



############## MAPA DE BIOMAS ######

mapa_biomas <- ggplot() +
  #Preenchimento dos Biomas
  geom_sf(data = biomas, aes(fill = name_biome), color = NA, alpha = 0.6) +
  #Contorno dos Estados
  geom_sf(data = estados, fill = NA, color = "black", size = 0.4) +
  geom_text(data = estados_centroides,
            aes(x = X, y = Y, label = abbrev_state),
            size = 3, fontface = "bold", color = "black") +
  # Paleta e legenda
  scale_fill_brewer(palette = "Set3", name = "Biomas") +
  # Título e legenda
  labs(title = "Biomas em Estados Brasileiros",
       caption = "Fonte: IBGE") +
  # seta do norte
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  #Escala:
  annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
  theme_minimal() +
  ### Definindo posição da legenda, titúlo e caption:
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  
    legend.position = "right", 
    plot.caption = element_text(hjust = 0.5)
  ) + 
  #Grid de coordenadas, ggplot sempre lat e long.
  coord_sf() + 
  # Caixa de texto com o código EPSG 
  annotation_custom(
    grob = grid::textGrob("CRS: SIRGAS 2000",
                          x = unit(0.95, "npc"),  
                          y = unit(0.1, "npc"), 
                          just = c("right", "bottom"),
                          gp = grid::gpar(fontsize = 9, fontface = "italic")))


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





############ TENTATIVA ATRIBUIR UM BIOMA AO BANCO DE DADOS SIH ###########

##### CRIANDO SHAPEFILE UTILIZANDO O OBJETO cod_municipais
##### COM A FINALIDADE DE ATRIBUIR UM BIOMA AO BANCO DE DADOS SIH
#### A IDEIA É CRUZAR AS COORDENADAS MUNICIPAIS COM AS COORDENADAS
#### DOS BIOMAS FORNECIDOS PELO IBGE. 


####BANCO DE DADOS APRESENTOU VALORES DE LAT
### E LONG VAZIOS, PROVAVELMENTE POR FALTA DE CÓDIGO 
##### EQUIVALENTE 


#sum(is.na(cod_municipais$lat))
#sum(is.na(cod_municipais$lon))
#sih_muni %>% filter(is.na(lat) | is.na(lon))

## LOCALIZAR CÓDIGO SIMILAR PARA ATRIBUIR COORDENADA. 

# Filtra só os códigos de SC e MS
#codigos_ms <- cod_municipais %>%
# filter(uf %in% c("MS")) %>%
#dplyr::select(uf, MUNIC_RES) %>%
#distinct()

##Substituir SC 422000 POR SC 421985 e SC 421265 POR 421270
### MS : 500627 POR 500625 ---- similaridade entre códigos vide ebook datasus

######### CÓDIGO PARA ADICIONAR COORDENADAS FALTANTES AOS MUNICIPIOS
######### COM CÓDIGO INCONGRUENTE DE ACORDO COM O DATASUS E IBGE. 

#library(dplyr)

#sih_muni <- sih_muni %>%
#mutate(
# lat = case_when(
# uf == "SC" & MUNIC_RES == "422000" ~ lat[uf == "SC" & MUNIC_RES == "421985"][1] + 0.033333,
# uf == "SC" & MUNIC_RES == "421265" ~ lat[uf == "SC" & MUNIC_RES == "421270"][1] + 0.033333,
# uf == "MS" & MUNIC_RES == "500627" ~ lat[uf == "MS" & MUNIC_RES == "500625"][1] + 0.033333,
#TRUE ~ lat
# ),
# lon = case_when(
#  uf == "SC" & MUNIC_RES == "422000" ~ lon[uf == "SC" & MUNIC_RES == "421985"][1] + 0.033333,
# uf == "SC" & MUNIC_RES == "421265" ~ lon[uf == "SC" & MUNIC_RES == "421270"][1] + 0.033333,
# uf == "MS" & MUNIC_RES == "500627" ~ lon[uf == "MS" & MUNIC_RES == "500625"][1] + 0.033333,
#TRUE ~ lon
#)
#)

###############################################################
################# MERGE - ERRO ###############################
##############################################################

#pontos_sf <- st_as_sf(sih_muni, coords = c("lon", "lat"), crs = 4674)

#cod_municipais_com_bioma <- st_join(pontos_sf, biomas, join = st_intersects)

#cod_municipais_com_bioma <- st_join(pontos_sf, biomas, join = st_intersects, left = FALSE) %>%
#group_by(uf, MUNIC_RES, MES_CMPT, ANO_CMPT, QT_DIARIAS, VAL_TOT) %>%
#slice(1) %>%  
#ungroup()


#save(cod_municipais_com_bioma, file = "cod_municipais_com_bioma.Shapefile")
#cod_municipais_com_bioma <- cod_municipais_com_bioma %>%
#dplyr::select(-year)

#cod_municipais %>% filter(is.na(lat) | is.na(lon))
#save(cod_municipais, file = "cod_municipais.RData")






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

mapa_focos_pontuais <- ggplot() +
  # Preenchimento dos Biomas
  geom_sf(data = biomas, aes(fill = name_biome), color = NA, alpha = 0.6) +
  
  # Contorno dos Estados
  geom_sf(data = estados, fill = NA, color = "black", size = 0.4) +
  
  # Pontos dos Focos (camada adicionada)
  geom_sf(data = pontos_sf, color = "red", size = 0.5, alpha = 0.6) +
  
  # Rótulos dos Estados (centróides)
  geom_text(data = estados_centroides,
            aes(x = X, y = Y, label = abbrev_state),
            size = 3, fontface = "bold", color = "black") +
  
  # Paleta e legenda
  scale_fill_brewer(palette = "Set3", name = "Biomas") +
  
  # Título e legenda
  labs(title = "Biomas, Estados e Focos de Calor (Pontos)",
       caption = "Fonte: IBGE / Dados de Satélite") +
  
  # Seta do Norte
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
  
  # Escala
  annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
  
  theme_minimal() +
  
  # Estilo dos textos e posição da legenda
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  
    legend.position = "right", 
    plot.caption = element_text(hjust = 0.5)
  ) + 
  
  # Grid e projeção
  coord_sf() + 
  
  # Texto do EPSG
  annotation_custom(
    grob = grid::textGrob("CRS: SIRGAS 2000",
                          x = unit(0.95, "npc"),  
                          y = unit(0.1, "npc"), 
                          just = c("right", "bottom"),
                          gp = grid::gpar(fontsize = 9, fontface = "italic"))
  )


########################################################
###### MORAN GLOBAL, LOCAL E LISAMAP FOCOS DE INCENDIO ############
########################################################

#Remover o Bioma Costeiro pois não há observações para ele. 

library(dplyr)

focos_bioma <- focos_bioma %>%
  filter(name_biome != "Sistema Costeiro")

save(focos_bioma, file = "focos_bioma.RData")


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


## ===============================
## MATRIZ DE VIZINHANÇA - BIOMAS
## ===============================

library(sf)
library(spdep)
library(ggplot2)

#focos_bioma para CRS projetado (SIRGAS 2000 / UTM zone 23S)
focos_bioma_proj <- st_transform(focos_bioma, 31983)
save(focos_proj, file = "focos_proj.RData")

#centroides no CRS projetado
coord_biomas_proj <- as.matrix(st_coordinates(st_centroid(focos_bioma_proj)))

#vizinhança entre os biomas
nb_bioma <- poly2nb(focos_bioma_proj)
#pesos espaciais normalizados
lw_bioma <- nb2listw(nb_bioma, style = "W")

#linhas de vizinhança como objeto sf
nb_bioma.sf <- as(nb2lines(nb_bioma, coords = coord_biomas_proj), "sf")
nb_bioma.sf <- st_set_crs(nb_bioma.sf, st_crs(focos_bioma_proj))

#Mapa de vizinhança por bioma
ggplot() +
  geom_sf(data = focos_bioma_proj, fill = "lightpink", color = "white") +
  geom_sf(data = nb_bioma.sf, color = "blue", size = 1.2) +
  labs(
    title = "Vizinhança por Contiguidade - Biomas",
    x = "Longitude", y = "Latitude"
  ) +
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

# LISA para Biomas
focos_bioma_proj$Ii <- lisa_bioma[, "Ii"]
focos_bioma_proj$pvalue <- lisa_bioma[, "Pr(z != E(Ii))"]

save(internacoes_proj, file = "internacoes_proj.RData")

# Correlograma (Estados)
correl_estado <- sp.correlogram(nb_focos_estados, focos_proj$QtdFocos, order = 4, method = "I")
print(correl_estado)
plot(correl_estado)

# Correlograma (Biomas)
correl_biomas <- sp.correlogram(nb_bioma, focos_bioma_proj$QtdFocos, order = 2, method = "I")
print(correl_biomas)
plot(correl_biomas)

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


## ===============================
## LISAMAP FOCOS POR BIOMA: 
## ===============================

focos_bioma_proj <- classificar_clusters(focos_bioma_proj, variavel = "QtdFocos", moran = "Ii", pvalor = "pvalue")
save(focos_bioma_proj, file = "focos_bioma_proj.RData")


tm_shape(focos_bioma_proj) +
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
    main.title = "Mapa LISA - Clusters Locais de Focos por Bioma",
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

############################# LISAMAP SEM TAXA 

# MORAN LOCAL para INTERNAÇÕES
lisa_internacoes2 <- localmoran(internacoes_proj$Internações, lw_internacoes)


internacoes_proj$Ii_sem_taxa <- lisa_internacoes2[, "Ii"]
internacoes_proj$pvalue_sem_taxa <- lisa_internacoes2[, "Pr(z != E(Ii))"]

internacoes_proj <- classificar_clusters2(
  internacoes_proj,
  variavel = "Internações",
  moran = "Ii_sem_taxa",
  pvalor = "pvalue_sem_taxa"
)


# LISAMAP para INTERNAÇÕES
tm_shape(internacoes_proj) +
  tm_fill(
    col = "cluster_type_2",
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
    main.title = "LISAMAP - Internações por Estado",
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
## MODELOS CAR E GWR ##########
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

#MODELO CAR

# Padronizar as variáveis para ajustar a escala
internacoes_proj <- internacoes_proj %>%
  mutate(
    taxa_incidencia_z = scale(taxa_incidencia),
    QtdFocos_z = scale(QtdFocos)
  )

saveRDS(car_modelo, file = "modelo_car.rds")
save(internacoes_proj, file = "internacoes_proj.RData")



# Rodar o modelo SAR de erros com variáveis padronizadas
car_modelo <- errorsarlm(taxa_incidencia_z ~ QtdFocos_z,
                         data = internacoes_proj,
                         listw = lw_internacoes)

# Mostrar resumo do modelo
summary(car_modelo)




AIC_car <- AIC(car_modelo)
R2_car <- car_modelo$fit$R2

##### MODELO LINEAR AIC MENOR, LR test value: 0.27546 e Lambda: 0.14558
### indicando fraca correlação espacial, dando preferência ao LM. 
## Variavel foco não foi significativa


##Checagem de resíduos: 
car_modelo$carresid <- residuals(car_modelo)
moran.test(car_modelo$carresid, lw_internacoes)

#h0: ausencia de autocorrelação nos resíduos

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

# ----------------------------------------- Predito - CAR
internacoes_proj$brks.car <- cut(internacoes_proj$pred.car,
                                 breaks = breaks,
                                 include.lowest = TRUE,
                                 right = TRUE,
                                 labels = labels)

pred.car <- ggplot(internacoes_proj) +
  geom_sf(aes(fill = brks.car), color = "black", size = 0.1) +
  scale_fill_brewer(palette = "YlOrRd", name = "Taxa") +
  ggtitle("Taxa Predita - CAR") +
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

# ----------------------------------------- Mostrar mapas lado a lado
grid.arrange(tx.bruta, pred.car, pred.gwr, ncol = 2)



#install.packages("shinyjs")

#install.packages('rsconnect')