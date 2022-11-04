#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)
#           Arthur (precisao da Geolocalização das Urnas)


library(tidyverse)



# Importing ---------------------------------------------------------------


# 1) DETALHES da Votação por Seção -------------------------------

# Esse data frame não mostra os votos em cada candidato por Seção.
# Somente os votos totais de cada urna"
# Ele vai dar a quantidade de vai ter só terá no final as seguintes colunas no final:
"    QT_APTOS = col_double()"
"..  QT_COMPARECIMENTO = col_double(),"
"..   QT_ABSTENCOES = col_double(),"
"..   QT_VOTOS_NOMINAIS = col_double(),"
"..   QT_VOTOS_BRANCOS = col_double(),"
"..   QT_VOTOS_NULOS = col_double(),"
"..   QT_VOTOS_LEGENDA = col_double(),"
"..   QT_VOTOS_ANULADOS_APU_SEP = col_double(),"
"..   NR_LOCAL_VOTACAO= = col_double()"

#NEM RODE para não gastar RAM à toa (MEU R CRASHOU)
#det_votacao_secao_2022_mg_raw <- readr::read_delim("tse_dados_abertos/data_raw/detalhe_votacao_secao_2022_MG.csv", 
  #                                     delim = ";",
  #                                    escape_double = FALSE,
  #                                    trim_ws = TRUE,
  #                                      locale = locale(encoding = "latin1")
  #                                      )
#

#det_votacao_secao_2022_jf <- det_votacao_secao_2022_mg_raw %>%
##                           janitor::clean_names() %>%
#                         filter(str_detect( nm_municipio, "JUIZ DE FORA"))



# 2) VOTAÇÃO POR SEÇÃO -----------------------------------

#Esse Data Frame parece ter a Votação em cada Candidato por Urna

#Cuidado que esse arquivo é grande, coloquei um n_max pra isso.

votacao_secao_2022_mg_raw <- readr::read_delim("tse_dados_abertos/data_raw/votacao_secao_2022_MG.csv", 
                                            delim = ";",
                                            escape_double = FALSE,
                                            trim_ws = TRUE,
                                            locale = locale(encoding = "latin1")
                                            #,n_max = 100000
                                        )




# Filtrando pra JF

votacao_secao_2022_jf<- votacao_secao_2022_mg_raw %>%
  janitor::clean_names() %>%
  filter(str_detect( nm_municipio, "JUIZ DE FORA")) %>%
    dplyr::select(!c(dt_geracao,
                     hh_geracao,
                     cd_tipo_eleicao,
                     nm_tipo_eleicao,
                     cd_eleicao,
                     sg_ue,
                     nm_ue
                     )
                )


# Comparando Bases

as_tibble(votacao_secao_2022_mg_raw)

as_tibble(votacao_secao_2022_jf)

#comparando bases- Counts

votacao_secao_2022_jf %>%
  count(ds_cargo)


votacao_secao_2022_mg_raw %>%
  count(TP_ABRANGENCIA)


# Porra PQP parece que só tem das estaduais
# Não das Eleições Federais


votacao_secao_2022_jf %>%
  filter(ds_cargo == "GOVERNADOR") %>% 
  dplyr::group_by(nm_votavel) %>%
    dplyr::tally(qt_votos, sort = TRUE)

votacao_secao_2022_jf %>% 
  filter(ds_cargo == "GOVERNADOR") %>% 
  count(sq_candidato, nm_votavel)
#colunas com indos Iguais


votacao_secao_2022_jf %>% 
  filter(ds_cargo == "GOVERNADOR") %>%
  count(nr_secao)

votacao_secao_2022_jf %>%
  group_by(nr_zona) %>%
  count(nr_secao)

#### ABRI OUTRO SCRIPT SÓ PARA BR (votos Presidenciais)


# Visualização ------------------------------------------------------------

votacao_secao_2022_jf %>%
   select( ds_eleicao,
           nm_municipio, nr_zona, nr_secao,
           ds_cargo, nm_votavel, qt_votos, nr_local_votacao) #%>% view()

