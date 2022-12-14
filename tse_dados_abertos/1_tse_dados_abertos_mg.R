#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)
#           Arthur (precisao da Geolocaliza??o das Urnas)


library(tidyverse)



# Importing ---------------------------------------------------------------


# 1) DETALHES da Vota??o por Se??o -------------------------------

# Esse data frame n?o mostra os votos em cada candidato por Se??o.
# Somente os votos totais de cada urna"
# Ele vai dar a quantidade de vai ter s? ter? no final as seguintes colunas no final:
"    QT_APTOS = col_double()"
"..  QT_COMPARECIMENTO = col_double(),"
"..   QT_ABSTENCOES = col_double(),"
"..   QT_VOTOS_NOMINAIS = col_double(),"
"..   QT_VOTOS_BRANCOS = col_double(),"
"..   QT_VOTOS_NULOS = col_double(),"
"..   QT_VOTOS_LEGENDA = col_double(),"
"..   QT_VOTOS_ANULADOS_APU_SEP = col_double(),"
"..   NR_LOCAL_VOTACAO= = col_double()"

#NEM RODE para n?o gastar RAM ? toa (MEU R CRASHOU)
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



# 2) VOTA??O POR SE??O -----------------------------------

#Esse Data Frame parece ter a Vota??o em cada Candidato por Urna

#Cuidado que esse arquivo ? grande, coloquei um n_max pra isso.

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


# Porra PQP parece que s? tem das estaduais
# N?o das Elei??es Federais


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

#### ABRI OUTRO SCRIPT S? PARA BR (votos Presidenciais)


# Visualiza??o ------------------------------------------------------------

votacao_secao_2022_jf %>%
   select( ds_eleicao,
           nm_municipio, nr_zona, nr_secao,
           ds_cargo, nm_votavel, qt_votos, nr_local_votacao) #%>% view()

