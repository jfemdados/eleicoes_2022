#Eleicoes 2022 - Mapa por bairro JF
#Autores:  Marcello (Count e Faxina de Dados TSE)
#           Arthur (precisao da Geolocaliza??o das Urnas)


library(tidyverse)

#O Objetivo desse Script ? comparar o voto Luzema x Lukalil

# Importing ---------------------------------------------------------------

vot_sec_2022_br_raw_select <- readr::read_delim("data/tse/data_raw/votacao_secao_2022_BR.csv", 
                                               delim = ";",
                                               escape_double = FALSE,
                                               trim_ws = TRUE,
                                               locale = locale(encoding = "latin1"),
                                               #Colunas "in?teis" que peguei em outros scripts
                                               col_select = !c("DT_GERACAO",
                                                               "HH_GERACAO",
                                                               "CD_TIPO_ELEICAO",
                                                               "NM_TIPO_ELEICAO",
                                                               "TP_ABRANGENCIA",
                                                               "NM_UE",
                                                               "NR_VOTAVEL",
                                                               "SQ_CANDIDATO"
                                                               )
                                               )
#Cuidado que esse arquivo ? grande, coloquei um n_max pra isso.

vot_sec_2022_mg_raw_select <- readr::read_delim("tse_dados_abertos/data_raw/votacao_secao_2022_MG.csv", 
                                               delim = ";",
                                               escape_double = FALSE,
                                               trim_ws = TRUE,
                                               locale = locale(encoding = "latin1"),
                                               #Colunas "in?teis" que peguei antes
                                               col_select = !c("DT_GERACAO",
                                                               "HH_GERACAO",
                                                               "CD_TIPO_ELEICAO",
                                                               "NM_TIPO_ELEICAO",
                                                               "TP_ABRANGENCIA",
                                                               "NM_UE",
                                                               "NR_VOTAVEL",
                                                               "SQ_CANDIDATO"
                                                              )
                                               #,n_max = 100000
)



colnames(vot_sec_2022_br_raw_select)
colnames(vot_sec_2022_mg_raw_select)

#Salvando pq arquivos sao gigantescos

vot_sec_2022_mg_governador <- vot_sec_2022_mg_raw_select %>%
  filter(DS_CARGO == "GOVERNADOR")%>%
  janitor::clean_names()

saveRDS(vot_sec_2022_mg_governador,
        file= "tse_dados_abertos/data_tratada/vot_sec_2022_mg_governador.RDS")


vot_sec_2022_br_presidente_mg <- vot_sec_2022_br_raw_select %>%
  filter(SG_UF == "MG") %>%
  janitor::clean_names()

saveRDS(vot_sec_2022_br_presidente_mg,
        file= "tse_dados_abertos/data_tratada/vot_sec_2022_br_presidente_mg.RDS")


remove(vot_sec_2022_br_raw_select)
remove(vot_sec_2022_mg_raw_select)



# Tidying Pre Join --------------------------------------------------------

vot_sec_2022_br_presidente_mg%>%
  count(nm_municipio)


municip_tse<- vot_sec_2022_mg_governador%>%
              count(nm_municipio)


# Mapas -------------------------------------------------------------------



mapas_minas <- geobr::read_municipality(code_muni = "MG") 

mapas_minas_cor <-mapas_minas %>%
  mutate(nm_municipio= str_to_upper(name_muni))

inner_join(municip_tse, mapas_minas_cor, by= "nm_municipio")
anti_join(municip_tse, mapas_minas_cor, by= "nm_municipio")

#D? pra fazer d?? Mas e Cod_muni_ibge =/= cod_muni_tse
# Esumarizar aquela quantidade de se??es tem uma probabilidade alta de dar um erro.
# Na base Resultados por Munic?pio, que tem na Base dos Dados parece tem sumarizado 
#e j? deve ter tratado
# 
