
# Código lá de 2021 sobre mapas -------------------------------------------------------


# CRIANDO GEOMS_MG - POLIGONOS DO MAPA DE MG     
library(readxl)
Base_informacoes_setores2010_sinopse_MG <- read_excel("~/R2/eleicoes_jf/base_ibge/Base_informacoes_setores2010_sinopse_MG.xls")


library("bigrquery")
billing_project_id = "covid-308601"
# Para baixar a tabela inteira
query = "SELECT * FROM `basedosdados.br_ibge_populacao.municipios`"
d <- bq_table_download(bq_project_query(billing_project_id, query), page_size=500, bigint="integer64")

pop<- d%>%
  filter(ano==2020)%>%
  mutate(id_municipio= as.numeric(id_municipio))

base_ibge_mg_antiga <-  Base_informacoes_setores2010_sinopse_MG%>%
  select(Cod_setor:Nome_do_bairro)%>%
  filter(Nome_da_UF== "Minas Gerais")%>%
  group_by(Nome_da_meso, Cod_meso, Nome_da_micro, Cod_micro, Cod_municipio)%>%
  count(Nome_do_municipio)%>%
  rename("code_muni"= Cod_municipio)%>%
  mutate(code_muni= as.numeric(code_muni))%>%
  inner_join(pop, by= c("code_muni" = "id_municipio"))

#Fazendo Polígonos pelo Geobr::
library(ggplot2)
library(geobr)
geoms_mg<- geobr::read_municipality(code_muni = "MG")%>%
  inner_join(base_ibge_mg, by= "code_muni")%>%
  sf::st_simplify(dTolerance = 0.005)%>%
  mutate(name_muni= str_to_title(abjutils::rm_accent(name_muni)))






ggplot() + geom_sf(data=geoms_mg , size= .15, 
                   show.legend = F) + geom_sf(data=geoms_mg, aes(fill= Nome_da_meso))

#Mapa Vacinação
library(ggplot2)
library(readr)


mapa_vacina<- geoms_mg%>%
  full_join(select(doses_totais_completo, DosesAplicadas, DosesDisponibilizadas, Eficiencia, Município, EficienciaGroups),
            by= c("name_muni" = "Município"))%>%
  tidyr::drop_na(code_muni)




mapa_vacina%>%
  filter(Nome_da_meso== "Zona da Mata")%>%
  ggplot() + geom_sf(aes( fill= EficienciaGroups), colour= "gray20", size= .15,  show.legend = TRUE) +
  theme_void()
#scale_fill_brewer()
scale_fill_manual( name= "Proporção Vacinas:\nAplicadas/Disponibilizadas",
                   label= c("Menos de 20%", "20% a 40%","40% a 60%","60% a 80%","80% a 100%", "Acima de 100%\nErros nos Dados"),
                   values= c("red2","red4", "yellow", "limegreen", "green4", "gray30"))
#labs(title = "Eficiência na Vacinação: Os Municípios estão Aplicando as Vacinas que Recebem?", 
#     subtitle= "Nº de Vacinas enviadas aos Municípios dividido Pelas nº de Vacinas Aplicadas")





todos_estados_shape_file <- geobr::read_state()

todos_estados_shape_file %>%
  ggplot() + geom_sf( fill= "gray", colour= "gray30", size= .15,  show.legend = TRUE) +
  theme_void()
