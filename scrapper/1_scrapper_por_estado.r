library(tidyverse)
library(jsonlite)
urls <- c(
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/ac/ac-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/al/al-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/am/am-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/ap/ap-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/ba/ba-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/ce/ce-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/df/df-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/es/es-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/go/go-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/ma/ma-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/mg/mg-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/ms/ms-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/mt/mt-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/pa/pa-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/pb/pb-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/pe/pe-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/pi/pi-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/pr/pr-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/rj/rj-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/ro/ro-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/rr/rr-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/rn/rn-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/rs/rs-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/sc/sc-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/se/se-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/sp/sp-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/to/to-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/zz/zz-c0001-e000545-r.json',
'https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/br/br-c0001-e000545-r.json')


apura <- function(url){
	aux <-  fromJSON(url, simplifyDataFrame = TRUE) %>%
	.[["cand"]] |>
	select('nm','vap','pvap')|>
	mutate(tempo=substr(Sys.time(),12,19),uf=substr(url,74,75))|>
	as_tibble()
	return(aux)
}

base <- map_dfr(urls,apura)
write_csv(base,file='apuracao.csv',append=TRUE)
base[57:58,]


