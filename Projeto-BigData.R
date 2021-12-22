#Instalando pacotes
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggplot")

# usado para fazer a leitura de dados csv e txt 
suppressMessages(require(readr))

# URL do arquivo csv
url_csv <- "http://www.geoservicos.ibge.gov.br/geoserver/wms?service=WFS&version=1.0.0&request=GetFeature&typeName=CGEO:vw_razao_medicos_1000_hab&outputFormat=CSV"

# Leitura do arquivo csv deixando a função definir o tipo de cada coluna
medicos <- readr::read_csv(url_csv)

medicos

#Função de arredondamento
arredondamento <- function(x){
  floor(100*x)/100
}

#-------------- ACRE ----------------------------------------
acre <- medicos[grep("ACRE", medicos$UF),]
ac_media <- sum(acre$razao_medicos_1000_hab) / length(acre$razao_medicos_1000_hab)

ac_media = arredondamento(ac_media)

razao_menor_ac <- min(acre$razao_medicos_1000_hab)

lugares_menor_ac <- acre[acre$razao_medicos_1000_hab == razao_menor_ac,]
cidade_menor_ac <- lugares_menor_ac[lugares_menor_ac$num_medicos == min(lugares_menor_ac$num_medicos),]
cidade_menor_ac <- cidade_menor_ac$nome


razao_maior_ac <- max(acre$razao_medicos_1000_hab)

lugares_maior_ac <- acre[acre$razao_medicos_1000_hab == razao_maior_ac,]
cidade_maior_ac <- lugares_maior_ac[lugares_maior_ac$num_medicos == min(lugares_maior_ac$num_medicos),]
cidade_maior_ac <- cidade_maior_ac$nome


capital_ac <- acre[acre$nome == "Rio Branco",]

razao_capital_ac <- capital_ac$razao_medicos_1000_hab

cidade_capital_ac <- capital_ac$nome


razao_menor_ac
cidade_menor_ac
razao_maior_ac
cidade_maior_ac
razao_capital_ac
cidade_capital_ac




#-------------- ALAGOAS ----------------------------------------
alagoas <- medicos[grep("ALAGOAS", medicos$UF),]
al_media <- sum(alagoas$razao_medicos_1000_hab) / length(alagoas$razao_medicos_1000_hab)

razao_menor_al <- min(alagoas$razao_medicos_1000_hab)

lugares_menor_al <- alagoas[alagoas$razao_medicos_1000_hab == razao_menor_al,]
cidade_menor_al <- lugares_menor_al[lugares_menor_al$num_medicos == min(lugares_menor_al$num_medicos),]
cidade_menor_al <- cidade_menor_al$nome


razao_maior_al <- max(alagoas$razao_medicos_1000_hab)

lugares_maior_al <- alagoas[alagoas$razao_medicos_1000_hab == razao_maior_al,]
cidade_maior_al <- lugares_maior_al[lugares_maior_al$num_medicos == min(lugares_maior_al$num_medicos),]
cidade_maior_al <- cidade_maior_al$nome


capital_al <- alagoas[alagoas$nome == "Maceió",]

razao_capital_al <- capital_al$razao_medicos_1000_hab

cidade_capital_al <- capital_al$nome


razao_menor_al
cidade_menor_al
razao_maior_al
cidade_maior_al
razao_capital_al
cidade_capital_al


#-------------- AMAPÁ ----------------------------------------
amapa <- medicos[grep("AMAPÁ", medicos$UF),]
ap_media <- sum(amapa$razao_medicos_1000_hab) / length(amapa$razao_medicos_1000_hab)

razao_menor_ap <- min(amapa$razao_medicos_1000_hab)

lugares_menor_ap <- amapa[amapa$razao_medicos_1000_hab == razao_menor_ap,]
cidade_menor_ap <- lugares_menor_ap[lugares_menor_ap$num_medicos == min(lugares_menor_ap$num_medicos),]
cidade_menor_ap <- cidade_menor_ap$nome


razao_maior_ap <- max(amapa$razao_medicos_1000_hab)

lugares_maior_ap <- amapa[amapa$razao_medicos_1000_hab == razao_maior_ap,]
cidade_maior_ap <- lugares_maior_ap[lugares_maior_ap$num_medicos == min(lugares_maior_ap$num_medicos),]
cidade_maior_ap <- cidade_maior_ap$nome


capital_ap <- amapa[amapa$nome == "Macapá",]

razao_capital_ap <- capital_ap$razao_medicos_1000_hab

cidade_capital_ap <- capital_ap$nome


razao_menor_ap
cidade_menor_ap
razao_maior_ap
cidade_maior_ap
razao_capital_ap
cidade_capital_ap


#-------------- AMAZONAS ----------------------------------------
amazonas <- medicos[grep("AMAZONAS", medicos$UF),]
am_media <- sum(amazonas$razao_medicos_1000_hab) / length(amazonas$razao_medicos_1000_hab)

razao_menor_am <- min(amazonas$razao_medicos_1000_hab)

lugares_menor_am <- amazonas[amazonas$razao_medicos_1000_hab == razao_menor_am,]
cidade_menor_am <- lugares_menor_am[lugares_menor_am$num_medicos == min(lugares_menor_am$num_medicos),]
cidade_menor_am <- cidade_menor_am$nome


razao_maior_am <- max(amazonas$razao_medicos_1000_hab)

lugares_maior_am <- amazonas[amazonas$razao_medicos_1000_hab == razao_maior_am,]
cidade_maior_am <- lugares_maior_am[lugares_maior_am$num_medicos == min(lugares_maior_am$num_medicos),]
cidade_maior_am <- cidade_maior_am$nome


capital_am <- amazonas[amazonas$nome == "Manaus",]

razao_capital_am <- capital_am$razao_medicos_1000_hab

cidade_capital_am <- capital_am$nome


razao_menor_am
cidade_menor_am
razao_maior_am
cidade_maior_am
razao_capital_am
cidade_capital_am


#-------------- BAHIA ----------------------------------------
bahia <- medicos[grep("BAHIA", medicos$UF),]
ba_media <- sum(bahia$razao_medicos_1000_hab) / length(bahia$razao_medicos_1000_hab)

razao_menor_ba <- min(bahia$razao_medicos_1000_hab)

lugares_menor_ba <- bahia[bahia$razao_medicos_1000_hab == razao_menor_ba,]
cidade_menor_ba <- lugares_menor_ba[lugares_menor_ba$num_medicos == min(lugares_menor_ba$num_medicos),]
cidade_menor_ba <- cidade_menor_ba$nome


razao_maior_ba <- max(bahia$razao_medicos_1000_hab)

lugares_maior_ba <- bahia[bahia$razao_medicos_1000_hab == razao_maior_ba,]
cidade_maior_ba <- lugares_maior_ba[lugares_maior_ba$num_medicos == min(lugares_maior_ba$num_medicos),]
cidade_maior_ba <- cidade_maior_ba$nome


capital_ba <- bahia[bahia$nome == "Salvador",]

razao_capital_ba <- capital_ba$razao_medicos_1000_hab

cidade_capital_ba <- capital_ba$nome


razao_menor_ba
cidade_menor_ba
razao_maior_ba
cidade_maior_ba
razao_capital_ba
cidade_capital_ba


#-------------- CEARA ----------------------------------------
ceara <- medicos[grep("CEARÁ", medicos$UF),]
ce_media <- sum(ceara$razao_medicos_1000_hab) / length(ceara$razao_medicos_1000_hab)

razao_menor_ce <- min(ceara$razao_medicos_1000_hab)

lugares_menor_ce <- ceara[ceara$razao_medicos_1000_hab == razao_menor_ce,]
cidade_menor_ce <- lugares_menor_ce[lugares_menor_ce$num_medicos == min(lugares_menor_ce$num_medicos),]
cidade_menor_ce <- cidade_menor_ce$nome


razao_maior_ce <- max(ceara$razao_medicos_1000_hab)

lugares_maior_ce <- ceara[ceara$razao_medicos_1000_hab == razao_maior_ce,]
cidade_maior_ce <- lugares_maior_ce[lugares_maior_ce$num_medicos == min(lugares_maior_ce$num_medicos),]
cidade_maior_ce <- cidade_maior_ce$nome


capital_ce <- ceara[ceara$nome == "Fortaleza",]

razao_capital_ce <- capital_ce$razao_medicos_1000_hab

cidade_capital_ce <- capital_ce$nome


razao_menor_ce
cidade_menor_ce
razao_maior_ce
cidade_maior_ce
razao_capital_ce
cidade_capital_ce


#-------------- DISTRITO FEDERAL ----------------------------------------
distrito_federal <- medicos[grep("DISTRITO FEDERAL", medicos$UF),]
df_media <- sum(distrito_federal$razao_medicos_1000_hab) / length(distrito_federal$razao_medicos_1000_hab)

razao_menor_df <- min(distrito_federal$razao_medicos_1000_hab)

lugares_menor_df <- distrito_federal[distrito_federal$razao_medicos_1000_hab == razao_menor_df,]
cidade_menor_df <- lugares_menor_df[lugares_menor_df$num_medicos == min(lugares_menor_df$num_medicos),]
cidade_menor_df <- cidade_menor_df$nome


razao_maior_df <- max(distrito_federal$razao_medicos_1000_hab)

lugares_maior_df <- distrito_federal[distrito_federal$razao_medicos_1000_hab == razao_maior_df,]
cidade_maior_df <- lugares_maior_df[lugares_maior_df$num_medicos == min(lugares_maior_df$num_medicos),]
cidade_maior_df <- cidade_maior_df$nome


capital_df <- distrito_federal[distrito_federal$nome == "Brasília",]

razao_capital_df <- capital_df$razao_medicos_1000_hab

cidade_capital_df <- capital_df$nome


razao_menor_df
cidade_menor_df
razao_maior_df
cidade_maior_df
razao_capital_df
cidade_capital_df


#-------------- ESPÍRITO SANTO ----------------------------------------
espirito_santo <- medicos[grep("ESPIRITO SANTO", medicos$UF),]
es_media <- sum(espirito_santo$razao_medicos_1000_hab) / length(espirito_santo$razao_medicos_1000_hab)

razao_menor_es <- min(espirito_santo$razao_medicos_1000_hab)

lugares_menor_es <- espirito_santo[espirito_santo$razao_medicos_1000_hab == razao_menor_es,]
cidade_menor_es <- lugares_menor_es[lugares_menor_es$num_medicos == min(lugares_menor_es$num_medicos),]
cidade_menor_es <- cidade_menor_es$nome


razao_maior_es <- max(espirito_santo$razao_medicos_1000_hab)

lugares_maior_es <- espirito_santo[espirito_santo$razao_medicos_1000_hab == razao_maior_es,]
cidade_maior_es <- lugares_maior_es[lugares_maior_es$num_medicos == min(lugares_maior_es$num_medicos),]
cidade_maior_es <- cidade_maior_es$nome


capital_es <- espirito_santo[espirito_santo$nome == "Vitória",]

razao_capital_es <- capital_es$razao_medicos_1000_hab

cidade_capital_es <- capital_es$nome


razao_menor_es
cidade_menor_es
razao_maior_es
cidade_maior_es
razao_capital_es
cidade_capital_es


#-------------- GOIÁS ----------------------------------------
goias <- medicos[grep("GOIÁS", medicos$UF),]
go_media <- sum(goias$razao_medicos_1000_hab) / length(goias$razao_medicos_1000_hab)

razao_menor_go <- min(goias$razao_medicos_1000_hab)

lugares_menor_go <- goias[goias$razao_medicos_1000_hab == razao_menor_go,]
cidade_menor_go <- lugares_menor_go[lugares_menor_go$num_medicos == min(lugares_menor_go$num_medicos),]
cidade_menor_go <- cidade_menor_go$nome


razao_maior_go <- max(goias$razao_medicos_1000_hab)

lugares_maior_go <- goias[goias$razao_medicos_1000_hab == razao_maior_go,]
cidade_maior_go <- lugares_maior_go[lugares_maior_go$num_medicos == min(lugares_maior_go$num_medicos),]
cidade_maior_go <- cidade_maior_go$nome


capital_go <- goias[goias$nome == "Goiânia",]

razao_capital_go <- capital_go$razao_medicos_1000_hab

cidade_capital_go <- capital_go$nome


razao_menor_go
cidade_menor_go
razao_maior_go
cidade_maior_go
razao_capital_go
cidade_capital_go


#-------------- MARANHÃO ----------------------------------------
maranhao <- medicos[grep("MARANHÃO", medicos$UF),]
ma_media <- sum(maranhao$razao_medicos_1000_hab) / length(maranhao$razao_medicos_1000_hab)

razao_menor_ma <- min(maranhao$razao_medicos_1000_hab)

lugares_menor_ma <- maranhao[maranhao$razao_medicos_1000_hab == razao_menor_ma,]
cidade_menor_ma <- lugares_menor_ma[lugares_menor_ma$num_medicos == min(lugares_menor_ma$num_medicos),]
cidade_menor_ma <- cidade_menor_ma$nome


razao_maior_ma <- max(maranhao$razao_medicos_1000_hab)

lugares_maior_ma <- maranhao[maranhao$razao_medicos_1000_hab == razao_maior_ma,]
cidade_maior_ma <- lugares_maior_ma[lugares_maior_ma$num_medicos == min(lugares_maior_ma$num_medicos),]
cidade_maior_ma <- cidade_maior_ma$nome


capital_ma <- maranhao[maranhao$nome == "São Luís",]

razao_capital_ma <- capital_ma$razao_medicos_1000_hab

cidade_capital_ma <- capital_ma$nome


razao_menor_ma
cidade_menor_ma
razao_maior_ma
cidade_maior_ma
razao_capital_ma
cidade_capital_ma



#-------------- MATO GROSSO ----------------------------------------
mato_grosso <- medicos[grep("MATO GROSSO", medicos$UF),]
mt_media <- sum(mato_grosso$razao_medicos_1000_hab) / length(mato_grosso$razao_medicos_1000_hab)

razao_menor_mt <- min(mato_grosso$razao_medicos_1000_hab)

lugares_menor_mt <- mato_grosso[mato_grosso$razao_medicos_1000_hab == razao_menor_mt,]
cidade_menor_mt <- lugares_menor_mt[lugares_menor_mt$num_medicos == min(lugares_menor_mt$num_medicos),]
cidade_menor_mt <- cidade_menor_mt$nome


razao_maior_mt <- max(mato_grosso$razao_medicos_1000_hab)

lugares_maior_mt <- mato_grosso[mato_grosso$razao_medicos_1000_hab == razao_maior_mt,]
cidade_maior_mt <- lugares_maior_mt[lugares_maior_mt$num_medicos == min(lugares_maior_mt$num_medicos),]
cidade_maior_mt <- cidade_maior_mt$nome


capital_mt <- mato_grosso[mato_grosso$nome == "Cuiabá",]

razao_capital_mt <- capital_mt$razao_medicos_1000_hab

cidade_capital_mt <- capital_mt$nome


razao_menor_mt
cidade_menor_mt
razao_maior_mt
cidade_maior_mt
razao_capital_mt
cidade_capital_mt


#-------------- MATO GROSSO DO SUL----------------------------------------
mato_grosso_do_sul <- medicos[grep("MATO GROSSO DO SUL", medicos$UF),]
ms_media <- sum(mato_grosso_do_sul$razao_medicos_1000_hab) / length(mato_grosso_do_sul$razao_medicos_1000_hab)

razao_menor_ms <- min(mato_grosso_do_sul$razao_medicos_1000_hab)

lugares_menor_ms <- mato_grosso_do_sul[mato_grosso_do_sul$razao_medicos_1000_hab == razao_menor_ms,]
cidade_menor_ms <- lugares_menor_ms[lugares_menor_ms$num_medicos == min(lugares_menor_ms$num_medicos),]
cidade_menor_ms <- cidade_menor_ms$nome


razao_maior_ms <- max(mato_grosso_do_sul$razao_medicos_1000_hab)

lugares_maior_ms <- mato_grosso_do_sul[mato_grosso_do_sul$razao_medicos_1000_hab == razao_maior_ms,]
cidade_maior_ms <- lugares_maior_ms[lugares_maior_ms$num_medicos == min(lugares_maior_ms$num_medicos),]
cidade_maior_ms <- cidade_maior_ms$nome


capital_ms <- mato_grosso_do_sul[mato_grosso_do_sul$nome == "Campo Grande",]

razao_capital_ms <- capital_ms$razao_medicos_1000_hab

cidade_capital_ms <- capital_ms$nome


razao_menor_ms
cidade_menor_ms
razao_maior_ms
cidade_maior_ms
razao_capital_ms
cidade_capital_ms


#-------------- MINAS GERAIS ----------------------------------------
minas_gerais <- medicos[grep("MINAS GERAIS", medicos$UF),]
mg_media <- sum(minas_gerais$razao_medicos_1000_hab) / length(minas_gerais$razao_medicos_1000_hab)

razao_menor_mg <- min(minas_gerais$razao_medicos_1000_hab)

lugares_menor_mg <- minas_gerais[minas_gerais$razao_medicos_1000_hab == razao_menor_mg,]

cidade_menor_mg <- c(lugares_menor_mg[lugares_menor_mg$num_medicos == min(lugares_menor_mg$num_medicos),])

cidade_menor_mg <- c(cidade_menor_mg$nome)

cidade_menor_mg <- cidade_menor_mg[1]

cidade_menor_mg


razao_maior_mg <- max(minas_gerais$razao_medicos_1000_hab)

lugares_maior_mg <- minas_gerais[minas_gerais$razao_medicos_1000_hab == razao_maior_mg,]

cidade_maior_mg <- lugares_maior_mg[lugares_maior_mg$num_medicos == min(lugares_maior_mg$num_medicos),]


cidade_maior_mg <- cidade_maior_mg$nome


capital_mg <- minas_gerais[minas_gerais$nome == "Belo Horizonte",]

razao_capital_mg <- capital_mg$razao_medicos_1000_hab

cidade_capital_mg <- capital_mg$nome


razao_menor_mg
cidade_menor_mg
razao_maior_mg
cidade_maior_mg
razao_capital_mg
cidade_capital_mg

#-------------- PARÁ ----------------------------------------
para <- medicos[grep("PARÁ", medicos$UF),]
pa_media <- sum(para$razao_medicos_1000_hab) / length(para$razao_medicos_1000_hab)

razao_menor_pa <- min(para$razao_medicos_1000_hab)

lugares_menor_pa <- para[para$razao_medicos_1000_hab == razao_menor_pa,]
cidade_menor_pa <- lugares_menor_pa[lugares_menor_pa$num_medicos == min(lugares_menor_pa$num_medicos),]
cidade_menor_pa <- cidade_menor_pa$nome


razao_maior_pa <- max(para$razao_medicos_1000_hab)

lugares_maior_pa <- para[para$razao_medicos_1000_hab == razao_maior_pa,]
cidade_maior_pa <- lugares_maior_pa[lugares_maior_pa$num_medicos == min(lugares_maior_pa$num_medicos),]
cidade_maior_pa <- cidade_maior_pa$nome


capital_pa <- para[para$nome == "Belém",]

razao_capital_pa <- capital_pa$razao_medicos_1000_hab

cidade_capital_pa <- capital_pa$nome


razao_menor_pa
cidade_menor_pa
razao_maior_pa
cidade_maior_pa
razao_capital_pa
cidade_capital_pa


#-------------- PARAÍBA ----------------------------------------
paraiba <- medicos[grep("PARAÍBA", medicos$UF),]
pb_media <- sum(paraiba$razao_medicos_1000_hab) / length(paraiba$razao_medicos_1000_hab)

razao_menor_pb <- min(paraiba$razao_medicos_1000_hab)

lugares_menor_pb <- paraiba[paraiba$razao_medicos_1000_hab == razao_menor_pb,]
cidade_menor_pb <- c(lugares_menor_pb[lugares_menor_pb$num_medicos == min(lugares_menor_pb$num_medicos),])
cidade_menor_pb <- c(cidade_menor_pb$nome)
cidade_menor_pb <- cidade_menor_pb[1]


razao_maior_pb <- max(paraiba$razao_medicos_1000_hab)

lugares_maior_pb <- paraiba[paraiba$razao_medicos_1000_hab == razao_maior_pb,]
cidade_maior_pb <- lugares_maior_pb[lugares_maior_pb$num_medicos == min(lugares_maior_pb$num_medicos),]
cidade_maior_pb <- cidade_maior_pb$nome


capital_pb <- paraiba[paraiba$nome == "João Pessoa",]

razao_capital_pb <- capital_pb$razao_medicos_1000_hab

cidade_capital_pb <- capital_pb$nome


razao_menor_pb
cidade_menor_pb
razao_maior_pb
cidade_maior_pb
razao_capital_pb
cidade_capital_pb


#-------------- PARANÁ ----------------------------------------
parana <- medicos[grep("PARANÁ", medicos$UF),]
pr_media <- sum(parana$razao_medicos_1000_hab) / length(parana$razao_medicos_1000_hab)

razao_menor_pr <- min(parana$razao_medicos_1000_hab)

lugares_menor_pr <- parana[parana$razao_medicos_1000_hab == razao_menor_pr,]
cidade_menor_pr <- c(lugares_menor_pr[lugares_menor_pr$num_medicos == min(lugares_menor_pr$num_medicos),])
cidade_menor_pr <- c(cidade_menor_pr$nome)

cidade_menor_pr <- cidade_menor_pr[1]


razao_maior_pr <- max(parana$razao_medicos_1000_hab)

lugares_maior_pr <- parana[parana$razao_medicos_1000_hab == razao_maior_pr,]
cidade_maior_pr <- lugares_maior_pr[lugares_maior_pr$num_medicos == min(lugares_maior_pr$num_medicos),]
cidade_maior_pr <- cidade_maior_pr$nome


capital_pr <- parana[parana$nome == "Curitiba",]

razao_capital_pr <- capital_pr$razao_medicos_1000_hab

cidade_capital_pr <- capital_pr$nome


razao_menor_pr
cidade_menor_pr
razao_maior_pr
cidade_maior_pr
razao_capital_pr
cidade_capital_pr



#-------------- PERNAMBUCO ----------------------------------------
pernambuco <- medicos[grep("PERNAMBUCO", medicos$UF),]
pe_media <- sum(pernambuco$razao_medicos_1000_hab) / length(pernambuco$razao_medicos_1000_hab)

razao_menor_pe <- min(pernambuco$razao_medicos_1000_hab)

lugares_menor_pe <- pernambuco[pernambuco$razao_medicos_1000_hab == razao_menor_pe,]
cidade_menor_pe <- lugares_menor_pe[lugares_menor_pe$num_medicos == min(lugares_menor_pe$num_medicos),]
cidade_menor_pe <- cidade_menor_pe$nome


razao_maior_pe <- max(pernambuco$razao_medicos_1000_hab)

lugares_maior_pe <- pernambuco[pernambuco$razao_medicos_1000_hab == razao_maior_pe,]
cidade_maior_pe <- lugares_maior_pe[lugares_maior_pe$num_medicos == min(lugares_maior_pe$num_medicos),]
cidade_maior_pe <- cidade_maior_pe$nome

capital_pe <- pernambuco[pernambuco$nome == "Recife",]

capital_pe <- capital_pe[-1,]

razao_capital_pe <- capital_pe$razao_medicos_1000_hab

cidade_capital_pe <- capital_pe$nome

razao_menor_pe
cidade_menor_pe
razao_maior_pe
cidade_maior_pe
razao_capital_pe
cidade_capital_pe


#-------------- PIAUÍ ----------------------------------------
piaui <- medicos[grep("PIAUÍ", medicos$UF),]
pi_media <- sum(piaui$razao_medicos_1000_hab) / length(piaui$razao_medicos_1000_hab)

razao_menor_pi <- min(piaui$razao_medicos_1000_hab)

lugares_menor_pi <- piaui[piaui$razao_medicos_1000_hab == razao_menor_pi,]
cidade_menor_pi <- lugares_menor_pi[lugares_menor_pi$num_medicos == min(lugares_menor_pi$num_medicos),]
cidade_menor_pi <- cidade_menor_pi$nome


razao_maior_pi <- max(piaui$razao_medicos_1000_hab)

lugares_maior_pi <- piaui[piaui$razao_medicos_1000_hab == razao_maior_pi,]
cidade_maior_pi <- lugares_maior_pi[lugares_maior_pi$num_medicos == min(lugares_maior_pi$num_medicos),]
cidade_maior_pi <- cidade_maior_pi$nome


capital_pi <- piaui[piaui$nome == "Teresina",]

razao_capital_pi <- capital_pi$razao_medicos_1000_hab

cidade_capital_pi <- capital_pi$nome


razao_menor_pi
cidade_menor_pi
razao_maior_pi
cidade_maior_pi
razao_capital_pi
cidade_capital_pi

#-------------- RIO DE JANEIRO ----------------------------------------
rio_de_janeiro <- medicos[grep("RIO DE JANEIRO", medicos$UF),]
rj_media <- sum(rio_de_janeiro$razao_medicos_1000_hab) / length(rio_de_janeiro$razao_medicos_1000_hab)

razao_menor_rj <- min(rio_de_janeiro$razao_medicos_1000_hab)

lugares_menor_rj <- rio_de_janeiro[rio_de_janeiro$razao_medicos_1000_hab == razao_menor_rj,]
cidade_menor_rj <- lugares_menor_rj[lugares_menor_rj$num_medicos == min(lugares_menor_rj$num_medicos),]
cidade_menor_rj <- cidade_menor_rj$nome


razao_maior_rj <- max(rio_de_janeiro$razao_medicos_1000_hab)

lugares_maior_rj <- rio_de_janeiro[rio_de_janeiro$razao_medicos_1000_hab == razao_maior_rj,]
cidade_maior_rj <- lugares_maior_rj[lugares_maior_rj$num_medicos == min(lugares_maior_rj$num_medicos),]
cidade_maior_rj <- cidade_maior_rj$nome


capital_rj <- rio_de_janeiro[rio_de_janeiro$nome == "Rio de Janeiro",]

razao_capital_rj <- capital_rj$razao_medicos_1000_hab

cidade_capital_rj <- capital_rj$nome


razao_menor_rj
cidade_menor_rj
razao_maior_rj
cidade_maior_rj
razao_capital_rj
cidade_capital_rj

#-------------- RIO GRANDE DO NORTE ----------------------------------------
rio_grande_do_norte <- medicos[grep("RIO GRANDE DO NORTE", medicos$UF),]
rn_media <- sum(rio_grande_do_norte$razao_medicos_1000_hab) / length(rio_grande_do_norte$razao_medicos_1000_hab)

razao_menor_rn <- min(rio_grande_do_norte$razao_medicos_1000_hab)

lugares_menor_rn <- rio_grande_do_norte[rio_grande_do_norte$razao_medicos_1000_hab == razao_menor_rn,]
cidade_menor_rn <- lugares_menor_rn[lugares_menor_rn$num_medicos == min(lugares_menor_rn$num_medicos),]
cidade_menor_rn <- cidade_menor_rn$nome


razao_maior_rn <- max(rio_grande_do_norte$razao_medicos_1000_hab)

lugares_maior_rn <- rio_grande_do_norte[rio_grande_do_norte$razao_medicos_1000_hab == razao_maior_rn,]
cidade_maior_rn <- lugares_maior_rn[lugares_maior_rn$num_medicos == min(lugares_maior_rn$num_medicos),]
cidade_maior_rn <- cidade_maior_rn$nome


capital_rn <- rio_grande_do_norte[rio_grande_do_norte$nome == "Natal",]

razao_capital_rn <- capital_rn$razao_medicos_1000_hab

cidade_capital_rn <- capital_rn$nome


razao_menor_rn
cidade_menor_rn
razao_maior_rn
cidade_maior_rn
razao_capital_rn
cidade_capital_rn

#-------------- RIO GRANDE DO SUL ----------------------------------------
rio_grande_do_sul <- medicos[grep("RIO GRANDE DO SUL", medicos$UF),]
rs_media <- sum(rio_grande_do_sul$razao_medicos_1000_hab) / length(rio_grande_do_sul$razao_medicos_1000_hab)

razao_menor_rs <- min(rio_grande_do_sul$razao_medicos_1000_hab)

lugares_menor_rs <- rio_grande_do_sul[rio_grande_do_sul$razao_medicos_1000_hab == razao_menor_rs,]
cidade_menor_rs <- lugares_menor_rs[lugares_menor_rs$num_medicos == min(lugares_menor_rs$num_medicos),]
cidade_menor_rs <- cidade_menor_rs$nome


razao_maior_rs <- max(rio_grande_do_sul$razao_medicos_1000_hab)

lugares_maior_rs <- rio_grande_do_sul[rio_grande_do_sul$razao_medicos_1000_hab == razao_maior_rs,]
cidade_maior_rs <- lugares_maior_rs[lugares_maior_rs$num_medicos == min(lugares_maior_rs$num_medicos),]
cidade_maior_rs <- cidade_maior_rs$nome


capital_rs <- rio_grande_do_sul[rio_grande_do_sul$nome == "Porto Alegre",]

razao_capital_rs <- capital_rs$razao_medicos_1000_hab

cidade_capital_rs <- capital_rs$nome


razao_menor_rs
cidade_menor_rs
razao_maior_rs
cidade_maior_rs
razao_capital_rs
cidade_capital_rs

#-------------- RONDONIA ----------------------------------------
rondonia <- medicos[grep("RONDÔNIA", medicos$UF),]
ro_media <- sum(rondonia$razao_medicos_1000_hab) / length(rondonia$razao_medicos_1000_hab)

razao_menor_ro <- min(rondonia$razao_medicos_1000_hab)

lugares_menor_ro <- rondonia[rondonia$razao_medicos_1000_hab == razao_menor_ro,]
cidade_menor_ro <- c(lugares_menor_ro[lugares_menor_ro$num_medicos == min(lugares_menor_ro$num_medicos),])
cidade_menor_ro <- c(cidade_menor_ro$nome)
cidade_menor_ro <- cidade_menor_ro[1]


razao_maior_ro <- max(rondonia$razao_medicos_1000_hab)

lugares_maior_ro <- rondonia[rondonia$razao_medicos_1000_hab == razao_maior_ro,]
cidade_maior_ro <- lugares_maior_ro[lugares_maior_ro$num_medicos == min(lugares_maior_ro$num_medicos),]
cidade_maior_ro <- cidade_maior_ro$nome


capital_ro <- rondonia[rondonia$nome == "Porto Velho",]

razao_capital_ro <- capital_ro$razao_medicos_1000_hab

cidade_capital_ro <- capital_ro$nome


razao_menor_ro
cidade_menor_ro
razao_maior_ro
cidade_maior_ro
razao_capital_ro
cidade_capital_ro

#-------------- RORAIMA ----------------------------------------
roraima <- medicos[grep("RORAIMA", medicos$UF),]
rr_media <- sum(roraima$razao_medicos_1000_hab) / length(roraima$razao_medicos_1000_hab)


razao_menor_rr <- min(roraima$razao_medicos_1000_hab)

lugares_menor_rr <- roraima[roraima$razao_medicos_1000_hab == razao_menor_rr,]
cidade_menor_rr <- lugares_menor_rr[lugares_menor_rr$num_medicos == min(lugares_menor_rr$num_medicos),]
cidade_menor_rr <- cidade_menor_rr$nome


razao_maior_rr <- max(roraima$razao_medicos_1000_hab)

lugares_maior_rr <- roraima[roraima$razao_medicos_1000_hab == razao_maior_rr,]
cidade_maior_rr <- lugares_maior_rr[lugares_maior_rr$num_medicos == min(lugares_maior_rr$num_medicos),]
cidade_maior_rr <- cidade_maior_rr$nome


capital_rr <- roraima[roraima$nome == "Boa Vista",]

razao_capital_rr <- capital_rr$razao_medicos_1000_hab

cidade_capital_rr <- capital_rr$nome


razao_menor_rr
cidade_menor_rr
razao_maior_rr
cidade_maior_rr
razao_capital_rr
cidade_capital_rr

#-------------- SANTA CATARINA ----------------------------------------
santa_catarina <- medicos[grep("SANTA CATARINA", medicos$UF),]
sc_media <- sum(santa_catarina$razao_medicos_1000_hab) / length(santa_catarina$razao_medicos_1000_hab)

razao_menor_sc <- min(santa_catarina$razao_medicos_1000_hab)

lugares_menor_sc <- santa_catarina[santa_catarina$razao_medicos_1000_hab == razao_menor_sc,]
cidade_menor_sc <- lugares_menor_sc[lugares_menor_sc$num_medicos == min(lugares_menor_sc$num_medicos),]
cidade_menor_sc <- cidade_menor_sc$nome


razao_maior_sc <- max(santa_catarina$razao_medicos_1000_hab)

lugares_maior_sc <- santa_catarina[santa_catarina$razao_medicos_1000_hab == razao_maior_sc,]
cidade_maior_sc <- lugares_maior_sc[lugares_maior_sc$num_medicos == min(lugares_maior_sc$num_medicos),]
cidade_maior_sc <- cidade_maior_sc$nome


capital_sc <- santa_catarina[santa_catarina$nome == "Florianópolis",]

razao_capital_sc <- capital_sc$razao_medicos_1000_hab

cidade_capital_sc <- capital_sc$nome


razao_menor_sc
cidade_menor_sc
razao_maior_sc
cidade_maior_sc
razao_capital_sc
cidade_capital_sc

#-------------- SÃO PAULO ----------------------------------------
sao_paulo <- medicos[grep("SÃO PAULO", medicos$UF),]
sp_media <- sum(sao_paulo$razao_medicos_1000_hab) / length(sao_paulo$razao_medicos_1000_hab)

razao_menor_sp <- min(sao_paulo$razao_medicos_1000_hab)

lugares_menor_sp <- sao_paulo[sao_paulo$razao_medicos_1000_hab == razao_menor_sp,]
cidade_menor_sp <- lugares_menor_sp[lugares_menor_sp$num_medicos == min(lugares_menor_sp$num_medicos),]
cidade_menor_sp <- cidade_menor_sp$nome


razao_maior_sp <- max(sao_paulo$razao_medicos_1000_hab)

lugares_maior_sp <- sao_paulo[sao_paulo$razao_medicos_1000_hab == razao_maior_sp,]
cidade_maior_sp <- lugares_maior_sp[lugares_maior_sp$num_medicos == min(lugares_maior_sp$num_medicos),]
cidade_maior_sp <- cidade_maior_sp$nome


capital_sp <- sao_paulo[sao_paulo$nome == "São Paulo",]

razao_capital_sp <- capital_sp$razao_medicos_1000_hab

cidade_capital_sp <- capital_sp$nome


razao_menor_sp
cidade_menor_sp
razao_maior_sp
cidade_maior_sp
razao_capital_sp
cidade_capital_sp

#-------------- SERGIPE ----------------------------------------
sergipe <- medicos[grep("SERGIPE", medicos$UF),]
se_media <- sum(sergipe$razao_medicos_1000_hab) / length(sergipe$razao_medicos_1000_hab)

razao_menor_se <- min(sergipe$razao_medicos_1000_hab)

lugares_menor_se <- sergipe[sergipe$razao_medicos_1000_hab == razao_menor_se,]
cidade_menor_se <- lugares_menor_se[lugares_menor_se$num_medicos == min(lugares_menor_se$num_medicos),]
cidade_menor_se <- cidade_menor_se$nome


razao_maior_se <- max(sergipe$razao_medicos_1000_hab)

lugares_maior_se <- sergipe[sergipe$razao_medicos_1000_hab == razao_maior_se,]
cidade_maior_se <- lugares_maior_se[lugares_maior_se$num_medicos == min(lugares_maior_se$num_medicos),]
cidade_maior_se <- cidade_maior_se$nome


capital_se <- sergipe[sergipe$nome == "Aracaju",]

razao_capital_se <- capital_se$razao_medicos_1000_hab

cidade_capital_se <- capital_se$nome


razao_menor_se
cidade_menor_se
razao_maior_se
cidade_maior_se
razao_capital_se
cidade_capital_se

#-------------- TOCONTINS ----------------------------------------
tocantins <- medicos[grep("TOCANTINS", medicos$UF),]
to_media <- sum(tocantins$razao_medicos_1000_hab) / length(tocantins$razao_medicos_1000_hab)

razao_menor_to <- min(tocantins$razao_medicos_1000_hab)

lugares_menor_to <- tocantins[tocantins$razao_medicos_1000_hab == razao_menor_to,]
cidade_menor_to <- lugares_menor_to[lugares_menor_to$num_medicos == min(lugares_menor_to$num_medicos),]
cidade_menor_to <- cidade_menor_to$nome


razao_maior_to <- max(tocantins$razao_medicos_1000_hab)

lugares_maior_to <- tocantins[tocantins$razao_medicos_1000_hab == razao_maior_to,]
cidade_maior_to <- lugares_maior_to[lugares_maior_to$num_medicos == min(lugares_maior_to$num_medicos),]
cidade_maior_to <- cidade_maior_to$nome


capital_to <- tocantins[tocantins$nome == "Palmas",]

razao_capital_to <- capital_to$razao_medicos_1000_hab

cidade_capital_to <- capital_to$nome


razao_menor_to
cidade_menor_to
razao_maior_to
cidade_maior_to
razao_capital_to
cidade_capital_to


###################################################
###################################################
#-------------- NORTE ----------------------------------------
norte <- (am_media+rr_media+ap_media+pa_media+to_media+ro_media+ac_media) / 7
#-------------- NORDESTE ----------------------------------------
nordeste <- (ma_media+pi_media+ce_media+rn_media+pe_media+pb_media+se_media+al_media+ba_media) / 9 
#-------------- CENTRO OESTE ----------------------------------------
centro_oeste <- (mt_media+ms_media+go_media) / 3
#-------------- SUDESTE ----------------------------------------
sudeste <- (sp_media+rj_media+es_media+mg_media) / 4
#-------------- SUL ----------------------------------------
sul <- (pr_media+rs_media+sc_media) / 3
#-------------- DISTRITO FEDERAL ----------------------------------------
brasilia <- df_media
###################################################
###################################################

#Regiões do Brasil
razao_regioes <- data.frame("regiao" = 1:5,"r_razao" = 1:5, "descricao" = 1:5)
razao_regioes$regiao <- c("N","NE","S","SE","O")

regiao_n <- paste("N", "NORTE", sep="-")
regiao_ne <- paste("NE", "NORDESTE", sep="-")
regiao_s <- paste("S", "SUL", sep="-")
regiao_se <- paste("SE", "SUDESTE", sep="-")
regiao_o <- paste("O", "CENTRO-OESTE", sep="-")


razao_regioes$descricao <- c(regiao_n,regiao_ne,regiao_s,regiao_se,regiao_o)
razao_regioes$r_razao <- c(norte,nordeste,sul,sudeste,centro_oeste)
razao_regioes$r_razao <- arredondamento(razao_regioes$r_razao)

razao_regioes %>% ggplot(aes(x = regiao, y = r_razao, fill = descricao)) +
  geom_col() + labs(fill = "Regiões") +
  scale_y_continuous(limits = c(0, 3))+
  geom_text(aes(label = r_razao), 
            vjust = -1) +
  labs(title = "Comparativo entre as Regiões em Razão de Médicos", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  theme_grey(base_size = 12)


#Graficos

library(ggplot2)
library(dplyr)

#Região Norte
razao_e_norte <- data.frame("estado" = 1:7,"r_razao" = 1:7, "descricao" = 1:7)
razao_e_norte$estado <- c("AC","AM","AP","PA","RO","RR","TO")
razao_e_norte$descricao <- c("ACRE","AMAZONAS","AMAPÁ","PARÁ","RONDÔNIA","RORAIMA","TOCANTINS")
razao_e_norte$r_razao <- c(ac_media,am_media,ap_media,pa_media,ro_media,rr_media,to_media)
razao_e_norte$r_razao <- arredondamento(razao_e_norte$r_razao)

razao_e_norte %>% ggplot(aes(x = estado, y = r_razao, fill = descricao)) +
  geom_col() + labs(fill = "Estados") +
  scale_y_continuous(limits = c(0, 3))+
  geom_text(aes(label = r_razao), 
            vjust = -1) +
  labs(title = "Comparativo Estados Região Norte", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  theme_grey(base_size = 12)

#Região Nordeste
razao_e_nordeste <- data.frame("estado" = 1:9,"r_razao" = 1:9, "descricao" = 1:9)
razao_e_nordeste$estado <- c("AL","BA","CE","MA","PB","PE","PI","RN","SE")
razao_e_nordeste$descricao <- c("ALAGOAS","BAHIA","CEARÁ","MARANHÃO","PARAÍBA","PERNAMBUCO","PIAUÍ","RIO GRANDE DO NORTE","SERGIPE")
razao_e_nordeste$r_razao <- c(al_media,ba_media,ce_media,ma_media,pb_media,pe_media,pi_media,rn_media,se_media)
razao_e_nordeste$r_razao <- arredondamento(razao_e_nordeste$r_razao)

razao_e_nordeste %>% ggplot(aes(x = estado, y = r_razao, fill = descricao)) +
  geom_col() + labs(fill = "Estados") +
  scale_y_continuous(limits = c(0, 2))+
  geom_text(aes(label = r_razao), 
            vjust = -1) +
  labs(title = "Comparativo Estados Região Nordeste", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  theme_grey(base_size = 12)
  

#Região Centro-Oeste
razao_e_centro_oeste <- data.frame("estado" = 1:3,"r_razao" = 1:3, "descricao" = 1:3)
razao_e_centro_oeste$estado <- c("GO","MT","MS")
razao_e_centro_oeste$descricao <- c("GOIÁS","MATO GROSSO","MATO GROSSO DO SUL")
razao_e_centro_oeste$r_razao <- c(go_media,mt_media,ms_media)
razao_e_centro_oeste$r_razao <- arredondamento(razao_e_centro_oeste$r_razao)

razao_e_centro_oeste %>% ggplot(aes(x = estado, y = r_razao, fill = descricao)) +
  geom_col() + labs(fill = "Estados") +
  scale_y_continuous(limits = c(0, 2))+
  geom_text(aes(label = r_razao), 
            vjust = -1) +
  labs(title = "Comparativo Estados Região Centro-Oeste", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  theme_grey(base_size = 12)


#Região SUDESTE
razao_e_sudeste <- data.frame("estado" = 1:4,"r_razao" = 1:4, "descricao" = 1:4)
razao_e_sudeste$estado <- c("ES","MG","RJ","SP")
razao_e_sudeste$descricao <- c("ESPÍRITO SANTO","MINAS GERAIS","RIO DE JANEIRO","SÃO PAULO")
razao_e_sudeste$r_razao <- c(es_media,mg_media,rj_media,sp_media)
razao_e_sudeste$r_razao <- arredondamento(razao_e_sudeste$r_razao)

razao_e_sudeste %>% ggplot(aes(x = estado, y = r_razao, fill = descricao)) +
  geom_col() + labs(fill = "Estados") +
  scale_y_continuous(limits = c(0, 5))+
  geom_text(aes(label = r_razao), 
            vjust = -1) +
  labs(title = "Comparativo Estados Região Sudeste", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  theme_grey(base_size = 12)

#Região Sul
razao_e_sul <- data.frame("estado" = 1:3,"r_razao" = 1:3, "descricao" = 1:3)
razao_e_sul$estado <- c("PR","SC","RS")
razao_e_sul$descricao <- c("PARANÁ","SANTA CATARINA","RIO GRANDE DO SUL")
razao_e_sul$r_razao <- c(pr_media,sc_media,rs_media)
razao_e_sul$r_razao <- arredondamento(razao_e_sul$r_razao)

razao_e_sul %>% ggplot(aes(x = estado, y = r_razao, fill = descricao)) +
  geom_col() + labs(fill = "Estados") +
  scale_y_continuous(limits = c(0, 3))+
  geom_text(aes(label = r_razao), 
            vjust = -1) +
  labs(title = "Comparativo Estados Região Sul", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  theme_grey(base_size = 12)


#Comparativo entre todos os estados
razao_estados <- data.frame("estado" = 1:27,"r_razao" = 1:27, "descricao" = 1:27)
razao_estados$estado <- c("AC","AL","AP","AM","BA","CE","ES","GO","MA","MT","MS","MG","PA","PB",
                          "PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO","DF")
razao_estados$descricao <- c("ACRE","ALAGOAS","AMAPÁ","AMAZONAS","BAHIA","CEARÁ","ESPÍRITO SANTO",
                             "GOIÁS","MARANHÃO","MATO GROSSO","MATO GROSSO DO SUL","MINAS GERAIS",
                             "PARÁ","PARAÍBA","PARANÁ","PERNAMBUCO","PIAUÍ","RIO DE JANEIRO",
                             "RIO GRANDE DO NORTE","RIO GRANDE DO SUL","RONDÔNIA","RORAIMA",
                             "SANTA CATARINA","SÃO PAULO","SERGIPE","TOCANTINS","DISTRITO FEDERAL")
razao_estados$r_razao <- c(ac_media,al_media,ap_media,am_media,ba_media,ce_media,es_media,go_media,
                           ma_media,mt_media,ms_media,mg_media,pa_media,pb_media,pr_media,pe_media,pi_media,
                           rj_media,rn_media,rs_media,ro_media,rr_media,sc_media,sp_media,
                           se_media,to_media,df_media)
razao_estados$r_razao <- arredondamento(razao_estados$r_razao)


ggplot(data = razao_estados, aes(x = estado, y = r_razao, colour = descricao, size=2)) + 
  labs(colour = "Estados") +
  labs(title = "Comparativo entre os Estados em Razão de Médicos", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  geom_point() +
  theme_grey(base_size = 15)


#Comparativo entre as cidades com menor razão de médicos
razao_cidades_menores <- data.frame("estado" = 1:26,"r_razao" = 1:26, "cidade" = 1:26)
razao_cidades_menores$estado <- c("AC","AL","AP","AM","BA","CE","ES","GO","MA","MT","MS","MG","PA","PB",
                          "PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")

ac_menor <- paste("AC", cidade_menor_ac, sep="-")
al_menor <- paste("AL", cidade_menor_al, sep="-")
ap_menor <- paste("AP", cidade_menor_ap, sep="-")
am_menor <- paste("AM", cidade_menor_am, sep="-")
ba_menor <- paste("BA", cidade_menor_ba, sep="-")
ce_menor <- paste("CE", cidade_menor_ce, sep="-")
es_menor <- paste("ES", cidade_menor_es, sep="-")
go_menor <- paste("GO", cidade_menor_go, sep="-")
ma_menor <- paste("MA", cidade_menor_ma, sep="-")
mt_menor <- paste("MT", cidade_menor_mt, sep="-")
ms_menor <- paste("MS", cidade_menor_ms, sep="-")
mg_menor <- paste("MG", cidade_menor_mg, sep="-")
pa_menor <- paste("PA", cidade_menor_pa, sep="-")
pb_menor <- paste("PB", cidade_menor_pb, sep="-")
pr_menor <- paste("PR", cidade_menor_pr, sep="-")
pe_menor <- paste("PE", cidade_menor_pe, sep="-")
pi_menor <- paste("PI", cidade_menor_pi, sep="-")
rj_menor <- paste("RJ", cidade_menor_rj, sep="-")
rn_menor <- paste("RN", cidade_menor_rn, sep="-")
rs_menor <- paste("RS", cidade_menor_rs, sep="-")
ro_menor <- paste("RO", cidade_menor_ro, sep="-")
rr_menor <- paste("RR", cidade_menor_rr, sep="-")
sc_menor <- paste("SC", cidade_menor_sc, sep="-")
sp_menor <- paste("SP", cidade_menor_sp, sep="-")
se_menor <- paste("SE", cidade_menor_se, sep="-")
to_menor <- paste("TO", cidade_menor_to, sep="-")

razao_cidades_menores$cidade <- c(ac_menor,al_menor,ap_menor,am_menor,ba_menor,
                                  ce_menor,es_menor,go_menor,ma_menor,mt_menor,
                                  ms_menor,mg_menor,pa_menor,pb_menor,pr_menor,
                                  pe_menor,pi_menor,rj_menor,rn_menor,rs_menor,
                                  ro_menor,rr_menor,sc_menor,sp_menor,se_menor,to_menor)

razao_cidades_menores$r_razao <- c(razao_menor_ac,razao_menor_al,razao_menor_ap,razao_menor_am,
                                   razao_menor_ba,razao_menor_ce,razao_menor_es,razao_menor_go,
                                   razao_menor_ma,razao_menor_mg,razao_menor_ms,razao_menor_mt,
                                   razao_menor_pa,razao_menor_pb,razao_menor_pe,razao_menor_pi,
                                   razao_menor_pr,razao_menor_rj,razao_menor_rn,razao_menor_ro,
                                   razao_menor_rr,razao_menor_rs,razao_menor_sc,razao_menor_se,
                                   razao_menor_sp,razao_menor_to)

ggplot(data = razao_cidades_menores, aes(x = estado, y = r_razao, colour = cidade, size=2)) +
  labs(colour = "Cidades") +
  labs(title = "Comparativo entre Cidades com Menor Razão de Médicos", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  geom_point() +
  theme_grey(base_size = 15)



#Comparativo entre as cidades com maior razão de médicos
razao_cidades_maiores <- data.frame("estado" = 1:27,"r_razao" = 1:27, "cidade" = 1:27)
razao_cidades_maiores$estado <- c("AC","AL","AP","AM","BA","CE","ES","GO","MA","MT","MS","MG","PA","PB",
                                  "PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO","DF")

ac_maior <- paste("AC", cidade_maior_ac, sep="-")
al_maior <- paste("AL", cidade_maior_al, sep="-")
ap_maior <- paste("AP", cidade_maior_ap, sep="-")
am_maior <- paste("AM", cidade_maior_am, sep="-")
ba_maior <- paste("BA", cidade_maior_ba, sep="-")
ce_maior <- paste("CE", cidade_maior_ce, sep="-")
es_maior <- paste("ES", cidade_maior_es, sep="-")
go_maior <- paste("GO", cidade_maior_go, sep="-")
ma_maior <- paste("MA", cidade_maior_ma, sep="-")
mt_maior <- paste("MT", cidade_maior_mt, sep="-")
ms_maior <- paste("MS", cidade_maior_ms, sep="-")
mg_maior <- paste("MG", cidade_maior_mg, sep="-")
pa_maior <- paste("PA", cidade_maior_pa, sep="-")
pb_maior <- paste("PB", cidade_maior_pb, sep="-")
pr_maior <- paste("PR", cidade_maior_pr, sep="-")
pe_maior <- paste("PE", cidade_maior_pe, sep="-")
pi_maior <- paste("PI", cidade_maior_pi, sep="-")
rj_maior <- paste("RJ", cidade_maior_rj, sep="-")
rn_maior <- paste("RN", cidade_maior_rn, sep="-")
rs_maior <- paste("RS", cidade_maior_rs, sep="-")
ro_maior <- paste("RO", cidade_maior_ro, sep="-")
rr_maior <- paste("RR", cidade_maior_rr, sep="-")
sc_maior <- paste("SC", cidade_maior_sc, sep="-")
sp_maior <- paste("SP", cidade_maior_sp, sep="-")
se_maior <- paste("SE", cidade_maior_se, sep="-")
to_maior <- paste("TO", cidade_maior_to, sep="-")
df_maior <- paste("DF", cidade_maior_df, sep="-")

razao_cidades_maiores$cidade <- c(ac_maior,al_maior,ap_maior,am_maior,ba_maior,
                                  ce_maior,es_maior,go_maior,ma_maior,mt_maior,
                                  ms_maior,mg_maior,pa_maior,pb_maior,pr_maior,
                                  pe_maior,pi_maior,rj_maior,rn_maior,rs_maior,
                                  ro_maior,rr_maior,sc_maior,sp_maior,se_maior,
                                  to_maior,df_maior)

razao_cidades_maiores$r_razao <- c(razao_maior_ac,razao_maior_al,razao_maior_ap,razao_maior_am,
                                   razao_maior_ba,razao_maior_ce,razao_maior_es,razao_maior_go,
                                   razao_maior_ma,razao_maior_mg,razao_maior_ms,razao_maior_mt,
                                   razao_maior_pa,razao_maior_pb,razao_maior_pe,razao_maior_pi,
                                   razao_maior_pr,razao_maior_rj,razao_maior_rn,razao_maior_ro,
                                   razao_maior_rr,razao_maior_rs,razao_maior_sc,razao_maior_se,
                                   razao_maior_sp,razao_maior_to,razao_maior_df)

ggplot(data = razao_cidades_maiores, aes(x = estado, y = r_razao, colour = cidade, size=2)) + 
  labs(colour = "Cidades") +
  labs(title = "Comparativo entre Cidades com Maior Razão de Médicos", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  geom_point() +
  theme_grey(base_size = 15)



#Comparativo entre as cidades da capital em razão de médicos
razao_cidades_capitais <- data.frame("estado" = 1:27,"r_razao" = 1:27, "cidade" = 1:27)
razao_cidades_capitais$estado <- c("AC","AL","AP","AM","BA","CE","ES","GO","MA","MT","MS","MG","PA","PB",
                                  "PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO","DF")

ac_capital <- paste("AC", cidade_capital_ac, sep="-")
al_capital <- paste("AL", cidade_capital_al, sep="-")
ap_capital <- paste("AP", cidade_capital_ap, sep="-")
am_capital <- paste("AM", cidade_capital_am, sep="-")
ba_capital <- paste("BA", cidade_capital_ba, sep="-")
ce_capital <- paste("CE", cidade_capital_ce, sep="-")
es_capital <- paste("ES", cidade_capital_es, sep="-")
go_capital <- paste("GO", cidade_capital_go, sep="-")
ma_capital <- paste("MA", cidade_capital_ma, sep="-")
mt_capital <- paste("MT", cidade_capital_mt, sep="-")
ms_capital <- paste("MS", cidade_capital_ms, sep="-")
mg_capital <- paste("MG", cidade_capital_mg, sep="-")
pa_capital <- paste("PA", cidade_capital_pa, sep="-")
pb_capital <- paste("PB", cidade_capital_pb, sep="-")
pr_capital <- paste("PR", cidade_capital_pr, sep="-")
pe_capital <- paste("PE", cidade_capital_pe, sep="-")
pi_capital <- paste("PI", cidade_capital_pi, sep="-")
rj_capital <- paste("RJ", cidade_capital_rj, sep="-")
rn_capital <- paste("RN", cidade_capital_rn, sep="-")
rs_capital <- paste("RS", cidade_capital_rs, sep="-")
ro_capital <- paste("RO", cidade_capital_ro, sep="-")
rr_capital <- paste("RR", cidade_capital_rr, sep="-")
sc_capital <- paste("SC", cidade_capital_sc, sep="-")
sp_capital <- paste("SP", cidade_capital_sp, sep="-")
se_capital <- paste("SE", cidade_capital_se, sep="-")
to_capital <- paste("TO", cidade_capital_to, sep="-")
df_capital <- paste("DF", cidade_capital_df, sep="-")

razao_cidades_capitais$cidade <- c(ac_capital,al_capital,ap_capital,am_capital,ba_capital,
                                   ce_capital,es_capital,go_capital,ma_capital,mt_capital,
                                   ms_capital,mg_capital,pa_capital,pb_capital,pr_capital,
                                   pe_capital,pi_capital,rj_capital,rn_capital,rs_capital,
                                   ro_capital,rr_capital,sc_capital,sp_capital,se_capital,
                                   to_capital,df_capital)

razao_cidades_capitais$r_razao <- c(razao_capital_ac,razao_capital_al,razao_capital_ap,razao_capital_am,
                                    razao_capital_ba,razao_capital_ce,razao_capital_es,razao_capital_go,
                                    razao_capital_ma,razao_capital_mg,razao_capital_ms,razao_capital_mt,
                                    razao_capital_pa,razao_capital_pb,razao_capital_pe,razao_capital_pi,
                                    razao_capital_pr,razao_capital_rj,razao_capital_rn,razao_capital_ro,
                                    razao_capital_rr,razao_capital_rs,razao_capital_sc,razao_capital_se,
                                    razao_capital_sp,razao_capital_to,razao_capital_df)

ggplot(data = razao_cidades_capitais, aes(x = estado, y = r_razao, colour = cidade, size=2)) + 
  labs(colour = "Cidades") +
  labs(title = "Comparativo entre as Capitais dos Estados em Razão de Médicos", x="Estados", y="Razão Médicos por 1000 Habitantes") +
  geom_point() +
  theme_grey(base_size = 15)


