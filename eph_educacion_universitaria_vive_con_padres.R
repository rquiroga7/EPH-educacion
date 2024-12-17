#install.packages('eph')
library(readxl)
library(eph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
year=c(2003,2008,2013,2018,2023)
base <- get_microdata(year = year, 
                    trimester = c(3),
                    type = 'individual')
dt<- base
 #Convert DECCFR to numeric
dt$DECCFR <- as.numeric(as.character(dt$DECCFR))
#Filtro NA y 0 de DECCFR
dt<-dt %>% filter(DECCFR > 0 & DECCFR < 11)
#Remove duplicated people based on CODUSU+COMPONENTE+NRO_HOGAR
#dt <- dt %>% distinct(CODUSU, COMPONENTE, NRO_HOGAR .keep_all = TRUE)

per_decil<-data.frame(dt %>% group_by(DECCFR) %>% summarise(n = sum(PONDIH,na.rm = T)))
names(per_decil)[2]<-"Personas"
per_decil$hogares<-dt %>% 
  group_by(DECCFR) %>%
  distinct(CODUSU,NRO_HOGAR,PONDIH) %>% 
  summarise(n = sum(PONDIH,na.rm = T))%>% 
  pull(n)

#Filtro sólo las personas entre 19 y 25 años y que sean hijos/nietos del jefe/a de hogar
dt_age<-dt %>% filter(CH06>=19 & CH06<=25 & CH03 %in% c(3,4,5))
#Personas 17-25 años por decil Ingreso per cápita familiar
per_decilpc<-data.frame(dt_age %>% group_by(DECCFR) %>% summarise(sum(PONDIH,na.rm = T)))
names(per_decilpc)[2]<-"Personas"

#Histograma de personas por decil de ingreso per cápita familiar como % del total
per_decilpc_s<-per_decilpc %>% mutate(perc=Personas/sum(Personas)*100)
#plot perc
ggplot(per_decilpc_s, aes(x=DECCFR, y=perc)) +
  geom_bar(stat="identity", alpha=0.5) +
  labs(title="Porcentaje de personas 19-25 años en cada decil de ingreso per cápita familiar",
       x="Decil de ingreso per cápita familiar",
       y="Porcentaje de personas") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::comma)
ggsave(paste0(year,"_con_padres_per_decilpc.png"))
#Remove rows with DECCFR==NA and DECCFR==0


#NIVEL_ED==5 Superior Universitaria Incompleta
#CH06 años cumplidos
#CH10==1 estudia actualmente
#CH11==1 pública
#CH11==2 privada
#CH12==7 universitario (nuivel mas alto que cursó o cursa)
#ESTADO==1 trabaja

#Personas por decil Ingreso per cápita familiar y estudia actualmente en la universidad
per_decilpc$estudiauniv<-dt_age %>% 
  group_by(DECCFR)  %>% 
  filter(CH10==1 & CH12 %in% c(7,8)) %>% #Universitario o posgrado 
  summarise(n = sum(PONDIH,na.rm = T)) %>% pull(n)
#Personas por decil Ingreso per cápita familiar y estudia actualmente en la universidad publica
per_decilpc$estudiaunivpub<-dt_age %>% filter(CH10==1 & CH12==7 & CH11==1)  %>% group_by(DECCFR) %>% summarise(n = sum(PONDIH,na.rm = T)) %>% pull(n)
per_decilpc$estudiatrabaja<-dt_age %>% filter(CH10==1 & CH12==7 & ESTADO==1)  %>% group_by(DECCFR) %>% summarise(n = sum(PONDIH,na.rm = T)) %>% pull(n)
per_decilpc<-per_decilpc %>% mutate(univ_perc=estudiauniv/Personas*100,univpub_perc=estudiaunivpub/Personas*100,univtrabaja_perc=estudiatrabaja/estudiauniv*100)
#per_decilpc<-per_decilpc %>% mutate(univpub_perc=estudiaunivpub/Personas*100,univpub_perc=estudiaunivpub/Personas*100)
View(per_decilpc)

# Create a new data frame with 'type' column
df1 <- transform(per_decilpc, type = "Privada", y = estudiauniv - estudiaunivpub)
df2 <- transform(per_decilpc, type = "Pública", y = estudiaunivpub)
df <- rbind(df1, df2)

# Plot
ggplot(df, aes(x=DECCFR, y=y, fill=type)) +
  geom_bar(stat="identity", alpha=0.5) +
  labs(title="Número de personas que estudian en la universidad por decil de ingreso per cápita familiar",
       x="Decil de ingreso per cápita familiar",
       y="Número de personas que estudian en la universidad") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("blue", "red"), name="Universidad", labels=c("Privada", "Pública"))
#ggsave("con_padres_universidad_absoluto.png")
ggsave(paste0(year,"_con_padres_universidad_absoluto.png"))





df1 <- transform(per_decilpc, type = "Privada", y = univ_perc - univpub_perc)
df2 <- transform(per_decilpc, type = "Pública", y = univpub_perc)
df <- rbind(df1, df2)
#now plot
ggplot(df, aes(x=DECCFR, y=y, fill=type)) +
  geom_bar(stat="identity", alpha=0.5) +
  labs(title="Porcentaje de personas que estudian en la universidad por decil de ingreso per cápita familiar",
       x="Decil de ingreso per cápita familiar",
       y="Porcentaje de personas que estudian en la universidad") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("blue", "red"), name="Universidad", labels=c("Privada", "Pública"))
#ggsave("con_padres_universidad_porcentaje.png")
ggsave(paste0(year,"_con_padres_universidad_porcentaje.png"))

graf_gw<- dt_age %>% 
  filter(CH10==1 & CH12==7) %>% 
  mutate(pub_priv = case_when(CH11 == 1~"Públicas",
                              CH11 != 1~"Privadas")) %>%
  group_by(DECCFR,pub_priv) %>% 
  summarise(n = sum(PONDIH,na.rm = T)) %>% 
  group_by(pub_priv) %>% 
  mutate(porcentaje = n/sum(n)) %>% 
  mutate(decil = factor(DECCFR,levels = 10:1))  
  

# Calculate cumulative sum beforehand
graf_gw <- graf_gw %>%
  arrange(pub_priv, rev(decil)) %>%
  group_by(pub_priv) %>%
  mutate(cumulative_porcentaje = cumsum(porcentaje)) %>%
  mutate(center=cumulative_porcentaje - porcentaje / 2)

# Plot
graf_gw %>% 
  ggplot(aes(x=pub_priv, y=porcentaje, fill=decil)) +
  geom_col() +
  geom_text(aes(label = decil, y = center), 
            color = "white", size = 8) +
  labs(title="Población estudiantil de universidades publicas y privadas según decil de ingreso per cápita familiar",
       x="Universidades",
       y="Porcentaje de personas que estudian en la universidad") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma, limits=c(0,1), breaks = seq(0,1,.1)) +
  scale_fill_viridis_d()
  #ggsave("con_padres_universidad_porcentaje_Gw.png")
  ggsave(paste0(year,"_con_padres_universidad_porcentaje_Gw.png"))

#Calculate the sum of porcentaje for decil 1-6 and 7-10, for each pub_priv value
tabla1<-graf_gw %>% 
  group_by(pub_priv) %>% 
  summarise(deciles_1_6 = sum(porcentaje[1:6]),
            deciles_7_10 = sum(porcentaje[7:10])) %>% 
  mutate( deciles_1_6 = round(deciles_1_6, 2),
         deciles_7_10 = round(deciles_7_10, 2))

names(tabla1)<-c("Universidades","deciles 1-6","deciles 7-10")
tabla1

#Create pretty table image from tabla1, with minimal table size
library(kableExtra)
kable(tabla1, "html") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = T, color = "white", background = "grey") %>%
  save_kable("con_padres_universidad_porcentaje_Gw_tabla.png")


tabla2 <- df %>% 
  group_by(type) %>%
  mutate(deciles_1_6 = ifelse(type=="Privada",sum(estudiauniv[1:6])-sum(estudiaunivpub[1:6]),sum(estudiauniv[1:6])),
         deciles_7_10 = ifelse(type=="Privada",sum(estudiauniv[7:10])-sum(estudiaunivpub[7:10]),sum(estudiauniv[7:10]))) %>%
  summarise(deciles_1_6 = first(deciles_1_6),
            deciles_7_10 = first(deciles_7_10)) %>%
  mutate(deciles_1_6 = round(deciles_1_6, 2),
         deciles_7_10 = round(deciles_7_10, 2))

names(tabla2)<-c("Universidades","deciles 1-6","deciles 7-10")
tabla2

#Create pretty table image from tabla1, with minimal table size
library(kableExtra)
kable(tabla2, "html") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = T, color = "white", background = "grey") %>%
  save_kable(file= "con_padres_universidad_absoluto_Gw_tabla.png",density=600)

#Qué porcentaje de los estudiantes universitarios podrían dejar la universidad si se les quita la gratuidad?
#Para realizar este cálculo, se asume que los deciles 7-10 no se ven afectados por la gratuidad. 
#Para estimar los estudiantes que dejarían la universidad pública se asume que la proporción de estudiantes 
# de deciles 7-10 y 1-6 pasaría a ser la misma que se observa en las privadas.
perc_total<-( tabla2[2,2]  - tabla2[1,2]/tabla2[1,3] *tabla2[2,3] ) / sum(tabla2[1:2,2:3] ) * 100
paste0("El ",round(perc_total,0),"% de los estudiantes universitarios podrían dejar la universidad si se les quita la gratuidad y las universidades públicas tuvieran precios similares a las universidades privadas")


#Cuantos de los estudiantes universitarios de deciles 1-6 podrían dejar la universidad publica?
perc_dec1_6<-( tabla2[2,2]  - tabla2[1,2]/tabla2[1,3] *tabla2[2,3] ) / sum(tabla2[1:2,2] ) * 100
paste0("El ",round(perc_dec1_6,0),"% de los estudiantes universitarios de deciles 1-6 podrían dejar la universidad si se les quita la gratuidad y las universidades públicas tuvieran precios similares a las universidades privadas")

#Calcular el porcentaje de estudiantes de deciles 1-6 por provincia
graf_gw2<- dt_age %>% 
  filter(CH10==1 & CH12==7) %>% 
  mutate(pub_priv = case_when(CH11 == 1~"Públicas",
                              CH11 != 1~"Privadas")) %>%
  group_by(DECCFR,pub_priv,REGION) %>% 
  summarise(n = sum(PONDIH,na.rm = T)) %>% 
  group_by(pub_priv,REGION) %>% 
  mutate(porcentaje = n/sum(n)) %>% 
  mutate(decil = factor(DECCFR,levels = 10:1)) %>%
  #casewhen 01 = Gran Buenos Aires      40 = NOA        41 = NEA        42 = Cuyo        43 = Pampeana       44 = Patagonia
  mutate(REGION = case_when(REGION == 1~"Gran Buenos Aires",
                            REGION == 40~"NOA",
                            REGION == 41~"NEA",
                            REGION == 42~"Cuyo",
                            REGION == 43~"Pampeana",
                            REGION == 44~"Patagonia"))
  
graf_gw3 <- graf_gw2 %>%
  mutate(decile_group = ifelse(decil %in% 1:6, "deciles_1_6", "deciles_7_10")) %>%
  group_by(pub_priv, REGION, decile_group) %>%
  summarise(n = sum(n)) %>%
  pivot_wider(names_from = decile_group, values_from = n)

tabla3<-graf_gw3 %>%
  group_by(pub_priv, REGION) %>%
  mutate(total=deciles_1_6+deciles_7_10) %>%
  mutate(deciles_1_6 = round(deciles_1_6/total*100,0),
         deciles_7_10 = round(deciles_7_10/total*100,0))

#Sacar los datos para Córdoba

#Como evolucionaron los % de la tabla 1 por años?

#Cuantos son primera generación de estudiantes universitarios?

#Recalcular deciles de jóvenes

#Qué % vive con los padres?

#Qué pasa con los que trabajan?

