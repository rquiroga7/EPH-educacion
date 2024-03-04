install.packages('eph')
library(readxl)
library(eph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
base <- get_microdata(year = 2019, 
                    trimester = c(4),
                    type = 'individual')
dt<- base
 #Convert DECCFR to numeric
dt$DECCFR <- as.numeric(as.character(dt$DECCFR))
#Filtro NA y 0 de DECCFR
dt<-dt %>% filter(DECCFR > 0 & DECCFR < 11)
#Remove duplicated people based on CODUSU+COMPONENTE
dt <- dt %>% distinct(CODUSU, COMPONENTE, NRO_HOGAR .keep_all = TRUE)

per_decil<-data.frame(dt %>% group_by(DECCFR) %>% summarise(n = sum(PONDIH,na.rm = T)))
names(per_decil)[2]<-"Personas"
per_decil$hogares<-dt %>% 
  group_by(DECCFR) %>%
  distinct(CODUSU,NRO_HOGAR,PONDIH) %>% 
  summarise(n = sum(PONDIH,na.rm = T))%>% 
  pull(n)
#Filtro sólo las personas entre 19 y 25 años
dt_age<-dt %>% filter(CH06>=19 & CH06<=25)
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
ggsave("per_decilpc.png")
#Remove rows with DECCFR==NA and DECCFR==0


#NIVEL_ED==5 Superior Universitaria Incompleta
#CH06 años cumplidos
#CH10==1 estudia actualmente
#CH11==1 pública
#CH11==2 privada
#CH12==7 universitario (nuivel mas alto que cursó o cursa)

#Personas por decil Ingreso per cápita familiar y estudia actualmente en la universidad
per_decilpc$estudiauniv<-dt_age %>% 
  group_by(DECCFR)  %>% 
  filter(CH10==1 & CH12==7) %>% 
  summarise(n = sum(PONDIH,na.rm = T)) %>% pull(n)
#Personas por decil Ingreso per cápita familiar y estudia actualmente en la universidad publica
per_decilpc$estudiaunivpub<-dt_age %>% filter(CH10==1 & CH12==7 & CH11==1)  %>% group_by(DECCFR) %>% summarise(n = sum(PONDIH,na.rm = T)) %>% pull(n)

per_decilpc<-per_decilpc %>% mutate(univ_perc=estudiauniv/Personas*100,univpub_perc=estudiaunivpub/Personas*100)
per_decilpc<-per_decilpc %>% mutate(univpub_perc=estudiaunivpub/Personas*100,univpub_perc=estudiaunivpub/Personas*100)


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
ggsave("universidad_absoluto.png")



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
ggsave("universidad_porcentaje.png")

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
  ggsave("universidad_porcentaje_Gw.png")