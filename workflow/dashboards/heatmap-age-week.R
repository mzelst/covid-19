#windowsFonts() deze alleen de eerste keer draaien om je fonts uit windows te zoeken/importeren

#leeftijdopbouw per gemeente ophalen
gemeente_leeftijd <- cbs_get_data("03759ned", Perioden = has_substring(c("2020JJ00")),RegioS = has_substring(c("GM")) ,Geslacht = has_substring(c("T001038")),
                                  BurgerlijkeStaat = has_substring(c("T001019")))

#Totalen eruit filteren + CBScode omzetten naar leeftijd
gemlftdb<-gemeente_leeftijd[!gemeente_leeftijd$Leeftijd=="10000"&!gemeente_leeftijd$Leeftijd=="22000",]
gemlftdb$Leeftijd2<-((as.numeric(gemlftdb$Leeftijd))-10000)/100

#Omzetten naar groepen uit casusdata
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2<10,"0-9","-")
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>9&gemlftdb$Leeftijd2<20,"10-19",gemlftdb$Leeftijd3)
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>19&gemlftdb$Leeftijd2<30,"20-29",gemlftdb$Leeftijd3)
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>29&gemlftdb$Leeftijd2<40,"30-39",gemlftdb$Leeftijd3)
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>39&gemlftdb$Leeftijd2<50,"40-49",gemlftdb$Leeftijd3)
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>49&gemlftdb$Leeftijd2<60,"50-59",gemlftdb$Leeftijd3)
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>59&gemlftdb$Leeftijd2<70,"60-69",gemlftdb$Leeftijd3)
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>69&gemlftdb$Leeftijd2<80,"70-79",gemlftdb$Leeftijd3)
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>79&gemlftdb$Leeftijd2<90,"80-89",gemlftdb$Leeftijd3)
gemlftdb$Leeftijd3<-ifelse(gemlftdb$Leeftijd2>89,"90+",gemlftdb$Leeftijd3)

#Lege kolom eruit gooien + kolomnaam aanpassen + oude gemeentes eruit knikkeren
gemlftdb<-gemlftdb[c(4,6,9)]
colnames(gemlftdb)<-c("Municipality_code","Aantalinwoners","Leeftijdsgroep")
gemlftdb<-gemlftdb[!is.na(gemlftdb$Aantalinwoners),]


#Casusdata lezen, datum omzetten + weeknummers
temp = tail(list.files(path = "data-rivm/casus-datasets/",pattern="*.csv.gz", full.names = T),1)
voorheat <- fread(temp)
voorheat$datum <- as.Date(parse_date_time(voorheat$Date_statistics, "Ymd"))
voorheat$week<-strftime(voorheat$datum,format = "%Y-%V")

#Aantal per week per groep tellen + leeftijdverdeling landelijk pakken
voorheat<-count(voorheat,week,Agegroup)
lftverdeling<-gemlftdb%>%group_by(Leeftijdsgroep)%>%summarise(Inwoners=sum(Aantalinwoners))
colnames(lftverdeling)[1]<-"Agegroup"

#mergen + per honderduizen berekenen
voorheat<-merge(voorheat,lftverdeling)
voorheat$phd<-round(voorheat$n*100000/voorheat$Inwoners,0)
voorheat <- voorheat[order(voorheat$week),]
weekstart <- unique(voorheat$week)[40]

#Gewenste weken subsetten
voorheat<-voorheat[voorheat$week>weekstart,]

#De plot
ggplot(voorheat,aes(week,Agegroup,fill=phd))+
  geom_tile(size=1.5,color="white")+
  geom_text(label=voorheat$phd,size=5)+
  scale_fill_gradient2(trans="sqrt",low = "lightyellow",mid="orange",midpoint = 5, 
                       high = "#f03b20")+
  ggtitle("Aantal geconstateerde besmettingen per 100.000 per week")+
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")+
  labs(title = "Geconstateerde besmettingen COVID-19",
       subtitle = "Aantal positief geteste mensen per 100.000 binnen de leeftijdsgroep ",fill=NULL,
       caption = paste("Bron data: RIVM / CBS ",Sys.Date()))+
  theme(plot.title = element_text(hjust = 0.5,size = 20,family  = "Corbel",face = "bold"),
        plot.subtitle =  element_text(hjust=0.5,color = "black", face = "italic",family  = "Corbel"),
        axis.text = element_text(size=10,color = "black",family  = "Corbel",face = "bold")) + 
  ggsave("plots/leeftijd_heatmap.png",width=15, height = 4)


git.credentials <- read_lines("git_auth.txt")
git.auth <- cred_user_pass(git.credentials[1],git.credentials[2])


repo <- init()
add(repo, path = "plots/leeftijd_heatmap.png")
commit(repo, all = T, paste0("Update heatmap age-week ",Sys.Date()))
push(repo, credentials = git.auth)
