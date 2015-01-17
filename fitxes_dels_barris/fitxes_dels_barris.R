%%%Aqui es el codi per crear el document
\documentclass{article}
\usepackage{graphicx}
\usepackage{textpos}
\usepackage{pgf}  
\usepackage{tikz} 
\usepackage{siunitx}
\usepackage[utf8]{inputenc}

\newcommand\AtPageUpperRight[1]{\AtPageUpperLeft{%
   \makebox[\paperwidth][r]{#1}}}

\title{Perfil de barri}
\date{}

\begin{document}
%\SweaveOpts{concordance=TRUE}

<<include=FALSE>>=
<<echo=FALSE>>=
library(knitr)
opts_chunk$set(
concordance=TRUE
)
@

\setlength{\parindent}{0pt}

\maketitle
\begin{tikzpicture}[remember picture,overlay]
   \node[anchor=north east,inner sep=50pt] at (current page.north east)
              {\includegraphics[width=5cm]{logo1}};
\end{tikzpicture}
%%%Ara es crea el document


%%%Aqui crida a R, pero li diu que no escrigui res al document (echo=FALSE)
<<echo=FALSE, warning=FALSE>>=
#Canviar el directori a on calgui
setwd("~/Dropbox/analisi/barris/Fitxes/Nuevas")
#setwd("~/Dropbox/guanyem/analisi/barris/Fitxes")
#setwd("~/Documents/Guanyem")
#Cridar la base de dades

d <- read.csv("bcn_tot_5.csv", sep=",", header=T, dec = ",")

# d$barri_nom <-as.character(d$barri_nom)
# d$barri_nom[7] <- "la Dreta de l\'Eixample"
# d$barri_nom[8] <-"l\'Antiga Esquerra de l\'Eixample"
# d$barri_nom[9] <- "la Nova Esquerra de l\'Eixample"
# d$barri_nom[32] <- "el Camp d\'en Grassot i Gràcia Nova"
# d$barri_nom[36] <- "la Font d\'en Fargues"
# d$barri_nom[41] <- "la Vall d\'Hebron"
# d$barri_nom[66] <-"el Camp de l\'Arpa del Clot"

d$total_aturats <- ifelse(d$total_aturats<100, d$total_aturats2 <- d$total_aturats*1000, d$total_aturats2 <- d$total_aturats)

#Definir el barri
barri <- as.data.frame(subset(d, barris==x))

options(digits=3)

#Abstencio generals
absG11 <- barri$abstencio_g11/barri$electors_g11*100
absG11m <- sum(d$abstencio_g11)/sum(d$electors_g11)*100

#Abstencio locals
absL11 <-  (barri$electors_l11 - barri$votants_l11)/barri$electors_l11*100
absL11m <- (sum(d$electors_l11) - sum(d$votants_l11))/sum(d$electors_l11)*100

#Vot a ciu a les locals
ciuL11 <-  barri$ciu_l11 / barri$votants_l11*100
ciuL11m <- sum(d$ciu_l11)/sum(d$votants_l11)*100

#Vot a psc a les locals
pscL11 <-  barri$psc_l11 / barri$votants_l11*100
pscL11m <- sum(d$psc_l11)/sum(d$votants_l11)*100

#Vot a pp a les locals
ppL11 <-  barri$pp_l11 / barri$votants_l11*100
ppL11m <- sum(d$pp_l11)/sum(d$votants_l11)*100

#Vot a icv a les locals
icvL11 <-  barri$icv_l11 / barri$votants_l11*100
icvL11m <- sum(d$icv_l11)/sum(d$votants_l11)*100

#Vot a erc a les locals
ercL11 <-  barri$erc_l11 / barri$votants_l11*100
ercL11m <- sum(d$erc_l11)/sum(d$votants_l11)*100

#Vot a ciu a les europees
ciuE14 <-  barri$ciu_eu14 / barri$votants_eu14*100
ciuE14m <- sum(d$ciu_eu14)/sum(d$votants_eu14)*100

#Vot a psc a les europees
pscE14 <-  barri$psc_eu14 / barri$votants_eu14*100
pscE14m <- sum(d$psc_eu14)/sum(d$votants_eu14)*100

#Vot a pp a les europees
ppE14 <-  barri$pp_eu14 / barri$votants_eu14*100
ppE14m <- sum(d$pp_eu14)/sum(d$votants_eu14)*100

#Vot a icv a les europees
icvE14 <-  barri$icv_eu14 / barri$votants_eu14*100
icvE14m <- sum(d$icv_eu14)/sum(d$votants_eu14)*100

#Vot a erc a les europees
ercE14 <-  barri$erc_eu14 / barri$votants_eu14*100
ercE14m <- sum(d$erc_eu14)/sum(d$votants_eu14)*100

#Vot a Podemos a les europees
podE14 <-  barri$pocem_eu14 / barri$votants_eu14*100
podE14m <- sum(d$pocem_eu14)/sum(d$votants_eu14)*100

#Preu habitatge compra al 2013
preucompra <-  as.numeric((barri$preum2_2013))                 
preucompram <- mean(as.numeric(d$preum2_2013), na.rm = T)

#Preu habitatge lloguer
preulloguer <-  as.numeric(barri$preulloguerm2_2011)
preulloguerm <-  mean(as.numeric(d$preulloguerm2_2011), na.rm = T)

#Persones per llar
persones_per_llar <-  (barri$persones_per_llar)                 
persones_per_llarm <- mean(d$persones_per_llar, na.rm = T)

#Densitat
densitat <-  (barri$densitat)
densitatm <-  mean(d$densitat, na.rm = T)

#Caracter??stiques de les llars MIRAR DENOMINADOR NUMERO DE LLARS SI ES CORRECTE

#Compra pagat
propi  <-  (barri$propi_compra_pagat) / barri$llars*100
propim  <-  (sum(d$propi_compra_pagat, na.rm = T)) / sum(d$llars, na.rm = T)*100

#Hipotecat
hipotecat  <-  (barri$propi_compra_hipoteca) / barri$llars*100
hipotecatm  <-  sum(d$propi_compra_hipoteca, na.rm = T) / sum(d$llars, na.rm = T)*100

#Lloguer
lloguer  <-  (barri$llogat) / barri$llars*100
lloguerm  <-  sum(d$llogat, na.rm = T) / sum(d$llars, na.rm = T)*100

#Sense calefacci??
nocale  <-  (barri$sense_calefaccio) / barri$llars*100
nocalem  <-  (sum(d$sense_calefaccio, na.rm = T)) / sum(d$llars, na.rm = T)*100

#Sense internet
nointernet  <-  (barri$sense_internet) / barri$llars*100
nointernetm  <-  sum(d$sense_internet, na.rm = T) / sum(d$llars, na.rm = T)*100

#Poblaci??
poblacio <-  (barri$poblacio)
poblaciom <-  sum(d$poblacio, na.rm = T)

#Nascuts a catalunya a la ciutat 2: 954305
nascattotsm <-  (sum(d$catalunya))

#Nascuts a espanya fora de catalunya a la ciutat: 306318
#nasespbcn <- sum(d$andalusia + d$arago + d$asturies + d$balears + d$canaries + #d$cantabria + d$castellalamanxa + d$castellalleo + d$valencia + d$extremadura + #d$galicia + d$madrid + d$murcia + d$navarra + d$pais_basc + d$rioja + d$ceuta_melilla)

#Nascuts a espanya a la ciutat 2: 1.330.597, hi ha una difer??ncia important
nasesptotsm <-  (sum(d$espanya))

#Nascuts a l'extranger
nasesttotsm <-  (sum(d$estranger))

#Total: No quadra exactament!
#nasesptotsm + nasesttotsm
#poblaciom          

nascat <-   barri$catalunya / barri$poblacio*100
nasesp <-   (barri$espanya - barri$catalunya) / barri$poblacio*100
nasest <-   barri$estranger / barri$poblacio*100

#nascat+nasesp+nasest

nascatm <-   sum(d$catalunya) / sum(d$poblacio)*100
nasespm <-   (sum(d$espanya) - sum(d$catalunya)) / sum(d$poblacio)*100
nasestm <-   sum(d$estranger) / sum(d$poblacio)*100

#nascatm+nasespm+nasestm

#Estudis
edubaix <- (barri$sense_estudis + barri$primaris)/barri$poblacio*100
edusec <- (barri$second_inferior + barri$second_superior)/barri$poblacio*100
edusup <- (barri$universitaris)/barri$poblacio*100

edubaixm <- ((sum(d$sense_estudis) + sum(d$primaris)) / sum(d$poblacio))*100
edusecm <- (sum(d$second_inferior) + sum(d$second_superior)) / sum(d$poblacio)*100
edusupm <- (sum(d$universitaris)) / sum(d$poblacio)*100

#Atur
#Dades EPA 4rt trimestre 2013 usades per calcular factors de ponderaci?? 
#Aturats: 139.500 
#Poblaci??: 1.611.000
#Poblaci?? activa: 820.000 (51%)
#Atur total registrat: 100.933 (pondero per 1.38)

barri$activa <-  barri$poblacio*0.51
d$activa <-  d$poblacio*0.51
#p <- sum(d$poblacio)

#Pondero a l'alca perque la EPA revela molt m??s d'atur que l'estadistica oficial
#PROBLEMA AQUI
atur  <-  (barri$total_aturats*1.38) / barri$activa*100
aturm  <-  (sum(d$total_aturats)*1.38) / sum(d$activa)*100

#Renda familiar bruta disponible 
rfbc <-  (barri$rfbc2012)

#Correlacio renda abstencio a les locals
d$absL11m <- (d$electors_l11 - d$votants_l11)/ d$electors_l11*100
d$nasest <-   d$estranger / d$poblacio*100
d$nasesp <-   d$espanya / d$poblacio*100

m1 <- lm(absL11m ~ rfbc2012, data=d)
m2 <- lm(absL11m ~ rfbc2012 + nasest + nasesp, data=d)

#Envelliment i dependencia demografica
dep <-  barri$dependencia_dem
depm <-  mean(d$dependencia_dem)

menys14 <-  barri$e0_14_anys / barri$poblacio*100
menys14m <-  (sum(d$e0_14_anys) / sum(d$poblacio))*100

mes65 <-  barri$e65anys_i_mes / barri$poblacio*100
mes65m <-  (sum(d$e65anys_i_mes) / sum(d$poblacio))*100

#Demografia
nat <-  barri$taxa_natalitat_2014 
natm <-  mean(d$taxa_natalitat_2014) 

mort <-  barri$taxa_mortalitat_2014 
mortm <-  mean(d$taxa_mortalitat_2014) 

imm <-  barri$taxa_immigracio_2014 
immm <-  mean(d$taxa_immigracio_2014) 

emi <-  barri$taxa_emigracio_2014 
emim <-  mean(d$taxa_emigracio_2014) 

#Mateix format: 

@

\section*{\Sexpr{barri$barri_nom}}

\bigskip

\subsection*{Dades pol\'itiques}

\begin{tabular}{ l c c }
\hline
  & Al barri & A la ciutat \\ 
 \hline
 Abstenci\'o a les generals de 2011 & \Sexpr{round(absG11,1)} & \Sexpr{round(absG11m,1)} \\  
 \hline
 Abstenci\'o a les locals de 2011 & \Sexpr{round(absL11,1)} & \Sexpr{round(absL11m,1)} \\  
 Vot a CIU a les locals de 2011 & \Sexpr{round(ciuL11,1)} & \Sexpr{round(ciuL11m,1)} \\  
 Vot a PSC a les locals de 2011 & \Sexpr{round(pscL11,1)} & \Sexpr{round(pscL11m,1)} \\  
 Vot a PP a les locals de 2011 & \Sexpr{round(ppL11,1)} & \Sexpr{round(ppL11m,1)} \\  
 Vot a ICV a les locals de 2011 & \Sexpr{round(icvL11,1)} & \Sexpr{round(icvL11m,1)} \\  
 Vot a ERC a les locals de 2011 & \Sexpr{round(ercL11,1)} & \Sexpr{round(ercL11m,1)} \\  
 \hline
 Vot a CIU a les europees de 2014 & \Sexpr{round(ciuE14,1)} & \Sexpr{round(ciuE14m,1)} \\  
 Vot a PSC a les europees de 2014 & \Sexpr{round(pscE14,1)} & \Sexpr{round(pscE14m,1)} \\  
  Vot a PP a les europees de 2014 & \Sexpr{round(ppE14,1)} & \Sexpr{round(ppE14m,1)} \\  
   Vot a ICV a les europees de 2014 & \Sexpr{round(icvE14,1)} & \Sexpr{round(icvE14m,1)} \\  
    Vot a ERC a les europees de 2014 & \Sexpr{round(ercE14,1)} & \Sexpr{round(ercE14m,1)} \\  
     Vot a Podemos a les europees de 2014 & \Sexpr{round(podE14,1)} & \Sexpr{round(podE14m,1)} \\  
\hline
\end{tabular}

\medskip

\subsection*{Dades socio-econ\`omiques}
\begin{tabular}{ l c c }
\hline
  & Al barri & A la ciutat \\ 
  \hline
 Atur (estimaci{\'o}) & \Sexpr{round(atur)} & \Sexpr{round(aturm)} \\  
Renda familiar bruta disponible ({\'i}ndex en qu{\`e} Barcelona=100) & \Sexpr{round(rfbc,1)} & 100 \\  
 \hline
\end{tabular}

Nota: Disposem a nivell de barri de dades sobre la poblaci{\'o} i sobre el nombre d'aturats registrats. Per calcular la taxa d'atur cal saber la taxa d'activitat (no disponible per barris) i el nombre d'aturats real (m{\'e}s alt que els registrats). Per tant, cal fer una estimaci{\'o} aproximada. Usant com a refer{\`e}ncia dades de la EPA del 4rt trimestre de 2013, hem multiplicat la poblaci{\'o} total per 0.51 per estimar la poblaci{\'o} activa i hem multiplicat l'atur registrat per 1.3 per estimar la poblaci{\'o} aturada.
La renda familiar bruta disponible per barris {\'e}s un {\'i}ndex en base a 100 que calcula el servei d'estad{\'i}stica de l'Ajuntament.

\medskip

\subsection*{Habitatge}

\begin{tabular}{ l c c }
\hline
  & Al barri & A la ciutat \\ 
  \hline
Preu del metre quadrat 2013 & \Sexpr{round(preucompra,0)} & \Sexpr{round(preucompram,0)} \\  
 Preu del metre quadrat de lloguer 2011 & \Sexpr{round(preulloguer,1)} & \Sexpr{round(preulloguerm,1)} \\  
 \hline
  Persones per llar & \Sexpr{round(persones_per_llar,1)} & \Sexpr{round(persones_per_llarm,1)} \\ 
 Densitat (habitants per hect{\`a}rea) & \Sexpr{round(densitat,0)} & \Sexpr{round(densitatm,0)} \\
 \hline
Llars amb titularitat (\%)  & \Sexpr{round(propi,0)} & \Sexpr{round(propim,0)} \\  
Llars de lloguer (\%) & \Sexpr{round(lloguer,0)} & \Sexpr{round(lloguerm,0)} \\ 
Llars comprades hipotecades (\%) & \Sexpr{round(hipotecat,0)} & \Sexpr{round(hipotecatm,0)} \\  
   \hline
   Llars sense calefacci{\'o}  (\%) & \Sexpr{round(nocale,0)} & \Sexpr{round(nocalem,0)} \\  
 Llars sense internet (\%) & \Sexpr{round(nointernet,0)} & \Sexpr{round(nointernetm,0)} \\  
 \hline
\end{tabular}

Nota: Les llars per tinen\c{c}a no sumen 100 perqu{\`e} hi ha altres tipus com cedides o per her{\`e}ncia que es computen per separat. Les dades sobre percentatges de llars contenen error perqu{\`e} sovint sembla que hi ha sobre-estimaci{\'o} del nombre de llars, el denominador.

\medskip

\subsection*{Proced{\`e}ncia}

\begin{tabular}{ l c c }
\hline
  & Al barri & A la ciutat \\ 
  \hline
 Poblaci{\'o} 2013 & \Sexpr{round(poblacio)} & \Sexpr{round(poblaciom)} \\  
 Nascuts a Catalunya (\%) & \Sexpr{round(nascat)} & \Sexpr{round(nascatm)} \\ 
 Nascuts a Espanya fora de Catalunya (\%) & \Sexpr{round(nasesp)} & \Sexpr{round(nasespm)} \\  
 Nascuts a l'extranger (\%) & \Sexpr{round(nasest)} & \Sexpr{round(nasestm)} \\ 
  \hline
\end{tabular}

Nota: Els percentatges no sumen 100 exactament per difer{\`e}ncies entre les fonts. S{\'o}n estimacions aproximades. 

\medskip

 \subsection*{Dades socio-demogr{\`a}fiques}

\begin{tabular}{ l c c }
\hline
  & Al barri & A la ciutat \\ 
  \hline
 Taxa de natalitat 2014 & \Sexpr{round(nat)} & \Sexpr{round(natm)} \\  
 Taxa de mortalitat 2014 & \Sexpr{round(mort)} & \Sexpr{round(mortm)} \\  
 Taxa bruta d'immigraci{\'o} 2014 & \Sexpr{round(imm)} & \Sexpr{round(immm)} \\ 
 Taxa bruta d'emigraci{\'o} 2014 & \Sexpr{round(emi)} & \Sexpr{round(emim)} \\  
  \hline
 {\'I}ndex de depend{\`e}ncia demogr{\`a}fica & \Sexpr{round(dep)} & \Sexpr{round(depm)} \\  
 Menors de 14 anys & \Sexpr{round(menys14)} & \Sexpr{round(menys14m)} \\  
 Majors de 65 anys & \Sexpr{round(mes65)} & \Sexpr{round(mes65m)} \\  
 \hline
\end{tabular}

{\'I}ndex de depend{\`e}ncia demogr{\`a}fica {\'e}s un {\'i}ndex calculat pel servei d'Estad{\'i}stica de l'Ajuntament en base a la seguent f{\'o}rmula: (Poblaci{\'o} 65 i m??s/poblaci{\'o} 16-64)x100

Les taxes s{\'o}n el nombre de naixements, defuncions, i migracions per cada 1000 habitants.

\medskip

\subsection*{Nivell educatiu}

\begin{tabular}{ l c c }
\hline
  & Al barri & A la ciutat \\ 
  \hline
 Estudis primaris (\%) & \Sexpr{round(edubaix)} & \Sexpr{round(edubaixm)} \\  
 Estudis secundaris (\%) & \Sexpr{round(edusec)} & \Sexpr{round(edusecm)} \\ 
 Estudis universitaris (\%) & \Sexpr{round(edusup)} & \Sexpr{round(edusupm)} \\  
  \hline
\end{tabular}

<<echo=FALSE>>=
plot <- plot(d$preulloguerm2_2011, d$preum2_2013, type="n",
     xlab = "Preu lloguer",     			
     ylab = "Preu compra")
text(d$preulloguerm2_2011, d$preum2_2013, d$barri)
@ 

\bigskip
\bigskip


Nota: Moltes estad{\'i}stiques oficials es calculen en base a enquestes que no s{\'o}n representatives sovint ni tant sols a nivell de districte. Per exemple, la taxa d atur, que {\'e}s el percentatge d aturats sobre la poblaci{\'o} activa, no es pot estimar a nivell de barri perqu{\`e} la poblaci{\'o} activa es calcula en base a la EPA que nom{\'e}s {\'e}s representativa a nivell de ciutat. Nom{\'e}s es pot fer una aproximaci{\'o} en base al nombre d aturats i el nombre de persones en edat de treballar. Un altre exemple {\'e}s la taxa de risc de pobresa est{\`a} nom{\'e}s a nivell de ciutat perqu{\`e} es calcula en base a l enquesta de condicions de vida.

\end{document}



