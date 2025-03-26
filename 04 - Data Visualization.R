# ORGANIZAR ESTE SCRIPT####
## GRAPHICS -----------------------------------------------------------------

### Scatter Plot ####
# aes x (Numeric), y (numeric), col (Categorical)
# Note each category has its shape
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df) + # DATA
  aes(x=Edad, y=EstanciaHospitalaria, col=Sexo, shape=Sexo) + # AESTHETICS
  geom_point() + # GEOMETRICS
  geom_smooth (position = "identity") + # GEOMETRICS
  ggtitle("Ocupación de los Pacientes que consultaron por Intento de Suicidio al Hospital Susana López de Valencia 2019", subtitle = "Rojo (Femenino) - Azul (Masculino)") +
  xlab("Edad") +
  ylab("Estancia Hospitalaria") +
  scale_alpha() + # SCALES
  scale_fill_brewer(palette = "Set1")+
  stat_smooth(NULL) + # STATISTICAL TRANSFORMATIONS
  facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))

theme(panel.border = element_rect(fill = "transparent", color = "#72efdd",linewidth = 4))+
  theme(panel.grid = element_line(color = "#3a0ca3",size = 1,linetype = 3))+
  theme(plot.background = element_rect(color = 1,size = 1),
        plot.margin = margin(t = 20,r = 50,b = 40,l = 30))+
  theme(plot.title = element_text(face = "bold", hjust = NULL, size = 16, color = "black")) +
  theme(plot.subtitle = element_text(face="italic", size = 14, color = "black")) +
  theme(axis.title.x=element_text(size=12)) +
  theme(axis.text.x=element_text(size=10)) +
  theme(axis.title.y=element_text(size=12)) +
  theme(axis.text.y=element_text(size=10)) +
  theme(axis.ticks.x=element_line(linewidth = 0.5)) +
  theme(axis.ticks.y=element_line(linewidth = 0.5)) +
  theme(axis.ticks.length=unit(0.1,"cm"))


# Create subplots using gridExtra    
# Remember to create the 4 grids by first, Second, Third, Fourth <- 
library(gridExtra)
grid.arrange(first,second,third,fourth, nrow = 2)

### Q-Q Plot Imagn 1: Distribución de la Edad ####
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df) + # DATA
  aes(sample=Edad, colour="Edad") + # AESTHETICS
  geom_qq(geom="path", position="identity")+ # GEOMETRICS
  geom_qq_line(geom="path", position="identity") + # GEOMETRICS
  ggtitle("Imagen 1: Distribución de la Edad de los Pacientes que consultaron por Intento de Suicidio", subtitle = "Hospital Susana López de Valencia 2019")+
  xlab("Theoretical Quantile") +
  ylab("Edad (Años)") + 
  
  stat_qq(color="red4") +
  stat_qq_line(color="blue", linewidth=1.50) + # STATISTICAL TRANSFORMATIONS
  
  scale_alpha()+
  scale_fill_brewer(palette = "Set1") +
  
  theme_grey()  +#VISUAL THEMES
  theme(plot.title = element_text(face="bold", size = 15)) +
  theme(plot.subtitle = element_text(size = 13)) +
  theme(panel.background = element_rect(fill = "#72efdd"))

### Barplot ####
# Remember to erase the coord_flip, fill and facet_grid.
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df) + # DATA
  aes(x=Edad, y=EstanciaHospitalaria, col=Sexo, shape=Sexo) + # AESTHETICS
  geom_point() + # GEOMETRICS
  geom_smooth (position = "identity") + # GEOMETRICS
  coord_flip()+ #Horizontal barplot
  ggtitle("Ocupación de los Pacientes que consultaron por Intento de Suicidio al Hospital Susana López de Valencia 2019", subtitle = "Rojo (Femenino) - Azul (Masculino)") +
  xlab("Edad") +
  ylab("Estancia Hospitalaria") +
  scale_alpha() + # SCALES
  scale_fill_brewer(palette = "Set1")+
  stat_smooth(NULL) + # STATISTICAL TRANSFORMATIONS
  facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))

### Barplot Imagen 2: Características Socio-Demográficas de los Pacientes con Intento de Suicidio HSLV 2019 #### 
par(ask=TRUE)
ggplot2::ggplot(dat.df) + #DATA
  aes(x=Escolaridad, fill=EstadoCivil) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  ggtitle("Imagen 2: Características Socio-Demográficas de los Pacientes con Intento de Suicidio", subtitle =  "Hospital Susana López de Valencia 2019") +
  xlab("Escolaridad") +
  ylab("Número de Pacientes") +
  labs(caption = "NA: Sin información")+
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  facet_grid(Procedencia ~ Sexo,labeller = "label_both") + #FACETS
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(plot.caption = element_text(hjust=0))

gridExtra::grid.arrange(
  BarSexo,
  BarEstadoCivil,
  BarProcedencia,
  BarEstrato,
  ncol=2, nrow=2,
  top = textGrob("Imagen 2: Sexo, Estado Civil, Procedencia y Estrato Socio-Económico de los Pacientes con Intento de Suicidio HSLV 2019",
                 gp=gpar(fontsize=15,
                         font=1)))


### Barplot Imagen 3: Características Socio-Demográficas de los Pacientes con Intento de Suicidio HSLV 2019 #### 
par(ask=TRUE)
ggplot2::ggplot(CEstrato) + #DATA
  aes(x=Ocupacion, fill=Estrato) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  ggtitle("Imagen 3: Características Socio-Demográficas de los Pacientes con Intento de Suicidio", subtitle =  "Hospital Susana López de Valencia 2019") +
  xlab("Ocupación") +
  ylab("Número de Pacientes") +
  labs(caption = "NA: Sin información") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  theme(plot.title = element_text(face="bold", size = 15)) +
  theme(plot.subtitle = element_text(size = 13)) +
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.caption = element_text(hjust=0))

gridExtra::grid.arrange(
  BarSexo,
  BarEstadoCivil,
  BarProcedencia,
  BarEstrato,
  ncol=2, nrow=2,
  top = textGrob("Imagen 2: Sexo, Estado Civil, Procedencia y Estrato Socio-Económico de los Pacientes con Intento de Suicidio HSLV 2019",
                 gp=gpar(fontsize=15,
                         font=1)))


### Barplot Imagen 5: Características Clínicas de los Pacientes con Intento de Suicidio HSLV 2019 #### 
par(ask=TRUE)
ggplot2::ggplot(CIntentosPrevios) + #DATA
  aes(x=IntentosPrevios, fill=Subregistro) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  ggtitle("Imagen 5: Características Clínicas de los Pacientes con Intento de Suicidio", subtitle = "Hospital Susana López de Valencia 2019") +
  xlab("Intentos de Sucidio Previos") +
  ylab("Número de Pacientes") +
  labs(caption = FALSE) +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  facet_grid(TieneEnfermedadFisica ~ TieneEnfermedadMental,labeller = "label_both") + #FACETS
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  theme(plot.caption = element_text(hjust=0))

BarEnfermedadFisica <- ggplot2::ggplot(dat.df) + #DATA
  aes(x=EnfermedadFisica, fill=NULL) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  coord_flip()+
  ggtitle(NULL) +
  xlab("Antecedentes Médicos") +
  ylab("Número de Pacientes") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

BarEnfermedadMental <- ggplot2::ggplot(dat.df) + #DATA
  aes(x=EnfermedadMental, fill=NULL) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  coord_flip()+
  ggtitle(NULL) +
  xlab("Antecedentes de Enfermedad Mental") +
  ylab("Número de Pacientes") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

gridExtra::grid.arrange(
  BarIntentosPrevios,
  BarEnfermedadFisica,
  BarEnfermedadMental,
  ncol=3, nrow=1,
  top = textGrob("Imagen 7: Características Clínicas de los Pacientes con Intento de Suicidio HSLV 2019",
                 gp=gpar(fontsize=15,
                         font=1)))


### Barplot Imagen 6: Características Clínicas de los Pacientes con Intento de Suicidio HSLV 2019 #### 
par(ask=TRUE)
BarEnfermedadFisica <- ggplot2::ggplot(CIntentosPrevios) + #DATA
  aes(x=EnfermedadFisica, fill=IntentosPrevios) + #AESTHETICS 
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  coord_flip()+
  ggtitle(NULL) +
  xlab("Antecedentes Médicos") +
  ylab("Número de Pacientes") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

BarEnfermedadMental <- ggplot2::ggplot(CIntentosPrevios) + #DATA
  aes(x=EnfermedadMental, fill=IntentosPrevios) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  coord_flip()+
  ggtitle(NULL) +
  xlab("Antecedentes de Enfermedad Mental") +
  ylab("Número de Pacientes") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

gridExtra::grid.arrange(
  BarEnfermedadFisica,
  BarEnfermedadMental,
  ncol=2, nrow=1,
  top = textGrob("Imagen 6: Características Clínicas de los Pacientes con Intento de Suicidio \nHospital Susana López de Valencia 2019",
                 gp=gpar(fontsize=15,
                         font=2)))


### Barplot Imagen 7: Características Del Evento en los Pacientes con Intento de Suicidio HSLV 2019 #### 
par(ask=TRUE)
ggplot2::ggplot(dat.df) + #DATA
  aes(x=Mes, fill=Dia) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  ggtitle("Imagen 7: Características del evento en pacientes con intento de suicidio", subtitle = "Hospital Susana López de Valencia 2019") +
  xlab("Dia y Mes") +
  ylab("Número de Pacientes") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  theme(plot.title = element_text(face="bold", size = 15)) +
  theme(plot.subtitle = element_text(size = 13)) +
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

gridExtra::grid.arrange(
  BarIntentosPrevios,
  BarEnfermedadFisica,
  BarEnfermedadMental,
  ncol=1, nrow=1,
  top = textGrob("Imagen 7: Características Clínicas de los Pacientes con Intento de Suicidio HSLV 2019",
                 gp=gpar(fontsize=15,
                         font=1)))

### Barplot Imagen 8: Características Del Evento en los Pacientes con Intento de Suicidio HSLV 2019 #### 
par(ask=TRUE)
ggplot2::ggplot(CPremeditacion) + #DATA
  aes(x=Motivos, fill=Premeditacion) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  coord_flip()+
  ggtitle("Imagen 8: Características del evento en pacientes con intento de suicidio", subtitle = "Hospital Susana López de Valencia 2019") +
  xlab("Motivos") +
  ylab("Número de Pacientes") +
  labs(caption = "NA: Sin información") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  theme(plot.title = element_text(face="bold", size = 15)) +
  theme(plot.subtitle = element_text(size = 13)) +
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  theme(plot.caption = element_text(hjust=0))



### Barplot Imagen 9: Características Del Evento en los Pacientes con Intento de Suicidio HSLV 2019 #### 
par(ask=TRUE)
ggplot2::ggplot(CPlanificacion) + #DATA
  aes(x=Metodo, fill=Planificacion) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  coord_flip()+
  ggtitle("Imagen 9: Características del evento en pacientes con intento de suicidio", subtitle = "Hospital Susana López de Valencia 2019") +
  xlab("Método empleado") +
  ylab("Número de Pacientes") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

### Barplot Imagen 10: Características de la atención del evento en los pacientes con Intento de Suicidio HSLV 2019 #### 
par(ask=TRUE)
ggplot2::ggplot(dat.df) + #DATA
  aes(x=Riesgo, fill=PersistenciaIdeacion) + #AESTHETICS
  geom_bar(stat="count",
           position = "stack") + # GEOMETRICS
  ggtitle("Imagen 10: Características de la atención y seguimiento del evento en los pacientes con Intento de Suicidio", subtitle =  "Hospital Susana López de Valencia 2019") +
  xlab("Riesgo") +
  ylab("Número de Pacientes") +
  labs(caption = "NA: Sin información") +
  scale_alpha()+
  scale_fill_brewer(palette = "Set1")+
  facet_grid(AltaVoluntaria ~ FueRemitido,labeller = "label_both") + #FACETS
  theme_grey() + #VISUAL THEMES
  theme(plot.title = element_text(face="bold", size = 15)) +
  theme(plot.subtitle = element_text(size = 13)) +
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)) +
  theme(plot.caption = element_text(hjust=0))

gridExtra::grid.arrange(
  BarSexo,
  BarEstadoCivil,
  BarProcedencia,
  BarEstrato,
  ncol=2, nrow=2,
  top = textGrob("Imagen 2: Sexo, Estado Civil, Procedencia y Estrato Socio-Económico de los Pacientes con Intento de Suicidio HSLV 2019",
                 gp=gpar(fontsize=15,
                         font=1)))


### Histogram ####
# Remember to erase the coord_flip, fill and facet_grid.
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df) + #DATA
  aes(x=Edad, fill=Sexo, after_stat(count)) + #AESTHETICS
  geom_histogram(binwidth = 2,
                 position = "identity",
                 alpha = 0.75,
                 na.rm = TRUE)+
  ggtitle("Edad de los Pacientes que consultaron por Intento de Suicidio", subtitle = "Hospital Susana López de Valencia 2019") +
  xlab("Edad") +
  ylab("Número de Pacientes") +
  scale_x_continuous(name = "Edad (Años)",
                     breaks = seq(10, 60, 5),
                     limits = c(10, 60))+
  scale_y_continuous(name = "Número de Pacientes")+
  scale_fill_brewer(palette = "Set1")
facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
  theme_grey() + #VISUAL THEMES
  theme(panel.background = element_rect(fill = "#72efdd"))+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

### BoxPlot ####
library(ggplot2) # Load the ggplot2 package.
library(ggthemes) # Load the ggthemes package.
library(ggmosaic) # Load the ggmosaic package.
library(gridExtra) # Load the gridExtra package.
library(grid) # Load the grid package.
library(scales) # Load the scales package.

par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
ggplot2::ggplot(dat.df, #DATA
                aes(x=Sexo, y=NumeroIntentosPrevios, fill=Sexo)) + #AESTHETICS
  geom_boxplot(position = "dodge") +
  ggtitle("Número de Intentos Previos de Acuerdo al Sexo", subtitle = "Hospital Susana López de Valencia 2019") +
  xlab("Sexo") +
  ylab("Número de Intentos Previos") +
  scale_alpha() + # SCALES
  scale_fill_brewer(palette = "Set1")+
  stat_summary(fun.y=mean, geom="point", shape=1, size=6, col="black") +
  theme_grey()+  #VISUAL THEMES
  theme(legend.position="none")+ # No legend
  facet_grid(Estrato ~ Procedencia,labeller = "label_both") + #FACETS
  
  ### Boxplot Imagen 4: Edad de los pacientes....  ####
par(ask=TRUE)
par(mfrow=c(1,1)) # To put several graphics on the same image
BoxEdad <- ggplot2::ggplot(dat.df, #DATA
                           aes(x=Edad)) + #AESTHETICS
  geom_boxplot(position = "dodge") +
  ggtitle(NULL) +
  xlab("Edad (Años)") +
  scale_alpha() + # SCALES
  scale_fill_brewer(palette = "Set1")+
  theme_grey()+  #VISUAL THEMES
  theme(legend.position="none")

BoxNumeroIntentos <- ggplot2::ggplot(dat.df, #DATA
                                     aes(x=NumeroIntentosPrevios)) + #AESTHETICS
  geom_boxplot(position = "dodge") +
  ggtitle(NULL) +
  xlab("Número de Intentos Previos") +
  scale_alpha() + # SCALES
  scale_fill_brewer(palette = "Set1")+
  theme_grey()+  #VISUAL THEMES
  theme(legend.position="none")

BoxEstanciaHospitalaria <- ggplot2::ggplot(dat.df, #DATA
                                           aes(x=EstanciaHospitalaria)) + #AESTHETICS
  geom_boxplot(position = "dodge") +
  scale_x_continuous(limits = c(0, 32), breaks = seq(0, 32, 3)) +
  ggtitle(NULL) + 
  xlab("Días de Estancia Hospitalaria") +
  scale_alpha() + # SCALES
  scale_fill_brewer(palette = "Set1")+
  theme_grey()+  #VISUAL THEMES
  theme(legend.position="none")

gridExtra::grid.arrange(
  BoxEdad,
  BoxNumeroIntentos,
  BoxEstanciaHospitalaria,
  ncol=2, nrow=2,
  top = textGrob("Imagen 4: Edad, Número de Intentos Previos y Estancia Hospitalaria de los Pacientes con Intento de Suicidio \nHospital Susana López de Valencia 2019",
                 gp=gpar(fontsize=15,
                         font=2)))
### Boxplot de la Edad ####


#### Complex Plots 1 var. Num * 2 var. Cat ####
ggplotEdad1 <-
  ggplot2::ggplot(dat.df,
                  aes(x=Edad)) +
  geom_density(alpha = 0.5) +
  ggtitle("Edad Pacientes con Intento de Suicidio HSLV 2019") +
  xlab("Edad (Años)") +
  ylab("Número de Pacientes") +
  theme_few() +   # Review the ggthemes package
  theme(legend.position="none") # No legend
ggplotEdad2 <-
  ggplot2::ggplot(dat.df,
                  aes(x=Sexo, y=Edad, fill=Sexo)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=1, size=6,
               col="black") +
  facet_grid( ~ as.factor(Procedencia)) +
  ggtitle("Edad Pacientes con Intento de Suicidio Hospital 
          Susana López de Valencia, Popayán - Cauca,  2019") +
  xlab("Sexo") +
  ylab("Edad") +
  scale_y_continuous(labels=scales::comma, limits=c(0,60),
                     breaks=scales::pretty_breaks(n = 3)) +
  theme_stata() + # Review the ggthemes package
  theme(legend.position="none") # No legend
par(ask=TRUE); gridExtra::grid.arrange(
  ggplotEdad1,
  ggplotEdad2, ncol=1)

#### Very Complex Density Facet 1 var. Numer * 2 var. Cat ####
DensityFacetSexoEdad <-
  ggplot2::ggplot(dat.df,
                  aes(x=Edad)) +
  geom_density(col="red", lwd=2) +
  facet_grid(. ~ Sexo) +
  ggtitle("Sexo por Edad (Años)\n") +
  labs(x = "\nEdad (Años)", y = "Density\n") +
  scale_x_continuous(labels=scales::comma, limits=c(10,60),
                     breaks=seq(10,60, by=25)) +
  theme_bw()
DensityFacetProcedenciaEdad <- # Procedencia (two breakouts) por Edad
  ggplot2::ggplot(dat.df,
                  aes(x=Edad)) +
  geom_density(col="red", lwd=2) +
  facet_grid(. ~ Procedencia) +
  ggtitle("Procedencia por Edad (Años)\n") +
  labs(x = "\nEdad (Años)", y = "Density\n") +
  scale_x_continuous(labels=scales::comma, limits=c(10,60),
                     breaks=seq(10,60, by=25)) +
  theme_bw()
BoxplotSexoEdad <- # Sexo (two breakouts) Por Edad
  ggplot(dat.df,
         aes(x=Sexo, y=Edad, fill=Sexo)) +
  geom_boxplot() +
  ggtitle("Sexo por Edad (Años)\n") +
  labs(x = "\nSexo", y = "Edad (Años)\n") +
  scale_y_continuous(labels=scales::comma, limits=c(10,50),
                     breaks=seq(10,50, by=25)) +
  theme_bw()
BoxplotProcedenciaEdad <- # Procedencia (two breakouts) por Edad
  ggplot(dat.df,
         aes(x=Procedencia, y=Edad, fill=Procedencia)) +
  geom_boxplot() +
  ggtitle("Procedencia por Edad (Años)\n") +
  labs(x = "\nProcedencia", y = "Edad (Años)\n") +
  scale_y_continuous(labels=scales::comma, limits=c(10,50),
                     breaks=seq(10,50, by=25)) +
  theme_bw()
gridExtra::grid.arrange(
  DensityFacetSexoEdad,
  DensityFacetProcedenciaEdad,
  BoxplotSexoEdad,
  BoxplotProcedenciaEdad, ncol=2)

