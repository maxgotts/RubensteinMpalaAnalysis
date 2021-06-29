if (FALSE) {
    rm(list=ls())
}

library(raster)
library(rgdal)
library(dplyr)
library(ggpubr)

if (TRUE) { # Run this code to create the NDVI data frame, otherwise just import it below
    NDVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/6-3-6-18/MOD13Q1.006__250m_16_days_NDVI_doy2021145_aid0001.tif")
    
    NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
    colnames(NDVI) <- c("Longitude","Latitude","raw.NDVI")
    NDVI <- filter(NDVI, !is.na(raw.NDVI))
    NDVI$NDVI <- NDVI$raw.NDVI * .0001
    NDVI$raw.NDVI <- NULL
    
    # EVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/6-3-6-18/MOD13Q1.006__250m_16_days_EVI_doy2021145_aid0001.tif")
    # EVI <- as.data.frame(EVI_raster, xy = TRUE)
    # colnames(EVI) <- c("Longitude","Latitude","raw.EVI")
    # EVI <- filter(EVI, !is.na(raw.EVI))
    # EVI$EVI <- EVI$raw.EVI * .0001
    # EVI$raw.EVI <- NULL
    
    df <- read.csv("~/Desktop/MPALA/Whitesheets/Gotts_ConvertedWhitesheets.csv")
    # df <- filter(df, !is.na(Total.animals))
    
    lat.steps <- 0.01/4 #mean(diff(NDVI$Latitude)) ## 1.11 km
    long.steps <- 0.01/4 #mean(diff(NDVI$Longitude)) ## 1.11 km
    
    for (species in unique(df$Species)) {
        present <- paste0(species,".present")
        total <- paste0("Total.",species)
        NDVI[,present] <- 0
        NDVI[,total] <- 0
        for (ndvi_row in 1:nrow(NDVI)) {
            for (dazzle in 1:nrow(df)) {
                lat.ok <- (df[dazzle,"Latitude"] <= NDVI[ndvi_row,"Latitude"]+lat.steps/2) && (NDVI[ndvi_row,"Latitude"]-lat.steps/2 <= df[dazzle,"Latitude"])
                long.ok <- (df[dazzle,"Longitude"] <= NDVI[ndvi_row,"Longitude"]+long.steps/2) && (NDVI[ndvi_row,"Longitude"]-long.steps/2 <= df[dazzle,"Longitude"])
                species.ok <- df[dazzle,"Species"] == species
                if (lat.ok && long.ok && species.ok) {
                    NDVI[ndvi_row,present] <- 1
                    NDVI[ndvi_row,total] <- NDVI[ndvi_row,total] + df[dazzle,"Total.animals"]
                }
            }
        }
    }
    
    NDVI$Total.zebras <- NDVI$Total.GZ+NDVI$Total.PZ
    NDVI$Zebras.present <- as.integer(NDVI$GZ.present+NDVI$PZ.present > 0)
    
    View(NDVI)
    
    write.csv(NDVI, "/Users/maxgotts/Desktop/MPALA/Analysis/Ttest_NDVI.csv",row.names = FALSE)
}

NDVI <- read.csv("/Users/maxgotts/Desktop/MPALA/Analysis/Ttest_NDVI/Ttest_NDVI.csv")






### TESTS ###

## RELATION
cor.test(NDVI$Total.zebras, NDVI$NDVI, method=c("pearson", "kendall", "spearman"))

## T-TEST
z <- filter(NDVI, Zebras.present == 1)
nz <- filter(NDVI, Zebras.present == 0)

# ggqqplot(z.sample$NDVI-nz.sample$NDVI)

bootstrap <- 20000
num.samples <- 300
ps_normal <- c()
ps_nonparam <- c()
for (i in 1:bootstrap) {
    z.sample <- z[sample(nrow(z), num.samples), ]
    nz.sample <- nz[sample(nrow(nz), num.samples), ]

    s <- shapiro.test(z.sample$NDVI-nz.sample$NDVI)
    if (s$p.value > 0.05) {
        t <- t.test(z.sample$NDVI, nz.sample$NDVI, paired = TRUE, alternative = "two.sided")
        ps_normal <- c(ps_normal, t$p.value)
    } else {
        w <- wilcox.test(z.sample$NDVI, nz.sample$NDVI, paired = TRUE, alternative = "two.sided")
        ps_nonparam <- c(ps_nonparam, w$p.value)
    }
}
p.values <- data.frame(
    p=c(
        ps_normal,
        ps_nonparam
    ),label=c(
        rep("Normal",times=length(ps_normal)),
        rep("Non-parametric",times=length(ps_nonparam))
    )
)

# ggplot(data.frame(p=ps_normal), aes(p))+geom_histogram(bins=100, fill="#34568B")+theme_bw()+
#     labs(x="P-values",y="Count")
# print(sum(ps_normal>0.05))
# 
# ggplot(data.frame(p=ps_nonparam), aes(p))+geom_histogram(bins=100, fill="#34568B")+theme_bw()+
#     labs(x="P-values",y="Count")
# print(sum(ps_nonparam>0.05))

ggplot(p.values, aes(p, fill=label))+
    geom_histogram(position = "stack", bins=round(bootstrap/50))+
    theme_bw()+
    labs(x="P-values",y="Count")









###### GZ vs PZ vs Cattle
rm(list=ls())

df <- read.csv("~/Desktop/MPALA/Whitesheets/Gotts_ConvertedWhitesheets.csv")
df_gz <- filter(df, Species=="GZ")
df_pz <- filter(df, Species=="PZ")
df_cattle <- filter(df, Species=="Cattle")

bootstrap <- 20000
num.samples <- 20
ps_ndvi_normal <- c()
ps_ndvi_nonparam <- c()
ps_evi_normal <- c()
ps_evi_nonparam <- c()
for (i in 1:bootstrap) {
    pz.sample <- df_pz[sample(nrow(df_pz), num.samples), ]
    gz.sample <- df_gz[sample(nrow(df_gz), num.samples), ]
    
    s.ndvi <- shapiro.test(pz.sample$NDVI-gz.sample$NDVI)
    if (s.ndvi$p.value > 0.05) {
        t <- t.test(pz.sample$NDVI, gz.sample$NDVI, paired = TRUE, alternative = "two.sided")
        ps_ndvi_normal <- c(ps_ndvi_normal, t$p.value)
    } else {
        w <- wilcox.test(pz.sample$NDVI, gz.sample$NDVI, paired = TRUE, alternative = "two.sided")
        ps_ndvi_nonparam <- c(ps_ndvi_nonparam, w$p.value)
    }
    
    s.evi <- shapiro.test(pz.sample$EVI-gz.sample$EVI)
    if (s.evi$p.value > 0.05) {
        t <- t.test(pz.sample$EVI, gz.sample$EVI, paired = TRUE, alternative = "two.sided")
        ps_evi_normal <- c(ps_evi_normal, t$p.value)
    } else {
        w <- wilcox.test(pz.sample$EVI, gz.sample$EVI, paired = TRUE, alternative = "two.sided")
        ps_evi_nonparam <- c(ps_evi_nonparam, w$p.value)
    }
}
p.values.ndvi <- data.frame(
    p=c(
        ps_ndvi_normal,
        ps_ndvi_nonparam
    ),label=c(
        rep("Normal",times=length(ps_ndvi_normal)),
        rep("Non-parametric",times=length(ps_ndvi_nonparam))
    )
)
p.values.evi <- data.frame(
    p=c(
        ps_evi_normal,
        ps_evi_nonparam
    ),label=c(
        rep("Normal",times=length(ps_evi_normal)),
        rep("Non-parametric",times=length(ps_evi_nonparam))
    )
)

ggplot(p.values.ndvi, aes(x=p, fill=label))+
    geom_histogram(position = "stack", bins=round(bootstrap/50))+
    theme_bw()+
    labs(x="P-values",y="Count")


ggplot(p.values.evi, aes(p, fill=label))+
    geom_histogram(position = "stack", bins=round(bootstrap/50))+
    theme_bw()+
    labs(x="P-values",y="Count")




# print(warning_ndvi)
# ggplot(data.frame(p=ps_ndvi), aes(p))+geom_histogram(bins=100, fill="#34568B")+theme_bw()+
#     labs(x="P-values",y="Count")
# print(sum(ps_ndvi>0.05))
# 
# print(warning_evi)
# ggplot(data.frame(p=ps_evi), aes(p))+geom_histogram(bins=100, fill="#34568B")+theme_bw()+
#     labs(x="P-values",y="Count")
# print(sum(ps_evi>0.05))




## T-TEST
z <- filter(NDVI, Zebras.present == 1)
nz <- filter(NDVI, Cattle.present == 1)

# ggqqplot(z.sample$NDVI-nz.sample$NDVI)

bootstrap <- 20000
num.samples <- 200
ps_normal <- c()
ps_nonparam <- c()
for (i in 1:bootstrap) {
    z.sample <- z[sample(nrow(z), num.samples), ]
    nz.sample <- nz[sample(nrow(nz), num.samples), ]
    
    s <- shapiro.test(z.sample$NDVI-nz.sample$NDVI)
    if (s$p.value > 0.05) {
        t <- t.test(z.sample$NDVI, nz.sample$NDVI, paired = TRUE, alternative = "two.sided")
        ps_normal <- c(ps_normal, t$p.value)
    } else {
        w <- wilcox.test(z.sample$NDVI, nz.sample$NDVI, paired = TRUE, alternative = "two.sided")
        ps_nonparam <- c(ps_nonparam, w$p.value)
    }
}
p.values <- data.frame(
    p=c(
        ps_normal,
        ps_nonparam
    ),label=c(
        rep("Normal",times=length(ps_normal)),
        rep("Non-parametric",times=length(ps_nonparam))
    )
)

# ggplot(data.frame(p=ps_normal), aes(p))+geom_histogram(bins=100, fill="#34568B")+theme_bw()+
#     labs(x="P-values",y="Count")
# print(sum(ps_normal>0.05))
# 
# ggplot(data.frame(p=ps_nonparam), aes(p))+geom_histogram(bins=100, fill="#34568B")+theme_bw()+
#     labs(x="P-values",y="Count")
# print(sum(ps_nonparam>0.05))

ggplot(p.values, aes(p, fill=label))+
    geom_histogram(position = "stack", bins=round(bootstrap/50))+
    theme_bw()+
    labs(x="P-values",y="Count")



