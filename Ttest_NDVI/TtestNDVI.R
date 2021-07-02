if (FALSE) {
    rm(list=ls())
}

library(raster)
library(rgdal)
library(dplyr)
library(ggpubr)

if (FALSE) { # Run this code to create the NDVI data frame, otherwise just import it below
    NDVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/6-3-6-18/MODUS_NDVI_6-3-6-18.tif")
    
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
    
    df <- read.csv("~/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")
    # df <- filter(df, !is.na(Total.animals))
    
    lat.steps <- 0.01/4 #mean(diff(NDVI$Latitude)) ## 1.11/4 km
    long.steps <- 0.01/4 #mean(diff(NDVI$Longitude)) ## 1.11/4 km
    
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

# if (FALSE) { # Run this code to create the EVI data frame, otherwise just import it below
#     NDVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/6-3-6-18/MODUS_NDVI_6-3-6-18.tif")
#     
#     NDVI <- as.data.frame(NDVI_raster, xy = TRUE)
#     colnames(NDVI) <- c("Longitude","Latitude","raw.NDVI")
#     NDVI <- filter(NDVI, !is.na(raw.NDVI))
#     NDVI$NDVI <- NDVI$raw.NDVI * .0001
#     NDVI$raw.NDVI <- NULL
#     
#     # EVI_raster <- stack("/Users/maxgotts/Desktop/MPALA/Maps/MODUS\ Data/6-3-6-18/MOD13Q1.006__250m_16_days_EVI_doy2021145_aid0001.tif")
#     # EVI <- as.data.frame(EVI_raster, xy = TRUE)
#     # colnames(EVI) <- c("Longitude","Latitude","raw.EVI")
#     # EVI <- filter(EVI, !is.na(raw.EVI))
#     # EVI$EVI <- EVI$raw.EVI * .0001
#     # EVI$raw.EVI <- NULL
#     
#     df <- read.csv("~/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")
#     # df <- filter(df, !is.na(Total.animals))
#     
#     lat.steps <- 0.01/4 #mean(diff(NDVI$Latitude)) ## 1.11/4 km
#     long.steps <- 0.01/4 #mean(diff(NDVI$Longitude)) ## 1.11/4 km
#     
#     for (species in unique(df$Species)) {
#         present <- paste0(species,".present")
#         total <- paste0("Total.",species)
#         NDVI[,present] <- 0
#         NDVI[,total] <- 0
#         for (ndvi_row in 1:nrow(NDVI)) {
#             for (dazzle in 1:nrow(df)) {
#                 lat.ok <- (df[dazzle,"Latitude"] <= NDVI[ndvi_row,"Latitude"]+lat.steps/2) && (NDVI[ndvi_row,"Latitude"]-lat.steps/2 <= df[dazzle,"Latitude"])
#                 long.ok <- (df[dazzle,"Longitude"] <= NDVI[ndvi_row,"Longitude"]+long.steps/2) && (NDVI[ndvi_row,"Longitude"]-long.steps/2 <= df[dazzle,"Longitude"])
#                 species.ok <- df[dazzle,"Species"] == species
#                 if (lat.ok && long.ok && species.ok) {
#                     NDVI[ndvi_row,present] <- 1
#                     NDVI[ndvi_row,total] <- NDVI[ndvi_row,total] + df[dazzle,"Total.animals"]
#                 }
#             }
#         }
#     }
#     
#     EVI$Total.zebras <- NDVI$Total.GZ+NDVI$Total.PZ
#     EVI$Zebras.present <- as.integer(NDVI$GZ.present+NDVI$PZ.present > 0)
#     
#     View(EVI)
#     
#     write.csv(EVI, "/Users/maxgotts/Desktop/MPALA/Analysis/Ttest_NDVI.csv",row.names = FALSE)
# }
# 
# EVI <- read.csv("/Users/maxgotts/Desktop/MPALA/Analysis/Ttest_NDVI/Ttest_NDVI.csv")
# 
# 








z <- filter(NDVI, Zebras.present == 1)
nz <- filter(NDVI, Zebras.present == 0)
(z.nz.boot <- bootstrap.t.test(z,nz,value=c("NDVI"),title="Comparison of zebra vs. random selection of NDVI"))$plot






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

df <- read.csv("~/Desktop/MPALA/Whitesheets/ConvertedWhitesheets.csv")
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







## Zebras vs Cattle ##

cattle.abbr <- c("Cattle","CKC","CC","MC")
zebra.abbr <- c("GZ","PZ")

z <- filter(df, Species %in% zebra.abbr)
c <- filter(df, Species %in% cattle.abbr)

bootstrap <- 20000
num.samples <- min(floor(0.85*nrow(z)),floor(0.85*nrow(c)))
cat("Running ",bootstrap," bootstrapped samples with n=",num.samples," on each interation",sep="")
ps_normal <- c()
ps_nonparam <- c()
for (i in 1:bootstrap) {
    z.sample <- z[sample(nrow(z), num.samples), ]
    c.sample <- c[sample(nrow(c), num.samples), ]
    
    s <- shapiro.test(z.sample$NDVI-c.sample$NDVI)
    if (s$p.value > 0.05) {
        t <- t.test(z.sample$NDVI, c.sample$NDVI, paired = TRUE, alternative = "two.sided")
        ps_normal <- c(ps_normal, t$p.value)
    } else {
        w <- wilcox.test(z.sample$NDVI, c.sample$NDVI, paired = TRUE, alternative = "two.sided")
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

ggplot(p.values, aes(p, fill=label))+
    geom_histogram(position = "stack", bins=round(bootstrap/50))+
    theme_bw()+
    labs(x="P-values",y="Count")+
    title("Zebra versus Cattle NDVI")





#### #### #### #### ####
bootstrap.t.test <- function(z,c,value=c("NDVI","EVI"),bootstrap=20000,title=paste("T-test on ",paste(value,collapse=", "),sep=""),colours=c("#34568B","#6B5B95")) {
    num.samples <- min(floor(0.85*nrow(z)),floor(0.85*nrow(c)))
    cat("Running ",bootstrap," bootstrapped samples with n=",num.samples," on each interation on ",length(value)," variables\n\n",sep="")
    
    p.values <- data.frame()
    for (val in value) {
        ps_normal <- c()
        ps_nonparam <- c()
        for (i in 1:bootstrap) {
            z.sample <- z[sample(nrow(z), num.samples), ]
            c.sample <- c[sample(nrow(c), num.samples), ]
            
            s <- shapiro.test(z.sample[,val]-c.sample[,val])
            if (s$p.value > 0.05) {
                t <- t.test(z.sample[,val], c.sample[,val], paired = TRUE, alternative = "two.sided")
                ps_normal <- c(ps_normal, t$p.value)
            } else {
                w <- wilcox.test(z.sample[,val], c.sample[,val], paired = TRUE, alternative = "two.sided")
                ps_nonparam <- c(ps_nonparam, w$p.value)
            }
        }
        temp.p.values <- data.frame(
            p=c(
                ps_normal,
                ps_nonparam
            ),label=c(
                rep("Normal",times=length(ps_normal)),
                rep("Non-parametric",times=length(ps_nonparam))
            ),variable=val
        )
        p.values <- rbind(p.values,temp.p.values)
        cat("RESULTS: variable=",val,"\n",sep="")
        cat("A total of ",sum(p.values$p>0.05)," out of ",length(p.values$p)," (",100*sum(p.values$p>0.05)/length(p.values$p),"%) of p-values were > 0.05\n",sep="")
        cat("Mean p-value = ",mean(p.values$p)," +/- ",sd(p.values$p),"\n\n",sep="")
    }
    
    par(mar=c(5,5,2,1)+.1)
    
    (gg <- ggplot(p.values, aes(p, fill=label))+
        geom_histogram(position = "stack", bins=round(bootstrap/50))+
        theme_classic()+
        facet_wrap(~variable)+
        aes(y=stat(count)/bootstrap)+
        scale_y_continuous(labels = scales::percent)+
        labs(x="P-values",y="Count",fill="Test",title=title)+
        scale_color_manual(values=colours))
    
    out <- list(p.values,gg)
    names(out) <- c("p.values","plot")
    
    return(out)
}


