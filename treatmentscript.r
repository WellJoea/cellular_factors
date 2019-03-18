################################################################################
# Fst Exect P test function  -  (matrix)
#
# Author: Zhou Wei
# Date: 09.10.2018
################################################################################
#install.packages(c("ggpubr","getopt","tidyverse","ggsci","Hmisc","pheatmap"))
#if(!require(devtools)) install.packages("devtools")
#library("devtools")
#devtools::install_github("kassambara/ggpubr")
#devtools::install_github("ropensci/plotly")
suppressPackageStartupMessages(library(ggpubr));
suppressPackageStartupMessages(library(tidyverse));
suppressPackageStartupMessages(library(magrittr));
suppressPackageStartupMessages(library(getopt));
suppressPackageStartupMessages(library(ggsci));
suppressPackageStartupMessages(library(dplyr));
suppressPackageStartupMessages(library(pheatmap));
suppressPackageStartupMessages(library(RColorBrewer));
suppressPackageStartupMessages(library(Hmisc));
### start part zero ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#get options, using the spec as defined by the enclosed list.
#we read the options from the default: commandArgs(TRUE).
spec <- matrix(c(
    'infile'   ,    'i',    1,    "character",    "the input file used for significance testing and genarating graphics ",
    'outdir'   ,    'o',    2,    "character",    "the out path used for generating the graphs, default = ./",
    'ncol'     ,    'c',    2,    "integer"  ,    "the numbers of columns used for allplot", 
    'scale'    ,    's',    2,    "character",    "the standarization method, default=none, you can set zscore,minmax, norm, or none",
    'groups'   ,    'g',    2,    "character",    "the groups can be find in all terms",
    'format'   ,    'f',    2,    "character",    "the format of the input file",
    'sample'   ,    'e',    2,    "character",    "the pair sampleId can be find in two groups,default=F,you can set T and F",
    'gorder'   ,    'd',    2,    "character",    "the order of groups used for comparing pre with on ",
    'xlabs'    ,    'x',    2,    "character",    "the xlab name",
    'ylabs'    ,    'y',    2,    "character",    "the ylab name",
    'cutx'     ,    'k',    2,    "integer"  ,    "the pheapmat cuttree values/groups used for spliting the matrix, default value = 1",
    'cuty'     ,    'j',    2,    "integer"  ,    "the pheapmat cuttree values/groups used for spliting the matrix, default value = 1",
    'meth'     ,    'm',    2,    "character",    "the statistics method, default value = wilcox.test, you also can use t.test,anova,kruskal.test ",
    'lgpos'    ,    'l',    2,    "character",    "the legend position, default=right, you can use top,bottom,right, left ",
    'picture'  ,    'p',    2,    "character",    "the out put pictures format, default = tiff",
    'pvalpos'  ,    'v',    2,    "integer"  ,    "the p vales postion  of columns , default=1", 
    'shwpall'  ,    'a',    2,    "character",    "the p values if you need to show on the grapth",
    'shwpart'  ,    'b',    2,    "integer"  ,    "the p values threshold values if you need to show on the grapth, you can set such as 0.05",
    'width'    ,    'w',    2,    "integer"  ,    "the output picture width,default = 12",
    'height'   ,    't',    2,    "integer"  ,    "the output picture height,default = 8",
    'help'     ,    'h',    0,    "logical"  ,    "show the useage"),
    byrow=TRUE, ncol=5
);
opt <- getopt(spec);
args <- commandArgs(T)
# if help was asked for print a friendly message 
# and exit with a non-zero error code
if ( !is.null(opt$help) || is.na(args[1]) ) {
  cat(getopt(spec, usage=TRUE));
  q(status=1);
}

#set some reasonable defaults for the options that are needed,
#but were not specified.
if ( is.null(opt$outdir  ) ) { opt$outdir  = "./"         };
if ( is.null(opt$ncol    ) ) { opt$ncol    = 5            };
if ( is.null(opt$picture ) ) { opt$picture = "pdf"       };
if ( is.null(opt$scale   ) ) { opt$scale   = "zscore"      };
if ( is.null(opt$xlab    ) ) { opt$xlab    = "Condition"  };
if ( is.null(opt$ylab    ) ) { opt$ylab    = "Value(%)"   };
if ( is.null(opt$meth    ) ) { opt$meth    = "wilcox.test"};
if ( is.null(opt$lgpos   ) ) { opt$lgpos   = "right"      };
if ( is.null(opt$pvalpos ) ) { opt$pvalpos = 1            };
if ( is.null(opt$width   ) ) { opt$width   = 12           };
if ( is.null(opt$height  ) ) { opt$height  = 8            };
if ( is.null(opt$cutx    ) ) { opt$cutx    = 7           };
if ( is.null(opt$cuty    ) ) { opt$cuty    = 7           };
if ( is.null(opt$sample  ) ) { opt$sample  = 'F'          }else{ opt$sample  = 'T'};
### staring the program ------------------------------------------------------------------
writeLines("
***********************************************************
* You are using the program scripted by Zhou Wei.         *
* If you find some bugs, please email to :                *
* zhou.wei@genecast.com.cn.                               *
* Please let me know and acknowledge in your publication. *
* Thank you!                                              *
* Best wishes!                                            *
***********************************************************")
Sys.setenv(TZ="Asia/Shanghai");
tima <- format(Sys.time(), "%Y-%m-%d-%H-%M");
timb <- format(Sys.time(), "%Y/%m/%d %H:%M:%S");
print(paste("staring the program at:", timb));


#************get dataframe ************************************
dfuniq <- function(df){
    dalist =list()
    n = 0
    for (i in colnames(df)[3:length(df)] ){
        n =n + 1
        data1 <- cbind(df[,1:2], data.frame(Values=df[i],Types=rep(i,length(df[i]))) )
        names(data1) = c("Groups","SampleIDs", "Values", "Types")
        dalist[[n]] = data1
    }
    return(dalist)
}

plotAllA <- function(treat){
    compare_means(Values ~ Groups, data = treat, group.by = "Types", paired = as.logical(opt$sample))
    l <- length(unique(treat$Types))
    p <- ggpaired(treat, x = "Groups", y = "Values", id="SampleIDs", 
            color = "Groups", palette = "lancet", line.color = "gray", 
            line.size = 0.5, facet.by = "Types", ncol = opt$ncol,
            font.label = list(size = 11, face = "bold", color ="black"),
            width = 0.5, xlab = opt$xlab, ylab = opt$xlab, panel.labs=NULL,
            short.panel.labs = FALSE, add = "jitter", shape = "Types") +
        labs( y = opt$ylab) + 
        scale_shape_manual(values=seq(1,l)) +
        stat_compare_means(method = opt$meth ,label = "p.format", label.x=1.25, label.y.npc="top", paired = as.logical(opt$sample)) +
        facet_wrap(~Types, scales = "free_y",ncol = opt$ncol,as.table=TRUE,labeller = label_value) +
        theme(text= element_text(color = "black"), panel.grid.major = element_blank(), axis.title.x=element_blank(),
            legend.text=element_text(color = "black",size = 11),legend.position =opt$lgpos) 
    
    if (opt$picture == 'pdf'){
        p + ggsave(filename=paste(opt$outdir,opt$scale,"_AllA.boxplot.pdf", sep = ""),width = opt$width, height = opt$height)
    }
    if (opt$picture == 'tiff'){
        p + ggsave(filename=paste(opt$outdir,opt$scale,"_AllA.boxplot.tiff", sep = ""), units="in", width = opt$width, height = opt$height, dpi=600, compression = 'lzw')
    }
      # geom_jitter(width = .08, alpha = 1, aes(colour = Groups)) 
}

plotAllB <- function(treat){
    m <- unique(treat$Types)
    l <- length(unique(treat$Types))
    co <- rep(c("#FF0000", "#0000FF", "#009E73", "#FFCC00", "#0072B2", "#ED5401", "#FF3399","#000000"),l)[1:l]
    pos <- c()
    for (n in seq(length(m))){
        subdf  <- subset(treat ,treat$Types==m[n])
        pos[n] <-max(subdf$Values) + opt$pvalpos
    }
    compare_means(Values ~ Groups, data = treat, group.by = "Types", paired = as.logical(opt$sample))
    p <-ggplot(treat, aes(x = Types, y = Values,colour=Groups) ) + 
        labs( y = opt$ylab) + 
        geom_boxplot(aes(fill = NULL), outlier.shape =NA, 
            position=position_dodge(width=.90), 
            alpha = 1,width=0.6, notch=FALSE, notchwidth = 0.5) + 
        geom_point(aes(shape =Types), 
            position=position_jitterdodge(jitter.width=0.4, dodge.width = 0.92),
            show.legend=FALSE) +
        #geom_hline(yintercept = 10,linetype="dashed", color = "gray55") +
        scale_shape_manual(values=seq(1,l)) +
        scale_color_lancet() +
        stat_compare_means(method = opt$meth,show.legend=FALSE,
            label = "p.format", label.x=1.39, label.y=pos, paired = as.logical(opt$sample)) + 
        theme(text= element_text(color = "black",size=15), panel.background=element_rect(color="black"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            legend.position =opt$lgpos, axis.title.x=element_blank()
        )

    if (opt$picture == 'pdf'){
        p + ggsave(filename=paste(opt$outdir,opt$scale,"_AllB1.boxplot.pdf", sep = ""), width = opt$width, height = opt$height)
    }
    if (opt$picture == 'tiff'){
        p + ggsave(filename=paste(opt$outdir,opt$scale,"_AllB1.boxplot.tiff", sep = ""), units="in", width = opt$width, height = opt$height, dpi=600, compression = 'lzw')
    }
}
plotAllB2 <- function(treat){
    m <- unique(treat$Types)
    l <- length(unique(treat$Types))
    co <- rep(c("#FF0000", "#0000FF", "#009E73", "#FFCC00", "#0072B2", "#ED5401", "#FF3399","#000000"),l)[1:l]
    pos <- c()
    for (n in seq(length(m))){
        subdf  <- subset(treat ,treat$Types==m[n])
        pos[n] <-max(subdf$Values) + opt$pvalpos
    }

    compare_means(Values ~ Groups, data = treat, group.by = "Types", paired = as.logical(opt$sample))
    q <-ggboxplot(treat,  x = "Groups", y = "Values", color = "Groups", 
            palette = "lancet", add = "jitter",
            bxp.errorbar=TRUE,bxp.errorbar.width=0.26,
            ncol = 4, font.label = list(size = 11, face = "bold", color ="black"),
            width = 0.5, outlier.shape =NA) +
        labs(y = opt$ylab) +
        stat_compare_means(method = opt$meth,show.legend=FALSE,
            label = "p.format", label.x=1.39, label.y.npc="top", paired = as.logical(opt$sample)) +
        facet_wrap(~Types, scales = "free_y",ncol = opt$ncol,as.table=TRUE,labeller = label_value) +
        scale_shape_manual(values=seq(1,l)) +
        theme(text= element_text(color = "black",size=10),  legend.text=element_text(color = "black",size = 10),
            panel.background=element_rect(color="black"), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), legend.position =opt$lgpos, 
            axis.title.x=element_blank())

    if (opt$picture == 'pdf'){
        q + ggsave(filename=paste(opt$outdir,opt$scale,"_AllB2.boxplot.pdf", sep = ""),width = opt$width, height = opt$height)
    }
    if (opt$picture == 'tiff'){
        q + ggsave(filename=paste(opt$outdir,opt$scale,"_AllB2.boxplot.tiff", sep = ""), units="in", width = opt$width, height = opt$height, dpi=600, compression = 'lzw')
    }
}

plotAllC <- function(treat){
    l <- length(unique(treat$Types))
    co <- rep(c("#FF0000", "#0000FF", "#009E73", "#FFCC00", "#0072B2", "#ED5401", "#FF3399","#000000"),l)[1:l]
    p <-ggplot(treat, aes(x = Types, y = Values,group = Types)  ) + 
        labs( y = opt$ylab) + 
        geom_violin(aes(fill =Types, colour=Types)) +
        geom_boxplot(aes(fill = NULL), outlier.shape =NA, 
            alpha = 0,width=0.4, notch=FALSE, notchwidth = 0.5) + 
        geom_point(aes(shape = Types), 
            position=position_jitterdodge(jitter.width=0.2, dodge.width = 0.62)) +
        #geom_hline(yintercept = 10,linetype="dashed", color = "gray55") +
        scale_shape_manual(values=seq(1,l)) +
        theme(text= element_text(color = "black",size=11),  legend.text=element_text(color = "black",size = 11),
            panel.background=element_rect(color="black"), panel.grid.major = element_blank(),
            axis.text.x = element_text(angle = 90), 
            panel.grid.minor = element_blank(), legend.position =opt$lgpos, 
            axis.title.x=element_blank())
     #scale_colour_manual(values=co) +
     
    if ( (is.null(opt$shwpall) ==FALSE) | (is.null(opt$shwpart) ==FALSE)) {
        aa <- compare_means(Values ~ Types, data = treat, paired = as.logical(opt$sample) )  %>%
            dplyr::mutate(y_pos = max(treat$Values) +opt$pvalpos, p.adj = format.pval(p.adj, digits = 4))
        if  (is.null(opt$shwpart) ==FALSE){
            aa <- subset(aa, aa$p.adj <=0.05) 
        }
        cp <- list()
        for (i in seq(1,length(aa$group1))){
            cp[[i]]=c(aa$group1[i],aa$group2[i])
        }
        p <- p + stat_compare_means(comparisons = cp, method = opt$meth,label = "p.format", map_signif_level = TRUE, paired = as.logical(opt$sample),show.legend=FALSE)
    }
    if (opt$picture == 'pdf'){
        p + ggsave(filename=paste(opt$outdir,opt$scale, "_AllC.boxplot.pdf", sep = ""), limitsize = FALSE, width = opt$width, height = opt$height)
    }
    if (opt$picture == 'tiff'){
        p + ggsave(filename=paste(opt$outdir,opt$scale, "_AllC.boxplot.tiff", sep = ""), limitsize = FALSE, units="in", width = opt$width, height = opt$height, dpi=600, compression = 'lzw')
    }
    #scale_colour_hue() +
    #library(ggsignif)
    #geom_signif(data= aa,
    #    aes(xmin = group1, xmax = group2, annotations = p, y_position = rep(max(treat$Values) +10,55)),
    #    manual= TRUE
    #   )
    # coord_flip()
    #install.packages("ggcorrplot")
}
plotAllD <- function(treat){
    l <- length(unique(treat$Types))
    co <- rep(c("#FF0000", "#0000FF", "#009E73", "#FFCC00", "#0072B2", "#ED5401", "#FF3399","#000000"),l)[1:l]
    lvl =  as.character(unique(treat$Groups))
    compare = combn(lvl, 2, simplify = FALSE)
    ypos = tapply(treat$Values, treat$Types ,max)+10

    p <-ggplot(treat, aes(x = Groups, y = Values, group = Groups, color=Groups)  ) + 
        labs( y = opt$ylab) + 
        #geom_violin(aes(fill =Groups, colour=Groups)) +
        geom_boxplot(aes(fill = NULL), outlier.shape =NA, 
            alpha = 0,width=0.8, notch=FALSE, notchwidth = 0.83) + 
        geom_point(aes(shape = Groups), size=1.5,
            position=position_jitterdodge(jitter.width=0.8, dodge.width = 0.9)) +
            #geom_blank(aes(y=Values*1.3))+
        facet_wrap(~Types, scales = "free", ncol = opt$ncol, as.table=TRUE, drop=FALSE, labeller = 'label_value') +
        #geom_hline(yintercept = 10,linetype="dashed", color = "gray55") +
        #scale_shape_manual(values=seq(1,l)) +
        stat_compare_means(label.x.npc = "left", size =2.5,label.y.npc=0.95) + 
        stat_compare_means(method = opt$meth, size =2.5, show.legend=FALSE, comparisons = compare, label = "p.format") + #,label.y=c(70,80,90), 
        #geom_signif(comparisons=compare) +
        theme(text= element_text(color = "black",size=10),  legend.text=element_text(color = "black",size = 11),
            panel.background=element_rect(color="black"), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), legend.position =opt$lgpos, 
            axis.title.x=element_blank())

    if (opt$picture == 'pdf'){
        p + ggsave(filename=paste(opt$outdir,opt$scale, "_AllD.boxplot.pdf", sep = ""),limitsize = FALSE, width = opt$width, height = opt$height)
    }
    if (opt$picture == 'tiff'){
        p + ggsave(filename=paste(opt$outdir,opt$scale, "_AllD.boxplot.tiff", sep = ""), units="in", width = opt$width, height = opt$height, dpi=600, compression = 'lzw')
    }
}

corplot <- function(aa){
    bb = data.frame(cbind(aa$group1,aa$group2,aa$p))
    cc = data.frame(cbind(aa$group2,aa$group1,aa$p))
    bb = rbind(bb,cc)
    names(bb)=c('group1','group2','p')

    typea <- unique(treat$Types)
    typel <- length(typea)
    typeM <- matrix(0, ncol=typel , nrow=typel )
    rownames(typeM) <- typea
    colnames(typeM) <- typea

    for (i in seq(typel)){
        for (j in seq(typel)){
            if (i > j){
                pvalue = bb[which(bb$group1==typea[[i]] & bb$group2==typea[[j]]),]$p
                typeM[i,j] = as.character(pvalue)
            }
        }
    }

    typeM <- apply(typeM,2,as.numeric)
    rownames(typeM) <- typea
    colnames(typeM) <- typea

    library(ggcorrplot)
    col1 <-rainbow(3, s = 1, v = 1, start = 0, end = 0.9, alpha = 1)
    col1 <- c("blue","white", "red")
    q <-ggcorrplot(typeM, method = "circle",type = "lower",colors=col1,sig.level=0.05) +
     theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
        panel.background=element_rect(color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position ="bottom"
     )

    library(corrplot)
    col1 <-rainbow(200, s = 1, v = 1, start = 0, end = 0.9, alpha = 1)
    corrplot(typeM, method = "circle",type = "lower",cl.lim=c(0,1), col=col1,addrect = 2)
    corrplot.mixed(typeM)

    library(gridExtra)
    grid.arrange(p, q,  ncol=2)
    lay <- rbind(c(1,1,2))
    grid.arrange(grobs=list(p, q), layout_matrix = lay)
}


plotA <- function(subdf,i){
    compare_means(Values ~ Groups, data = subdf, paired = as.logical(opt$sample))
    leve <- unique(subdf$Groups)
    if (is.null(opt$gorder) ==FALSE){
        leve <- strsplit(opt$gorder,split = "[,;]+")[[1]]
        subdf <- subdf %>%
            tidyr::spread(Groups, Values) %>%
            dplyr::mutate(is_increasing = get(leve[1]) <= get(leve[2]) ) %>%
            tidyr::gather("Groups", "Values", 3:4)
    }else{
        subdf <- subdf %>%
            tidyr::spread(Groups, Values) %>%
            dplyr::mutate(is_increasing = leve[1] <= leve[2] ) %>%
            tidyr::gather("Groups", "Values", 3:4)
    }
    subdf$Groups <- factor(subdf$Groups, levels=leve)

    p <-ggplot(subdf,aes(x = Groups, y = Values)) + 
        labs( y = opt$ylab) +  
        geom_boxplot(aes(fill = Groups), alpha = 1,width=0.6, 
            notch=TRUE, notchwidth = 0.5) +
        geom_point(aes(fill = Groups,shape =Groups),color="gray50") + 
        geom_line(aes(group = SampleIDs, col = is_increasing), 
            show.legend=FALSE, linetype = 2, 
            arrow = arrow(angle = 23, length = unit(0.07, "inches"), 
                ends = "last", type = "closed")
            ) +
        #geom_hline(yintercept = 10,linetype="dashed", color = "gray55") +
        scale_colour_manual(values = c("red", "blue")) +
        stat_compare_means(method = "wilcox.test",label = "p.format", 
            label.x=1.39, label.y.npc="top", paired = as.logical(opt$sample)) + 
        theme(text= element_text(color = "black",size=15),
            panel.background=element_rect(color="black"), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), legend.position ="right", axis.title.x=element_blank())
    if (opt$picture == 'pdf'){
        p + ggsave(filename=paste(opt$outdir,opt$scale, '_', i, "boxplotA.pdf", sep = ""),width =7, height = 6 )
    }
    if (opt$picture == 'tiff'){
        p + ggsave(filename=paste(opt$outdir,opt$scale, '_', i, "boxplotA.tiff", sep = ""), units="in", width =7, height = 6, dpi=600, compression = 'lzw')
    }
}

plotB <-function(subdf,i){
    compare_means(Values ~ Groups, data = subdf, paired = as.logical(opt$sample))
    p <- ggpaired(subdf, x = "Groups", y = "Values", id="SampleIDs", color = "Groups", palette = "lancet", 
            line.color = "gray", line.size = 0.5, linetype = "dashed",  showStrips = FALSE,
            font.label = list(size = 15, face = "bold", color ="black"),
            width = 0.5, ylab = opt$ylab,  panel.labs=NULL,
            short.panel.labs = FALSE, add = "jitter") +
        stat_compare_means(method = "wilcox.test",label = "p.format", 
            label.x=1.39, label.y.npc="top", paired = as.logical(opt$sample)) + 
        theme(text= element_text(color = "black"), panel.grid.major = element_blank(),
            legend.position ="top",axis.title.x=element_blank())

    if (opt$picture == 'pdf'){
        p + ggsave(filename=paste(opt$outdir,opt$scale, '_', i,"_boxplotB.pdf", sep = ""), width =7, height = 6)
    }
    if (opt$picture == 'tiff'){
        p + ggsave(filename=paste(opt$outdir,opt$scale, '_', i,"_boxplotB.tiff", sep = ""), units="in", width =7, height = 6, dpi=600, compression = 'lzw')
    }
}

heatM <- function(treaT,out){
    #Matrxi = rcorr(as.matrix(treaT))
    #Matrxi = as.data.frame(Matrxi$r)
    Matrxi = cor(as.matrix(treaT))
    write.table(Matrxi,file=paste(opt$outdir,"data.pearson.matrix.xls", sep = ""),sep="\t",col.names = TRUE,row.names=FALSE,quote=FALSE)
    coll <- ncol(Matrxi)
    rowl <- nrow(Matrxi)
    pheatmap(Matrxi, filename = out, main = "The \ matrix of pearson heatmap",
        #border_color = "black",
        color = colorRampPalette(c("blue", "white", "red"))(100),
        cluster_cols = TRUE, display_numbers = FALSE,
        fontsize_row = 7, fontsize_col=7,  fontsize=7,
        clustering_distance_rows = "euclidean", clustering_method = "average",
        cutree_rows = opt$cutx,
        cutree_cols = opt$cuty,
        #number_format = fper,
        #width = (coll+6)/2, height = (rowl+6)/2, 
        #number_format = fper,
        #annotation_row = aa,
        #annotation_col = aa,
    )
}

#dir.create(dirname(opt$outdir))
dir.create(opt$outdir)
print(paste("the work directory is: ", getwd()))
#treat <- read.table(opt$infile,sep="\t",header = TRUE,encoding="UTF-8")
treat  <- read.table(opt$infile,sep="\t",header = TRUE,fileEncoding="UTF-8")
treatS <- ''
if(opt$scale =='zscore') {
    treatS = as.data.frame(scale(treat[3:length(treat)]))
}else if( opt$scale =='minmax') {
    mms=function(x){return ((x-min(x))/(max(x)-min(x)))}
    treatS = apply(treat[3:length(treat)],2, mms)
}else if( opt$scale =='norm') {
    mms=function(x){return ( 2*(x-min(x))/(max(x)-min(x))-1 )}
    treatS = apply(treat[3:length(treat)],2, mms)
}else{
    treatS = treat[3:length(treat)]
}
treat  = cbind(treat[1:2], treatS)
print(paste('you are using the ', opt$scale, ' method for standarization'))
write.table(treat,file=paste(opt$outdir,"data.scalar.change.xls", sep = ""),sep="\t",col.names = TRUE,row.names=FALSE,quote=FALSE)
heatM(treatS,paste(opt$outdir,opt$scale,"_pearson.heatmap.pdf", sep = ""))

if (is.null(opt$format) ==TRUE) {
    treat <- Reduce(function(x,y) merge(x, y, all=T), dfuniq(treat))
}


Mean_qua <- c()
typename <- unique(treat$Types)
for (i in seq(typename)){
    typei = typename[i]
    vc = treat[treat$Types==typei,]$Values
    vi = c(length(vc),mean(vc),sd(vc),quantile(vc))
    Mean_qua[i] =as.data.frame(vi)
}
Mean_qua = as.data.frame(Mean_qua, col.names = typename,
            row.names=c('length','mean','sd','0%','25%','50%','75%','100%'))
write.table(t(Mean_qua), file=paste(opt$outdir,"data.statistics.xls", sep = ""),
            sep="\t", col.names = TRUE,row.names=TRUE,quote=FALSE, na = "")


o <- order(treat[,4],treat[,1])
treat <- treat[o,]
names(treat) = c("Groups","SampleIDs", "Values", "Types")

leve <- unique(treat$Groups)
if (is.null(opt$gorder) ==FALSE){
    leve <- strsplit(opt$gorder,split = "[,;]+")[[1]]
}
treat$Groups <- factor(treat$Groups, levels=leve)
write.table(treat,file=paste(opt$outdir,"data.final.change.xls", sep = ""),sep="\t",col.names = TRUE,row.names=FALSE,quote=FALSE)

if (length(unique(treat$Groups))==2){
        plotAllA(treat)
        for (i in unique(treat$Types)) {
            subdf <- subset(treat ,treat$Types==i)
            plotA(subdf,i)
            plotB(subdf,i)
        }
    plotAllB(treat)
    plotAllB2(treat)
    plotAllC(treat)
}else if (length(unique(treat$Groups))>2){
    subdf <- subset(treat ,treat$Types=='IFN.gamma')
    plotAllC(treat)
    plotAllD(treat)
}else{
    plotAllC(treat)
}
