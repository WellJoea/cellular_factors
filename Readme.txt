命令行参数：
    -i|--infile     the input file used for significance testing and genarating graphics
    -o|--outdir     the out path used for generating the graphs, default = ./
    -c|--ncol       the numbers of columns used for allplot
    -s|--scale      the standarization method, default=none, you can set zscore,minmax, norm, or none
    -g|--groups     the groups can be find in all terms
    -f|--format     the format of the input file
    -e|--sample     the pair sampleId can be find in two groups,default=F,you can set T and F
    -d|--gorder     the order of groups used for comparing pre with on
    -x|--xlabs      the xlab name
    -y|--ylabs      the ylab name
    -k|--cutx       the pheapmat cuttree values/groups used for spliting the matrix, default value = 1
    -j|--cuty       the pheapmat cuttree values/groups used for spliting the matrix, default value = 1
    -m|--meth       the statistics method, default value = wilcox.test, you also can use t.test,anova,kruskal.test
    -l|--lgpos      the legend position, default=right, you can use top,bottom,right, left
    -p|--picture    the out put pictures format, default = tiff
    -v|--pvalpos    the p vales postion  of columns , default=1
    -a|--shwpall    the p values if you need to show on the grapth
    -b|--shwpart    the p values threshold values if you need to show on the grapth, you can set such as 0.05
    -w|--width      the output picture width,default = 12
    -t|--height     the output picture height,default = 8
    -h|--help       show the useage

命令行参数：
    -i|--infile     输入文件，文件格式第一列为group，第二列为sampleID，
                    从第三列开始为对应的作图项目，字段分割为tab。
                    group和sampleID如不存在，填入任意相同字符串即可（可选）
    -o|--outdir     输入文件夹, 默认为当前文件夹（可选）
    -c|--ncol       每行展现图的个数（可选参数，默认：5）
    -s|--scale      数据标准化方法：（未去掉离群值，默认：zscore）
                       zscore: (x-minx)/sd(x)
                       minmax: (x-minx)/((max-minx), [0, -1]
                       norm  : 2*(x-minx)/((max-minx)-1 , [-1, -1]
                       none  : 不进行标准化
    -g|--groups     多余参数，用于待需
    -f|--format     输入文件格式，用于待需
    -e|--sample     sampleID是否为组间对应，如对应，则生成带线图A，（默认为对应T，可选T/F）
    -d|--gorder     分组顺序，如pre_treatment,on_treatment，表示pre_treatment在前，on_treatment在后，只限于分为两组
    -x|--xlabs      横坐标名称，默认Condition
    -y|--ylabs      纵坐标名称，默认Value(%)
    -k|--cutx       相关性分析（采用pearson系数）绘制的热图横轴分组（默认：7）
    -j|--cuty       相关性分析（采用pearson系数）绘制的热图纵轴分组（默认：7）
    -m|--meth       统计方法, 默认 wilcox.test, 可选 t.test,anova,kruskal.test
    -l|--lgpos      图例位置，默认 right, 可选 top,bottom,right, left
    -p|--picture    输入图片格式，默认 pdf，可选pdf tiff
    -v|--pvalpos    P值显示位置 , 默认 最大值+1
    -a|--shwpall    显示所有P值
    -b|--shwpart    显示统计检验小于0.05的P值
    -w|--width      输出图像的宽度，默认12
    -t|--height     输出图像的高度，默认8
    -h|--help       显示说明


输出文件描述：
1.文件描述：文件包括至少3列，文件格式第一列为group，第二列为sampleID，
    从第三列开始为对应的作图项目，字段分割为tab。
    group和sampleID如不存在，填入任意相同字符串即可
2.输出文件：
2.1 data.statistics.xls：统计每个指标的数量，均值，标准差，4分位数
2.2 data.scalar.change.xls：数据标准化后的数据
2.3 图形，各组的检验图形。在根据自己的数据多少，适当的调整输出图像的高(h)宽(t)大小，确保图形的美观

test1使用方法:
1.文件描述：test1为8个指标，每个指标分为2组，且样本一一对应
2.使用方法：Rscript treatmentscript.r -i test1.txt -o test1/  -s norm -b 

test2使用方法:
1.文件描述：test1为59个指标，每个指标分为3组，且每组样本无对应编号
2.使用方法：Rscript treatmentscript.r -i test1.txt -o test1/  -t 60


I.初次使用配置
第一步：下载R，地址：https://mirrors.tuna.tsinghua.edu.cn/CRAN/
第二步：安装环境变量：D:\ProgramData\R\R-3.5.1\bin\x64
第三步：打开R，逐条执行命令：
install.packages(c("ggpubr","getopt","tidyverse","ggsci","Hmisc","pheatmap"))
install.packages("devtools")
library(devtools)
install_github("kassambara/ggpubr")
install_github("ropensci/plotly")
##update.packages()

II.脚本使用方法
进入文件夹，使用powershell或者shell。
输入文件：treatment3
输出路径：test 
输入:Rscript treatmentscript.r -i treatment3.txt -o test\ 

如果需要调整输出图像大小，输出pdf矢量图像：
Rscript treatmentscript.r -i treatment3.txt -o test\ -w 15 -t 57 -p pdf

如果sampleID不存在对应关系：
Rscript treatmentscript.r -i treatment3.txt -o test\ -s F

如果需要调整分组顺序，并设置图例位置在上方
Rscript treatmentscript.r -i treatment3.txt -o test\ -d pre_treatment,on_treatment -l top

如果只显示p<0.05的数据，并调整xy轴名称
Rscript treatmentscript.r -i treatment3.txt -o test\ -b  -x hengzhou -y zongzhou

如果显示所有P值的数据
Rscript treatmentscript.r -i treatment3.txt -o test\ -a
