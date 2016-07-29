Shiny App for exploring data
=========================================

*Code*

Use the Code box within the Data tab to run R functions on your table by referring to the table as "df". Separate multiple expressions with the '@' character. Code expressions will be applied in order.

**Subsetting examples**

```
df<-subset(df,df$biotype=="protein_coding")
df<-subset(df,df$pvalue<0.05 & df$log2FC>0)
```

**Adding new data to the table**

```
df$fraction<-df$percent*100
df$l2fc<-log2(df$sample1/df$sample2)
```

**Multiple expressions**

```
df$l2fc<-log2(df$sample1/df$sample2) @ df<-subset(df,abs(df$l2fc)>=1)
```
