# -----------------------
# App Title: t-test
# Author: Jimmy Wong
# -----------------------

#####################################################################################################################
#####################################################################################################################
## t-distribution shading area function
#####################################################################################################################
#####################################################################################################################

t.dist.area = function(tstat,tail,df)
{
  x = seq(-5,5,length.out=200)
  df = round(df, digits=3)
  
  if(tail=="right")
  {
    xmin=tstat
    xmax=5
    
    area = seq(xmin,xmax,length.out=200)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="navy") + 
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_bw()
  } else if(tail=="left")
  {
    xmin=-5
    xmax=tstat
    
    area = seq(xmin,xmax,length.out=200)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="navy") +
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_bw()
  } else if(tail=="both")
  {
    xmin1=abs(tstat)
    xmax1=5
    area1 = seq(xmin1,xmax1,length.out=200)
    dat1 = data.frame(x=area1,ymin1=0,ymax1=dt(area1,df=df))
    
    xmin2=-5
    xmax2=-abs(tstat)
    area2 = seq(xmin2,xmax2,length.out=200)
    dat2 = data.frame(x=area2,ymin2=0,ymax2=dt(area2,df=df))
    
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat1, mapping=aes(x=x, ymin=ymin1, ymax=ymax1),fill="navy") +
      geom_ribbon(data=dat2, mapping=aes(x=x, ymin=ymin2, ymax=ymax2),fill="navy") +
      ggtitle(paste("t-distribution with", df, "degrees of freedom")) +
      xlab("t-values") + ylab("Relative frequency") + theme_bw()
  }
  return(graph)
}

#####################################################################################################################
#####################################################################################################################
## Library and data sets
#####################################################################################################################
#####################################################################################################################

options(shiny.maxRequestSize=30*1024^2)
library(ggplot2)
library(shinyBS)
data(faithful)
data(mtcars)

#####################################################################################################################
#####################################################################################################################
## Shiny server
#####################################################################################################################
#####################################################################################################################

shinyServer(function(input, output) {
  #####################################################################################################################
  #####################################################################################################################
  ## Data Exploration Panel
  #####################################################################################################################
  #####################################################################################################################
  
  data = reactive({
    if(is.null(input$file) & !input$usedata) 
    {
      return(NULL)
    } else if(!is.null(input$file) & !input$usedata)
    {
      file = read.csv(input$file$datapath, header=input$header, sep=input$sep, quote=input$quote)
      return(file)
    } else if(input$sampdat==1 & input$usedata)
    {
      return(data.frame(eruptions=faithful$eruptions))
    } else if(input$sampdat==2 & input$usedata)
    {
      mtcars$amcoded = rep(NA,length(mtcars$hp))
      mtcars$amcoded[which(mtcars$am==0)] = "automatic"
      mtcars$amcoded[which(mtcars$am==1)] = "manual"
      return(data.frame(transmission=mtcars$amcoded, horsepower=mtcars$hp))
    }
    })
    
  output$data.tab = renderDataTable({
    if(!input$usedata) data()
  })
  
  output$data.tab1 = renderDataTable({
    if(input$usedata) data()
  })
  
  output$datagraph = renderPlot({
    if((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata)) 
    {
      dat=unlist(data())
      dat1=data.frame(x=as.numeric(as.character(dat)))
        
      if(input$usedata)
        lab = "Eruption times"
      else
        lab = paste(names(data())[1])
        
      ggplot(data=dat1) + geom_histogram(aes(x=x), fill="navy", alpha=.5) +
        xlab(lab) + ylab("Frequency") +
        ggtitle(paste("Histogram of",lab)) + theme_bw()
    } else if((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      dat=data()
      dat1=data.frame(x=dat[[1]],y=dat[[2]])
      if(length(unique(dat1$x))>length(unique(dat1$y)))
      {
        dat1$x = as.numeric(as.character(dat1$x))
        ggplot(data=dat1) + geom_boxplot(aes(x=factor(y),y=x,fill=factor(y)),alpha=.5) + 
          xlab(paste(names(dat)[2])) + ylab(paste(names(dat)[1])) + theme_bw() +
          ggtitle(paste("Boxplots of",paste(names(dat)[1]),"by",paste(names(dat)[2]))) +
          scale_fill_manual(name=paste(names(dat)[1]),values=c("seagreen2","gold2")) +
          theme(legend.position="bottom")
      } else
      { 
        dat1$y = as.numeric(as.character(dat1$y))
        ggplot(data=dat1) + geom_boxplot(aes(x=factor(x),y=y,fill=factor(x)),alpha=.5) +
          xlab(paste(names(dat)[1])) + ylab(paste(names(dat)[2])) + theme_bw() +
          ggtitle(paste("Boxplots of",paste(names(dat)[2]),"by",paste(names(dat)[1]))) +
          scale_fill_manual(name=paste(names(dat)[2]),values=c("seagreen2","gold2")) +
          theme(legend.position="bottom")
      }
    } else if((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      dat=data()
      dat1=data.frame(x=c(as.numeric(as.character(dat[[1]])),as.numeric(as.character(dat[[2]]))),
                       y=c(rep(names(dat)[1],length(dat[[1]])),rep(names(dat)[2],length(dat[[2]]))))
       ggplot(data=dat1) + geom_boxplot(aes(x=factor(y),y=x,fill=factor(y)),alpha=.5) + 
        xlab("Explanatory variable") + ylab("Response variable") +
        scale_fill_manual(name="",values=c("seagreen2","gold2")) +
        ggtitle("Boxplots") + theme_bw() + theme(legend.position="bottom")
    }
  })
  
  output$summarystats = renderTable({
    if(input$displaystats & ((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata)))
    {
      vec = as.numeric(as.character(data()[[1]]))
      table = t(matrix(c((as.matrix(summary(vec)[1:6])),
                         round(sd(vec,na.rm=TRUE)))))
      
      if(input$usedata)
        rownames(table) = "Eruption times"
      else
        rownames(table) = names(data())[[1]]
      
      colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
      return(table)
    } else if(input$displaystats & ((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata)))
    {
      dat=data()
      dat1=data.frame(x=dat[[1]],y=dat[[2]])
      if(length(unique(dat1$x)) > length(unique(dat1$y)))
      {
        dat1$y = factor(dat1$y)
        dat1$x = as.numeric(as.character(dat1$x))
        dat1 = dat1[which(complete.cases(dat1)),]
        sum = tapply(dat1$x,dat1$y,summary)
        table = data.frame(matrix(c(sum[[1]],sum[[2]]),nrow=2,ncol=6,byrow=TRUE))
        std = tapply(dat1$x,dat1$y,sd,na.rm=TRUE)
        table$sd[1] = round(std[1],digits=2)
        table$sd[2] = round(std[2],digits=2)
        table = as.matrix(table)
        colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
        rownames(table) = c(levels(dat1$y)[1],levels(dat1$y)[2])
        return(table)        
      } else if(length(unique(dat1$x)) < length(unique(dat1$y)))
      {
        dat1$x = factor(dat1$x)
        dat1$y = as.numeric(as.character(dat1$y))
        dat1 = dat1[which(complete.cases(dat1)),]
        sum = tapply(dat1$y,dat1$x,summary)
        table = data.frame(matrix(c(sum[[1]],sum[[2]]),nrow=2,ncol=6,byrow=TRUE))
        std = tapply(dat1$y,dat1$x,sd)
        table$sd[1] = round(std[1],digits=2)
        table$sd[2] = round(std[2],digits=2)
        table = as.matrix(table)
        colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
        rownames(table) = c(levels(dat1$x)[1],levels(dat1$x)[2])
        return(table)         
      }
    } else if(input$displaystats & ((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata)))
    {
      dat = data()
      dat[,1] = as.numeric(as.character(dat[,1]))
      dat[,2] = as.numeric(as.character(dat[,2]))
      table = data.frame(t(as.matrix(apply(dat,2,summary)[-7,])))
      table$sd[1] = round(sd(dat[,1],na.rm=TRUE),digits=2)
      table$sd[2] = round(sd(dat[,2],na.rm=TRUE),digits=2)
      table = as.matrix(table)
      colnames(table) = c("Min","Q1","Median","Mean","Q3","Max","SD")
      return(table)
    }
  })
    
  #####################################################################################################################
  #####################################################################################################################
  ## T-test Panel
  #####################################################################################################################
  #####################################################################################################################
  
  output$info = renderUI({
    HTML(as.character(code("Click here for hypothesis test information.")))
  })
  
  output$onesample = renderUI({
    HTML(as.character(code("Click here for one-sample t-test information.")))
  })
  
  output$twosample = renderUI({
    HTML(as.character(code("Click here for two-sample t-test information.")))
  })
  
  output$hypo1 = renderUI({
    if((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata))
    {
      if(input$alt1=="less than") 
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; <",input$null1)
      else if(input$alt1=="greater than")
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; >",input$null1)
      else 
        HTML("Ho: &mu; =", input$null1,"<p> Ha: &mu; &ne;",input$null1)
    } 
  })
    
  output$hypo2 = renderUI({
    if((input$datformat!=1 & !input$usedata) | (input$sampdat!=1 & input$usedata))
    {
      if(input$alt2=="less than") 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> <",input$null2)
      else if(input$alt2=="greater than")
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> >",input$null2)
      else 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> &ne;",input$null2)
    } 
  })
  
  mod = reactive({
    input$teststart
    isolate({
    if(input$teststart>0)
    {
      if((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata))
      {
        if(input$alt1=="less than") 
          mod = t.test(x=as.numeric(as.character(unlist(data()))),alternative="less",mu=input$null1,conf.level=1-input$alpha)
        else if(input$alt1=="greater than") 
          mod = t.test(x=as.numeric(as.character(unlist(data()))),alternative="greater",mu=input$null1,conf.level=1-input$alpha)
        else 
          mod = t.test(x=as.numeric(as.character(unlist(data()))),alternative="two.sided",mu=input$null1,conf.level=1-input$alpha)
      } else if((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata))
      {
        dat=data()
        if(length(unique(dat[[1]])) > length(unique(dat[[2]])))
        {
          if(input$alt2=="less than")
            mod = t.test(as.numeric(as.character(dat[[1]]))~dat[[2]],
                         alternative="less",mu=input$null2,conf.level=1-input$alpha)
          else if(input$alt2=="greater than")
            mod = t.test(as.numeric(as.character(dat[[1]]))~dat[[2]],
                         alternative="greater",mu=input$null2,conf.level=1-input$alpha)
          else 
            mod = t.test(as.numeric(as.character(dat[[1]]))~dat[[2]],
                         alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
        } else
        {
          if(input$alt2=="less than")
            mod = t.test(as.numeric(as.character(dat[[2]]))~dat[[1]],
                         alternative="less",mu=input$null2,conf.level=1-input$alpha)
          else if(input$alt2=="greater than")
            mod = t.test(as.numeric(as.character(dat[[2]]))~dat[[1]], 
                         alternative="greater",mu=input$null2,conf.level=1-input$alpha)
          else 
            mod = t.test(as.numeric(as.character(dat[[2]]))~dat[[1]],
                         alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
        }
      } else if((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata))
      {
        dat=data()
        if(input$alt2=="less than")
          mod = t.test(x=as.numeric(as.character(dat[[1]])),y=as.numeric(as.character(dat[[2]])),
                       alternative="less",mu=input$null2,conf.level=1-input$alpha)
        else if(input$alt2=="greater than")
          mod = t.test(x=as.numeric(as.character(dat[[1]])),y=as.numeric(as.character(dat[[2]])),
                       alternative="greater",mu=input$null2,conf.level=1-input$alpha)
        else 
          mod = t.test(x=as.numeric(as.character(dat[[1]])),y=as.numeric(as.character(dat[[2]])),
                       alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
      }
    }
    })
  })
  
  output$est=renderUI({
    if(input$teststart>0 & input$showpoint & ((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata)))
    {
      HTML("x&#773; =",round(mod()$estimate[1],2))
    } else if(input$teststart>0 & input$showpoint & ((input$datformat!=1 & !input$usedata) | (input$sampdat!=1 & input$usedata)))
    {
      HTML("x&#773<sub>1</sub> =",round(mod()$estimate[1],2),"<p> x&#773<sub>2</sub> =",round(mod()$estimate[2],2),
           "<p> x&#773<sub>1</sub> - x&#773<sub>2</sub> =",round(mod()$estimate[1]-mod()$estimate[2],2))
    }
  })
 
  output$test = renderTable({
    input$teststart
    isolate({
    if(input$teststart>0)
    {
      tab = matrix(c(mod()$parameter,mod()$statistic,mod()$p.value),nrow=1)
      colnames(tab) = c("df","t-statistic","p-value")
      rownames(tab) = "Values"
      tab
    } 
    })
  })
  
  output$tdist = renderPlot({
    input$teststart
    isolate({
    if(input$alt1=="less than" | input$alt2=="less than")
    {
      tail="left"
    } else if(input$alt1=="greater than" | input$alt2=="greater than")
    {
      tail="right"
    } else if(input$alt1=="two-sided" | input$alt2=="two-sided")
    {
      tail="both"
    } 
    
    return(t.dist.area(mod()$statistic,tail=tail,mod()$parameter))
    })
  })

  output$citab = renderTable({
    if(input$ci & input$teststart>0)
    {
      tab = matrix(c(mod()$conf.int[1],mod()$conf.int[2]),nrow=1)
      colnames(tab) = c("Lower bound","Upper bound")
      rownames(tab) = paste(round(1-input$alpha, digits=3)*100,"% CI",sep="")
      tab
    }
  })
  
  #####################################################################################################################
  #####################################################################################################################
  ## Diagnostics Panel
  #####################################################################################################################
  #####################################################################################################################
  
  output$qqplot = renderPlot({
      if((input$datformat==1 & !input$usedata) | (input$sampdat==1 & input$usedata))
      {
        dat=unlist(data())
        dat1=data.frame(x=as.numeric(as.character(dat)))
        ggplot(data=dat1, aes(sample=x)) + stat_qq(geom="point",color="navy",shape=1) +
          theme_bw() + theme(text=element_text(size=15)) + ggtitle("Q-Q Plot")
      } else if((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata))
      {
        dat=data()
        dat1=data.frame(x=dat[[1]],y=dat[[2]])
        if(length(unique(dat[[1]])) > length(unique(dat[[2]])))
        {
          dat1$x=as.numeric(as.character(dat1$x))
          ggplot(data=dat1, aes(sample=x)) + stat_qq(aes(color=factor(y)),geom="point",shape=1) + theme_bw() +
            theme(text=element_text(size=15)) + ggtitle("Q-Q Plot") + facet_wrap(~y) +
            scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE)
        } else
        {
          dat1$y=as.numeric(as.character(dat1$y))
          ggplot(data=dat1, aes(sample=y)) + stat_qq(aes(color=factor(x)),geom="point",shape=1) + theme_bw() +
            theme(text=element_text(size=15)) + ggtitle("Q-Q Plot") + facet_wrap(~x) +
            scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE)      
        }
      } else if((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata))
      {
        dat=data()
        dat1=data.frame(x=c(as.numeric(as.character(dat[[1]])),as.numeric(as.character(dat[[2]]))),
                        y=c(rep(names(dat)[1],length(dat[[1]])),rep(names(dat)[2],length(dat[[2]]))))
        ggplot(data=dat1, aes(sample=x)) + stat_qq(aes(color=factor(y)),geom="point",shape=1) + theme_bw() +
          theme(text=element_text(size=15)) + ggtitle("Q-Q plot") + facet_wrap(~y) +
          scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE)      
      }
  })
  
  output$sw = renderTable({
      if((input$datformat==1 & !input$usedata) | (input$sampdat!=2 & input$usedata))
      {
        dat=unlist(data())
        dat1=data.frame(x=as.numeric(as.character(dat)))
        
        validate(
          need(try(shapiro.test(dat1$x)), "Do not need to conduct normality test")
        )
        
        norm = shapiro.test(dat1$x)
        
        tab = matrix(c(norm$statistic,norm$p.value),nrow=1)
        colnames(tab) = c("W statistic","p-value")
        rownames(tab) = "Data"
        tab
      } else if((input$datformat==2 & !input$usedata) | (input$sampdat!=1 & input$usedata))
      {
        dat=data()
        dat1=data.frame(x=dat[[1]],y=dat[[2]])
        if(length(unique(dat[[1]])) > length(unique(dat[[2]])))
        {
          dat1$x=as.numeric(as.character(dat1$x))
          
          norm1 = shapiro.test(dat1$x[which(dat1$y==unique(dat1$y)[1])])
          norm2 = shapiro.test(dat1$x[which(dat1$y==unique(dat1$y)[2])])
          
          tab = matrix(c(norm1$statistic,norm2$statistic,norm1$p.value,norm2$p.value),ncol=2)
          colnames(tab) = c("W statistic","p-value")
          rownames(tab) = c("Data 1","Data 2")
          tab
        } else
        {
          dat1$y=as.numeric(as.character(dat1$y))
          
          norm1 = shapiro.test(dat1$y[which(dat1$x==unique(dat1$x)[1])])
          norm2 = shapiro.test(dat1$y[which(dat1$x==unique(dat1$x)[2])])
          
          tab = matrix(c(norm1$statistic,norm2$statistic,norm1$p.value,norm2$p.value),ncol=2)
          colnames(tab) = c("W statistic","p-value")
          rownames(tab) = c("Data 1","Data 2")
          tab   
        }
      } else if((input$datformat==3 & !input$usedata) | (input$sampdat!=1 & input$usedata))
      {
        dat=data()
        
        norm1 = shapiro.test(as.numeric(as.character(dat[[1]])))
        norm2 = shapiro.test(as.numeric(as.character(dat[[2]])))
        
        tab = matrix(c(norm1$statistic,norm2$statistic,norm1$p.value,norm2$p.value),ncol=2)
        colnames(tab) = c("W statistic","p-value")
        rownames(tab) = c("Data 1","Data 2")
        tab    
      }
  })
  
})