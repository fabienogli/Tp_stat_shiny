

#distribution t
#permet de tracer l'affichage graphique de la distribution t
# Le résultat de la fonction t.test() est une liste contenant, entre autres, les éléments suivants :
# statistic : La valeur de la statistique t.
# parameter : Le degré de liberté
# p.value : p-value du test
# conf.int : L’intervalle de confiance de la moyenne (à 95% par défaut) en fonction du type de test
# estimate : Il correspond aux valeurs moyennes des deux groupes à comparer (dans le cas d’un test-t indépendant) ou la moyenne de la différence (dans le cas d’un test apparié).
#ici on passe en parametre la statistique t, le type de test(inferieur,superieur ou bilateral) tail,et le degré de liberté df
t.dist.area = function(statistique,tail,df)
{
#plage de valeur de l'axe x
  x = seq(-5,5,length.out=200)
  df = round(df, digits=3)
  
  if(tail=="right")
  {
    xmin=statistique
    xmax=5
    
    area = seq(xmin,xmax,length.out=200)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    #tracé du graphe
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="navy") + 
      ggtitle(paste("Distribution-t avec", df, "dégré de liberté")) +
      xlab("Valeurs de t") + ylab("Frequences relatives") + theme_bw()
  } else if(tail=="left")
  {
    xmin=-5
    xmax=statistique
    
    area = seq(xmin,xmax,length.out=200)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    #tracé du graphe
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="navy") +
      ggtitle(paste("Distribution-t avec", df, "dégré de liberté")) +
      xlab("Valeurs de t") + ylab("Frequences relatives") + theme_bw()
  } else if(tail=="both")
  {
    xmin1=abs(statistique)
    xmax1=5
    area1 = seq(xmin1,xmax1,length.out=200)
    dat1 = data.frame(x=area1,ymin1=0,ymax1=dt(area1,df=df))
    
    xmin2=-5
    xmax2=-abs(statistique)
    area2 = seq(xmin2,xmax2,length.out=200)
    dat2 = data.frame(x=area2,ymin2=0,ymax2=dt(area2,df=df))
    #tracé du graphe
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat1, mapping=aes(x=x, ymin=ymin1, ymax=ymax1),fill="navy") +
      geom_ribbon(data=dat2, mapping=aes(x=x, ymin=ymin2, ymax=ymax2),fill="navy") +
      ggtitle(paste("Distribution-t avec", df, "dégré de liberté")) +
      xlab("Valeurs de t") + ylab("Frequences relatives") + theme_bw()
  }
  return(graph)
}

## Chargement des librairies
options(shiny.maxRequestSize=30*1024^2)
library(ggplot2)
library(shinyBS)
data(faithful)
data(mtcars)

## Le serveur shiny

shinyServer(function(input, output) {
  ## Page de chargement de données
  
  data = reactive({
  #si il n'y a pas de fichier spécifié
    if(is.null(input$file)) 
    {
      return(NULL)
    } #si il y a un fichier spécifié
	else if(!is.null(input$file) )
    {
	#lecture du fichier csv
      file = read.csv(input$file$datapath, header=input$header, sep=input$sep, quote=input$quote)
      return(file)
    } 
	# else if(input$sampdat==2 & input$usedata)
    # {
      # mtcars$amcoded = rep(NA,length(mtcars$hp))
      # mtcars$amcoded[which(mtcars$am==0)] = "automatic"
      # mtcars$amcoded[which(mtcars$am==1)] = "manual"
      # return(data.frame(transmission=mtcars$amcoded, horsepower=mtcars$hp))
    # }
    })
    
  output$data.tab = renderDataTable({
   data()
  })
  
  # output$data.tab1 = renderDataTable({
    # if(input$usedata) data()
  # })
  
  output$boxPlot = renderPlot({
	if((input$datformat==2 ))
    {
      dat=data()
      dat1=data.frame(x=dat[[1]],y=dat[[2]])
      if(length(unique(dat1$x))>length(unique(dat1$y)))
      {
        dat1$x = as.numeric(as.character(dat1$x))
        ggplot(data=dat1) + geom_boxplot(aes(x=factor(y),y=x,fill=factor(y)),alpha=.5) + 
          xlab(paste(names(dat)[2])) + ylab(paste(names(dat)[1])) + theme_bw() +
          ggtitle(paste("Boxplots de",paste(names(dat)[1]),"par",paste(names(dat)[2]))) +
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
    } else if((input$datformat==3 ))
    {
      donnees=data()
      dat1=data.frame(x=c(as.numeric(as.character(donnees[[1]])),as.numeric(as.character(donnees[[2]]))),
                       y=c(rep(names(donnees)[1],length(donnees[[1]])),rep(names(donnees)[2],length(donnees[[2]]))))
       ggplot(data=dat1) + geom_boxplot(aes(x=factor(y),y=x,fill=factor(y)),alpha=.5) + 
        xlab("Nom de variable") + ylab("Valeurs") +
        scale_fill_manual(name="",values=c("seagreen2","gold2")) +
        ggtitle("Boxplots") + theme_bw() + theme(legend.position="bottom")
    }
  })
  
  output$resumeStats = renderTable({
	if(input$displaystats & ((input$datformat==2 )))
    {
      donnees=data()
      dat1=data.frame(x=donnees[[1]],y=donnees[[2]])
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
		#attribution des noms de colonnes
        colnames(table) = c("Min","Q1","Mediane","Moyenne","Q3","Max","SD")
		#attribution des noms de lignes
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
		#attribution des noms de colonnes
        colnames(table) = c("Min","Q1","Mediane","Moyenne","Q3","Max","SD")
		#attribution des noms de lignes
        rownames(table) = c(levels(dat1$x)[1],levels(dat1$x)[2])
        return(table)         
      }
    } else if(input$displaystats & ((input$datformat==3 )))
    {
      donnees = data()
      donnees[,1] = as.numeric(as.character(donnees[,1]))
      donnees[,2] = as.numeric(as.character(donnees[,2]))
      table = data.frame(t(as.matrix(apply(donnees,2,summary)[-7,])))
      table$sd[1] = round(sd(donnees[,1],na.rm=TRUE),digits=2)
      table$sd[2] = round(sd(donnees[,2],na.rm=TRUE),digits=2)
      table = as.matrix(table)
	  #attribution des noms de colonnes
      colnames(table) = c("Min","Q1","Mediane","Moyenne","Q3","Max","SD")
      return(table)
    }
  })
    
  ## T-test 
    
  output$hypo = renderUI({
    if((input$datformat==2 || input$datformat==3 ))
    {
      if(input$alt2=="inferieur") 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> <",input$null2)
      else if(input$alt2=="superieur")
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> >",input$null2)
      else 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$null2,
             "<p> Ha: &mu;<sub>1</sub>-&mu;<sub>2</sub> &ne;",input$null2)
    } 
  })
  
  resultat = reactive({
    input$teststart
    isolate({
    if(input$teststart>0)
    {
	  if((input$datformat==2 ))
      {
        donnees=data()
        if(length(unique(donnees[[1]])) > length(unique(donnees[[2]])))
        {
          if(input$alt2=="inferieur")
            resultat = t.test(as.numeric(as.character(donnees[[1]]))~donnees[[2]],
                         alternative="less",mu=input$null2,conf.level=1-input$alpha)
          else if(input$alt2=="superieur")
            resultat = t.test(as.numeric(as.character(donnees[[1]]))~donnees[[2]],
                         alternative="greater",mu=input$null2,conf.level=1-input$alpha)
          else 
            resultat = t.test(as.numeric(as.character(donnees[[1]]))~donnees[[2]],
                         alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
        } else
        {
          if(input$alt2=="inferieur")
            resultat = t.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]],
                         alternative="less",mu=input$null2,conf.level=1-input$alpha)
          else if(input$alt2=="superieur")
            resultat = t.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]], 
                         alternative="greater",mu=input$null2,conf.level=1-input$alpha)
          else 
            resultat = t.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]],
                         alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
        }
      } else if((input$datformat==3 ))
      {
        donnees=data()
        if(input$alt2=="inferieur")
          resultat = t.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),
                       alternative="less",mu=input$null2,conf.level=1-input$alpha)
        else if(input$alt2=="superieur")
          resultat = t.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),
                       alternative="greater",mu=input$null2,conf.level=1-input$alpha)
        else 
          resultat = t.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),
                       alternative="two.sided",mu=input$null2,conf.level=1-input$alpha)
      }
    }
    })
  })
 #fonction permettant d'afficher les estimations des moyennes
  output$estim=renderUI({
     
	if(input$teststart>0 & input$showpoint & ((input$datformat==2 | input$datformat==3 )))
    {
      HTML("x&#773<sub>1</sub> =",round(resultat()$estimate[1],2),"<p> x&#773<sub>2</sub> =",round(resultat()$estimate[2],2),
           "<p> x&#773<sub>1</sub> - x&#773<sub>2</sub> =",round(resultat()$estimate[1]-resultat()$estimate[2],2))
    }
  })
 
  output$test = renderTable({
    input$teststart
    isolate({
	 #si le test a démarré
    if(input$teststart>0)
    {
      tab = matrix(c(resultat()$parameter,resultat()$statistic,resultat()$p.value),nrow=1)
      colnames(tab) = c("df","t-statistic","p-value")
      rownames(tab) = "Values"
      tab
    } 
    })
  })
  
  #permet d'afficher dans la page la representation graphique de la distribution de t
  output$tdistrib = renderPlot({
    input$teststart
    isolate({
    if(input$alt1=="inferieur" | input$alt2=="inferieur")
    {
      tail="left"
    } else if(input$alt1=="superieur" | input$alt2=="superieur")
    {
      tail="right"
    } else if(input$alt1=="bilateral" | input$alt2=="bilateral")
    {
      tail="both"
    } 
    #on passe en parametre a la fonction t.dist.area la valeur de la statistique t,le type de test(inferieur,superieur ou bilateral) ,et le degré de liberté
    return(t.dist.area(resultat()$statistic,tail=tail,resultat()$parameter))
    })
  })

  #permet d'afficher le tableau de l'intervalle de confiance
  output$ictab = renderTable({
  #si la variable ci est définie et le test a démarré
    if(input$ci & input$teststart>0)
    {
      tab = matrix(c(resultat()$conf.int[1],resultat()$conf.int[2]),nrow=1)
      colnames(tab) = c("Limite à gauche","Limite à droite")
      rownames(tab) = paste(round(1-input$alpha, digits=3)*100,"% IC",sep="")
      tab
    }
  })
  
  #####################################################################################################################
  #####################################################################################################################
  ## Diagnostics Panel
  #####################################################################################################################
  #####################################################################################################################
  
  # output$qqplot = renderPlot({
      # if((input$datformat==1 ) | (input$sampdat==1 & input$usedata))
      # {
        # donnees=unlist(data())
        # dat1=data.frame(x=as.numeric(as.character(donnees)))
        # ggplot(data=dat1, aes(sample=x)) + stat_qq(geom="point",color="navy",shape=1) +
          # theme_bw() + theme(text=element_text(size=15)) + ggtitle("Q-Q Plot")
      # } else if((input$datformat==2 ) | (input$sampdat!=1 & input$usedata))
      # {
        # donnees=data()
        # dat1=data.frame(x=donnees[[1]],y=donnees[[2]])
        # if(length(unique(donnees[[1]])) > length(unique(donnees[[2]])))
        # {
          # dat1$x=as.numeric(as.character(dat1$x))
          # ggplot(data=dat1, aes(sample=x)) + stat_qq(aes(color=factor(y)),geom="point",shape=1) + theme_bw() +
            # theme(text=element_text(size=15)) + ggtitle("Q-Q Plot") + facet_wrap(~y) +
            # scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE)
        # } else
        # {
          # dat1$y=as.numeric(as.character(dat1$y))
          # ggplot(data=dat1, aes(sample=y)) + stat_qq(aes(color=factor(x)),geom="point",shape=1) + theme_bw() +
            # theme(text=element_text(size=15)) + ggtitle("Q-Q Plot") + facet_wrap(~x) +
            # scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE)      
        # }
      # } else if((input$datformat==3 ) | (input$sampdat!=1 & input$usedata))
      # {
        # donnees=data()
        # dat1=data.frame(x=c(as.numeric(as.character(donnees[[1]])),as.numeric(as.character(donnees[[2]]))),
                        # y=c(rep(names(donnees)[1],length(donnees[[1]])),rep(names(donnees)[2],length(donnees[[2]]))))
        # ggplot(data=dat1, aes(sample=x)) + stat_qq(aes(color=factor(y)),geom="point",shape=1) + theme_bw() +
          # theme(text=element_text(size=15)) + ggtitle("Q-Q plot") + facet_wrap(~y) +
          # scale_color_manual(name="", values=c("navy","gold2")) + guides(color=FALSE)      
      # }
  # })
  
  # output$sw = renderTable({
      # if((input$datformat==1 ) | (input$sampdat!=2 & input$usedata))
      # {
        # donnees=unlist(data())
        # dat1=data.frame(x=as.numeric(as.character(donnees)))
        
        # validate(
          # need(try(shapiro.test(dat1$x)), "Do not need to conduct normality test")
        # )
        
        # norm = shapiro.test(dat1$x)
        
        # tab = matrix(c(norm$statistic,norm$p.value),nrow=1)
        # colnames(tab) = c("W statistic","p-value")
        # rownames(tab) = "Data"
        # tab
      # } else if((input$datformat==2 ) | (input$sampdat!=1 & input$usedata))
      # {
        # donnees=data()
        # dat1=data.frame(x=donnees[[1]],y=donnees[[2]])
        # if(length(unique(donnees[[1]])) > length(unique(donnees[[2]])))
        # {
          # dat1$x=as.numeric(as.character(dat1$x))
          
          # norm1 = shapiro.test(dat1$x[which(dat1$y==unique(dat1$y)[1])])
          # norm2 = shapiro.test(dat1$x[which(dat1$y==unique(dat1$y)[2])])
          
          # tab = matrix(c(norm1$statistic,norm2$statistic,norm1$p.value,norm2$p.value),ncol=2)
          # colnames(tab) = c("W statistic","p-value")
          # rownames(tab) = c("Data 1","Data 2")
          # tab
        # } else
        # {
          # dat1$y=as.numeric(as.character(dat1$y))
          
          # norm1 = shapiro.test(dat1$y[which(dat1$x==unique(dat1$x)[1])])
          # norm2 = shapiro.test(dat1$y[which(dat1$x==unique(dat1$x)[2])])
          
          # tab = matrix(c(norm1$statistic,norm2$statistic,norm1$p.value,norm2$p.value),ncol=2)
          # colnames(tab) = c("W statistic","p-value")
          # rownames(tab) = c("Data 1","Data 2")
          # tab   
        # }
      # } else if((input$datformat==3 ) | (input$sampdat!=1 & input$usedata))
      # {
        # donnees=data()
        
        # norm1 = shapiro.test(as.numeric(as.character(donnees[[1]])))
        # norm2 = shapiro.test(as.numeric(as.character(donnees[[2]])))
        
        # tab = matrix(c(norm1$statistic,norm2$statistic,norm1$p.value,norm2$p.value),ncol=2)
        # colnames(tab) = c("W statistic","p-value")
        # rownames(tab) = c("Data 1","Data 2")
        # tab    
      # }
  # })
  
})