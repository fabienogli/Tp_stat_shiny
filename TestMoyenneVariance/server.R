library(shiny)
 f.dist.area = function(statistique,tail,df)
{
#plage de valeur de l'axe x
  x = seq(0,5,length.out=200)
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
      ggtitle(paste("Distribution-f avec", df, "dégré de liberté")) +
      xlab("Valeurs de t") + ylab("Frequences relatives") + theme_bw()
  } else if(tail=="left")
  {
    xmin=0
    xmax=statistique
    
    area = seq(xmin,xmax,length.out=200)
    dat = data.frame(x=area,ymin=0,ymax=dt(area,df=df))
    #tracé du graphe
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat, mapping=aes(x=x, ymin=ymin, ymax=ymax), fill="navy") +
      ggtitle(paste("Distribution-f avec", df, "dégré de liberté")) +
      xlab("Valeurs de t") + ylab("Frequences relatives") + theme_bw()
  } else if(tail=="both")
  {
    xmin1=abs(statistique)
    xmax1=5
    area1 = seq(xmin1,xmax1,length.out=200)
    dat1 = data.frame(x=area1,ymin1=0,ymax1=dt(area1,df=df))
    
    xmin2=0
	#
    xmax2=abs(statistique)
    area2 = seq(xmin2,xmax2,length.out=200)
    dat2 = data.frame(x=area2,ymin2=0,ymax2=dt(area2,df=df))
    #tracé du graphe
    graph = ggplot() + geom_line(data.frame(x=x, y=dt(x,df=df)), mapping=aes(x=x, y=y)) + 
      geom_ribbon(data=dat1, mapping=aes(x=x, ymin=ymin1, ymax=ymax1),fill="navy") +
      geom_ribbon(data=dat2, mapping=aes(x=x, ymin=ymin2, ymax=ymax2),fill="navy") +
      ggtitle(paste("Distribution-f avec", df, "dégré de liberté")) +
      xlab("Valeurs de t") + ylab("Frequences relatives") + theme_bw()
  }
  return(graph)
}
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

shinyServer(function(input, output) 
{
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
      file = read.csv(input$file$datapath, header = FALSE,sep=input$sep, quote=input$quote)
      return(file)
    } 
	
    })
    
  output$data.tab = renderDataTable({
   data()
  })
  
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
  #Affichage des hypthèses  
  output$hypo = renderUI({
    if((input$datformat==2 || input$datformat==3 ))
    {
      if(input$alt2=="inferieur") 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$difmoy,
             "<p> H1: &mu;<sub>1</sub>-&mu;<sub>2</sub> <",input$difmoy)
      else if(input$alt2=="superieur")
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$difmoy,
             "<p> H1: &mu;<sub>1</sub>-&mu;<sub>2</sub> >",input$difmoy)
      else 
        HTML("Ho: &mu;<sub>1</sub>-&mu;<sub>2</sub> =",input$difmoy,
             "<p> H1: &mu;<sub>1</sub>-&mu;<sub>2</sub> &ne;",input$difmoy)
    } 
  })
  
  #permet d'afficher dans la page la representation graphique de la distribution de t
  output$tdistrib = renderPlot({
    input$teststart
    isolate({
    if( input$alt2=="inferieur")
    {
      tail="left"
    } else if(input$alt2=="superieur")
    {
      tail="right"
    } else if(input$alt2=="bilateral")
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
      colnames(tab) = c("Dégré de liberté","T-statistic","P-value")
      rownames(tab) = "Values"
      tab
    } 
    })
  })
  #Permet de récupérer le résultat du t-test
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
                         alternative="less",mu=input$difmoy,conf.level=1-input$alpha)
          else if(input$alt2=="superieur")
            resultat = t.test(as.numeric(as.character(donnees[[1]]))~donnees[[2]],
                         alternative="greater",mu=input$difmoy,conf.level=1-input$alpha)
          else 
            resultat = t.test(as.numeric(as.character(donnees[[1]]))~donnees[[2]],
                         alternative="two.sided",mu=input$difmoy,conf.level=1-input$alpha)
        } else
        {
          if(input$alt2=="inferieur")
            resultat = t.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]],
                         alternative="less",mu=input$difmoy,conf.level=1-input$alpha)
          else if(input$alt2=="superieur")
            resultat = t.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]], 
                         alternative="greater",mu=input$difmoy,conf.level=1-input$alpha)
          else 
            resultat = t.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]],
                         alternative="two.sided",mu=input$difmoy,conf.level=1-input$alpha)
        }
      } else if((input$datformat==3 ))
      {
        donnees=data()
        if(input$alt2=="inferieur")
          resultat = t.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),
                       alternative="less",mu=input$difmoy,conf.level=1-input$alpha)
        else if(input$alt2=="superieur")
          resultat = t.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),
                       alternative="greater",mu=input$difmoy,conf.level=1-input$alpha)
        else 
          resultat = t.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),
                       alternative="two.sided",mu=input$difmoy,conf.level=1-input$alpha)
      }
    }
    })
  })
 # Permet de récupérer le résultat du f-test
  resultat1 = reactive({
    input$test1start
    isolate({
    if(input$test1start>0)
    {
	  if((input$datformat==2 ))
      {
        donnees=data()
        if(length(unique(donnees[[1]])) > length(unique(donnees[[2]])))
        {
          if(input$alt3=="inferieur")
            resultat1 = var.test(as.numeric(as.character(donnees[[1]]))~donnees[[2]],ratio=input$difmoy1,
                         alternative="less",conf.level=1-input$alpha1)
          else if(input$alt3=="superieur")
            resultat1 = var.test(as.numeric(as.character(donnees[[1]]))~donnees[[2]],ratio=input$difmoy1,
                         alternative="greater",conf.level=1-input$alpha1)
          else 
            resultat1 = var.test(as.numeric(as.character(donnees[[1]]))~donnees[[2]],ratio=input$difmoy1,
                         alternative="two.sided",conf.level=1-input$alpha1)
        } else
        {
          if(input$alt3=="inferieur")
            resultat1 = var.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]],ratio=input$difmoy1,
                         alternative="less",conf.level=1-input$alpha1)
          else if(input$alt3=="superieur")
            resultat1 = var.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]],ratio=input$difmoy1, 
                         alternative="greater",conf.level=1-input$alpha1)
          else 
            resultat1 = var.test(as.numeric(as.character(donnees[[2]]))~donnees[[1]],ratio=input$difmoy1,
                         alternative="two.sided",conf.level=1-input$alpha1)
        }
	}
       else if((input$datformat==3 ))
      {
        donnees=data()
        if(input$alt3=="inferieur")
          resultat1 = var.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),ratio=input$difmoy1,
                       alternative="less",conf.level=1-input$alpha1)
        else if(input$alt3=="superieur")
          resultat1 = var.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),ratio=input$difmoy1,
                       alternative="greater",conf.level=1-input$alpha1)
        else 
          resultat1 = var.test(x=as.numeric(as.character(donnees[[1]])),y=as.numeric(as.character(donnees[[2]])),ratio=input$difmoy1,
                       alternative="two.sided",conf.level=1-input$alpha1)
		}
    }
    })
  })
  #Affichage des hypthèses sur les variances
    output$hypo1 = renderUI({
    if((input$datformat==2 || input$datformat==3 ))
    {
      if(input$alt3=="inferieur") 
        HTML("Ho: &sigma;<sub>1</sub><sup>2</sup>/&sigma;<sub>2</sub><sup>2</sup> =",input$difmoy1,
             "<p> H1: &sigma;<sub>1</sub><sup>2</sup><&sigma;<sub>2</sub><sup>2</sup>")
      else if(input$alt3=="superieur")
        HTML("Ho: &sigma;<sub>1</sub><sup>2</sup>/&sigma;<sub>2</sub><sup>2</sup> =",input$difmoy1,
             "<p> H1: &sigma;<sub>1</sub><sup>2</sup>>&sigma;<sub>2</sub><sup>2</sup>")
      else 
        HTML("Ho: &sigma;<sub>1</sub><sup>2</sup>/&sigma;<sub>2</sub><sup>2</sup> =",input$difmoy1,
             "<p> H1: &sigma;<sub>1</sub><sup>2</sup>&ne;&sigma;<sub>2</sub><sup>2</sup> ")
    } 
  })
  
 

#permet d'afficher dans la page la representation graphique de la distribution de t
  output$fdistrib = renderPlot({
    input$test1start
    isolate({
    if( input$alt3=="inferieur")
    {
      tail="left"
    } else if(input$alt3=="superieur")
    {
      tail="right"
    } else if(input$alt3=="bilateral")
    {
      tail="both"
    } 
    #on passe en parametre a la fonction t.dist.area la valeur de la statistique t,le type de test(inferieur,superieur ou bilateral) ,et le degré de liberté
    return(f.dist.area(resultat1()$statistic,tail=tail,resultat1()$parameter))
    })
  }) 
  #fonction permettant d'afficher les estimations du ratio des variances
  output$estim1=renderUI({
     
	if(input$test1start>0 & input$showpoint1 & ((input$datformat==2 | input$datformat==3 )))
    {
      HTML("ratio des variances =",round(resultat1()$estimate,2))
    }
  })
  #permet d'afficher le tableau de l'intervalle de confiance
  output$ictab1 = renderTable({
  #si la variable ci est définie et le test a démarré
    if(input$ci1 & input$test1start>0)
    {
      tab = matrix(c(resultat1()$conf.int[1],resultat1()$conf.int[2]),nrow=1)
      colnames(tab) = c("Limite à gauche","Limite à droite")
      rownames(tab) = paste(round(1-input$alpha1, digits=3)*100,"% IC",sep="")
      tab
    }
  })
  
  output$test1 = renderTable({
    input$test1start
    isolate({
	 #si le test a démarré
    if(input$test1start>0)
    {
      tab1 = matrix(c(resultat1()$parameter[1],resultat1()$parameter[2],resultat1()$statistic,resultat1()$p.value),nrow=1)
      colnames(tab1) = c("Dégré de liberté Numérateur","Dégré de liberté Denominateur","F-statistic","P-value")
      rownames(tab1) = "Values"
      tab1
    } 
    })
  })
  
})
