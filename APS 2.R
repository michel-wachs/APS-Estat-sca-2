length(SP$`math score`[SP$`math score`==100])
length(SP$`reading score`[SP$`reading score`==100])
length(SP$`writing score`[SP$`writing score`==100])




cor(SP$`math score`,SP$`reading score`)
cor(SP$`math score`,SP$`writing score`)
cor(SP$`writing score`,SP$`reading score`)



#comparando as notas entre meninos e meninas


#matemática

#meninos:
par(mfrow=c(1,2))
hist(SP$`math score`[SP$gender=="male"],
     main = "Notas em matemática Meninos", xlab = "nota", ylab="freqência",
     xlim = c(0,100))
summary(SP$`math score`[SP$gender=="male"])
sd(SP$`math score`[SP$gender=="male"])


#meninas
hist(SP$`math score`[SP$gender=="female"],
     main = "Notas em matemática Meninas", xlab = "nota", ylab="freqência",
     xlim = c(0,100))
summary(SP$`math score`[SP$gender=="female"])
sd(SP$`math score`[SP$gender=="female"])


#leitura
#meninos
par(mfrow=c(1,2))
hist(SP$`reading score`[SP$gender=="male"],
     main = "Notas em leitura Meninos", xlab = "nota", ylab="freqência",
     xlim = c(0,100))
summary(SP$`reading score`[SP$gender=="male"])
sd(SP$`reading score`[SP$gender=="male"])


#meninas
hist(SP$`reading score`[SP$gender=="female"],
     main = "Notas em leitura Meninas", xlab = "nota", ylab="freqência",
     xlim = c(0,100))
summary(SP$`reading score`[SP$gender=="female"])
sd(SP$`reading score`[SP$gender=="female"])



#redação
#meninos:
par(mfrow=c(1,2))
hist(SP$`writing score`[SP$gender=="male"],
     main = "Notas em redação Meninos", xlab = "nota", ylab="freqência",
     xlim = c(0,100))
summary(SP$`writing score`[SP$gender=="male"])
sd(SP$`writing score`[SP$gender=="male"])


#meninas
hist(SP$`writing score`[SP$gender=="female"],
     main = "Notas em redação Meninas", xlab = "nota", ylab="freqência",
     xlim = c(0,100))
summary(SP$`writing score`[SP$gender=="female"])
sd(SP$`writing score`[SP$gender=="female"])



#Matemática 

par(mfrow=c(3,2))
hist(SP$`math score`[SP$`parental level of education`=="some high school"], col = "red",
     main = "some high school", xlim = c(0,100), ylim=c(0,70))


hist(SP$`math score`[SP$`parental level of education`=="high school"], col = "blue",
     main = "high school", xlim = c(0,100), ylim=c(0,70))


hist(SP$`math score`[SP$`parental level of education`=="some college"], col = "green",
     main = "some college", xlim = c(0,100), ylim=c(0,70))


hist(SP$`math score`[SP$`parental level of education`=="associate's degree"], col = "gray",
     main = "associate's degree", xlim = c(0,100), ylim=c(0,70))


hist(SP$`math score`[SP$`parental level of education`=="bachelor's degree"], col = "black",
     main = "bachelor's degree", xlim = c(0,100), ylim=c(0,70))


hist(SP$`math score`[SP$`parental level of education`=="master's degree"], col = "white",
     main = "master's degree", xlim = c(0,100), ylim=c(0,70))

#COMPARANDO DESEMPENHO POR LUNCH



#matematica



par(mfrow=c(3,2))
hist(SP$`math score`[SP$lunch=="standard"], col = "red",
     main = "standard in math", xlim = c(0,120), ylim=c(0,200))
hist(SP$`math score`[SP$lunch=="free/reduced"], col = "blue",
     main = "free/reduced in math", xlim = c(0,120), ylim=c(0,200))



#leitura
hist(SP$`reading score`[SP$lunch=="standard"],col = "green",
     main = "standard in reading", xlim = c(0,120), ylim=c(0,200))
hist(SP$`reading score`[SP$lunch=="free/reduced"],col = "gray",
     main = "free/reduced in reading", xlim = c(0,120), ylim=c(0,200))



#redação
hist(SP$`writing score`[SP$lunch=="standard"],col = "black",
     main = "standard in writing", xlim = c(0,120), ylim=c(0,200))
hist(SP$`writing score`[SP$lunch=="free/reduced"],col = "white",
     main = "free/reduced in writing", xlim = c(0,120), ylim=c(0,200))



#COMPARANDO TEST PREPARATION COURSE




par(mfrow=c(3,2))
#matematica
hist(SP$`math score`[SP$`test preparation course`=="none"], col = "red",
     main = "none preparation math", xlim = c(0,120), ylim=c(0,180))
hist(SP$`math score`[SP$`test preparation course`=="completed"], col = "blue",
     main = "completed preparation math", xlim = c(0,120), ylim=c(0,180))



#leitura
hist(SP$`reading score`[SP$`test preparation course`=="none"],col = "green",
     main = "none preparation reading", xlim = c(0,120), ylim=c(0,180))
hist(SP$`reading score`[SP$`test preparation course`=="completed"],col = "gray",
     main = "completed preparation reading", xlim = c(0,120), ylim=c(0,180))



#redação
hist(SP$`writing score`[SP$`test preparation course`=="none"],col = "black",
     main = "none preparation writing", xlim = c(0,120), ylim=c(0,200))
hist(SP$`writing score`[SP$`test preparation course`=="completed"],col = "white",
     main = "completed preparation writing", xlim = c(0,120), ylim=c(0,180))

##nivel de educação dos pais:
#matemática
shm=summary(SP$`math score`[SP$`parental level of education`=="some high school"])
hs=summary(SP$`math score`[SP$`parental level of education`=="high school"])
sc=summary(SP$`math score`[SP$`parental level of education`=="some college"])
ad=summary(SP$`math score`[SP$`parental level of education`=="associate's degree"])
bd=summary(SP$`math score`[SP$`parental level of education`=="bachelor's degree"])
md=summary(SP$`math score`[SP$`parental level of education`=="master's degree"])



tabpar=cbind(shm,hs,sc,ad,bd,md)
colnames(tabpar)=c("some high school", "high school","some college","associate's degree","bachelor's degree",
                   "master's degree")
View(tabpar)



#reading
shmr=summary(SP$`reading score`[SP$`parental level of education`=="some high school"])
hsr=summary(SP$`reading score`[SP$`parental level of education`=="high school"])
scr=summary(SP$`reading score`[SP$`parental level of education`=="some college"])
adr=summary(SP$`reading score`[SP$`parental level of education`=="associate's degree"])
bdr=summary(SP$`reading score`[SP$`parental level of education`=="bachelor's degree"])
mdr=summary(SP$`reading score`[SP$`parental level of education`=="master's degree"])



tabparr=cbind(shmr,hsr,scr,adr,bdr,mdr)
colnames(tabparr)=c("some high school", "high school","some college","associate's degree","bachelor's degree",
                   "master's degree")
View(tabparr)



#writing
shmw=summary(SP$`writing score`[SP$`parental level of education`=="some high school"])
hsw=summary(SP$`writing score`[SP$`parental level of education`=="high school"])
scw=summary(SP$`writing score`[SP$`parental level of education`=="some college"])
adw=summary(SP$`writing score`[SP$`parental level of education`=="associate's degree"])
bdw=summary(SP$`writing score`[SP$`parental level of education`=="bachelor's degree"])
mdw=summary(SP$`writing score`[SP$`parental level of education`=="master's degree"])



tabparw=cbind(shmw,hsw,scw,adw,bdw,mdw)
colnames(tabparw)=c("some high school", "high school","some college","associate's degree","bachelor's degree",
                   "master's degree")
View(tabparw)


View(SP)

##lunch:
#matemática
free=summary(SP$`math score`[SP$lunch=="free/reduced"])
stan=summary(SP$`math score`[SP$lunch=="standard"])
tablun=cbind(free,stan)
colnames(tablun)=c("free/reduced","standard")
View(tablun)

#reading
freer=summary(SP$`reading score`[SP$lunch=="free/reduced"])
stanr=summary(SP$`reading score`[SP$lunch=="standard"])
tablunr=cbind(freer,stanr)
colnames(tablunr)=c("free/reduced","standard")
View(tablunr)

#writing
freew=summary(SP$`writing score`[SP$lunch=="free/reduced"])
stanw=summary(SP$`writing score`[SP$lunch=="standard"])
tablunw=cbind(freew,stanw)
colnames(tablunw)=c("free/reduced","standard")
View(tablunw)

##gênero:
#matemática
mas=summary(SP$`math score`[SP$gender=="male"])
fem=summary(SP$`math score`[SP$gender=="female"])
tabgen=cbind(mas,fem)
colnames(tabgen)=c("meninos","meninas")
View(tabgen)

#reading
masr=summary(SP$`reading score`[SP$gender=="male"])
femr=summary(SP$`reading score`[SP$gender=="female"])
tabgenr=cbind(masr,femr)
colnames(tabgenr)=c("meninos","meninas")
View(tabgenr)

#writing
masw=summary(SP$`writing score`[SP$gender=="male"])
femw=summary(SP$`writing score`[SP$gender=="female"])
tabgenw=cbind(masw,femw)
colnames(tabgenw)=c("meninos","meninas")
View(tabgenw)

View(SP)

##curso preparatório
#matemática
fez=summary(SP$`math score`[SP$`test preparation course`=="completed"])
nfez=summary(SP$`math score`[SP$`test preparation course`=="none"])
tabcur=cbind(fez,nfez)
colnames(tabcur)=c("fez curso","não fez curso")
View(tabcur)

#reading
fezr=summary(SP$`reading score`[SP$`test preparation course`=="completed"])
nfezr=summary(SP$`reading score`[SP$`test preparation course`=="none"])
tabcurr=cbind(fezr,nfezr)
colnames(tabcurr)=c("fez curso","não fez curso")
View(tabcurr)

#writing
fezw=summary(SP$`writing score`[SP$`test preparation course`=="completed"])
nfezw=summary(SP$`writing score`[SP$`test preparation course`=="none"])
tabcurw=cbind(fezw,nfezw)
colnames(tabcurw)=c("fez curso","não fez curso")
View(tabcurw)



#Exercício B
matematica<-SP$`math score`
leitura<-SP$`reading score`
escrita<-SP$`writing score`
performance=(matematica+leitura+escrita)/3
SP$performance<-performance
View(SP)

##nivel de educação dos pais:

shmp=summary(SP$performance[SP$`parental level of education`=="some high school"])
hsp=summary(SP$performance[SP$`parental level of education`=="high school"])
scp=summary(SP$performance[SP$`parental level of education`=="some college"])
adp=summary(SP$performance[SP$`parental level of education`=="associate's degree"])
bdp=summary(SP$performance[SP$`parental level of education`=="bachelor's degree"])
mdp=summary(SP$performance[SP$`parental level of education`=="master's degree"])

tabparp=cbind(shmp,hsp,scp,adp,bdp,mdp)
colnames(tabparp)=c("some high school", "high school","some college","associate's degree","bachelor's degree",
                   "master's degree")
View(tabparp)


##lunch:

freep=summary(SP$performance[SP$lunch=="free/reduced"])
stanp=summary(SP$performance[SP$lunch=="standard"])
tablunp=cbind(freep,stanp)
colnames(tablunp)=c("free/reduced","standard")
View(tablunp)


##gênero:

masp=summary(SP$performance[SP$gender=="male"])
femp=summary(SP$performance[SP$gender=="female"])
tabgenp=cbind(masp,femp)
colnames(tabgenp)=c("meninos","meninas")
View(tabgenp)


##curso preparatório

fezp=summary(SP$performance[SP$`test preparation course`=="completed"])
nfezp=summary(SP$performance[SP$`test preparation course`=="none"])
tabcurp=cbind(fezp,nfezp)
colnames(tabcurp)=c("fez curso","não fez curso")
View(tabcurp)










#ITEM C
#teste 1: gênero
#h0: performance homens = performance mulheres
#ha: performance homens diferente de performance mulheres



#teste 2: parental level of education



#teste 3: lunch
#h0: performance standard lunch = performance free/reduced lunch
#ha:performance standard lunch diferente de performance free/reduced lunch



#teste 4: test preparation course
#h0: performance test prep course none = performance test prep course completed
#ha:performance test prep course none diferente de performance test prep course completed





var.test(SP$performance[SP$gender=="male"],SP$performance[SP$gender=="female"],
         alternative = c("two.sided"))
t.test(SP$performance[SP$gender=="female"], SP$performance[SP$gender=="male"],
       var.equal = TRUE, paired = FALSE, alternative = c("two.sided"))



var.test(SP$performance[SP$`test preparation course`=="completed"],
         SP$performance[SP$`test preparation course`=="none"],
         alternative = c("two.sided"))
t.test(SP$performance[SP$`test preparation course`=="completed"], 
       SP$performance[SP$`test preparation course`=="none"],
       var.equal = FALSE, alternative = c("two.sided"))





#D)

##nível educacional pais:
length(SP$performance[SP$`parental level of education`=="bachelor's degree" & performance>=90])/length(SP$performance[SP$`parental level of education`=="bachelor's degree"])
length(SP$performance[SP$`parental level of education`=="master's degree" & performance>=90])/length(SP$performance[SP$`parental level of education`=="master's degree"])
length(SP$performance[SP$`parental level of education`=="associate's degree" & performance>=90])/length(SP$performance[SP$`parental level of education`=="associate's degree"])
length(SP$performance[SP$`parental level of education`=="some college" & performance>=90])/length(SP$performance[SP$`parental level of education`=="some college"])
length(SP$performance[SP$`parental level of education`=="high school" & performance>=90])/length(SP$performance[SP$`parental level of education`=="high school"])
length(SP$performance[SP$`parental level of education`=="some high school" & performance>=90])/length(SP$performance[SP$`parental level of education`=="some high school"])
#bachelor's degree


##gênero
length(SP$performance[SP$gender=="male" & performance>=90])/length(SP$performance[SP$gender=="male"])
length(SP$performance[SP$gender=="female" & performance>=90])/length(SP$performance[SP$gender=="female"])
#female

##almoço
length(SP$performance[SP$lunch=="free/reduced" & performance>=90])/length(SP$performance[SP$lunch=="free/reduced"])
length(SP$performance[SP$lunch=="standard" & performance>=90])/length(SP$performance[SP$lunch=="standard"])
#standard

##curso preparatório
length(SP$performance[SP$`test preparation course`=="completed" & performance>=90])/length(SP$performance[SP$`test preparation course`=="completed"])
length(SP$performance[SP$`test preparation course`=="none" & performance>=90])/length(SP$performance[SP$`test preparation course`=="none"])
#feito


####Buscando evidências se a média da performance muda segundo os diferentes níveis de educação parental
#bachelor's degree
LIbd=mean(SP$performance[SP$`parental level of education`=="bachelor's degree"])-
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="bachelor's degree"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="bachelor's degree"]))
LSbd=mean(SP$performance[SP$`parental level of education`=="bachelor's degree"])+
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="bachelor's degree"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="bachelor's degree"]))
ICbd=c(LIbd,LSbd)

#master's degree
LImd=mean(SP$performance[SP$`parental level of education`=="master's degree"])-
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="master's degree"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="master's degree"]))
LSmd=mean(SP$performance[SP$`parental level of education`=="master's degree"])+
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="master's degree"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="master's degree"]))
ICmd=c(LImd,LSmd)


#associate's degree
LIad=mean(SP$performance[SP$`parental level of education`=="associate's degree"])-
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="associate's degree"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="associate's degree"]))
LSad=mean(SP$performance[SP$`parental level of education`=="associate's degree"])+
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="associate's degree"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="associate's degree"]))
ICad=c(LIad,LSad)


#some college
LIsc=mean(SP$performance[SP$`parental level of education`=="some college"])-
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="some college"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="some college"]))
LSsc=mean(SP$performance[SP$`parental level of education`=="some college"])+
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="some college"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="some college"]))
ICsc=c(LIsc,LSsc)

#high school
LIhc=mean(SP$performance[SP$`parental level of education`=="high school"])-
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="high school"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="high school"]))
LShc=mean(SP$performance[SP$`parental level of education`=="high school"])+
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="high school"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="high school"]))
IChc=c(LIhc,LShc)

#some high school
LIshc=mean(SP$performance[SP$`parental level of education`=="some high school"])-
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="some high school"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="some high school"]))
LSshc=mean(SP$performance[SP$`parental level of education`=="some high school"])+
        qnorm(0.975)*sd(SP$performance[SP$`parental level of education`=="some high school"])/
        sqrt(length(SP$performance[SP$`parental level of education`=="some high school"]))
ICshc=c(LIshc,LSshc)

ICbd 
ICmd 
ICad 
ICsc 
IChc 
ICshc 