# Synchronisation du 
set.seed(1)

# Jeu aleatoire de donnees
mat=matrix(rnorm(20000),1000,20)
mat2=mat  # archivage

# Calcul du test de Student sur la 1ere ligne
res=t.test(mat[1,1:10],mat[1,11:20]); res
str(res)  # structure du résultat
res$p.value  # extraction de la p-value


# Boucle pour extraire toutes les p-values

# Version avec accumulation
vec=NULL
for(i in 1:nrow(mat)) {
  res2=t.test(mat[i,1:10],mat[i,11:20])
  #print(res2$p.value)
  vec=c(vec,res2$p.value)
}
vec

# Version avec pre-initilisation
vec = rep(NA, 1000)
for(i in 1:nrow(mat)) {
  res2 = t.test(mat[i,1:10], mat[i,11:20])
  #print(res2$p.value)
  vec[i] = res2$p.value
}
vec

# Description des p-values
summary(vec)
# les valeurs s'étendent de 0 à 1
X11()
hist(vec, nclass = 20) # 1 barre = 0.05
# C'est une loi uniforme
abline(h=50, lw=2, col="red")  # la hauteur moyenne des barres
# la hauteur moyenne est égale aux nombres de tests
# multiplié par la largeur de la barre



mat3 = mat
mat3[1:100, 1:10] = mat[1:100, 1:10] + 2.50


# Version avec pre-initilisation
vec3 = rep(NA, 1000)
for(i in 1:nrow(mat3)) {
  res2 = t.test(mat3[i,1:10], mat3[i,11:20])
  #print(res2$p.value)
  vec3[i] = res2$p.value
}
vec3


hist(vec3, nclass = 20) # 1 barre = 0.05
# C'est une loi uniforme
abline(h=50, lw=2, col="red")  # la hauteur moyenne des barres


#

# ANOVA
df=data.frame(group=rep(c("A","B","C"),each=10), mesure=rnorm(30))
df
df$mesure[11:20]=df$mesure[11:20]+4
df$mesure[11:20]
plot(df$mesure)
plot(df$mesure ~ df$group)
plot(df)
boxplot(df$mesure)
boxplot(df)
r=aov(mesure~group,data=df)
anova(r)
TukeyHSD(r)
