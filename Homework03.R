library(ade4)
data(doubs)
env <- doubs$env
fish <- doubs$fish
dim(fish)
fish <- fish[-8,]
env <- env[-8,]
dim(fish)
dim(env)

site.fish <- rowSums(fish > 0)
barplot(site.fish, main = "Species richness",
        xlab = "Sites", ylab="Number of species",
        col = "grey", las= 1)

site.fish <- colSums(fish > 0)
barplot(site.fish, main = "Species distribution",
        xlab = "Name of species", ylab="Number of species distribution sites",
        col = "red", las= 1)

library(vegan)
fish.pa <- decostand(fish,method = "pa")
fish.db.pa <- vegdist(fish.pa, method = "bray")

fish.hel <- decostand(fish, method = "hellinger")
dim(fish.hel)
fish.chi <- decostand(fish, method = "chi.square")
fish.dhe1 <- vegdist(fish.hel, method = "euclidean")
fish.dhe1.single <- hclust(fish.dhe1, method = "single")
plot(fish.dhe1.single)

env.pa <- decostand(env,method = "pa")
env.db.pa <- vegdist(env.pa, method = "bray")

env.hel <- decostand(env, method = "hellinger")
env.chi <- decostand(env, method = "chi.square")
env.dhe1 <- vegdist(env.hel, method = "euclidean")
env.dhe1.single <- hclust(env.dhe1, method = "single")
plot(env.dhe1.single)

dca <- decorana(fish)
dca
fish.h.pca <- rda(fish.hel)
ev <- fish.h.pca$CA$eig
ev[ev>mean(ev)] 
n <- length(ev) 
barplot(ev, main = "Eigenvalues", col = "grey", las = 2)
abline(h = mean(ev), col = "red3",lwd = 2)
legend("topright","Average eigenvalue",
       lwd = 2, col ="red3" , bty = "n")
biplot(fish.h.pca)

env.z <- decostand(env, method = "standardize") 
dim(env.z)
env.pca <- rda(env.z)
ev <- env.pca$CA$eig
ev[ev>mean(ev)] 
n <- length(ev) 
barplot(ev, main = "Eigenvalues", col = "grey", las = 2)
abline(h = mean(ev), col = "red3",lwd = 2)
legend("topright","Average eigenvalue",
       lwd = 2, col ="red3" , bty = "n")
biplot(env.pca)

dca <- decorana(fish)
dca
dca1 <- decorana(fish.hel) 
dca1
fish.rda <- rda(fish.hel ~ ., data = env.z)
fwd.sel <- ordiR2step(rda(fish.hel ~ 1, data = env.z),
                    scope = formula(fish.rda), 
                    direction = "forward",
                    R2scope = TRUE,
                    pstep = 1000,
                    trace = FALSE) 
fwd.sel$call
fish.rda.signif <- rda(fish.hel ~ alt + oxy + bdo, data = env.z)
anova.cca(fish.rda.signif, step = 1000, by = "axis")
plot(fish.rda.signif)        
