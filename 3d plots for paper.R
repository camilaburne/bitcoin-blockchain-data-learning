library(plotly)

getwd()
setwd("/Users/camilaburne/UT/Geometric Methods/Paper")
list.files()

df2  <-  read.csv('transactions_2021_type.csv')
df2u <-  df2[duplicated(df2$txn_hash)==F,]

df2u = df2u[is.na(df2u$txn_input_value)==F,]
df2u$btc_input_value = log(df2u$txn_input_value/100000000)
df2u$btc_input_ogvalue = df2u$txn_input_value/100000000
df2u$btc_output_ogvalue = df2u$txn_output_value/100000000

df2u$log_fee = log(df2u$txn_fee)


df2u = df2u[is.na(df2u$txn_output_value)==F,]
df2u$btc_output_value = log(df2u$txn_output_value/100000000)

names(df)

# Inputs
fig <- plot_ly(df2u, x = ~txn_input_count, y = ~btc_input_value, z = ~txn_size, 
               color = ~input_type)
fig <- fig %>% add_markers(size = 2, alpha=0.75)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Input Count' ),
                                   yaxis = list(title = 'Log of Input Value' ),
                                   zaxis = list(title = 'Size'        )
))
fig



# Outputs
df3u = df2u[df2u$txn_output_count <500, ]
#df3u = df3u[df3u$btc_output_value <5000000000, ]
fig <- plot_ly(df3u, x = ~txn_output_count, y = ~btc_output_value, z = ~log_fee, 
               color = ~input_type)
fig <- fig %>% add_markers(size = 2, alpha=0.75)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Output Count' ),
                                   yaxis = list(title = 'Log of Output Value' ),
                                   zaxis = list(title = 'Log of Fee'        )
))
fig

# 
# 
# 
# 
# 
# 
# 
# df <- read.csv('transactions_taproot2.csv')
# 
# # Taproot flag
# df['taproot'] = 0
# df[df$txn_block_number>=709632,'taproot'] = 1
# 
# df_small = df[0:100000,]
# df_small = df_small[is.na(df_small$txn_input_value)==F,]
# df_small$btc_input_value = df_small$txn_input_value/100000000
# df_small$btc_fee
# 
# names(df)
# 
# # remnant_mass
# fig <- plot_ly(df_small, x = ~txn_input_count, y = ~btc_input_value, z = ~txn_size, 
#                color = ~txn_fee)
# fig <- fig %>% add_markers(size = 3)
# fig <- fig %>% layout(scene = list(xaxis = list(title = 'Input Count', range=c(0,2000)),
#                                    yaxis = list(title = 'Input Value', range=c(0,20000)),
#                                    zaxis = list(title = 'Size',        range=c(0,25000))
# ))
# fig
# 
# 
# max(df_small$txn_size) 
# 
# 
# 


library(glmpca)

#create a simple dataset with two clusters
mu<-rep(c(.5,3),each=10)
mu<-matrix(exp(rnorm(100*20)),nrow=100)
mu[,1:10]<-mu[,1:10]*exp(rnorm(100))
clust<-rep(c("red","black"),each=10)
Y<-matrix(rpois(prod(dim(mu)),mu),nrow=nrow(mu))


#visualize the latent structure
res<-glmpca(Y, 2)
factors<-res$factors
plot(factors[,1],factors[,2],col=clust,pch=19)

X <- df3u[,c('btc_input_ogvalue',
              'txn_fee','txn_input_count',
              'txn_output_count','input_type')]

X <- X[X$btc_input_ogvalue>0.001,]
X <- X[X$txn_fee>0,]



res<-glmpca(X, 2)
factors<-res$factors
plot(factors[,1],factors[,2],col=X$btc_input_ogvalue,pch=19)

plot(my_df$dim1,my_df$dim2, col=my_df$btc_input)


dim1 = res$loadings[1]
dim2 = res$loadings[2]

my_df <- cbind(dim1, dim2)
my_df$btc_input = X$btc_input_ogvalue
my_df$type = X$input_type


 

# Optionally set colours using RColorBrewer
library(RColorBrewer)
cols = brewer.pal(8, "Spectral")
pal = colorRampPalette(cols)

# Rank variable for colour assignment
my_df$order = findInterval(my_df$btc_input, sort(my_df$btc_input))

# Make plot
plot(dim1 ~ dim2, my_df, col=pal(nrow(my_df))[my_df$order])
# Add a simple legend
legend("topright", col=pal(2), pch=19,
       legend=c(round(range(my_df$btc_input), 1)))


# Optionally set colours using RColorBrewer
library(RColorBrewer)
cols = brewer.pal(6, "Set2")
pal = colorRampPalette(cols)

my_df$type = as.factor(my_df$type)


# Make plot
plot(dim1 ~ dim2, my_df, col=type)
# Add a simple legend
legend("topright", col=pal(2), pch=19,
       legend=c(round(range(my_df$btc_input), 1)))


