setwd( "C:\\WorkspaceR")
df_train.csv <- ( read.csv( file = "train.csv",
                      header = T,
                      encoding = "UTF-8"))
df_train.csv

class(df_train.csv)
dim(df_train.csv)
str(df_train.csv)
head(df_train.csv)
tail(df_train.csv)

