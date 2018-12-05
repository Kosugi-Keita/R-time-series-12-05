library(rvest)
library(tidyverse)
library(tseries)

kabu_url <- "https://kabutan.jp/stock/kabuka?code=3287&ashi=day&page=1"
kabuka <- read_html(kabu_url) %>%
  html_node(xpath="//*[@id='stock_kabuka_table']/table[2]") %>%
  html_table()
kabuka

urls <- NULL
kabukas <- list()
base_url <- "https://kabutan.jp/stock/kabuka?code=3287&ashi=day&page="

for (i in 1:9){
  pgnum <- as.character(i)
  urls[i] <- paste0(base_url,pgnum)
  
  kabukas[[i]] <- read_html(urls[i]) %>%
    html_node(xpath="//*[@id='stock_kabuka_table']/table[2]") %>%
    html_table()
  #dplyr::mutate_at("前日比",as.character)
  Sys.sleep(1)
}
hoshino <- dplyr::bind_rows(kabukas)
colnames(hoshino) <- c("date","p_ini","p_max","p_min","p_end","dod","dodr","amount")
write.csv(hoshino, "C:\\Users\\kkeit\\Desktop\\R\\For Ruser\\hoshino.csv")

#edit by excel
#cleaning the data(hoshino)

hoshi <- read_csv("hoshino.csv")
colnames(hoshi) <- c("num","date","p_int","p_max","p_min","p_end","dod","dodr","amount")
as.Date(hoshi$date)

head(hoshi)

plot(hoshi$date,hoshi$p_end,type="l",main="hoshino")
#グラフを表示できます

plot(diff(log(hoshi$p_end))*100,type = "l",main = "hoshino")
#収益率です

hist(diff(log(hoshi$p_end))*100,main='hoshino')

shapiro.test(diff(log(hoshi$p_end))*100)
#正規性の確認

acf(hoshi$p_end)
#終値のコレログラム
Box.test(hoshi$p_end)
#自己相関の有無を検定→関係あり
acf(diff(log(hoshi$p_end))*100)
#終値の収益率のコレログラム
Box.test(diff(log(hoshi$p_end))*100)
#自己相関の有無を検定→関係なし
ar(diff(log(hoshi$p_end))*100,aic=T,order.max=1)
#AR(1)モデルに当てはめる
adf.test(diff(log(hoshi$p_end))*100)
#単位根の有無を検定→単位根はありません

hoshi_log <- diff(log(hoshi$p_end))*100
#収益率を簡略化します
ar.fit1 <- ar(hoshi_log,aic=T)
#自己回帰モデルを入れます


#残差の系列を調べる段階
acf(ar.fit1$resid[-1])
  #残差のコレログラムを書きます
ar(ar.fit1$resid[-1])$order
　#残差について自己回帰を当てはめる
Box.test(ar.fit1$resid[-1],type="L")
　#残差についてBox検定→自己相関はありません

#2乗のモデルでコレログラム、自己回帰、Box検定
acf(ar.fit1$resid[-1]^2)
ar(ar.fit1$resid[-1]^2)
Box.test(ar.fit1$resid[-1]^2,type="L")


#分散不均一性がない
#好ましいけれど、GARCHを使えない