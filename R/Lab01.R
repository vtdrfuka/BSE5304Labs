if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
print("hello world version 3")

data <- data.frame(
  day = as.Date("2019-01-01") + 0:99,
  tmin = runif(100) + seq(1,100)^2.5 / 10000,
  price = runif(100) + seq(100,1)^1.5 / 10
)

data$tmax = runif(100)*10  + data$tmin
coeff <- 6
# A few constants
tminColor <- "#0000ff"
tmaxColor <- "#ff0000"
priceColor <- rgb(0.2, 0.6, 0.9, 1)
dir.create("pdfs")
basestr=format(Sys.time(),"./pdfs/%Y%m%d%H%M")

p1= ggplot(data, aes(x=day)) +
  geom_line( aes(y=tmin), linewidth=2, color=tminColor) + 
  geom_line( aes(y=tmax), linewidth=2, color=tmaxColor) + 
  geom_line( aes(y=price / coeff), linewidth=2, color=priceColor) +
    scale_y_continuous(
      # Features of the first axis
      name = "Temp(C)",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name="Price")
    ) + 
    # theme_ipsum() +
    theme(
      axis.title.y = element_text(color = "black", size=13),
      axis.title.y.right = element_text(color = priceColor, size=13)
    ) +
    ggtitle("Temperature down, price up")

filename=paste0(basestr,"graph01.pdf")
pdf(filename) 
plot(p1)
dev.off()
print("file size")
print(file.size(filename))
print("I finished!")
