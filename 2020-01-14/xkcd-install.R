library(xkcd)
vignette("xkcd-intro")



# installs xkcd fonts
library(extrafont)

download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf", mode="wb")
system("mkdir ~/.fonts")
system("cp xkcd.ttf ~/.fonts")
font_import(paths = getwd(), pattern = "[X/x]kcd", prompt=FALSE)
fonts()
fonttable()
if(.Platform$OS.type != "unix") {
    ## Register fonts for Windows bitmap output
    loadfonts(device="win") ## in win10, have to open the ttf file, then click install to install font
} else {
    loadfonts()
}
