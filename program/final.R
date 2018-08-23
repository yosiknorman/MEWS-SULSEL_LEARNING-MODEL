#!/usr/bin/Rscript

rm(list = ls())
out='/home/yosik/Data_riset/AWS/CSV'
OUTPUT = '/home/yosik/Data_riset/AWS/OUTPUT/'

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

is.character0 <- function(x)
{
  is.character(x) && length(x) == 0L
}


ori = list.files(out)
fl = c()

for(i in 1:length(ori)){
  fl[i] = strsplit(ori[i], split = "_")[[1]][1]
  if(!is.integer0(grep(fl[i], pattern = "-"))){
    fl[i] = strsplit(ori[i], split = "-")[[1]][1]
  }
  if(!is.integer0(grep(fl[i], pattern = " "))){
    fl[i] = gsub(fl[i], pattern = " ", replacement = "")
  }
}

resid = c()
for(i in 1:length(fl)){
  resid[i] = strsplit(ori[i], split = fl[i])[[1]][2]
  resid[i] = substr(resid[i], 2, nchar(resid[i]))
  if(substr(resid[i],1,1) == " "){
    resid[i] = substr(resid[i], 2, nchar(resid[i]))
  }
}

ufl = unique(fl)
ifl = list()
buka = list()
juml = list()
for(i in 1:length(ufl)){
  ifl[[i]] = which(fl ==  ufl[i])
  buka[[i]] = ori[ifl[[i]]]
  system( paste0("mkdir ", "'",OUTPUT, ufl[i], "'") )
  for(j in 1:length(buka[[i]])){
    system(paste0("cp '", out,"/", buka[[i]][j], "' ", "'",OUTPUT, ufl[i], "'" ))
  }
  juml[[i]] = nchar( list.files( paste0("/home/yosik/Data_riset/AWS/OUTPUT/", ufl[i]) ) )
}


xlsdate2num = function(x){
  h1 = c()
  h2 = c()
  out = c()
  # i=7
  for(i in 1:length(x)){
    if( !is.integer0(grep(x[i], pattern = "AM"))  ){
      h1[i] = strsplit(x[i], split = ":")[[1]][1]
      if(h1[i] == "12" & !is.na(h1[i])){
        h1[i] = 0
      }else if (h1[i] != "12" & !is.na(h1[i])){
        h1[i] = h1[i]
      }
      h2[i] = as.numeric(strsplit(x[i], split = ":")[[1]][2])
      out[i] = as.numeric(paste0(h1[i], ".", h2[i]))
    }else if( !is.integer0(grep(x[i], pattern = "PM"))   ) {
      h1[i] = strsplit(x[i], split = ":")[[1]][1]
      h2[i] = as.numeric(strsplit(x[i], split = ":")[[1]][2])
      h1[i] = as.numeric(h1[i]) + 12
      out[i] = as.numeric(paste0(h1[i], ".", h2[i]))
    }else{
      if( !is.integer0(grep(x[i], pattern = ":")) ){
        h1[i] = strsplit(x[i], split = ":")[[1]][1]
        h2[i] = as.numeric(strsplit(x[i], split = ":")[[1]][2])
        out[i] = as.numeric(paste0(h1[i], ".", h2[i]))
      }else{
        out[i] = as.numeric(x[i])
      }
    }
  }
  return(out)
}

collect_all = function(xall){
  ind = 4:length(names(xall))
  hasil = list()
  for(i in 1:length(ind)){
    hasil[[i]] = xlsdate2num(x = as.character(xall[,ind[i]]))
  }
  names(hasil) = names(xall)[ind]
  res = data.frame(ID = as.character(xall$ID.Station[2]), Time = xall$Date.Time)
  for(i in 1:length(hasil)){
    res = cbind(res, hasil[[i]])
  }
  names(res)[3:dim(res)[2]] = names(xall)[ind]
  return(res)
}

# ------------------------- Sampling --------------------------

# lis_col = list.files("/home/yosik/Data_riset/AWS/OUTPUT/STA5024/")
# opendata(namedata = "~/Data_riset/AWS/CSV/14032792_ARG Benteng_2015_01_12.csv")
prt = "/home/yosik/Data_riset/AWS/OUTPUT/"
opendata = function(namedata){
  xall = read.table(namedata, header = T, sep = ",")
  # xall = read.delim(namedata, header = T)
  if( all(dim(xall) == c(1,1))  ){
    print("ora bisa No data !!!")
    sip = "lanjut no data"
  }else if(dim(xall)[2] == 1 & dim(xall)[1] != 1){
    print("ganti baca tab delimiter")
    xall = read.delim(namedata, header = T)
    sip = collect_all(xall = xall)
  }else
    if(dim(xall)[2] != 1 & dim(xall)[1] != 1)
    {
    print("bisa comma separated")
    sip = collect_all(xall = xall)
  }
  return(sip)
}

# ufli = ufl[2] #
per_folder = function(ufli){
  prfx = paste0(prt,  ufli)
  lis_col = list.files(prfx)
  if( is.character0(lis_col) ) {
    print("Tidak Ada Data")
    akhirnya = "No Data !!"
  }else{
    ok = list()
    for(i in 1:length(lis_col)){
      ok[[i]] = opendata(namedata = paste0(prfx,"/", lis_col[i]))   ##
    }
    
    ind = c()
    for(i in 1:length(ok)){
      if( is.null(dim(ok[[i]])) ){
        print("nggk_pake")
      }else{
        ind[i] = i
      }
    }
    
    iakh = ind[!is.na(ind)]
    akhirnya = list()
    for(i in 1:length(iakh)){
      akhirnya[[i]] = ok[[iakh[i]]]
    }
    names(akhirnya)  = lis_col[iakh]
  }
  
  return(list(nyoh = akhirnya, daridata = lis_col))
}

# !!! inspeksi [6 dan 7]
iddd = 1:length(ufl)
iddd = iddd[!iddd %in% c(6,7)]
for(i in 1:length(iddd)){
  assign(ufl[iddd[i]], per_folder(ufli = ufl[iddd[i]]))
  Data_Grup = get(ufl[iddd[i]])
  save(file = paste0("~/Data_riset/AWS/FINAL/",ufl[iddd[i]], ".bin" ), Data_Grup)
}