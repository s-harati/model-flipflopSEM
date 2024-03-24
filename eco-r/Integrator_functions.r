f <- function(aa,
              th = th,
              factor.rescale = 0.001) {
  aa <- ((aa * factor.rescale) > th) * 1
  aa[is.na(aa)] <- 0
  return(aa)
}

find.map <- function(yr , year.calculation , time.step , folder.out , years , 
                     str.algorithm , str.cutoff) {
  # function added in V9p2
  if (yr <= year.calculation + time.step) {
    a = paste("Infestation_data-cumulative/cumkill", yr, ".asc", sep = "")
    factor.rescale = 0.001
  } else {
    a = paste(
      folder.out,
      #"Random_forest_-_",
      #"Binomial_parabollic_elevation_-_",
      str.algorithm,"_-_",
      "year_calculation_",
      year.calculation,
      "__start_",
      yr - time.step ,
      "_-_",
      "final_",
      yr,
      "_-_",
      years,
      "_-_",
      #"Quantity",
      str.cutoff,
      "_cutoff",
      "_-_",
      ".tif",
      sep = ""
    )
    factor.rescale = 1.0
  }
  aa = raster(readGDAL(a, silent = TRUE))
  return(f(
    aa = aa,
    th = threshold.initial,
    factor.rescale = factor.rescale
  ))
}

focal.selection = function (ras.main = raster() ,
                            ras.mask = raster()) {
  my.extent = extent(ras.main)
  my.proj = proj4string(ras.main)
  mat.main = as.matrix(ras.main)
  mat.mask = as.matrix(ras.mask)
  # finding coordinates of the mask
  #ind.rowsums = which(rowSums(!is.na(mat.mask)) > 0)
  #ind.colsums = which(colSums(!is.na(mat.mask)) > 0)
  ind.rowsums = which(rowSums(mat.mask) > 0) # attention: new masks are 0/1
  ind.colsums = which(colSums(mat.mask) > 0) # attention: new masks are 0/1
  r.top = min(ind.rowsums)
  r.bottom = max(ind.rowsums)
  c.left = min(ind.colsums)
  c.right = max(ind.colsums)
  mat.sel = mat.main[r.top:r.bottom , c.left:c.right]
  # applying focal.parallel
  ras.fp = focal.parallel(raster(mat.sel), w.halfsize, T)
  # ras.fp is a raster stack!
  ras.out = raster::stack()
  for (i in 1:nlayers(ras.fp)) {
    mat.out.i = mat.main
    mat.fp.i = as.matrix(ras.fp[[i]])
    mat.out.i[r.top:r.bottom , c.left:c.right] = mat.fp.i
    ras.out = stack(ras.out , raster(mat.out.i))
  }
  # putting processed selection back in main image
  # back to raster
  extent(ras.out) = my.extent
  proj4string(ras.out) = my.proj
  names(ras.out) = names(ras.fp)
  return(ras.out)
}

divide.area = function(ras=raster() , nr=3 , nc=3) {
  exte = extent(ras)
  proj = proj4string(ras)
  mat = matrix(0,nrow(ras),ncol(ras))
  r = seq(fr=1,to=nrow(ras),len=nr+1)
  r.fr = 1+floor(r[1:nr])
  r.fr[1] = 1
  r.to = floor(r[2:(nr+1)])
  c = seq(fr=1,to=ncol(ras),len=nc+1)
  c.fr = 1+floor(c[1:nc])
  c.fr[1] = 1
  c.to = floor(c[2:(nc+1)])
  for (i in 1:nr) {
    for (j in 1:nc) {
      clus = i + (j-1)*nr
      mat[c(r.fr[i]:r.to[i]) , c(c.fr[j]:c.to[j])] = clus
    }
  }
  ras.out = raster(mat)
  extent(ras.out) = exte
  proj4string(ras.out) = proj
  return(ras.out)
}

divide.selection = function (ras.mask=raster() , nr=3 , nc=3) {
  my.extent = extent(ras.mask)
  my.proj = proj4string(ras.mask)
  mat.main = as.matrix(ras.mask) * 0 # modifying focal.selection
  mat.mask = as.matrix(ras.mask)
  # finding coordinates of the mask
  ind.rowsums = which(rowSums(mat.mask) > 0) # attention: new masks are 0/1
  ind.colsums = which(colSums(mat.mask) > 0) # attention: new masks are 0/1
  r.top = min(ind.rowsums)
  r.bottom = max(ind.rowsums)
  c.left = min(ind.colsums)
  c.right = max(ind.colsums)
  mat.sel = mat.main[r.top:r.bottom , c.left:c.right]
  # calling divide.area function here
  mat.sel = as.matrix(divide.area(ras=raster(mat.sel),nr=nr,nc=nc))
  # putting processed selection back in main image
  mat.main[r.top:r.bottom , c.left:c.right] = mat.sel
  # back to raster
  ras.out = raster(mat.main)
  extent(ras.out) = my.extent
  proj4string(ras.out) = my.proj
  return(ras.out)
}

