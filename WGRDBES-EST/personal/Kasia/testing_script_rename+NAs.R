myObject <- createRDBESDataObject("WGRDBES-EST/personal/Kasia/example1")
myObject <- createRDBESDataObject("WGRDBES-EST/personal/Kasia/example2")
renamedata<-renameSpeciesSA(myObject)
generateNAsUsingSL(myObject,myObject$SA$SAspeCode)->aaa
