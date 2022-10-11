# Library load message
#' @importFrom utils packageDescription

.onAttach<- function (lib, pkg){
  if(interactive()){
    vers <- packageDescription("icesRDBES", fields = "Version")
    start_msg <- paste("
======================================================================
icesRDBES", vers, "
======================================================================

Please Note:

This package is under development under the WGRDBES-EST.

Code has been tested, however you should consider outputs as
experimental.

Please report any issues or bugs you encounter here:

    https://github.com/ices-tools-dev/icesRDBES


======================================================================")

    packageStartupMessage(start_msg)
  }
}
