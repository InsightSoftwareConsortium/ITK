INSTALL_FILES(/include/InsightToolkit/Utilities/vxl/vcl "(\\.h|\\.txx)$")
INSTALL_FILES(/include/InsightToolkit/Utilities/vxl/vcl ".h"
              "vcl_config_compiler"
              "vcl_config_headers"
              "vcl_config_manual")

SUBDIRS(borland egcs gcc generic iso mwerks sgi sunpro vcl_sys win32)
IF(NOT BORLAND)
  SUBDIRS(gcc-295 gcc-libstdcxx-v3 win32-vc60 win32-vc70)
ENDIF(NOT BORLAND)
