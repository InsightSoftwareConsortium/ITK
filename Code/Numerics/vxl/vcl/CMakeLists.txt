INSTALL_FILES(/include/InsightToolkit/Numerics/vxl/vcl "(\\.h)$")
INSTALL_FILES(/include/InsightToolkit/Numerics/vxl/vcl ".h"
              "vcl_config_compiler"
              "vcl_config_headers"
              "vcl_config_manual")

SUBDIRS(gcc iso sgi egcs win32 emulation generic
        vcl_sys sunpro borland )
IF(NOT BORLAND)
  SUBDIRS(gcc-295 gcc-libstdcxx-v3)
ENDIF(NOT BORLAND)
