INSTALL_FILES(/include/InsightToolkit/Numerics/vxl/vcl "(\\.h)$")
INSTALL_FILES(/include/InsightToolkit/Numerics/vxl/vcl ".h"
              "vcl_config_compiler"
              "vcl_config_headers"
              "vcl_config_manual")

SUBDIRS(gcc iso sgi egcs win32 emulation gcc-295 generic
        vcl_sys sunpro borland gcc-libstdcxx-v3)
