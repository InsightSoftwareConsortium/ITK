SET(ITKIO_SRCS
itkWriter.cxx
itkImageIOBase.cxx
itkImageIOFactory.cxx
itkMetaImageIOFactory.cxx
itkMetaImageIO.cxx
itkPNGImageIOFactory.cxx
itkPNGImageIO.cxx
itkVOLImageIOFactory.cxx
itkVOLImageIO.cxx
itkVTKImageIOFactory.cxx
itkVTKImageIO.cxx
itkDicomImageIOFactory.cxx
itkDicomImageIO.cxx
)

IF(WIN32)
  IF(BUILD_SHARED_LIBS)
    ADD_DEFINITIONS(-DPNG_BUILD_DLL -DZLIB_DLL -DPNG_NO_MODULEDEF)
  ELSE(BUILD_SHARED_LIBS)
    SET(PNG_STATIC 1)
    ADD_DEFINITIONS(-DPNG_STATIC)
  ENDIF(BUILD_SHARED_LIBS)
ENDIF(WIN32)

# Add ITKIO library
ADD_LIBRARY(ITKIO ${ITKIO_SRCS})
TARGET_LINK_LIBRARIES(ITKIO ITKCommon itkpng itkzlib ITKMetaIO)

INSTALL_TARGETS(/lib/InsightToolkit ITKIO)
INSTALL_FILES(/include/InsightToolkit/IO "(\\.h|\\.txx)$")
