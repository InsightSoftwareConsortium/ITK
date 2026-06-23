set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.hdfgroup.org/HDF5/\">HDF5</a> library.
HDF5 is a data model, library, and file format for storing and managing data."
)

if(ITK_USE_SYSTEM_HDF5)
  itk_module(ITKHDF5 DESCRIPTION "${DOCUMENTATION}")
else()
  itk_module(ITKHDF5 DEPENDS ITKZLIB DESCRIPTION "${DOCUMENTATION}")
endif()
