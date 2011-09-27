set(DOCUMENTATION "This module contains the third party <a
href=\"http://www.hdfgroup.org/HDF5/\">HDF5</a> library.
HDF5 is a data model, library, and file format for storing and managing data.")

itk_module(ITKHDF5
  DEPENDS
    ITKZLIB
  DESCRIPTION
    "${DOCUMENTATION}"
)
