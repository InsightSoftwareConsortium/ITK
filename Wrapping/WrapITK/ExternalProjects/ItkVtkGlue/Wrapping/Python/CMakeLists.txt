SUBDIRS(Tests)
SUBDIRS(itkExtras)

# will be removed in the future
WRAP_ITK_INSTALL("/Python" "${CMAKE_CURRENT_SOURCE_DIR}/itkvtk.py")

