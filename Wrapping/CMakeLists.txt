OPTION(USE_WRAP_ITK "Build external languages support" OFF)
IF(USE_WRAP_ITK)
  SUBDIRS(WrapITK)
ENDIF(USE_WRAP_ITK)

OPTION(ITK_EXPLICIT_INSTANTIATION "BETA-Try to explicitly build some of the core templates explictitly rather than over and over again." OFF)
IF(ITK_EXPLICIT_INSTANTIATION)
  SUBDIRS(ExplicitITK)
ENDIF(ITK_EXPLICIT_INSTANTIATION)
  
