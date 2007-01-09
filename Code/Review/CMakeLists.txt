# Sources of non-templated classes.
SET(ITKReview_SRCS
)


#
# So far we do not have non-templated classes in this directory.
# Whenever we add non-templated classes, then the following lines
# should be moved out of the comments.
#
# ADD_LIBRARY(ITKReview ${ITK_REVIEW_BUILD_TYPE} ${ITKReview_SRCS})
# TARGET_LINK_LIBRARIES (ITKReview ITKCommon)

ADD_LIBRARY(ITKDistributions ${ITK_REVIEW_BUILD_TYPE}
  itkGaussianDistribution.cxx
  itkTDistribution.cxx
  itkChiSquareDistribution.cxx

  # from SLATEC/FNLIB
  d1mach.c
  d9lgmc.c
  d9gmit.c
  d9lgic.c
  d9lgit.c
  dbetai.c
  dcsevl.c
  dgami.c
  dgamit.c
  dgamlm.c
  dgamma.c
  dgamr.c
  dlbeta.c
  dlgams.c
  dlngam.c
  dlnrel.c
  initds.c
  xermsg.c
  d_int.c
  d_mod.c
 )

TARGET_LINK_LIBRARIES(ITKDistributions ITKStatistics ITKCommon)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKDistributions PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)


# Build ITKQuadEdgeMesh library

ADD_LIBRARY(ITKQuadEdgeMesh ${ITK_REVIEW_BUILD_TYPE}
  itkQEQuadEdge.cxx
  )

TARGET_LINK_LIBRARIES(ITKQuadEdgeMesh ITKCommon)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKQuadEdgeMesh PROPERTIES
    ${ITK_LIBRARY_PROPERTIES}
    )
ENDIF(ITK_LIBRARY_PROPERTIES)


# install the development files
IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL_FILES(${ITK_INSTALL_INCLUDE_DIR}/Review "(\\.h|\\.txx)$")
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)

# Warn the user about the license issues and lack of stability of the API
# of the code in this directory
SET(msg "Warning: You have chosen to use the files in the Review directory.")
SET(msg "${msg} The Copyright of these files has not been cleared up, and the coding")
SET(msg "${msg} style and API of the classes in the Review directory may change")
SET(msg "${msg} drastically as it get code reviews from the developers.")
SET(msg "${msg} Please set ITK_USE_REVIEW to OFF if you don't want to use these files.")

# display the message during the setup
MESSAGE("${msg}")

# and also display it during the install
INSTALL(CODE "MESSAGE(\"${msg}\")")
