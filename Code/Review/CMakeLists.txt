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


# install the development files
IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL_FILES(${ITK_INSTALL_INCLUDE_DIR}/Review "(\\.h|\\.txx)$")
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
