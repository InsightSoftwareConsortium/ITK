include(${CMAKE_SOURCE_DIR}/IJMacros.txt)

#Change PROJECT_NAME to the name of your project
project(ITKVariationalRegistration)

#The following lines are required to use Dart
enable_testing()
include(Dart)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(Required_Packages ITK)

#this foreach loads all of the packages that you specified as required.
#It shouldn't need to be modified.
foreach(Package ${Required_Packages})
  loadpackage(${Package})
endforeach(Package)

#Set any libraries that your project depends on.
#examples: ITKCommon, VTKRendering, etc
set(Libraries ${ITK_LIBRARIES})

add_executable(itkVariationalRegistration VariationalRegistrationMain.cxx)
target_link_libraries(itkVariationalRegistration ${Libraries})

add_executable(itkVariationalRegistration2D VariationalRegistrationMain.cxx)
target_link_libraries(itkVariationalRegistration2D ${Libraries})
set_target_properties(
  itkVariationalRegistration2D
  PROPERTIES
    COMPILE_FLAGS
      -DUSE_2D_IMPL
)

# add a target to generate API documentation with Doxygen
find_package(Doxygen)
if(DOXYGEN_FOUND)
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in
    ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
    @ONLY
  )
  add_custom_target(
    doc
    ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating API documentation with Doxygen"
    VERBATIM
  )
endif(DOXYGEN_FOUND)

#the following line is an example of how to add a test to your project.
#Testname is the title for this particular test.  ExecutableToRun is the
#program which will be running this test.  It can either be a part of this
#project or an external executable.  After that list any args that are needed
#for this test.  Include as many tests as you like.  If your project doesn't have
#any tests you can comment out or delete the following line.

#####################################
#
# ADD TESTS
#
#####################################
set(BASELINE ${CMAKE_SOURCE_DIR}/Baseline)
set(INPUTDIR ${CMAKE_SOURCE_DIR}/Input)
set(TEMP ${CMAKE_BINARY_DIR}/Testing/Temporary)

#####################################
# 2D tests
#####################################
set(
  COMMON_PARAMS2D
  -F
  ${INPUTDIR}/img1.png
  -M
  ${INPUTDIR}/img2.png
  -l
  4
  -p
  1
  -g
  0.00001
)
set(EXT tif)

# Active Thirion forces and gaussian smoothing (sigma=3)
set(TESTNAME ActiveThirionGauss2D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D} -v 1.5 -u 0 -W ${TEMP}/${TESTNAME}.${EXT}
)

# Active Thirion forces and diffusive regularization (alpha=3)
set(TESTNAME ActiveThirionDiffusive2D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D} -r 1 -a 1.5 -W ${TEMP}/${TESTNAME}.${EXT}
)

# Active Thirion forces and elastic regularization (lambda=mu=3)
set(TESTNAME ActiveThirionElastic2D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D} -r 2 -m 0.5 -b 1.0 -W ${TEMP}/${TESTNAME}.${EXT}
)

# Passive Thirion forces and gaussian smoothing (sigma=3)
set(TESTNAME PassiveThirionGauss2D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D} -d 1 -v 1.5 -u 0 -W ${TEMP}/${TESTNAME}.${EXT}
)

# Symmetric Thirion forces and gaussian smoothing (sigma=3)
set(TESTNAME SymmetricThirionGauss2D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D} -d 2 -v 1.5 -u 0 -W ${TEMP}/${TESTNAME}.${EXT}
)

# SSD forces and gaussian smoothing (sigma=3)
set(TESTNAME SSDGauss2D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D} -f 1 -t 0.001 -v 1.0 -u 0 -W ${TEMP}/${TESTNAME}.${EXT}
)

# NCC forces and gaussian smoothing (sigma=3)
set(TESTNAME NCCGauss2D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration2D>
    ${COMMON_PARAMS2D} -f 2 -t 40 -v 1.5 -u 0 -q 3 -p 2 -W
    ${TEMP}/${TESTNAME}.${EXT}
)

#####################################
# 3D tests
#####################################
set(
  COMMON_PARAMS3D
  -F
  ${INPUTDIR}/10-P_5x5x5.nii.gz
  -M
  ${INPUTDIR}/60-P_5x5x5.nii.gz
  -l
  3
  -p
  1
  -g
  0.00001
)
set(EXT nii.gz)

# Active Thirion forces and gaussian smoothing (sigma=3)
set(TESTNAME ActiveThirionGauss3D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D} -v 1.5 -u 0 -W ${TEMP}/${TESTNAME}.${EXT} -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

# Active Thirion forces and diffusive regularization (alpha=3)
set(TESTNAME ActiveThirionDiffusive3D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.nii.gz
    ${TEMP}/${TESTNAME}.nii.gz $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D} -r 1 -a 1 -W ${TEMP}/${TESTNAME}.nii.gz -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

# Active Thirion forces and elastic regularization (lambda=mu=3)
set(TESTNAME ActiveThirionElastic3D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.nii.gz
    ${TEMP}/${TESTNAME}.nii.gz $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D} -r 2 -m 0.25 -b 0.25 -W ${TEMP}/${TESTNAME}.nii.gz -O
    ${TEMP}/${TESTNAME}_displ.mhd
)

# SSD forces and gaussian smoothing (sigma=3)
set(TESTNAME SSDGauss3D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D} -f 1 -t 0.00001 -v 1 -u 0 -W ${TEMP}/${TESTNAME}.${EXT}
    -O ${TEMP}/${TESTNAME}_displ.mhd
)

# NCC forces and gaussian smoothing (sigma=3)
set(TESTNAME NCCGauss3D)
add_test(
  NAME ${TESTNAME}
  COMMAND
    itkTestDriver --compare ${BASELINE}/${TESTNAME}.${EXT}
    ${TEMP}/${TESTNAME}.${EXT} $<TARGET_FILE:itkVariationalRegistration>
    ${COMMON_PARAMS3D} -f 2 -t 40 -v 1 -u 0 -q 2 -p 1 -W
    ${TEMP}/${TESTNAME}.${EXT} -O ${TEMP}/${TESTNAME}_displ.mhd
)
