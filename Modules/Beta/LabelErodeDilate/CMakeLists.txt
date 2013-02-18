#Change PROJECT_NAME to the name of your project
PROJECT(LabelErodeDilate)

# set the name of the input image used to run the test
SET(BRAIN_ATLAS ${CMAKE_CURRENT_SOURCE_DIR}/images/HarvardOxford-cort-maxprob-thr50-1mm.nii.gz)
SET(POINTIM ${CMAKE_CURRENT_SOURCE_DIR}/BugsFromDzenan/vCenters10.mha)
SET(BRAIN_SLICE ${CMAKE_CURRENT_SOURCE_DIR}/images/axial.png)

#include some macros from another file...
INCLUDE(IJMacros.txt)

CMAKE_MINIMUM_REQUIRED(VERSION 2.4)

if(COMMAND cmake_policy)
           cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)


#The following lines are required to use Dart
ENABLE_TESTING()
INCLUDE(Dart)
FIND_PROGRAM(ITK_TEST_DRIVER itkTestDriver)
SET(TEST_COMMAND ${ITK_TEST_DRIVER} --add-before-env PATH ${CMAKE_CURRENT_BINARY_DIR})

#Declare any external dependencies that your project may have here.
#examples include: ITK, VTK, JPEG, PNG, OpenGL, ZLIB, Perl, Java
#If you're not sure what name to use, look in the Modules directory of your
#cmake install and check that a file named Find(Package).cmake exists

SET(Required_Packages
ITK4
)


INCLUDE_DIRECTORIES(
  ${CMAKE_CURRENT_SOURCE_DIR}
)

#this foreach loads all of the packages that you specified as required.
#It shouldn't need to be modified.
FOREACH(Package ${Required_Packages})
  LOADPACKAGE(${Package})
ENDFOREACH(Package)

#Set any libraries that your project depends on.
#examples: ITKCommon, VTKRendering, etc
SET(Libraries
${ITK_LIBRARIES}
)


# install devel files
OPTION(INSTALL_DEVEL_FILES "Install C++ headers" ON)
IF(INSTALL_DEVEL_FILES)
FILE(GLOB develFiles *.h *.txx)
FOREACH(f ${develFiles})
  INSTALL_FILES(/include/InsightToolkit/BasicFilters FILES ${f})
ENDFOREACH(f)
ENDIF(INSTALL_DEVEL_FILES)



# option for wrapping
OPTION(BUILD_WRAPPERS "Wrap library" OFF)
IF(BUILD_WRAPPERS)
   SUBDIRS(Wrapping)
ENDIF(BUILD_WRAPPERS)



#the following block of code is an example of how to build an executable in
#cmake.  Unmodified, it will add an executable called "MyExe" to the project.
#MyExe will be built using the files MyClass.h and MyClass.cxx, and it will
#be linked to all the libraries you specified above.
#You can build more than one executable per project

IF(BUILD_TESTING)

FOREACH(CurrentExe labelSetsErode labelSetsPerf
labelSetsDilatePerf labelSetsDilateWSPerf labelSetsDilateDanielssonPerf labelSetsDilate labelSetsDilateWS labelSetsDilateDanielsson reportNonZero)
ADD_EXECUTABLE(${CurrentExe} ${CurrentExe}.cxx)
TARGET_LINK_LIBRARIES(${CurrentExe} ${Libraries})
ENDFOREACH(CurrentExe)

ENDIF(BUILD_TESTING)

#the following line is an example of how to add a test to your project.
#Testname is the title for this particular test.  ExecutableToRun is the
#program which will be running this test.  It can either be a part of this
#project or an external executable.  After that list any args that are needed
#for this test.  Include as many tests as you like.  If your project doesn't have
#any tests you can comment out or delete the following line.
# ADD_TEST(Testname ExecutableToRun arg1 arg2 arg3)


ADD_TEST(testLabelErode3d ${TEST_COMMAND} labelSetsErode  -i
${BRAIN_ATLAS} -o laberode3d.nii.gz -r 2
--compare laberode3d.nii.gz ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/laberode3d.nii.gz
)

ADD_TEST(testLabelDilate3d ${TEST_COMMAND} labelSetsDilate  -i
${BRAIN_ATLAS} -o labdilate3d.nii.gz -r 5
--compare labdilate3d.nii.gz ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/labdilate3d.nii.gz
)

ADD_TEST(testLabelDilate3dDez ${TEST_COMMAND} labelSetsDilate  -i
${POINTIM} -o labdilate3d_dez.nii.gz -r 50
--compare labdilate3d_dez.nii.gz ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/labdilate3d_dez.nii.gz
)

ADD_TEST(testLabelErode2d ${TEST_COMMAND} labelSetsErode  -i
${BRAIN_SLICE} -o laberode2d.png -r 2
--compare laberode2d.png  ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/laberode2d.png

)

ADD_TEST(testLabelDilate2d ${TEST_COMMAND} labelSetsDilate  -i
${BRAIN_SLICE} -o labdilate2d.png -r 5
--compare labdilate2d.png  ${CMAKE_CURRENT_SOURCE_DIR}/images/baseline/labdilate2d.png
)

ADD_TEST(threadDilateTest  ${TEST_COMMAND} ${CMAKE_COMMAND} -DTESTIMAGE=${CMAKE_CURRENT_SOURCE_DIR}/BugsFromDzenan/vCenters10.mha -P ${CMAKE_CURRENT_SOURCE_DIR}/threadtest.cmake
--compare dil_4.mha dil_1.mha)







