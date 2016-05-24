CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
INCLUDE(${CMAKE_SOURCE_DIR}/IJMacros.txt)

#Change PROJECT_NAME to the name of your project
PROJECT(ImageCopy)

#The following lines are required to use Dart
ENABLE_TESTING()
INCLUDE(Dart)

SET(CMAKE_EXPORT_COMPILE_COMMANDS ON)
SET(Required_Packages
ITK
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

SET(CurrentExe "DumpIntensities")
ADD_EXECUTABLE(${CurrentExe} DumpIntensities.cxx itkMultipleImageIterator.h)
TARGET_LINK_LIBRARIES(${CurrentExe} ${Libraries})

#the following line is an example of how to add a test to your project.
#Testname is the title for this particular test.  ExecutableToRun is the
#program which will be running this test.  It can either be a part of this
#project or an external executable.  After that list any args that are needed
#for this test.  Include as many tests as you like.  If your project doesn't have
#any tests you can comment out or delete the following line.
ADD_TEST(DumpIntensities DumpIntensities ${CMAKE_SOURCE_DIR}/img1.png ${CMAKE_SOURCE_DIR}/img2.png ${CMAKE_SOURCE_DIR}/img3.png)
ADD_TEST(CheckValues diff output.txt ${CMAKE_SOURCE_DIR}/reference.txt)
