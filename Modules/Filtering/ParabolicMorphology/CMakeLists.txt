#Change PROJECT_NAME to the name of your project
PROJECT(parabolicMorpholgy)

#set the name of the input image used to run the test
SET(INPUT_IMAGE ${ CMAKE_CURRENT_SOURCE_DIR } / images / cthead1.png)

#include some macros from another file...
INCLUDE(IJMacros.txt)

#The following lines are required to use Dart
ENABLE_TESTING()
INCLUDE(Dart)
FIND_PROGRAM(ITK_TEST_DRIVER itkTestDriver)
SET(TEST_COMMAND $ { ITK_TEST_DRIVER } --add - before - env PATH ${ CMAKE_CURRENT_BINARY_DIR })

#Declare any external dependencies that your project may have here.
#examples include : ITK, VTK, JPEG, PNG, OpenGL, ZLIB, Perl, Java
#If you're not sure what name to use, look in the Modules directory of your
#cmake install and check that a file named Find(Package).cmake exists
SET(Required_Packages ITK)

#this foreach loads all of the packages that you specified as required.
#It shouldn't need to be modified.
FOREACH(Package ${ Required_Packages })
LOADPACKAGE(${ Package })
ENDFOREACH(Package)

#Set any libraries that your project depends on.
#examples : ITKCommon, VTKRendering, etc
SET(Libraries ITKCommon ITKBasicFilters ITKIO)


#install devel files
OPTION(INSTALL_DEVEL_FILES "Install C++ headers" ON)
IF(INSTALL_DEVEL_FILES)
FILE(GLOB develFiles *.h *.txx)
FOREACH(f ${ develFiles })
INSTALL_FILES(/ include / InsightToolkit / BasicFilters FILES ${ f })
ENDFOREACH(f)
ENDIF(INSTALL_DEVEL_FILES)


#option for wrapping
OPTION(BUILD_WRAPPERS "Wrap library" OFF)
IF(BUILD_WRAPPERS)
SUBDIRS(Wrapping)
ENDIF(BUILD_WRAPPERS)


#the following block of code is an example of how to build an executable in
#cmake.Unmodified, it will add an executable called "MyExe" to the project.
#MyExe will be built using the files MyClass.h and MyClass.cxx, and it will
#be linked to all the libraries you specified      above.
#You can build more than one executable            per project

IF(BUILD_TESTING)

FOREACH(CurrentExe "check"
                   "testOpen"
                   "testSpacing"
                   "testSharpen"
                   "testDT")
ADD_EXECUTABLE(${ CurrentExe } ${ CurrentExe }.cxx)
TARGET_LINK_LIBRARIES(${ CurrentExe } ${ Libraries })
ENDFOREACH(CurrentExe)

ENDIF(BUILD_TESTING)

#the following line is an example of how to add a test to your project.
#Testname is the title for this particular test.ExecutableToRun is the
#program which will be running this test.It can either be a part of this
#project or an external executable.After that list any args that are needed
#for this test.Include as many tests as you like.If your project doesn't have
#any tests you can comment out or delete the following           line.
#ADD_TEST(Testname ExecutableToRun arg1 arg2 arg3)

ADD_TEST(Test ${ TEST_COMMAND } check ${ INPUT_IMAGE } out.png-- compare out.png ${ CMAKE_CURRENT_SOURCE_DIR } /
         images / test.png)

ADD_TEST(Open ${ TEST_COMMAND } testOpen ${ INPUT_IMAGE } open.png-- compare open.png ${ CMAKE_CURRENT_SOURCE_DIR } /
         images / open.png)
ADD_TEST(Spacing ${ TEST_COMMAND } testSpacing ${ INPUT_IMAGE } o1.png o2.png-- compare o1.png o2.png)

ADD_TEST(Sharpen1 ${ TEST_COMMAND } testSharpen 1 sharp1.mha sharp1.txt)

ADD_TEST(Sharpen2 ${ TEST_COMMAND } testSharpen 2 sharp2.mha sharp2.txt)

ADD_TEST(Sharpen3 ${ TEST_COMMAND } testSharpen 3 sharp3.mha sharp3.txt)
ADD_TEST(Sharpen10 ${ TEST_COMMAND } testSharpen 10 sharp10.mha sharp10.txt)
ADD_TEST(Sharpen100 ${ TEST_COMMAND } testSharpen 100 sharp100.mha sharp100.txt)

ADD_TEST(Dist1 ${ TEST_COMMAND } testDT $ { INPUT_IMAGE } 100 0 mask.png dist1.mha)

ADD_TEST(Dist2 ${ TEST_COMMAND } testDT $ { INPUT_IMAGE } 100 255 mask.png dist2.mha)
