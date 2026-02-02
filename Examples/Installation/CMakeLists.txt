# This is the root ITK CMakeLists file.
cmake_minimum_required(VERSION 3.22.1...3.29.0 FATAL_ERROR)

# This project is designed to be built outside the Insight source tree.
project(HelloWorld)

# Find ITK.
find_package(ITK REQUIRED)

# Generate the ITK factory registration code.
itk_generate_factory_registration()

add_executable(HelloWorld HelloWorld.cxx)

# Required ITK modules can be identified using the WhatModulesITK.py utility:
# python <ITK_SOURCE>/Utilities/Maintenance/WhatModulesITK.py <ITK_SOURCE> HelloWorld.cxx

# Link the executable to the specific ITK interface library modules used.
target_link_libraries(HelloWorld ITK::ITKCommonModule)
#  Alternatively, all the ITK interface libraries can be linked as shown below.
# target_link_libraries(HelloWorld ${ITK_INTERFACE_LIBRARIES})
