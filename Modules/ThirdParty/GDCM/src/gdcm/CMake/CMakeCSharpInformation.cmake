
# This file sets the basic flags for the CSharp language in CMake.
# It also loads the available platform file for the system-compiler
# if it exists.

#SET(CMAKE_BASE_NAME)
#GET_FILENAME_COMPONENT(CMAKE_BASE_NAME ${CMAKE_CSharp_COMPILER} NAME_WE)
#SET(CMAKE_SYSTEM_AND_CSharp_COMPILER_INFO_FILE
#  #${CMAKE_ROOT}/Modules/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_BASE_NAME}.cmake)
#  ${CMAKE_ROOT}/Modules/Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_BASE_NAME}.cmake)
#MESSAGE(${CMAKE_SYSTEM_AND_CSharp_COMPILER_INFO_FILE})
#INCLUDE(Platform/${CMAKE_SYSTEM_NAME}-${CMAKE_BASE_NAME} OPTIONAL)

# This should be included before the _INIT variables are
# used to initialize the cache.  Since the rule variables
# have if blocks on them, users can still define them here.
# But, it should still be after the platform file so changes can
# be made to those values.

IF(CMAKE_USER_MAKE_RULES_OVERRIDE)
   INCLUDE(${CMAKE_USER_MAKE_RULES_OVERRIDE})
ENDIF(CMAKE_USER_MAKE_RULES_OVERRIDE)

# Copy from CSharp, ref CXX ???
IF(CMAKE_USER_MAKE_RULES_OVERRIDE_CSHARP)
   INCLUDE(${CMAKE_USER_MAKE_RULES_OVERRIDE_CSHARP})
ENDIF(CMAKE_USER_MAKE_RULES_OVERRIDE_CSHARP)

# <TARGET>
# <TARGET_BASE> the target without the suffix
# <OBJECTS>
# <OBJECT>
# <LINK_LIBRARIES>
# <FLAGS>
# <LINK_FLAGS>

# this is a place holder if java needed flags for javac they would go here.
IF(NOT CMAKE_CSharp_CREATE_STATIC_LIBRARY)
#  IF(WIN32)
#    SET(class_files_mask "*.class")
#  ELSE(WIN32)
    SET(class_files_mask ".")
#  ENDIF(WIN32)

  SET(CMAKE_CSharp_CREATE_STATIC_LIBRARY
      #"<CMAKE_CSharp_ARCHIVE> -cf <TARGET> -C <OBJECT_DIR> <OBJECTS>")
      "<CMAKE_CSharp_COMPILER> <CMAKE_STATIC_LIBRARY_CSharp_FLAGS> <OBJECTS> -out:<TARGET>")
ENDIF(NOT CMAKE_CSharp_CREATE_STATIC_LIBRARY)

# compile a CSharp file into an object file
IF(NOT CMAKE_CSharp_COMPILE_OBJECT)
  # there is no such thing as intermediate representation (object file) in C#.
  # Instead to avoid multiple recompilation of the same src file, I could use the .dll form, since
  # one can add src / .dll that same way

  # copy src version
  SET(CMAKE_CSharp_COMPILE_OBJECT "<CMAKE_COMMAND> -E copy <SOURCE> <OBJECT>")
ENDIF(NOT CMAKE_CSharp_COMPILE_OBJECT)

IF(NOT CMAKE_CSharp_LINK_EXECUTABLE)
  SET(CMAKE_CSharp_LINK_EXECUTABLE
    # I could not references 'SOURCES' so I simply copy all source to fake OBJECTS

    # .exe is required otherwise I get:
    #  Unhandled Exception: System.ArgumentException: Module file name
    #  'cmTryCompileExec' must have file extension.
    #"<CMAKE_CSharp_COMPILER> <FLAGS> <OBJECTS> -out:<TARGET>.exe <LINK_FLAGS> <LINK_LIBRARIES>")
    "<CMAKE_CSharp_COMPILER> <FLAGS> <OBJECTS> -out:<TARGET_BASE>.exe <LINK_FLAGS> <LINK_LIBRARIES>")
ENDIF(NOT CMAKE_CSharp_LINK_EXECUTABLE)

IF(NOT CMAKE_CSharp_CREATE_SHARED_LIBRARY)
  SET(CMAKE_CSharp_CREATE_SHARED_LIBRARY
      #"<CMAKE_CSharp_COMPILER> /target:library <OBJECTS> -out:<TARGET>")
      "<CMAKE_CSharp_COMPILER> <CMAKE_SHARED_LIBRARY_CSharp_FLAGS> <OBJECTS> -out:<TARGET>")
ENDIF(NOT CMAKE_CSharp_CREATE_SHARED_LIBRARY)

# set java include flag option and the separator for multiple include paths
#SET(CMAKE_INCLUDE_FLAG_CSharp "-classpath ")
#IF(WIN32 AND NOT CYGWIN)
#  SET(CMAKE_INCLUDE_FLAG_SEP_CSharp ";")
#ELSE(WIN32 AND NOT CYGWIN)
#  SET(CMAKE_INCLUDE_FLAG_SEP_CSharp ":")
#ENDIF(WIN32 AND NOT CYGWIN)

SET(CMAKE_CSharp_FLAGS_INIT "$ENV{CSFLAGS} ${CMAKE_CSharp_FLAGS_INIT}")

# avoid just having a space as the initial value for the cache
IF(CMAKE_CSharp_FLAGS_INIT STREQUAL " ")
  SET(CMAKE_CSharp_FLAGS_INIT)
ENDIF(CMAKE_CSharp_FLAGS_INIT STREQUAL " ")
SET (CMAKE_CSharp_FLAGS "${CMAKE_CSharp_FLAGS_INIT}" CACHE STRING
     "Flags used by the compiler during all build types.")
