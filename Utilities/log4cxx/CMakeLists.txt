PROJECT(log4cxx)


INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFiles.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/FindThreads.cmake)

# Check for pthread
CHECK_INCLUDE_FILES("pthread.h" HAVE_PTHREAD)
CHECK_INCLUDE_FILES("stdint.h" HAVE_STDINT_H)
CHECK_INCLUDE_FILES("stdlib.h" HAVE_STDLIB_H)
CHECK_INCLUDE_FILES("strings.h" HAVE_STRINGS_H)
CHECK_INCLUDE_FILES("string.h" HAVE_STRING_H)
CHECK_INCLUDE_FILES("sys/stat.h" HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILES("sys/types.h" HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILES("sys/types.h" HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILES("unistd.h" HAVE_UNISTD_H)
CHECK_INCLUDE_FILES("alloca.h" HAVE_ALLOCA_H)
CHECK_INCLUDE_FILES("boost/regexp.hpp" HAVE_BOOST_REGEX_HPP)
CHECK_INCLUDE_FILES("dlfcn.h" HAVE_DLFCN_H)
CHECK_INCLUDE_FILES("inttypes.h" HAVE_INTTYPES_H)
CHECK_INCLUDE_FILES("io.h" HAVE_IO_H)
CHECK_INCLUDE_FILES("memory.h" HAVE_MEMORY_H)


CONFIGURE_FILE(${PROJECT_SOURCE_DIR}/include/log4cxx/config_auto.h.in
               ${PROJECT_BINARY_DIR}/include/log4cxx/config_auto.h)

INCLUDE_DIRECTORIES(
${log4cxx_SOURCE_DIR}/include
${log4cxx_BINARY_DIR}/include
)

IF(WIN32)
  IF(NOT CYGWIN)
    IF(BORLAND)
    ELSE(BORLAND)
       LINK_LIBRARIES( wsock32 comctl32 )
    ENDIF(BORLAND)
  ENDIF(NOT CYGWIN)
ENDIF(WIN32)


IF(NOT BUILD_SHARED_LIBS)
  ADD_DEFINITIONS("-DLOG4CXX_STATIC")
ENDIF(NOT BUILD_SHARED_LIBS)

SET(log4cxx_SOURCES
src/appenderattachableimpl.cpp
src/appenderskeleton.cpp
src/asyncappender.cpp
src/basicconfigurator.cpp
src/boundedfifo.cpp
src/class.cpp
src/condition.cpp
src/configurator.cpp
src/consoleappender.cpp
src/criticalsection.cpp
src/cyclicbuffer.cpp
src/dailyrollingfileappender.cpp
src/datagrampacket.cpp
src/datagramsocket.cpp
src/dateformat.cpp
src/datelayout.cpp
src/defaultcategoryfactory.cpp
src/domconfigurator.cpp
src/event.cpp
src/fallbackerrorhandler.cpp
src/fileappender.cpp
src/filewatchdog.cpp
src/formattinginfo.cpp
src/gnomexml.cpp
src/hierarchy.cpp
src/htmllayout.cpp
src/inetaddress.cpp
src/layout.cpp
src/level.cpp
src/levelmatchfilter.cpp
src/levelrangefilter.cpp
src/loader.cpp
src/locale.cpp
src/logger.cpp
src/loggingevent.cpp
src/loglog.cpp
src/logmanager.cpp
src/mdc.cpp
src/msxml.cpp
src/mutex.cpp
src/ndc.cpp
src/nteventlogappender.cpp
src/objectimpl.cpp
src/odbcappender.cpp
src/onlyonceerrorhandler.cpp
src/optionconverter.cpp
src/patternconverter.cpp
src/patternlayout.cpp
src/patternparser.cpp
src/properties.cpp
src/propertyconfigurator.cpp
src/propertyresourcebundle.cpp
src/propertysetter.cpp
src/resourcebundle.cpp
src/rollingfileappender.cpp
src/rootcategory.cpp
src/semaphore.cpp
src/serversocket.cpp
src/simplelayout.cpp
src/simplesocketserver.cpp
src/smtpappender.cpp
src/socket.cpp
src/socketappender.cpp
src/sockethubappender.cpp
src/socketimpl.cpp
src/socketinputstream.cpp
src/socketnode.cpp
src/socketoutputstream.cpp
src/stringhelper.cpp
src/stringmatchfilter.cpp
src/stringtokenizer.cpp
src/syslogappender.cpp
src/syslogwriter.cpp
src/system.cpp
src/telnetappender.cpp
src/thread.cpp
src/threadspecificdata.cpp
src/timezone.cpp
src/transform.cpp
src/ttcclayout.cpp
src/writerappender.cpp
src/xmllayout.cpp
src/xmlsocketappender.cpp
)


ADD_LIBRARY(log4cxx ${log4cxx_SOURCES} )
IF( NOT ITK_DISABLE_CXX_TESTING )
  ADD_EXECUTABLE(log4cxxTest log4cxxTest.cxx)
  TARGET_LINK_LIBRARIES(log4cxxTest log4cxx ${CMAKE_THREAD_LIBS})
  ADD_TEST(log4cxxTest ${CXX_TEST_PATH}/log4cxxTest)
ENDIF( NOT ITK_DISABLE_CXX_TESTING )

