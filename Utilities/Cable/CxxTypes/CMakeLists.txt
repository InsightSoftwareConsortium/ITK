SOURCE_FILES(CxxTypes_SRCS
cxxArrayType
cxxClassType
cxxConversions
cxxCvQualifiedType
cxxEnumerationType
cxxFunctionType
cxxFundamentalType
cxxPointerToMemberType
cxxPointerType
cxxReferenceType
cxxType
cxxTypeSystem
)

IF (WIN32)
    SOURCE_FILES(CxxTypes_SRCS cxxDllAllocator)
ENDIF (WIN32)

ADD_LIBRARY(CxxTypes SHARED CxxTypes_SRCS)
ADD_DEFINITIONS(-D_cxx_SHARED)
