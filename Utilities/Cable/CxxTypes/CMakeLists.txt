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
cxxTypedefType
)

IF (WIN32)
    SOURCE_FILES(CxxTypes_SRCS cxxDllAllocator)
    ADD_DEFINITIONS(-D_cxx_LIBRARY)
ENDIF (WIN32)

ADD_LIBRARY(CxxTypes CxxTypes_SRCS)

