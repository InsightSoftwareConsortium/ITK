LIBRARY(CxxTypes)

SOURCE_FILES(
cxxArrayType
cxxClassType
cxxConversions
cxxCvQualifiedType
cxxFunctionType
cxxFundamentalType
cxxPointerToMemberType
cxxPointerType
cxxReferenceType
cxxType
cxxTypeSystem
cxxTypedefType
)

SOURCE_FILES_REQUIRE(WIN32 SOURCES_BEGIN cxxDllAllocator)

WIN32_DEFINES(-D_cxx_DLL)
