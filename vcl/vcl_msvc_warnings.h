#ifndef vcl_msvc_warnings_h__
#define vcl_msvc_warnings_h__
/** Disable some common warnings in MS VC++ */
/* This is the set of warnings suppressed by ITK */
#if defined( _MSC_VER )

// 'conversion' conversion from 'type1' to 'type2', possible loss of data
#pragma warning ( disable : 4244 )

// 'identifier' : truncation from 'type1' to 'type2'
#pragma warning ( disable : 4305 )

// 'conversion' : truncation of constant value
#pragma warning ( disable : 4309 )

// decorated name length exceeded, name was truncated
#pragma warning ( disable : 4503 )

// 'type' : forcing value to bool 'true' or 'false' (performance warning)
#pragma warning ( disable : 4800 )

// 'identifier' : class 'type' needs to have dll-interface to be used by
// clients of class 'type2'
#pragma warning ( disable : 4251 )

// non dll-interface class 'type' used as base for dll-interface class 'type2'
#pragma warning ( disable : 4275 )

// C++ exception specification ignored except to indicate a
// function is not __declspec(nothrow)
#pragma warning ( disable : 4290 )

// 'type' : inconsistent dll linkage.  dllexport assumed.
#pragma warning ( disable : 4273 )

// conditional expression is constant
#pragma warning ( disable : 4127 )

// unreferenced local function has been removed
#pragma warning ( disable : 4505 )

// 'identifier' : identifier was truncated to 'number' characters in the debug information
#pragma warning ( disable : 4786 )

// nonstandard extension used : 'extern' before template explicit instantiation
#pragma warning ( disable : 4231 )

// data-conversion related to 'size_t'
#pragma warning ( disable : 4267 )

#endif // _MSC_VER
#endif //vcl_msvc_warnings_h__
