#ifndef _win32Header_h
#define _win32Header_h

// add in the Windows variants
#if defined(_WIN32) || defined(WIN32)

// for-loop scoping hack
#define for if(false) {} else for

//
// Disable some common warnings in MS VC++
//

// 'identifier' : identifier was truncated to 'number' characters in the
// debug information
#pragma warning ( disable : 4786 )

#endif

#endif
