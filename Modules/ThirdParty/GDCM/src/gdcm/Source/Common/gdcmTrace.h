/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMTRACE_H
#define GDCMTRACE_H

#include "gdcmTypes.h"

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <assert.h>
#include <errno.h>
#include <string.h> // strerror


namespace gdcm
{

/**
 * \brief Trace
 * \details Debug / Warning and Error are encapsulated in this class
 */
class GDCM_EXPORT Trace
{
public :
  Trace();
  ~Trace();

  static void SetDebug(bool debug); //  { DebugFlag = true; };
  static void DebugOn(); //  { DebugFlag = true; };
  static void DebugOff(); // { DebugFlag = false; };
  static bool GetDebugFlag(); // { DebugFlag = false; };

  static void SetWarning(bool debug); //  { DebugFlag = true; };
  static void WarningOn(); //  { WarningFlag = true; };
  static void WarningOff(); // { WarningFlag = false; };
  static bool GetWarningFlag();

  static void SetError(bool debug); //  { DebugFlag = true; };
  static void ErrorOn(); //  { ErrorFlag = true; };
  static void ErrorOff(); // { ErrorFlag = false; };
  static bool GetErrorFlag();

  static bool GetDebugToFile ();
  static std::ofstream &GetDebugFile ();

protected:
private:
//  static bool DebugFlag;
//  static bool WarningFlag;
//  static bool ErrorFlag;
};

// Here we define function this is the only way to be able to pass
// stuff with indirection like:
// gdcmDebug( "my message:" << i << '\t' );
// You cannot use function unless you use vnsprintf ...

// __FUNCTION is not always defined by preprocessor
// In c++ we should use __PRETTY_FUNCTION__ instead...
#ifdef GDCM_CXX_HAS_FUNCTION
// Handle particular case for GNU C++ which also defines __PRETTY_FUNCTION__
// which is a lot nice in C++
#ifdef __BORLANDC__
#  define __FUNCTION__ __FUNC__
#endif
#ifdef __GNUC__
#  define GDCM_FUNCTION __PRETTY_FUNCTION__
#else
#  define GDCM_FUNCTION __FUNCTION__
#endif //__GNUC__
#else
#  define GDCM_FUNCTION "<unknow>"
#endif //GDCM_CXX_HAS_FUNCTION

/**
 * \brief   Debug
 * @param msg message part
 */
#ifdef NDEBUG
#define gdcmDebugMacro(msg) {}
#else
#define gdcmDebugMacro(msg)                                       \
{                                                                 \
   if( gdcm::Trace::GetDebugFlag() )                              \
   {                                                              \
   std::ostringstream osmacro;                                    \
   osmacro << "Debug: In " __FILE__ ", line " << __LINE__         \
           << ", function " << GDCM_FUNCTION << '\n'              \
           << "Last system error was: " << strerror(errno)        \
           << '\n' << msg << "\n\n";                              \
   if( gdcm::Trace::GetDebugToFile() )                            \
      gdcm::Trace::GetDebugFile() << osmacro.str() << std::endl;  \
   else                                                           \
      std::cerr << osmacro.str() << std::endl;                    \
   }                                                              \
}
#endif //NDEBUG

/**
 * \brief   Warning
 * @param msg message part
 */
#ifdef NDEBUG
#define gdcmWarningMacro(msg) {}
#else
#define gdcmWarningMacro(msg)                                     \
{                                                                 \
   if( gdcm::Trace::GetWarningFlag() )                            \
   {                                                              \
   std::ostringstream osmacro;                                    \
   osmacro << "Warning: In " __FILE__ ", line " << __LINE__       \
           << ", function " << GDCM_FUNCTION << "\n"              \
           << msg << "\n\n";                                      \
   if( gdcm::Trace::GetDebugToFile() )                            \
      gdcm::Trace::GetDebugFile() << osmacro.str() << std::endl;  \
   else                                                           \
      std::cerr << osmacro.str() << std::endl;                    \
   }                                                              \
}
#endif //NDEBUG

/**
 * \brief   Error this is pretty bad, more than just warning
 * It could mean lost of data, something not handle...
 * @param msg second message part
 */
#ifdef NDEBUG
#define gdcmErrorMacro(msg) {}
#else
#define gdcmErrorMacro(msg)                                       \
{                                                                 \
   if( gdcm::Trace::GetErrorFlag() )                              \
   {                                                              \
   std::ostringstream osmacro;                                    \
   osmacro << "Error: In " __FILE__ ", line " << __LINE__         \
           << ", function " << GDCM_FUNCTION << '\n'              \
           << msg << "\n\n";                                      \
   if( gdcm::Trace::GetDebugToFile() )                            \
      gdcm::Trace::GetDebugFile() << osmacro.str() << std::endl;  \
   else                                                           \
      std::cerr << osmacro.str() << std::endl;                    \
   }                                                              \
}
#endif //NDEBUG

/**
 * \brief   Assert
 * @param arg argument to test
 *        An easy solution to pass also a message is to do:
 *        gdcmAssertMacro( "my message" && 2 < 3 )
 */
#ifdef NDEBUG
#define gdcmAssertMacro(arg) {}
#else
#define gdcmAssertMacro(arg)                                      \
{                                                                 \
   if( !(arg) )                                                   \
   {                                                              \
   std::ostringstream osmacro;                                    \
   osmacro << "Assert: In " __FILE__ ", line " << __LINE__        \
           << ", function " << GDCM_FUNCTION                      \
           << "\n\n";                                             \
   if( gdcm::Trace::GetDebugToFile() )                            \
      gdcm::Trace::GetDebugFile() << osmacro.str() << std::endl;  \
   else                                                           \
      std::cerr << osmacro.str() << std::endl;                    \
   assert ( arg );                                                \
   }                                                              \
}
#endif //NDEBUG

/**
 * \brief   AssertAlways
 * @param arg argument to test
 *        An easy solution to pass also a message is to do:
 *        gdcmAssertMacro( "my message" && 2 < 3 )
 */
#ifdef NDEBUG
// User asked for release compilation, but still need to report
// if grave issue.
#define gdcmAssertAlwaysMacro(arg) \
{                                                                 \
   if( !(arg) )                                                   \
   {                                                              \
   std::ostringstream osmacro;                                    \
   osmacro << "Assert: In " __FILE__ ", line " << __LINE__        \
           << ", function " << GDCM_FUNCTION                      \
           << "\n\n";                                             \
   throw osmacro.str();                                           \
   }                                                              \
}
#else
// Simply reproduce gdcmAssertMacro behavior:
#define gdcmAssertAlwaysMacro(arg) gdcmAssertMacro(arg)
#endif //NDEBUG

} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif //GDCMTRACE_H
