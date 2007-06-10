/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmCommon.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/

#ifndef GDCMCOMMON_H
#define GDCMCOMMON_H

#include "gdcmConfigure.h"

//-----------------------------------------------------------------------------
//This is needed when compiling in debug mode
#ifdef _MSC_VER
// 'identifier' : class 'type' needs to have dll-interface to be used by
// clients of class 'type2'
#pragma warning ( disable : 4251 )
// non dll-interface class 'type' used as base for dll-interface class 'type2'
#pragma warning ( disable : 4275 )
// 'identifier' : identifier was truncated to 'number' characters in the
// debug information
#pragma warning ( disable : 4786 )
//'identifier' : decorated name length exceeded, name was truncated
#pragma warning ( disable : 4503 )
// C++ exception specification ignored except to indicate a 
// function is not __declspec(nothrow)
#pragma warning ( disable : 4290 )
// signed/unsigned mismatch
#pragma warning ( disable : 4018 )
// return type for 'identifier' is '' (ie; not a UDT or reference to UDT. Will
// produce errors if applied using infix notation
#pragma warning ( disable : 4284 )
// 'type' : forcing value to bool 'true' or 'false' (performance warning)
// //#pragma warning ( disable : 4800 )
#endif //_MSC_VER

//-----------------------------------------------------------------------------
#ifdef CMAKE_HAVE_STDINT_H
   #include <stdint.h>
#else
#ifdef CMAKE_HAVE_INTTYPES_H
   // Old system only have this
   #include <inttypes.h>   // For uint8_t uint16_t and uint32_t
#else
// Broken plateforms do not respect C99 and do not provide those typedef
// Special case for recent Borland compiler, comes with stdint.h
#if defined(_MSC_VER) || defined(__BORLANDC__) && (__BORLANDC__ < 0x0560)  \
                      || defined(__MINGW32__)
typedef  signed char         int8_t;
typedef  signed short        int16_t;
typedef  signed int          int32_t;
typedef  unsigned char       uint8_t;
typedef  unsigned short      uint16_t;
typedef  unsigned int        uint32_t;
#else
#error "Sorry your plateform is not supported"
#endif // defined(_MSC_VER) || defined(__BORLANDC__) && (__BORLANDC__ < 0x0560)  || defined(__MINGW32__)
#endif // CMAKE_HAVE_INTTYPES_H
#endif // CMAKE_HAVE_STDINT_H

// Basically for VS6 and bcc 5.5.1:
#ifndef UINT32_MAX
#define UINT32_MAX    (4294967295U)
#endif

#if defined(_WIN32) && defined(BUILD_SHARED_LIBS)
  #ifdef gdcm_EXPORTS
    #define GDCM_EXPORT __declspec( dllexport )
  #else
    #define GDCM_EXPORT __declspec( dllimport )
  #endif
#else
  #define GDCM_EXPORT
#endif

#include <string>
#define FASTTAGKEY 0

// FIXME: Should rewrite this:
#if FASTTAGKEY
#include <iostream>
#include <iomanip>
#endif
#if defined(_MSC_VER) && (_MSC_VER == 1200)
/* ostream operator for std::string since VS6 does not provide it*/
#include <iostream>
#endif


//-----------------------------------------------------------------------------
/// \brief namespace for Grass root DiCoM
namespace gdcm
{

// Centralize information about the gdcm dictionary in only one file:
#ifndef PUB_DICT_PATH
#  define PUB_DICT_PATH   "../Dicts/"
#endif
#define PUB_DICT_NAME     "DicomV3Dict"
#define PUB_DICT_FILENAME "gdcm.dic"
#define DICT_ELEM         "DicomDir.dic"
#define DICT_TS           "dicomTS.dic"
#define DICT_VR           "dicomVR.dic"
#define DICT_GROUP_NAME   "DictGroupName.dic"

GDCM_EXPORT extern const std::string GDCM_UNKNOWN;
GDCM_EXPORT extern const std::string GDCM_UNFOUND;
GDCM_EXPORT extern const std::string GDCM_BINLOADED;
GDCM_EXPORT extern const std::string GDCM_NOTLOADED;
GDCM_EXPORT extern const std::string GDCM_UNREAD;
GDCM_EXPORT extern const std::string GDCM_NOTASCII;
GDCM_EXPORT extern const std::string GDCM_PIXELDATA;
/// \brief TagKey is made to hold the standard Dicom Tag 
///               (Group number, Element number)
/// Instead of using the two '16 bits integers' as the Hask Table key, we
/// converted into a string (e.g. 0x0018,0x0050 converted into "0018|0050")
/// It appears to be a huge waste of time.
/// We'll fix the mess up -without any change in the API- as soon as the bench
/// marks are fully performed.

#if FASTTAGKEY
typedef union   {
      uint16_t  tab[2];
      uint32_t  tagkey;
    } TagKey;
/* ostream operator for TagKey */
inline std::ostream& operator<<(std::ostream& _O, TagKey _val)
{
  _O.setf( std::ios::right);
  return (_O << std::hex << std::setw( 4 ) << std::setfill( '0' )
     << _val.tab[0] << '|' << std::setw( 4 ) << std::setfill( '0' )
     << _val.tab[1] << std::setfill( ' ' ) << std::dec);
}
inline bool operator==(TagKey _self, TagKey _val)
{
  return _self.tagkey == _val.tagkey;
}
inline bool operator<(TagKey _self, TagKey _val)
{
  // This expression is a tad faster but PrintFile output
  // is more difficult to read
  //return _self.tagkey < _val.tagkey;

  // More usal order of dicom tags:
  if( _self.tab[0] == _val.tab[0] )
    return _self.tab[1] < _val.tab[1];
  return _self.tab[0] < _val.tab[0];
}
#else
typedef std::string TagKey;
#endif
#if defined(_MSC_VER) && (_MSC_VER == 1200)
// Doing everything within gdcm namespace to avoid polluting 3d party software
inline std::ostream& operator<<(std::ostream& _O, std::string _val)
{
  return _O << _val.c_str();
}
#endif

/// \brief TagName is made to hold the 'non hexa" fields (VR, VM, Name) 
///        of Dicom Entries
typedef std::string TagName;

/// \brief various types of a DICOM file (for internal use only)
enum FileType {
   Unknown = 0,
   ExplicitVR, // DicomDir is in this case. Except when it's ImplicitVR !...
   ImplicitVR,
   ACR,
   ACR_LIBIDO,
   JPEG,
   JPEG2000
};

/// \brief type of the elements composing a DICOMDIR (for internal use only)
enum DicomDirType {
   DD_UNKNOWN = 0,
   DD_META,
   DD_PATIENT,
   DD_STUDY,
   DD_SERIE,
   DD_IMAGE,
   DD_VISIT
};

/// \brief comparison operators (as used in SerieHelper::AddRestriction() )
enum CompOperators {
   GDCM_EQUAL = 0,
   GDCM_DIFFERENT,
   GDCM_GREATER,
   GDCM_GREATEROREQUAL,
   GDCM_LESS,
   GDCM_LESSOREQUAL
};

/// \brief Loading mode
enum LodModeType
{
   LD_ALL         = 0x00000000, // Load all
   LD_NOSEQ       = 0x00000001, // Don't load Sequences
   LD_NOSHADOW    = 0x00000002, // Don't load odd groups
   LD_NOSHADOWSEQ = 0x00000004  // Don't load Sequences if they belong 
                                //            to an odd group
                                // (*exclusive* from LD_NOSEQ and LD_NOSHADOW)
};

/**
 * \brief structure, for internal use only
 */  
struct Element
{
   /// Dicom Group number
   unsigned short int Group;
   /// Dicom Element number
   unsigned short int Elem;
   /// value (coded as a std::string) of the Element
   std::string Value;
};

} //namespace gdcm
//-----------------------------------------------------------------------------
#endif
