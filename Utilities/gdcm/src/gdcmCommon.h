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

#include <string>
#include <assert.h>

#ifdef CMAKE_HAVE_STDINT_H
#include <stdint.h>   // For uint8_t uint16_t and uint32_t
#else
#if defined(_MSC_VER) || defined(__BORLANDC__)
typedef    signed char   int8_t;
#endif
typedef  unsigned char    uint8_t;
typedef  unsigned short   uint16_t;
typedef  unsigned int     uint32_t;
typedef  unsigned __int64 uint64_t;
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

namespace gdcm
{

// Centralize information about the gdcm dictionary in only one file:
#ifndef PUB_DICT_PATH
#  define PUB_DICT_PATH   "../Dicts/"
#endif
#define PUB_DICT_NAME     "DicomV3Dict"
#define PUB_DICT_FILENAME "dicomV3.dic"
#define DICT_ELEM         "DicomDir.dic"
#define DICT_TS           "dicomTS.dic"
#define DICT_VR           "dicomVR.dic"

GDCM_EXPORT extern const std::string GDCM_UNFOUND;
GDCM_EXPORT extern const std::string GDCM_BINLOADED;
GDCM_EXPORT extern const std::string GDCM_NOTLOADED;
GDCM_EXPORT extern const std::string GDCM_UNREAD;


/// \brief TagKey is made to hold an "universal" (as in URL, Universal
///        Ressource Locator)  key to a DocEntry i.e. a dicom tag.
///        A dicom tag always has a group and an element, but a set of tags
///        embeded in various (optionally nested) sequences and sharing
///        the same group and element all share the same (group, element)
///        "identifier". Hence the (group, element) cannot be used as an
///        identifier (in gdcm we shall refer to a "TagKey") of a tag.
///        In order to construct a proper tag identifier (i.e. a key) we
///        consider the following definition of a TagKey:
///        - let Group, Element be the string representation of the
///          group and element dicom tag members,
///        - let ItemNumber be the string representation of the integer
///          index of the considered item number of a sequence,
///        Let the key of a tag embeded in a sequence, noted SeqTag, be
///        the form:
///           /ItemNumber#Group|Element
///        where "/", "#" and "|" are characters acting as separators.
///        Then the general form of a TagKey is given by:
///           Group|Element[SeqTag]
///        where [SeqTag] means NO or many instances of SeqTag.
///        Hence the TagKey of a tag not "leaving" in a sequence is the
///        string e.g. 
///            0028|1201
///        but the TagKey of a tag "embeded" is the first item of
///        a sequence, itself nested in the third item of a sequence is the
///        string e.g.
///            0004|1220/2#0008|0082/0#0008|0090
typedef std::string TagKey;
typedef std::string TagName;

enum FileType {
      Unknown = 0,
      ExplicitVR, // DicomDir is in this case
      ImplicitVR,
      ACR,
      ACR_LIBIDO
};
} //namespace gdcm
//-----------------------------------------------------------------------------
#endif
