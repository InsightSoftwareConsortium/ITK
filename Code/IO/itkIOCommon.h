/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIOCommon.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIOCommon_h
#define __itkIOCommon_h

#include "itkIntTypes.h"
#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include <string>

namespace itk
{

/** \class IOCommon
 * \brief Centralized funtionality for IO classes.
 *
 * This class provides encapsulated functionality to support the IO classes.
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT IOCommon 
{
public:
  typedef enum
  {
    ITK_UCHAR,         // aka uint8_t
    ITK_CHAR,         
    ITK_USHORT,        // aka uint16_t
    ITK_SHORT,        
    ITK_UINT,          // aka uint32_t
    ITK_INT,          
    ITK_ULONG,         // aka uint64_t
    ITK_LONG,         
    ITK_FLOAT,        
    ITK_DOUBLE        
  } AtomicPixelType;   // enumerated constants for the different data types

  /** Convert the enumerated type to a string representation. */
  static std::string AtomicPixelTypeToString(const AtomicPixelType pixelType);

  /** Calculate the size, in bytes, that the atomic pixel type occupies. */
  static unsigned int ComputeSizeOfAtomicPixelType(const AtomicPixelType pixelType);

  /** Cross-platform case-insensitive string comparison
   * (MSVC++ doesn't provide strucmp, so necessary to define our own
   * This function taken from /usr/Image library developed at UNC. */
  static int Strucmp(const char *s1, const char *s2);

  /** Given a full filename, extracts just the pathname. */
  static char* ExtractFilePath (const char* fileName);

  /** Given a full filename, extracts just the file extension. */
  static char* ExtractFileExtension (const char* fileName);

  /** Given a full filename, extracts just the filename. */
  static char* ExtractFileName (const char* fileName);

  /** Given a filename determine whether it exists and 
   * return true if it does. */
  static bool FileExists(const char* filename);
};
  

} // end namespace itk

#endif // __itkIOCommon_h
