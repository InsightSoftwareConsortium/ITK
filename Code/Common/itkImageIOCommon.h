/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIOCommon.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkImageIOCommon_h
#define __itkImageIOCommon_h

#include "itkIntTypes.h"
#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include <string>
#include "itkIndent.h"
#include <deque>
#include <ctype.h>

namespace itk
{

typedef enum
{
  ITK_UCHAR,              // aka uint8_t
  ITK_CHAR,
  ITK_USHORT,             // aka uint16_t
  ITK_SHORT,
  ITK_UINT,               // aka uint32_t
  ITK_INT,
  ITK_ULONG,              // aka uint64_t
  ITK_LONG,
  ITK_FLOAT,
  ITK_DOUBLE
} AtomicPixelType;        // enumerated constants for the different data types

const unsigned int ITK_MAX_DIMENSIONS = 10;

/**
 * Convert the enumerated type to a string representation
 */
std::string AtomicPixelTypeToString(const AtomicPixelType pixelType);

/**
 * Calculate the size, in bytes, that the atomic pixel type occupies
 */
unsigned int CalcSizeOfAtomicPixelType(const AtomicPixelType pixelType);

/**
 * Cross-platform case-insensitive string comparison
 * (MSVC++ doesn't provide strucmp, so necessary to define our own
 * This function taken from /usr/Image library developed at UNC
 */
int Strucmp(const char *s1, const char *s2);

/**
 * Given a full filename, extracts just the pathname
 */
char* ExtractFilePath (const char* fileName);

/**
 * Given a full filename, extracts just the file extension
 */
char* ExtractFileExtension (const char* fileName);

/**
 * Given a full filename, extracts just the filename
 */
char* ExtractFileName (const char* fileName);

} // end namespace itk

#endif // __itkImageIOCommon_h
