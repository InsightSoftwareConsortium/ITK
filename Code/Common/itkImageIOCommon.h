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
