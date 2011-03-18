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
#ifndef GDCMBASE64_H
#define GDCMBASE64_H

#include "gdcmTypes.h"

namespace gdcm
{
//-----------------------------------------------------------------------------
class Base64Internals;
/**
 * \brief Class for Base64
 *
 */
class GDCM_EXPORT Base64
{
public :
  Base64();
  ~Base64();

/**
 *                 Call this function with dlen = 0 to obtain the
 *                 required buffer size in dlen
 */
static int GetEncodeLength(const char *src, int  slen );

/**
 * \brief          Encode a buffer into base64 format
 *
 * \param dst      destination buffer
 * \param dlen     size of the buffer
 * \param src      source buffer
 * \param slen     amount of data to be encoded
 *
 * \return         0 if successful
 *
 */
static int Encode( char *dst, int dlen,
                   const char *src, int  slen );

/**
 *                 Call this function with *dlen = 0 to obtain the
 *                 required buffer size in *dlen
 */
static int GetDecodeLength( const char *src, int  slen );

/**
 * \brief          Decode a base64-formatted buffer
 *
 * \param dst      destination buffer
 * \param dlen     size of the buffer
 * \param src      source buffer
 * \param slen     amount of data to be decoded
 *
 * \return         0 if successful
 */
static int Decode( char *dst, int dlen,
                   const char *src, int  slen );

private:
  Base64Internals *Internals;
private:
  Base64(const Base64&);  // Not implemented.
  void operator=(const Base64&);  // Not implemented.
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif //GDCMBASE64_H
