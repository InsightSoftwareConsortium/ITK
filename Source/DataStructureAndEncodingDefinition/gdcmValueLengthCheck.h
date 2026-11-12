/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMVALUELENGTHCHECK_H
#define GDCMVALUELENGTHCHECK_H

#include "gdcmByteValue.h"
#include "gdcmTrace.h"
#include "gdcmVL.h"

#include <istream>

namespace gdcm_ns
{
namespace details
{
// True when a ByteValue of length vl cannot fit in the whole stream; the
// stream position is left unchanged and the bytes remaining are returned.
inline bool LengthCannotFit(std::istream &is, const Value &v, const VL &vl,
                            uint64_t &remaining)
{
  // Small allocations are harmless; skipping them avoids a seek per element.
  const uint32_t smallLength = 1024 * 1024;
  if( vl.IsUndefined() || vl <= smallLength
    || dynamic_cast<const ByteValue*>(&v) == nullptr )
    {
    return false;
    }
  const std::streampos cur = is.tellg();
  if( cur == std::streampos(-1) )
    {
    return false;
    }
  is.seekg(0, std::ios::end);
  const std::streampos end = is.tellg();
  is.clear();
  is.seekg(static_cast<std::streamoff>(cur), std::ios::beg);
  if( end == std::streampos(-1)
    || static_cast<uint64_t>(end) >= static_cast<uint64_t>(vl) )
    {
    return false;
    }
  remaining = static_cast<uint64_t>(end - cur);
  return true;
}
} // end namespace details

/**
 * \brief Tell whether reading a ByteValue of length vl from is can only fail.
 *
 * A length larger than the whole stream can never be satisfied. For such a
 * length this returns true and leaves the stream as a short read would (at
 * its end, failbit and eofbit set), so the caller can take its existing
 * read-failure path without allocating vl bytes first.
 */
inline bool ValueLengthExceedsStream(std::istream &is, const Value &v, const VL &vl)
{
  uint64_t remaining;
  if( !details::LengthCannotFit(is, v, vl, remaining) )
    {
    return false;
    }
  is.seekg(0, std::ios::end);
  is.setstate(std::ios::failbit | std::ios::eofbit);
  return true;
}

/**
 * \brief Shorten a length that cannot fit to the bytes that remain.
 *
 * Lets a truncated value keep the bytes the file still holds without
 * allocating a buffer larger than the file.
 */
inline VL TruncateLengthToStream(std::istream &is, const Value &v, const VL &vl)
{
  uint64_t remaining;
  if( !details::LengthCannotFit(is, v, vl, remaining) )
    {
    return vl;
    }
#ifdef GDCM_SUPPORT_BROKEN_IMPLEMENTATION
  return static_cast<uint32_t>(remaining);
#else
  // ByteValue rejects an odd length in a strict build.
  return static_cast<uint32_t>(remaining & ~uint64_t(1));
#endif
}

/**
 * \brief Shorten a Pixel Data length that cannot fit to the bytes that remain.
 *
 * Keeps the "Incomplete Pixel Data" tolerance for a truncated file. The
 * result is even, as a Pixel Data length must be.
 */
inline VL TruncatePixelDataLength(std::istream &is, const Value &v, const VL &vl)
{
  const VL fit = TruncateLengthToStream(is, v, vl);
  if( fit == vl )
    {
    return vl;
    }
  gdcmWarningMacro( "Incomplete Pixel Data found, use file at own risk" );
  return static_cast<uint32_t>(fit) & ~uint32_t(1);
}

} // end namespace gdcm_ns

#endif // GDCMVALUELENGTHCHECK_H
