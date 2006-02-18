/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmRLEFrame.cxx
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
                                                                                
#include "gdcmRLEFrame.h"
#include "gdcmDebug.h"
                                                                                
namespace gdcm
{
//-------------------------------------------------------------------------
// Constructor / Destructor

//-----------------------------------------------------------------------------
// Public
void RLEFrame::SetOffset(unsigned int id,long offset)
{
   gdcmAssertMacro(id<15);
   Offset[id] = offset;
}

long RLEFrame::GetOffset(unsigned int id)
{
   gdcmAssertMacro(id<15);
   return Offset[id];
}

void RLEFrame::SetLength(unsigned int id,long length)
{
   gdcmAssertMacro(id<15);
   Length[id] = length;
}

long RLEFrame::GetLength(unsigned int id)
{
   gdcmAssertMacro(id<15);
   return Length[id];
}

uint8_t *RLEFrame::ReadAndDecompressRLEFrame( uint8_t *subRaw,
                                          long rawSegmentSize,
                                          std::ifstream *fp )
{
   // Loop on the fragments
   for( unsigned int k = 1; k <= NumberOfFragments; k++ )
   {
      // First thing need to reset file to proper position:
      fp->seekg(Offset[k], std::ios::beg);
      ReadAndDecompressRLEFragment(subRaw, Length[k],
                                   rawSegmentSize, fp);
      subRaw += rawSegmentSize;
   }

   return subRaw;
}

/**
 * \brief Implementation of the RLE decoding algorithm for decompressing
 *        a RLE fragment. [refer to PS 3.5-2003, section G.3.2 p 86]
 * @param subRaw Sub region where the decoded fragment should be placed.
 * @param fragmentSize The length of the binary fragment as found on the disk.
 * @param rawSegmentSize The expected length of the fragment ONCE Raw.
 * @param fp File Pointer: on entry the position should be the one of
 *        the fragment to be decoded.
 */
bool RLEFrame::ReadAndDecompressRLEFragment( uint8_t *subRaw,
                                             long fragmentSize,
                                             long rawSegmentSize,
                                             std::ifstream *fp )
{
   int8_t count;
   long numberOfOutputBytes = 0;
   long numberOfReadBytes = 0;

   while( numberOfOutputBytes < rawSegmentSize )
   {
      fp->read( (char*)&count, 1 );
      numberOfReadBytes += 1;
      if ( count >= 0 )
      // Note: count <= 127 comparison is always true due to limited range
      //       of data type int8_t [since the maximum of an exact width
      //       signed integer of width N is 2^(N-1) - 1, which for int8_t
      //       is 127].
      {
         fp->read( (char*)subRaw, count + 1);
         numberOfReadBytes   += count + 1;
         subRaw     += count + 1;
         numberOfOutputBytes += count + 1;
      }
      else
      {
         if ( count <= -1 && count >= -127 )
         {
            int8_t newByte;
            fp->read( (char*)&newByte, 1);
            numberOfReadBytes += 1;
            for( int i = 0; i < -count + 1; i++ )
            {
               subRaw[i] = newByte;
            }
            subRaw     += -count + 1;
            numberOfOutputBytes += -count + 1;
         }
      }
      // if count = 128 output nothing
                                                                                
      if ( numberOfReadBytes > fragmentSize )
      {
         gdcmWarningMacro( "Read more bytes (" << numberOfReadBytes
                              << " ) than the segment size. (" 
                              << fragmentSize << ")" );
         return false;
      }
   }
   return true;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief        Print self.
 * @param indent Indentation string to be prepended during printing.
 * @param os     Stream to print to.
 */
void RLEFrame::Print( std::ostream &os, std::string const &indent )
{
   os << indent
      << "--- fragments"
      << std::endl;
   for ( unsigned int i = 0; i < NumberOfFragments; i++ )
   {
      os << indent
         << "   offset : " <<  Offset[i]
         << "   length : " <<  Length[i]
         << std::endl;
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm

