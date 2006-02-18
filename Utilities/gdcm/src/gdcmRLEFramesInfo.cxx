/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmRLEFramesInfo.cxx
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

#include "gdcmRLEFramesInfo.h"
#include "gdcmDebug.h"
#include "gdcmUtil.h"

#if defined(__BORLANDC__)
   #include <mem.h> // for memset
#endif 

namespace gdcm 
{
//-------------------------------------------------------------------------
// Constructor / Destructor
RLEFramesInfo::~RLEFramesInfo()
{
   for(RLEFrameList::iterator it = Frames.begin(); it != Frames.end(); ++it)
   {
      delete (*it);
   }
   Frames.clear();
}

//-----------------------------------------------------------------------------
// Public
void RLEFramesInfo::AddFrame(RLEFrame *frame)
{
   Frames.push_back(frame);
}

RLEFrame *RLEFramesInfo::GetFirstFrame()
{
   ItFrames = Frames.begin();
   if (ItFrames != Frames.end())
      return  *ItFrames;
   return NULL;
}

RLEFrame *RLEFramesInfo::GetNextFrame()
{
   gdcmAssertMacro (ItFrames != Frames.end());

   ++ItFrames;
   if (ItFrames != Frames.end())
      return  *ItFrames;
   return NULL;
}

/**
 * \brief     Reads from disk the Pixel Data of 'Run Length Encoded'
 *            Dicom encapsulated file and decompress it.
 * @param     fp already open File Pointer
 *            from which the pixel data should be read
 * @param raw raw
 * @param xSize x Size
 * @param ySize y Size
 * @param zSize z Size
 * @param bitsAllocated Bits allocated
 * @return    Boolean
 */
bool RLEFramesInfo::DecompressRLEFile( std::ifstream *fp , uint8_t *raw, 
                                       int xSize, int ySize, int zSize, 
                                       int bitsAllocated )
{
   uint8_t *subRaw = raw;
   long rawSegmentSize = xSize * ySize;

   // Loop on the frame[s]
   for(RLEFrameList::iterator it = Frames.begin(); it != Frames.end(); ++it)
   {
      subRaw = (*it)->ReadAndDecompressRLEFrame( subRaw, rawSegmentSize, fp);
   }

   if ( bitsAllocated == 16 )
   {
      // Try to deal with RLE 16 Bits
      ConvertRLE16BitsFromRLE8Bits( raw, xSize, ySize, zSize );
   }

   return true;
}

/**
 * \brief  We assume Raw contains the decoded RLE pixels but as
 *         8 bits per pixel. We convert those pixels to 16 bits
 *         per pixel.
 * @param raw raw 
 * @param xSize x Size
 * @param ySize y Size
 * @param numberOfFrames number of frames 
 * @return    Boolean always true
 */
bool RLEFramesInfo::ConvertRLE16BitsFromRLE8Bits(uint8_t *raw, int xSize, 
                                                 int ySize, int numberOfFrames)
{
   size_t pixelNumber = xSize * ySize;
   size_t rawSize     = pixelNumber * numberOfFrames * 2;

   // We assumed Raw contains the decoded RLE pixels but as
   // 8 bits per pixel. In order to convert those pixels to 16 bits
   // per pixel we cannot work in place within Raw and hence
   // we copy it in a safe place, say copyRaw.

   uint8_t *copyRaw = new uint8_t[rawSize];
   memmove( copyRaw, raw, rawSize );

   uint8_t *x = raw;
   uint8_t *a;
   uint8_t *b;

   // Warning : unckecked patch to see the behaviour on Big Endian Processors

   if ( !Util::IsCurrentProcessorBigEndian() )
   { 
      a = copyRaw;         // beginning of 'low bytes'
      b = a + pixelNumber; // beginning of 'hight bytes'
   }
   else
   {
      b = copyRaw;         // beginning of 'low bytes'
      a = b + pixelNumber; // beginning of 'hight bytes'
   } 

   // Re order bytes
   for ( int i = 0; i < numberOfFrames; i++ )
   {
      for ( unsigned int j = 0; j < pixelNumber; j++ )
      {
         *(x++) = *(b++);
         *(x++) = *(a++);
      }
   }

   delete[] copyRaw;

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
void RLEFramesInfo::Print( std::ostream &os, std::string indent )
{
   os << std::endl;
   os << indent
      << "----------------- RLE frames --------------------------------"
      << std::endl;
   os << indent
      << "Total number of Frames : " << Frames.size()
      << std::endl;
   int frameNumber = 0;
   for(RLEFrameList::iterator it = Frames.begin(); it != Frames.end(); ++it)
   {
      os << indent
         << "   frame number :" << frameNumber++
         << std::endl;
      (*it)->Print( os, indent + "   " );
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
