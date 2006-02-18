/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmRLEFramesInfo.h
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


#ifndef GDCMRLEFRAMESINFO_H
#define GDCMRLEFRAMESINFO_H

#include "gdcmRLEFrame.h"

#include <list>

namespace gdcm 
{
/**
 * \brief Utility class for gathering the informations of the collection
 *        of RLE frame[s] (see \ref RLEFrame)  when handling
 *        "Encapsulated RLE Compressed Images" (see PS 3.5-2003 annex G). 
 *        Note: a classical image can be considered as the degenerated case
 *              of a multiframe image. In this case the collection is limited
 *              to a single individual frame.
 *        The informations on each frame are obtained during the pixel parsing
 *        of a gdcm::File (refer to
 *          \ref File::ComputeRLEInfo() ).
 *        They shall be used when (if necessary) decoding the frames.
 *
 *        This class is simply a stl list<> of \ref RLEFrame.
 */
class GDCM_EXPORT RLEFramesInfo
{
friend class PixelReadConvert;
friend class File;

private:
   ~RLEFramesInfo();
   void Print( std::ostream &os = std::cout, std::string indent = "" );
   bool DecompressRLEFile( std::ifstream *fp, uint8_t *subRaw, int xSize, 
                           int ySize, int zSize, int bitsAllocated );
   bool ConvertRLE16BitsFromRLE8Bits( uint8_t *subRaw, int xSize, int ySize, 
                                      int numberOfFrames );

   void AddFrame(RLEFrame *frame);

   RLEFrame *GetFirstFrame();
   RLEFrame *GetNextFrame();

   typedef std::list<RLEFrame *> RLEFrameList;

   RLEFrameList Frames;
   RLEFrameList::iterator ItFrames;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
