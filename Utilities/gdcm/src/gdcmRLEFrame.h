/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmRLEFrame.h
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


#ifndef GDCMRLEFRAME_H
#define GDCMRLEFRAME_H

#include "gdcmCommon.h"
#include <iostream>

namespace gdcm 
{
/**
 * \brief Utility class for summerizing the informations of a SINGLE RLE
 *        frame of an "Encapsulated RLE Compressed Image" (refer to
 *        PS 3.5-2003 annex G).
 *        This information is a mix of:
 *        - the RLE Header (see PS 3.5-2003 section G5) and
 *        - the lengths of each RLE segment [ which can be decuded from
 *          both the above RLE Header and the itemlength of the frame).
 *
 *        Each instance of this class (they can be as many instances for
 *        a given Document as they are frames and they are collected in
 *        a \ref RLEFramesInfo ) describes :
 *        - the total number of segments (up to 15),
 *        - the offsets of each segment of the frame,
 *        - the (corresponding) lengths of each segment of the frame.
 */
class GDCM_EXPORT RLEFrame
{
friend class Document;
friend class PixelConvert;
   unsigned int NumberFragments;
   long    Offset[15];
   long    Length[15];
public:
   RLEFrame() { NumberFragments = 0; }
   void Print( std::string indent = "", std::ostream &os = std::cout );
   
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
