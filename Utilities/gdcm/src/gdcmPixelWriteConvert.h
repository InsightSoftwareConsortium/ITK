/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmPixelWriteConvert.h
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


#ifndef GDCMPIXELWRITECONVERT_H
#define GDCMPIXELWRITECONVERT_H

#include "gdcmCommon.h"
#include "gdcmBase.h"

namespace gdcm
{
/**
 * \brief Utility container for gathering the various forms the pixel data
 *        migth take during the user demanded processes.
 */
class GDCM_EXPORT PixelWriteConvert : public Base
{
public:
   PixelWriteConvert();
   virtual ~PixelWriteConvert();

   // Set/Get of images and their size
   void SetReadData(uint8_t *data,size_t size);
   uint8_t *GetReadData() { return ReadData; }
   size_t   GetReadDataSize() { return ReadDataSize; }

   void SetUserData(uint8_t *data,size_t size);
   uint8_t *GetUserData() { return UserData; }
   size_t   GetUserDataSize() { return UserDataSize; }

   // Get the used image and its size
   uint8_t *GetData();
   size_t   GetDataSize();

private:
// Variables
   /// Pixel data represented as RGB after LUT color interpretation.
   uint8_t *ReadData;
   /// Size of RGB image.
   size_t   ReadDataSize;

   /// User pixel data
   uint8_t *UserData;
   /// Size of User image.
   size_t   UserDataSize;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
