/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmPixelWriteConvert.cxx
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

#include "gdcmDebug.h"
#include "gdcmPixelWriteConvert.h"

namespace gdcm
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Constructor
 */
PixelWriteConvert::PixelWriteConvert() 
{
   ReadData     = 0;
   ReadDataSize = 0;

   UserData     = 0;
   UserDataSize = 0;
}

/**
 * \brief Destructor
 */
PixelWriteConvert::~PixelWriteConvert() 
{
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   SetReadData
 * @param   data data
 * @param   size size
 */
void PixelWriteConvert::SetReadData(uint8_t *data, size_t size)
{
   ReadData = data;
   ReadDataSize = size;
}

/**
 * \brief   Sets User Data
 * @param   data data
 * @param   size size
 */
void PixelWriteConvert::SetUserData(uint8_t *data, size_t size)
{
   UserData = data;
   UserDataSize = size;
}

/**
 * \brief   Get Data (UserData or ReadData)
 * @return  data data
 */
uint8_t *PixelWriteConvert::GetData()
{
   if(UserData)
   {
      return UserData;
   }
   else
   {
      return ReadData;
   }
}

/**
 * \brief   Get Data Size (UserData or ReadData)
 * @return  size size
 */
size_t PixelWriteConvert::GetDataSize()
{
   if(UserData)
   {
      return UserDataSize;
   }
   else
   {
      return ReadDataSize;
   }
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
} // end namespace gdcm
