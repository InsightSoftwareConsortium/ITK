/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJPEGFragment.cxx
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
                                                                                
#include "gdcmJPEGFragment.h"
#include "gdcmDebug.h"

namespace gdcm
{
//-------------------------------------------------------------------------
// For JPEG 2000, body in file gdcmJpeg2000.cxx
// Not yet made
bool gdcm_read_JPEG2000_file (std::ifstream *fp, void *image_buffer);

// For JPEG-LS, body in file gdcmJpegLS.cxx
// Not yet made
bool gdcm_read_JPEGLS_file (std::ifstream *fp, void *image_buffer);

//-------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Default constructor.
 */
JPEGFragment::JPEGFragment()
{
   Offset = 0;
   Length = 0;

   pImage = 0;

}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief Decompress 8bits JPEG Fragment
 * @param fp ifstream to write to
 * @param buffer     output (data decompress)
 * @param nBits      8/12 or 16 bits jpeg
 * @param statesuspension state suspension
 */
void JPEGFragment::DecompressJPEGFramesFromFile(std::ifstream *fp,
                                                uint8_t *buffer, int nBits, 
                                                int &statesuspension)
{
   // First thing need to reset file to proper position:
   fp->seekg( Offset, std::ios::beg);

   if ( nBits == 8 )
   {
      // JPEG Lossy : call to IJG 6b - 8 bits
      ReadJPEGFile8( fp, buffer, statesuspension);
   }
   else if ( nBits <= 12 )
   {
      // JPEG Lossy : call to IJG 6b - 12 bits
      ReadJPEGFile12 ( fp, buffer, statesuspension);
   }
   else if ( nBits <= 16 )
   {
      // JPEG Lossy : call to IJG 6b - 16 bits
      ReadJPEGFile16 ( fp, buffer, statesuspension);
      //gdcmAssertMacro( IsJPEGLossless );
   }
   else
   {
      // FIXME : only the bits number is checked,
      //         NOT the compression method

      // other JPEG lossy not supported
      gdcmErrorMacro( "Unknown jpeg lossy compression ");
   }
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief        Print self.
 * @param os     Stream to print to.
 * @param indent Indentation string to be prepended during printing.
 */
void JPEGFragment::Print( std::ostream &os, std::string const &indent )
{
   os << indent
      << "JPEG fragment: offset : " <<  Offset
      << "   length : " <<  Length
      << std::endl;
}

//-----------------------------------------------------------------------------
} // end namespace gdcm

