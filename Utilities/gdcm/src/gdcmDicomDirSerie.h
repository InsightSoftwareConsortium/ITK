/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirSerie.h
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

#ifndef GDCMDICOMDIRSERIE_H
#define GDCMDICOMDIRSERIE_H

#include "gdcmDicomDirObject.h"
#include "gdcmDicomDirImage.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
typedef std::list<DicomDirImage *> ListDicomDirImage;

//-----------------------------------------------------------------------------
class GDCM_EXPORT DicomDirSerie : public DicomDirObject 
{
public:
   DicomDirSerie( SQItem* s, TagDocEntryHT* ptagHT ); 
   DicomDirSerie( TagDocEntryHT* ptagHT ); 

   ~DicomDirSerie();

   void Print( std::ostream& os = std::cout );
   void Write( std::ofstream* fp, FileType t );

   /**
    * \ingroup DicomDirSerie
    * \brief   returns the IMAGE chained List for this SERIE.
    */
   ListDicomDirImage const & GetDicomDirImages() const { return Images; };

   /**
    * \ingroup DicomDirSerie
    * \brief   adds the passed IMAGE to the IMAGE chained List for this SERIE.
    */       
   void AddDicomDirImage(DicomDirImage *obj) { Images.push_back(obj); };

/**
 * \ingroup DicomDirSerie
 * \brief   TODO
 */ 
   DicomDirImage* NewImage();
    
private:
/**
* \brief chained list of DicomDirImages
*/ 
   ListDicomDirImage Images;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
