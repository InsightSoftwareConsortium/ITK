/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirStudy.h
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

#ifndef GDCMDICOMDIRSTUDY_H
#define GDCMDICOMDIRSTUDY_H

#include "gdcmDicomDirObject.h"
#include "gdcmDicomDirSerie.h"
namespace gdcm 
{

//-----------------------------------------------------------------------------
typedef std::list<DicomDirSerie *> ListDicomDirSerie;

//-----------------------------------------------------------------------------
class GDCM_EXPORT DicomDirStudy : public DicomDirObject
{
public:
   DicomDirStudy(SQItem *s, TagDocEntryHT *ptagHT); 
   DicomDirStudy(TagDocEntryHT *ptagHT); 

   ~DicomDirStudy();

   void Print(std::ostream &os = std::cout);
   void Write(std::ofstream *fp, FileType t);

   /**
    * \ingroup DicomDirStudy
    * \brief   returns the SERIE chained List for this STUDY.
    */
   ListDicomDirSerie const &GetDicomDirSeries() const { return Series; };

   /**
    * \ingroup DicomDirStudy
    * \brief   adds the passed SERIE to the SERIE chained List for this STUDY.
    */ 
   void AddDicomDirSerie(DicomDirSerie *obj) { Series.push_back(obj); };

   /**
    * \ingroup DicomDirStudy
    * \brief   TODO
    */ 
   DicomDirSerie* NewSerie();
    
private:
/**
* \brief chained list of DicomDirSeries (to be exploited recursively)
*/ 
   ListDicomDirSerie Series;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
