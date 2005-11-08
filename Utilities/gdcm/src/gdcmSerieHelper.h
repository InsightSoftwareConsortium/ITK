/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmSerieHelper.h
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

#ifndef GDCMSERIEHELPER_H
#define GDCMSERIEHELPER_H

#include "gdcmCommon.h" 

#include <vector>
#include <iostream>
#include <map>

namespace gdcm 
{
class File;
typedef std::vector<File* > GdcmFileList;

//-----------------------------------------------------------------------------
/**
 * \brief  
 * - This class should be used for a stack of 2D dicom images.
 *   It allows to explore (recursively or not) a directory and 
 *   makes a set of 'Coherent Files' list (coherent : same Serie UID)
 *   It allows to sort any of the Coherent File list on the image postion
 */
class GDCM_EXPORT SerieHelper 
{
public:
   typedef std::map<std::string, GdcmFileList *> CoherentFileListmap;
   typedef std::vector<File* > GdcmFileVector;

   SerieHelper();
   ~SerieHelper();
   void Print(std::ostream &os = std::cout, std::string const &indent = "" );

   /// \todo should return bool or throw error ?
   void AddFileName(std::string const &filename);
   void SetDirectory(std::string const &dir, bool recursive=false);
   void OrderGdcmFileList(GdcmFileList *coherentGdcmFileList);
   
   /// \brief Gets the FIRST *coherent* File List.
   ///        Deprecated; kept not to break the API
   /// \note Caller must call OrderGdcmFileList first
   /// @return the (first) *coherent* File List
   const GdcmFileList &GetGdcmFileList() { return
                       *CoherentGdcmFileListHT.begin()->second; }
  
   GdcmFileList *GetFirstCoherentFileList();
   GdcmFileList *GetNextCoherentFileList();
   GdcmFileList *GetCoherentFileList(std::string serieUID);
   /// All the following allow user to restrict DICOM file to be part
   /// of a particular serie
   void AddRestriction(TagKey const &key, std::string const &value);

   /// \brief Use additional series information such as ProtocolName
   ///        and SeriesName to identify when a single SeriesUID contains
   ///        multiple 3D volumes - as can occur with perfusion and DTI imaging
   void SetUseSeriesDetails( bool useSeriesDetails )
     {
     m_UseSeriesDetails = useSeriesDetails;
     }
   bool GetUseSeriesDetails( void )
     {
     return m_UseSeriesDetails;
     }

   /// \brief Create a string that uniquely identifies a series.   By default
   //         uses the SeriesUID.   If UseSeriesDetails(true) has been called,
   //         then additional identifying information is used.
   std::string CreateUniqueSeriesIdentifier( File * inFile );

private:
   bool ImagePositionPatientOrdering(GdcmFileList *coherentGdcmFileList);
   bool ImageNumberOrdering(GdcmFileList *coherentGdcmFileList);
   bool FileNameOrdering(GdcmFileList *coherentGdcmFileList);
   
   static bool ImageNumberLessThan(File *file1, File *file2);
   static bool FileNameLessThan(File *file1, File *file2);
   CoherentFileListmap CoherentGdcmFileListHT;
   CoherentFileListmap::iterator ItListHt;

   typedef struct {
     uint16_t group;
     uint16_t elem;
     std::string value;
   } Rule;
   typedef std::vector<Rule> SerieRestrictions;
   SerieRestrictions Restrictions;
   bool m_UseSeriesDetails;
};

} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
