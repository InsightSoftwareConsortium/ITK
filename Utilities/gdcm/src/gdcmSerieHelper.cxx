/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmSerieHelper.cxx
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

#include "gdcmSerieHelper.h"
#include "gdcmDirList.h"
#include "gdcmFile.h"
#include "gdcmDebug.h"
#include "gdcmUtil.h"

#include <math.h>
#include <vector>
#include <algorithm>
#include <map>

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor from a given SerieHelper
 */
SerieHelper::SerieHelper()
{
  m_UseSeriesDetails = false;
   // For all the File lists of the gdcm::Serie
   GdcmFileList *l = GetFirstCoherentFileList();
   while (l)
   { 
      // For all the files of a File list
      for (GdcmFileList::iterator it  = l->begin();
                                  it != l->end(); 
                                ++it)
      {
         delete *it;
      }
      l->clear();
      delete l;;
      l = GetNextCoherentFileList();
   }
}

/**
 * \brief   Canonical destructor.
 */
SerieHelper::~SerieHelper()
{
   // For all the Coherent File lists of the gdcm::Serie
   GdcmFileList *l = GetFirstCoherentFileList();
   while (l)
   { 
      // For all the files of a Coherent File list
      for (GdcmFileList::iterator it  = l->begin();
                                  it != l->end(); 
                                ++it)
      {
         delete *it;
      }
      l->clear();
      delete l;
      l = GetNextCoherentFileList();
   }
}

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------

// Public
/**
 * \brief add a gdcm::File to the list corresponding to its Serie UID
 * @param   filename Name of the file to deal with
 */
void SerieHelper::AddFileName(std::string const &filename)
{
   //directly use string and not const char*:
   File *header = new File( filename ); 
   if( header->IsReadable() )
   {
      int allrules = 1;
      // First step : the user defined a set of rules for the DICOM file
      // he is looking for.
      // Make sure the file corresponds to his set of rules:

      std::string s;
      for(SerieRestrictions::iterator it2 = Restrictions.begin();
          it2 != Restrictions.end();
          ++it2)
      {
         const Rule &r = *it2;
         s = header->GetEntryValue( r.group, r.elem );
         if ( !Util::DicomStringEqual(s, r.value.c_str() ))
         {
           // Argh ! This rule is unmatched; let's just quit

           allrules = 0;
           break;
         }
      }

      if ( allrules ) // all rules are respected:
      {
         std::string id = CreateUniqueSeriesIdentifier( header );
         // if id == GDCM_UNFOUND then consistently we should find GDCM_UNFOUND
         // no need here to do anything special

         if ( CoherentGdcmFileListHT.count(id) == 0 )
         {
            gdcmWarningMacro(" New Serie UID :[" << id << "]");
            // create a std::list in 'id' position
            CoherentGdcmFileListHT[id] = new GdcmFileList;
         }
         // Current Serie UID and DICOM header seems to match add the file:
         CoherentGdcmFileListHT[id]->push_back( header );
      }
      else
      {
         // at least one rule was unmatched we need to deallocate the file:
         delete header;
      }
   }
   else
   {
      gdcmWarningMacro("Could not read file: " << filename );
      delete header;
   }
}

/**
 * \brief add a rules for restricting a DICOM file to be in the serie we are
 * trying to find. For example you can select only the DICOM file from a
 * directory which would have a particular EchoTime==4.0.
 * This method is a user level, value is not required to be formatted as a DICOM
 * string
 * @param   group  Group number of the target tag.
 * @param   elem Element number of the target tag.
 * @param value value to be checked to exclude File
 * @param op  operator we want to use to check
 */
void SerieHelper::AddRestriction(TagKey const &key, 
                                 std::string const &value)
{
   Rule r;
   unsigned int group, elem;
   sscanf(key.c_str(), "%04x|%04x", &group, &elem);
   r.group = group;
   r.elem = elem;
   //std::cout << "Elem: " << r.group << "," << r.elem << std::endl;
   r.value = value;
   Restrictions.push_back( r ); 
}

/**
 * \brief Sets the root Directory
 * @param   dir Name of the directory to deal with
 * @param recursive whether we want explore recursively the Directory
 */
void SerieHelper::SetDirectory(std::string const &dir, bool recursive)
{
   DirList dirList(dir, recursive); // OS specific
  
   DirListType filenames_list = dirList.GetFilenames();
   for( DirListType::const_iterator it = filenames_list.begin(); 
        it != filenames_list.end(); ++it)
   {
      AddFileName( *it );
   }
}

/**
 * \brief Sorts the given File List
 * \warning This could be implemented in a 'Strategy Pattern' approach
 *          But as I don't know how to do it, I leave it this way
 *          BTW, this is also a Strategy, I don't know this is the best approach :)
 */
void SerieHelper::OrderGdcmFileList(GdcmFileList *CoherentGdcmFileList)
{
   if( ImagePositionPatientOrdering( CoherentGdcmFileList ) )
   {
      return ;
   }
   else if( ImageNumberOrdering(CoherentGdcmFileList ) )
   {
      return ;
   }
   else  
   {
      FileNameOrdering(CoherentGdcmFileList );
   }
}

/**
 * \brief   Get the first List while visiting the CoherentFileListHT
 * @return  The first GdcmFileList if found, otherwhise NULL
 */
GdcmFileList *SerieHelper::GetFirstCoherentFileList()
{
   ItListHt = CoherentGdcmFileListHT.begin();
   if( ItListHt != CoherentGdcmFileListHT.end() )
      return ItListHt->second;
   return NULL;
}

/**
 * \brief   Get the next List while visiting the CoherentFileListHT
 * \note : meaningfull only if GetFirstCoherentFileList already called
 * @return  The next GdcmFileList if found, otherwhise NULL
 */
GdcmFileList *SerieHelper::GetNextCoherentFileList()
{
   gdcmAssertMacro (ItListHt != CoherentGdcmFileListHT.end());
  
   ++ItListHt;
   if ( ItListHt != CoherentGdcmFileListHT.end() )
      return ItListHt->second;
   return NULL;
}

/**
 * \brief   Get the Coherent Files list according to its Serie UID
 * @param SerieUID SerieUID
 * \return  pointer to the Coherent Filseslist if found, otherwhise NULL
 */
GdcmFileList *SerieHelper::GetCoherentFileList(std::string SerieUID)
{
   if ( CoherentGdcmFileListHT.count(SerieUID) == 0 )
      return 0;     
   return CoherentGdcmFileListHT[SerieUID];
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private
/**
 * \brief sorts the images, according to their Patient Position
 *  We may order, considering :
 *   -# Image Position Patient
 *   -# Image Number
 *   -# More to come :-)
 * @param fileList Coherent File list (same Serie UID) to sort
 * @return false only if the header is bugged !
 */
bool SerieHelper::ImagePositionPatientOrdering( GdcmFileList *fileList )
//based on Jolinda's algorithm
{
   //iop is calculated based on the file file
   float cosines[6];
   double normal[3];
   double ipp[3];
   double dist;
   double min = 0, max = 0;
   bool first = true;

   std::multimap<double,File *> distmultimap;

   // Use a multimap to sort the distances from 0,0,0
   for ( GdcmFileList::const_iterator 
         it = fileList->begin();
         it != fileList->end(); ++it )
   {
      if( first ) 
      {
         (*it)->GetImageOrientationPatient( cosines );
      
         // You only have to do this once for all slices in the volume. Next, 
         // for each slice, calculate the distance along the slice normal 
         // using the IPP tag ("dist" is initialized to zero before reading 
         // the first slice) :
         normal[0] = cosines[1]*cosines[5] - cosines[2]*cosines[4];
         normal[1] = cosines[2]*cosines[3] - cosines[0]*cosines[5];
         normal[2] = cosines[0]*cosines[4] - cosines[1]*cosines[3];
  
         ipp[0] = (*it)->GetXOrigin();
         ipp[1] = (*it)->GetYOrigin();
         ipp[2] = (*it)->GetZOrigin();

         dist = 0;
         for ( int i = 0; i < 3; ++i )
         {
            dist += normal[i]*ipp[i];
         }
    
         distmultimap.insert(std::pair<const double,File *>(dist, *it));

         max = min = dist;
         first = false;
      }
      else 
      {
         ipp[0] = (*it)->GetXOrigin();
         ipp[1] = (*it)->GetYOrigin();
         ipp[2] = (*it)->GetZOrigin();

         dist = 0;
         for ( int i = 0; i < 3; ++i )
         {
            dist += normal[i]*ipp[i];
         }

         distmultimap.insert(std::pair<const double,File *>(dist, *it));

         min = (min < dist) ? min : dist;
         max = (max > dist) ? max : dist;
      }
   }
   
   // Find out if min/max are coherent
   if ( min == max )
   {
     gdcmWarningMacro("Looks like all images have the exact same image position"
                      << ". No PositionPatientOrdering sort performed" );
     return false;
   }

   // Check to see if image shares a common position
   bool ok = true;
   for (std::multimap<double, File *>::iterator it2 = distmultimap.begin();
        it2 != distmultimap.end();
        ++it2)
     {
     if (distmultimap.count((*it2).first) != 1)
       {
       gdcmErrorMacro("File: " 
                        << ((*it2).second->GetFileName())
                        << " Distance: "
                        << (*it2).first
                        << " position is not unique");
       
       ok = false;
       }
     }
   if (!ok)
     {
       return false;
     }

   fileList->clear();  // doesn't delete list elements, only node

   for (std::multimap<double, File *>::iterator it3 =
          distmultimap.begin();
        it3 != distmultimap.end();
        ++it3)
     {
       fileList->push_back( (*it3).second );
     }

   distmultimap.clear();

   return true;
}

bool SerieHelper::ImageNumberLessThan(File *file1, File *file2)
{
  return file1->GetImageNumber() < file2->GetImageNumber();
}

/**
 * \brief sorts the images, according to their Image Number
 * \note Works only on bona fide files  (i.e image number is a character string
 *                                      corresponding to an integer)
 *             within a bona fide serie (i.e image numbers are consecutive)
 * @param fileList Coherent File list (same Serie UID) to sort 
 * @return false if non nona fide stuff encountered
 */
bool SerieHelper::ImageNumberOrdering(GdcmFileList *fileList) 
{
   int min, max, pos;
   int n = fileList->size();

   GdcmFileList::const_iterator it = fileList->begin();
   min = max = (*it)->GetImageNumber();

   for (; it != fileList->end(); ++it, ++n)
   {
      pos = (*it)->GetImageNumber();
      min = (min < pos) ? min : pos;
      max = (max > pos) ? max : pos;
   }

   // Find out if image numbers are coherent (consecutive)
   if( min == max || max == 0 || max >= (n+min))
      return false;

   std::sort(fileList->begin(), fileList->end(), SerieHelper::ImageNumberLessThan );

   return true;
}

bool SerieHelper::FileNameLessThan(File *file1, File *file2)
{
   return file1->GetFileName() < file2->GetFileName();
}

/**
 * \brief sorts the images, according to their File Name
 * @param fileList Coherent File list (same Serie UID) to sort
 * @return false only if the header is bugged !
 */
bool SerieHelper::FileNameOrdering(GdcmFileList *fileList)
{
   std::sort(fileList->begin(), fileList->end(), SerieHelper::FileNameLessThan);
   return true;
}

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Canonical printer.
 */
void SerieHelper::Print(std::ostream &os, std::string const &indent)
{
   // For all the Coherent File lists of the gdcm::Serie
   CoherentFileListmap::iterator itl = CoherentGdcmFileListHT.begin();
   if ( itl == CoherentGdcmFileListHT.end() )
   {
      gdcmWarningMacro( "No Coherent File list found" );
      return;
   }
   while (itl != CoherentGdcmFileListHT.end())
   { 
      os << "Serie UID :[" << itl->first << "]" << std::endl;

      // For all the files of a Coherent File list
      for (GdcmFileList::iterator it =  (itl->second)->begin();
                                  it != (itl->second)->end(); 
                                ++it)
      {
         os << indent << " --- " << (*it)->GetFileName() << std::endl;
      }
      ++itl;
   }
}

std::string SerieHelper::CreateUniqueSeriesIdentifier( File * inFile )
{
  if( inFile->IsReadable() )
    {
    // 0020 000e UI REL Series Instance UID
    std::string uid =  inFile->GetEntryValue (0x0020, 0x000e);
    std::string id = uid.c_str();
    if(m_UseSeriesDetails)
      {
      // If the user requests, additional information can be appended
      // to the SeriesUID to further differentiate volumes in the DICOM
      // objects being processed.

      // 0020 0011 Series Number
      // A scout scan prior to a CT volume scan can share the same
      //   SeriesUID, but they will sometimes have a different Series Number
      std::string sNum = inFile->GetEntryValue(0x0020, 0x0011);
      if( sNum == gdcm::GDCM_UNFOUND )
        {
        sNum = "";
        }
      // 0018 0024 Sequence Name
      // For T1-map and phase-contrast MRA, the different flip angles and
      //   directions are only distinguished by the Sequence Name
      std::string sName = inFile->GetEntryValue(0x0018, 0x0024);
      if( sName == gdcm::GDCM_UNFOUND )
        {
        sName = "";
        }
      // 0018 0050 Slice Thickness
      // On some CT systems, scout scans and subsequence volume scans will
      //   have the same SeriesUID and Series Number - YET the slice 
      //   thickness will differ from the scout slice and the volume slices.
      std::string sThick = inFile->GetEntryValue (0x0018, 0x0050);
      if( sThick == gdcm::GDCM_UNFOUND )
        {
        sThick = "";
        }
      // 0028 0010 Rows
      // If the 2D images in a sequence don't have the same number of rows,
      // then it is difficult to reconstruct them into a 3D volume.
      std::string sRows = inFile->GetEntryValue (0x0028, 0x0010);
      if( sRows == gdcm::GDCM_UNFOUND )
        {
        sRows = "";
        }
      // 0028 0011 Columns
      // If the 2D images in a sequence don't have the same number of columns,
      // then it is difficult to reconstruct them into a 3D volume.
      std::string sColumns = inFile->GetEntryValue (0x0028, 0x0011);
      if( sColumns == gdcm::GDCM_UNFOUND )
        {
        sColumns = "";
        }

      // Concat the new info
      std::string num = sNum.c_str();
      num += sName.c_str();
      num += sThick.c_str();
      num += sRows.c_str();
      num += sColumns.c_str();

      // Append the new info to the SeriesUID
      id += ".";
      id += num.c_str();
      }

    // Eliminate non-alnum characters, including whitespace...
    //   that may have been introduced by concats.
    for(unsigned int i=0; i<id.size(); i++)
      {
      while(i<id.size() 
            && !( id[i] == '.'
                 || (id[i] >= 'a' && id[i] <= 'z')
                 || (id[i] >= '0' && id[i] <= '9')
                 || (id[i] >= 'A' && id[i] <= 'Z')))
        {
        id.erase(i, 1);
        }
      }
    return id;
    }
  else // Could not open inFile
    {
    gdcmWarningMacro("Could not parse series info.");
    std::string id = gdcm::GDCM_UNFOUND;
    return id;
    }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
