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

#include <math.h>
#include <vector>
#include <algorithm>

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor from a given SerieHelper
 */
SerieHelper::SerieHelper()
{
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
      // 0020 000e UI REL Series Instance UID
      std::string uid =  header->GetEntryValue (0x0020, 0x000e);
      // if uid == GDCM_UNFOUND then consistently we should find GDCM_UNFOUND
      // no need here to do anything special

      if ( CoherentGdcmFileListHT.count(uid) == 0 )
      {
         gdcmWarningMacro(" New Serie UID :[" << uid << "]");
         // create a std::list in 'uid' position
         CoherentGdcmFileListHT[uid] = new GdcmFileList;
      }
      // Current Serie UID and DICOM header seems to match add the file:
      CoherentGdcmFileListHT[uid]->push_back( header );
   }
   else
   {
      gdcmWarningMacro("Could not read file: " << filename );
      delete header;
   }
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
   float normal[3];
   float ipp[3];
   float dist;
   float min = 0, max = 0;
   bool first = true;
   int n=0;
   std::vector<float> distlist;

   //!\todo rewrite this for loop.
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
    
         distlist.push_back( dist );

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

         distlist.push_back( dist );

         min = (min < dist) ? min : dist;
         max = (max > dist) ? max : dist;
      }
      ++n;
   }

   // Then I order the slices according to the value "dist". Finally, once
   // I've read in all the slices, I calculate the z-spacing as the difference
   // between the "dist" values for the first two slices.
   GdcmFileVector CoherentGdcmFileVector(n);
   // CoherentGdcmFileVector.reserve( n );
   CoherentGdcmFileVector.resize( n );
   // gdcmAssertMacro( CoherentGdcmFileVector.capacity() >= n );

   float step = (max - min)/(n - 1);
   int pos;
   n = 0;
    
   //VC++ don't understand what scope is !! it -> it2
   for (GdcmFileList::const_iterator it2  = fileList->begin();
        it2 != fileList->end(); ++it2, ++n)
   {
      //2*n sort algo !!
      //Assumption: all files are present (no one missing)
      pos = (int)( fabs( (distlist[n]-min)/step) + .5 );

      // a Dicom 'Serie' may contain scout views
      // and images may have differents directions
      // -> More than one may have the same 'pos'
      // Sorting has then NO meaning !
      if (CoherentGdcmFileVector[pos]==NULL)
         CoherentGdcmFileVector[pos] = *it2;
      else
      {
         gdcmWarningMacro( "2 files same position");
         return false;
      }
   }

   fileList->clear();  // doesn't delete list elements, only node
  
   //VC++ don't understand what scope is !! it -> it3
   for (GdcmFileVector::const_iterator it3  = CoherentGdcmFileVector.begin();
        it3 != CoherentGdcmFileVector.end(); ++it3)
   {
      fileList->push_back( *it3 );
   }

   distlist.clear();
   CoherentGdcmFileVector.clear();

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
void SerieHelper::Print(std::ostream &os, std::string const & indent)
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

//-----------------------------------------------------------------------------
} // end namespace gdcm
