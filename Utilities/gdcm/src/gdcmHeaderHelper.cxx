/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmHeaderHelper.cxx
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

#include "gdcmHeaderHelper.h"
#include "gdcmDirList.h"
#include "gdcmDebug.h"

#include <math.h>
#include <algorithm>
#include <vector>

namespace gdcm 
{

typedef std::vector<Header* > GdcmHeaderVector;
//-----------------------------------------------------------------------------
// Constructor / Destructor
SerieHeader::SerieHeader()
{
   CoherentGdcmFileList.clear();
   // Later will contains: 0020 000e UI REL Series Instance UID
   CurrentSerieUID = "";
}

SerieHeader::~SerieHeader()
{
   /// \todo
   for ( GdcmHeaderList::const_iterator it = CoherentGdcmFileList.begin();
         it != CoherentGdcmFileList.end(); ++it)
   {
      delete *it;
   }
   CoherentGdcmFileList.clear();
}

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
// Public
/**
 * \brief add a File to the list based on file name
 * @param   filename Name of the file to deal with
 */
void SerieHeader::AddFileName(std::string const & filename)
{
   //directly use string and not const char*:
   Header *header = new Header( filename ); 
   if( header->IsReadable() )
   {
      // 0020 000e UI REL Series Instance UID
      std::string uid =  header->GetEntryByNumber (0x0020, 0x000e);
      if( CurrentSerieUID == "" )
      {
         // Set the current one
         CurrentSerieUID = uid;
      }
      if( CurrentSerieUID == uid )
      {
         // Current Serie UID and DICOM header seems to match add the file:
         CoherentGdcmFileList.push_back( header );
      }
      else
      {
         dbg.Verbose(0, "Wrong Serie Instance UID should be:", CurrentSerieUID.c_str());
      }
   }
   else
   {
      dbg.Verbose(0, "Could not read file: ", filename.c_str() );
      delete header;
   }
}

/**
 * \brief add a File to the list
 * @param   file Header to add
 */
void SerieHeader::AddGdcmFile(Header *file)
{
   if( file->IsReadable() )
   {
      CoherentGdcmFileList.push_back( file );
   }
   else
   {
      dbg.Verbose(0, "Could not add file: ", file->GetFileName().c_str() );
   }
}

/**
 * \brief Sets the Directory
 * @param   dir Name of the directory to deal with
 */
void SerieHeader::SetDirectory(std::string const & dir)
{
   DirList filenames_list(dir);  //OS specific
  
   for( DirList::const_iterator it = filenames_list.begin(); 
        it != filenames_list.end(); ++it)
   {
      AddFileName( *it );
   }
}

/**
 * \brief Sorts the File List
 * \warning This could be implemented in a 'Strategy Pattern' approach
 *          But as I don't know how to do it, I leave it this way
 *          BTW, this is also a Strategy, I don't know this is the best approach :)
 */
void SerieHeader::OrderGdcmFileList()
{
   if( ImagePositionPatientOrdering() ) 
   {
      return ;
   }
   else if( ImageNumberOrdering() )
   {
      return ;
   }
   else  
   {
      FileNameOrdering();
   }
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private
/**
 * \ingroup Header
 * \brief sorts the images, according to their Patient Position
 *  We may order, considering :
 *   -# Image Number
 *   -# Image Position Patient
 *   -# More to come :)
 * @return false only if the header is bugged !
 */
bool SerieHeader::ImagePositionPatientOrdering()
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
   for ( GdcmHeaderList::const_iterator 
         it = CoherentGdcmFileList.begin();
         it != CoherentGdcmFileList.end(); ++it )
   {
      if( first ) 
      {
         (*it)->GetImageOrientationPatient( cosines );
      
         //You only have to do this once for all slices in the volume. Next, 
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
    
         if( dist == 0 )
         {
            return false;
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

         if( dist == 0 )
         {
            return false;
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
   GdcmHeaderVector CoherentGdcmFileVector(n);
   // CoherentGdcmFileVector.reserve( n );
   CoherentGdcmFileVector.resize( n );
   // assert( CoherentGdcmFileVector.capacity() >= n );

   float step = (max - min)/(n - 1);
   int pos;
   n = 0;
    
   //VC++ don't understand what scope is !! it -> it2
   for (GdcmHeaderList::const_iterator it2  = CoherentGdcmFileList.begin();
        it2 != CoherentGdcmFileList.end(); ++it2, ++n)
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
         return false;
   }

   CoherentGdcmFileList.clear();  //this doesn't delete list's element, node only
  
   //VC++ don't understand what scope is !! it -> it3
   for (GdcmHeaderVector::const_iterator it3  = CoherentGdcmFileVector.begin();
        it3 != CoherentGdcmFileVector.end(); ++it3)
   {
      CoherentGdcmFileList.push_back( *it3 );
   }

   distlist.clear();
   CoherentGdcmFileVector.clear();

   return true;
}

/**
 * \ingroup Header
 * \brief sorts the images, according to their Image Number
 * @return false only if the header is bugged !
 */

bool SerieHeader::ImageNumberOrdering() 
{
   int min, max, pos;
   int n = 0;//CoherentGdcmFileList.size() is a O(N) operation
   unsigned char *partition;
  
   GdcmHeaderList::const_iterator it = CoherentGdcmFileList.begin();
   min = max = (*it)->GetImageNumber();

   for (; it != CoherentGdcmFileList.end(); ++it, ++n)
   {
      pos = (*it)->GetImageNumber();

      //else
      min = (min < pos) ? min : pos;
      max = (max > pos) ? max : pos;
   }

   // Find out if sorting worked:
   if( min == max || max == 0 || max > (n+min)) return false;

   //bzeros(partition, n); //This function is deprecated, better use memset.
   partition = new unsigned char[n];
   memset(partition, 0, n);

   GdcmHeaderVector CoherentGdcmFileVector(n);

   //VC++ don't understand what scope is !! it -> it2
   for (GdcmHeaderList::const_iterator it2 = CoherentGdcmFileList.begin();
        it2 != CoherentGdcmFileList.end(); ++it2)
   {
      pos = (*it2)->GetImageNumber();
      CoherentGdcmFileVector[pos - min] = *it2;
      partition[pos - min]++;
   }
  
   unsigned char mult = 1;
   for( int i=0; i<n ; i++ )
   {
      mult *= partition[i];
   }

   //VC++ don't understand what scope is !! it -> it3
   CoherentGdcmFileList.clear();  //this doesn't delete list's element, node only
   for ( GdcmHeaderVector::const_iterator it3 = CoherentGdcmFileVector.begin();
         it3 != CoherentGdcmFileVector.end(); ++it3 )
   {
      CoherentGdcmFileList.push_back( *it3 );
   }
   CoherentGdcmFileVector.clear();
  
   delete[] partition;

   return (mult != 0);
}


/**
 * \ingroup Header
 * \brief sorts the images, according to their File Name
 * @return false only if the header is bugged !
 */
bool SerieHeader::FileNameOrdering()
{
   //using the sort
   //sort(CoherentGdcmFileList.begin(), CoherentGdcmFileList.end());
   return true;
}

} // end namespace gdcm
//-----------------------------------------------------------------------------
