/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJPEGFragmentsInfo.h
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


#ifndef GDCMJPEGFRAGMENTSINFO_H
#define GDCMJPEGFRAGMENTSINFO_H

#include "gdcmJPEGFragment.h"

#include <list>
#include <iostream>

namespace gdcm 
{
/**
 * \brief Utility class for gathering the informations of the collection
 *        of JPEG fragment[s] (see \ref JPEGFragment)  when handling
 *        "Encapsulated JPEG Compressed Images". 
 *        The informations on each frame are obtained during the pixel parsing
 *        of a gdcm::File (refer to \ref File::ComputeJPEGFragmentInfo() ).
 *        They shall be used when (if necessary) decoding the fragments.
 *
 *        This class is simply a stl list<> of \ref JPEGFragment.
 */
class GDCM_EXPORT JPEGFragmentsInfo
{
public:
   JPEGFragmentsInfo();
   ~JPEGFragmentsInfo();
   void Print( std::ostream &os = std::cout, std::string const &indent = "" );

   void DecompressFromFile(std::ifstream *fp, uint8_t *buffer, int nBits,
                           int numBytes, int length);

   void AddFragment(JPEGFragment *fragment);
   JPEGFragment *GetFirstFragment();
   JPEGFragment *GetNextFragment();
   unsigned int GetFragmentCount();

private:
   typedef std::list<JPEGFragment *> JPEGFragmentsList;

    //Some mathieu hack:
   int StateSuspension;
   void *SampBuffer;
   char *pimage;
   JPEGFragmentsList Fragments;
   JPEGFragmentsList::iterator ItFragments;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif

