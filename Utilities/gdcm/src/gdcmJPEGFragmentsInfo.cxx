/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJPEGFragmentsInfo.cxx
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

#include "gdcmJPEGFragmentsInfo.h"
#include "gdcmDebug.h"

#include <fstream>

namespace gdcm 
{
//-------------------------------------------------------------------------
// Constructor / Destructor
JPEGFragmentsInfo::JPEGFragmentsInfo()
{
   StateSuspension = 0;
}

/**
 * \brief Default destructor
 */
JPEGFragmentsInfo::~JPEGFragmentsInfo()
{
   for(JPEGFragmentsList::iterator it  = Fragments.begin();
                                   it != Fragments.end();
                                 ++it )
   {
      delete *it;
   }
   Fragments.clear();
}

//-----------------------------------------------------------------------------
// Public
void JPEGFragmentsInfo::DecompressFromFile(std::ifstream *fp, uint8_t *buffer, int nBits, int , int )
{
   // Pointer to the Raw image
   uint8_t *localRaw = buffer;

  // Loop on the fragment[s]
   JPEGFragmentsList::const_iterator it;
   for( it  = Fragments.begin();
        it != Fragments.end();
        ++it )
   {
     (*it)->DecompressJPEGFramesFromFile(fp, localRaw, nBits, StateSuspension);
     // update pointer to image after some scanlines read:
     localRaw = (*it)->GetImage();
   }
}

void JPEGFragmentsInfo::AddFragment(JPEGFragment *fragment)
{
   Fragments.push_back(fragment);
}

JPEGFragment *JPEGFragmentsInfo::GetFirstFragment()
{
   ItFragments = Fragments.begin();
   if (ItFragments != Fragments.end())
      return  *ItFragments;
   return NULL;
}

JPEGFragment *JPEGFragmentsInfo::GetNextFragment()
{
   gdcmAssertMacro (ItFragments != Fragments.end());

   ++ItFragments;
   if (ItFragments != Fragments.end())
      return  *ItFragments;
   return NULL;
}

unsigned int JPEGFragmentsInfo::GetFragmentCount()
{
   return Fragments.size();
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
void JPEGFragmentsInfo::Print( std::ostream &os, std::string const &indent )
{
   os << std::endl;
   os << indent
      << "----------------- JPEG fragments --------------------------------"
      << std::endl << std::endl;
   os << indent
      << "Total number of fragments : " << Fragments.size()
      << std::endl;
   int fragmentNumber = 0;
   for(JPEGFragmentsList::iterator it  = Fragments.begin();
                                   it != Fragments.end();
                                 ++it)
   {
      os << indent
         << "   fragment number :" << fragmentNumber++;
      (*it)->Print( os, indent + "   ");
   }
   os << std::endl;
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
