/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedBoundary.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedBoundary_txx
#define __itkWatershedBoundary_txx
#include "itkWatershedBoundary.h"

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageRegionIterator.h"

namespace itk
{
namespace watershed
{
template<class TScalarType, unsigned int TDimension >
Boundary<TScalarType, TDimension>
::Boundary()
{
  unsigned int i;
  FacePointer p;
  flat_hash_t f;
  std::pair<FacePointer, FacePointer> i_pair;
  std::pair<flat_hash_t, flat_hash_t> c_pair;
  std::pair<bool, bool> v_pair;

  // Initialize all the members of the lists, etc.
  for (i = 0; i < Dimension; ++i)
    {
    p = face_t::New();
    i_pair.first = p;
    c_pair.first = flat_hash_t();
    v_pair.first = false;

    p = face_t::New();
    i_pair.second = p;
    c_pair.second = flat_hash_t();
    v_pair.second = false;

    m_Faces.push_back(i_pair);
    m_FlatHashes.push_back(c_pair);
    m_Valid.push_back(v_pair);
    }
}
  
template<class TScalarType,  unsigned int TDimension >
void
Boundary<TScalarType, TDimension>
::UpdateOutputInformation()
{
  if (this->GetSource())
    {
    this->GetSource()->UpdateOutputInformation();
    }
  else
    {
      
    }
  
  // Now we should know what our largest possible region is. If our 
  // requested region was not set yet, (or has been set to something 
  // invalid - with no data in it ) then set it to the largest possible
  // region.
  if ( ! m_RequestedRegionInitialized)
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    m_RequestedRegionInitialized = true;
    }
  
  m_LastRequestedRegionWasOutsideOfTheBufferedRegion = 0;
}

template<class TScalarType,  unsigned int TDimension >
void 
Boundary<TScalarType, TDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{  Superclass::PrintSelf(os,indent); }

}// end namespace watershed
}// end namespace itk




#endif
