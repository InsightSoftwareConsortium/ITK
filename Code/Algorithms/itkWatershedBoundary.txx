/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedBoundary.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkWatershedBoundary_txx
#define __itkWatershedBoundary_txx
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
{
  Superclass::PrintSelf(os,indent);
}

}// end namespace watershed
}// end namespace itk




#endif
