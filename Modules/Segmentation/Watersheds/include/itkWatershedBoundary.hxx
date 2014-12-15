/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkWatershedBoundary_hxx
#define itkWatershedBoundary_hxx
#include "itkWatershedBoundary.h"


#include "itkImageRegionIterator.h"

namespace itk
{
namespace watershed
{
template< typename TScalar, unsigned int TDimension >
Boundary< TScalar, TDimension >
::Boundary()
{
  unsigned int i;
  FacePointer  p;
  flat_hash_t  f;

  std::pair< FacePointer, FacePointer > i_pair;
  std::pair< flat_hash_t, flat_hash_t > c_pair;
  std::pair< bool, bool >               v_pair;

  // Initialize all the members of the lists, etc.
  for ( i = 0; i < Dimension; ++i )
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

template< typename TScalar,  unsigned int TDimension >
void
Boundary< TScalar, TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{  Superclass::PrintSelf(os, indent); }
} // end namespace watershed
} // end namespace itk
#endif
