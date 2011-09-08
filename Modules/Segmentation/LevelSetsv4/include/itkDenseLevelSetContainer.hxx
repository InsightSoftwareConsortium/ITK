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

#ifndef __itkDenseLevelSetContainer_hxx
#define __itkDenseLevelSetContainer_hxx

#include "itkDenseLevelSetContainer.h"

namespace itk
{
template< class TIdentifier, class TLevelSet >
DenseLevelSetContainer< TIdentifier, TLevelSet >
::DenseLevelSetContainer()
{
}

template< class TIdentifier, class TLevelSet >
DenseLevelSetContainer< TIdentifier, TLevelSet >
::~DenseLevelSetContainer()
{
}

template< class TIdentifier, class TLevelSet >
void
DenseLevelSetContainer< TIdentifier, TLevelSet >
::CopyInformationAndAllocate( const Self * iOther, const bool& iAllocate )
{
  LevelSetContainerType internalContainer = iOther->GetContainer();
  LevelSetContainerConstIteratorType it = internalContainer.begin();

  LevelSetContainerType newContainer;

  while( it != internalContainer.end() )
    {
    if( iAllocate )
      {
      LevelSetPointer temp_ls = LevelSetType::New();

      LevelSetImagePointer image = LevelSetImageType::New();
      const LevelSetImageType * otherImage = (it->second)->GetImage();

      image->CopyInformation( otherImage );
      image->SetBufferedRegion( otherImage->GetBufferedRegion() );
      image->SetRequestedRegion( otherImage->GetRequestedRegion() );
      image->SetLargestPossibleRegion( otherImage->GetLargestPossibleRegion() );
      image->Allocate();
      image->FillBuffer( NumericTraits< OutputPixelType >::Zero );

      temp_ls->SetImage( image );
      newContainer[ it->first ] = temp_ls;
      }
    else
      {
      LevelSetPointer temp_ls;
      newContainer[ it->first ] = temp_ls;
      }
    ++it;
    }

  this->SetContainer( newContainer );
}
}
#endif // __itkDenseLevelSetContainer_h
