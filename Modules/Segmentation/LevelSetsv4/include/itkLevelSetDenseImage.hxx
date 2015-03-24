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

#ifndef itkLevelSetDenseImage_hxx
#define itkLevelSetDenseImage_hxx

#include "itkLevelSetDenseImage.h"

namespace itk
{
// ----------------------------------------------------------------------------
template< typename TImage >
LevelSetDenseImage< TImage >
::LevelSetDenseImage()
{}

// ----------------------------------------------------------------------------
template< typename TImage >
LevelSetDenseImage< TImage >
::~LevelSetDenseImage()
{}

// ----------------------------------------------------------------------------
template< typename TImage >
void
LevelSetDenseImage< TImage >
::SetImage( ImageType* inputImage )
{
  this->m_Image = inputImage;
  typename ImageType::SpacingType spacing = m_Image->GetSpacing();

  for( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    this->m_NeighborhoodScales[dim] =
      NumericTraits< OutputRealType >::OneValue() / static_cast< OutputRealType >( spacing[dim ] );
    }
  this->Modified();
}

// ----------------------------------------------------------------------------
template< typename TImage >
typename LevelSetDenseImage< TImage >::OutputType
LevelSetDenseImage< TImage >::Evaluate( const InputType& inputIndex ) const
{
  InputType mapIndex = inputIndex - this->m_DomainOffset;
  return this->m_Image->GetPixel( mapIndex );
}

// ----------------------------------------------------------------------------
template< typename TImage >
void
LevelSetDenseImage< TImage >::Evaluate( const InputType& inputIndex, LevelSetDataType& data ) const
{
  Superclass::Evaluate( inputIndex, data );
}

// ----------------------------------------------------------------------------
template< typename TImage >
void
LevelSetDenseImage< TImage >
::Initialize()
{
  Superclass::Initialize();

  this->m_Image = ITK_NULLPTR;
}

// ----------------------------------------------------------------------------
template< typename TImage >
void
LevelSetDenseImage< TImage >
::CopyInformation(const DataObject *data)
{
  Superclass::CopyInformation( data );

  const Self *LevelSet = dynamic_cast< const Self * >( data );

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetDenseImage::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }
}

// ----------------------------------------------------------------------------
template< typename TImage >
void
LevelSetDenseImage< TImage >
::Graft( const DataObject* data )
{
  Superclass::Graft( data );
  const Self *LevelSet = dynamic_cast< const Self* >( data );

  if ( !LevelSet )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::LevelSetDenseImage::CopyInformation() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( Self * ).name() );
    }

  this->m_Image = LevelSet->m_Image;
  this->m_NeighborhoodScales = LevelSet->m_NeighborhoodScales;
}

// ----------------------------------------------------------------------------
template< typename TImage >
bool
LevelSetDenseImage< TImage >
::IsInsideDomain(const InputType &inputIndex) const
{
  const RegionType largestRegion = this->m_Image->GetLargestPossibleRegion();
  InputType mapIndex = inputIndex - this->m_DomainOffset;
  return largestRegion.IsInside( mapIndex );
}

}
#endif // itkLevelSetDenseImage_hxx
