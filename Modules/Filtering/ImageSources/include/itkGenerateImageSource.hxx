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

#ifndef itkGenerateImageSource_hxx
#define itkGenerateImageSource_hxx

#include "itkGenerateImageSource.h"

namespace itk
{
template< typename TOutputImage >
GenerateImageSource< TOutputImage >
::GenerateImageSource()
  : m_Spacing( 1.0 ),
    m_Origin( 0.0 ),
    m_UseReferenceImage( false )
{
  this->m_Size.Fill( 64 ); // arbitrary default size
  this->m_Direction.SetIdentity();
  this->m_StartIndex.Fill( 0 );

  // Pipeline input configuration

  // implicit: No Input Required
  // "ReferenceImage" optional
  Self::AddOptionalInputName("ReferenceImage",1);
}

template< typename TOutputImage >
void
GenerateImageSource< TOutputImage >
::GenerateOutputInformation()
{
  // No need to call Superclass::GenerateOutputInformation. No input.
  for (unsigned int n = 0; n < this->GetNumberOfOutputs(); ++n)
    {
    OutputImageType *outputPtr = this->GetOutput(n);

    if ( !outputPtr )
      {
      continue;
      }

    const ReferenceImageBaseType *referenceImage = this->GetReferenceImage();

    // Set size, spacing, origin
    if ( m_UseReferenceImage && referenceImage )
      {
      outputPtr->SetLargestPossibleRegion(
        referenceImage->GetLargestPossibleRegion() );

      outputPtr->SetSpacing( referenceImage->GetSpacing() );
      outputPtr->SetOrigin( referenceImage->GetOrigin() );
      outputPtr->SetDirection( referenceImage->GetDirection() );
      }
    else
      {
      typename TOutputImage::RegionType outputLargestPossibleRegion;
      outputLargestPossibleRegion.SetSize(m_Size);
      outputLargestPossibleRegion.SetIndex(m_StartIndex);
      outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);

      outputPtr->SetSpacing(m_Spacing);
      outputPtr->SetOrigin(m_Origin);
      outputPtr->SetDirection(m_Direction);
      }
    }
}

template< typename TOutputImage >
void
GenerateImageSource< TOutputImage >
::SetOutputParametersFromImage(const ReferenceImageBaseType *image)
{
  this->SetOrigin( image->GetOrigin() );
  this->SetSpacing( image->GetSpacing() );
  this->SetDirection( image->GetDirection() );
  this->SetStartIndex( image->GetLargestPossibleRegion().GetIndex() );
  this->SetSize( image->GetLargestPossibleRegion().GetSize() );
}

template< typename TOutputImage >
void
GenerateImageSource< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Size: "
     << static_cast< typename NumericTraits< SizeType >::PrintType >( m_Size ) << std::endl;
  os << indent << "Spacing: "
     << static_cast< typename NumericTraits< SpacingType >::PrintType >( m_Spacing ) << std::endl;
  os << indent << "Origin: "
     << static_cast< typename NumericTraits< PointType >::PrintType >( m_Origin ) << std::endl;
  os << indent << "Direction: "
     << static_cast< typename NumericTraits< DirectionType >::PrintType >( m_Direction ) << std::endl;
  os << indent << "UseReferenceImage: " << this->GetUseReferenceImage() << std::endl;
}
} // end namespace itk

#endif // itkGenerateImageSour_hxx
