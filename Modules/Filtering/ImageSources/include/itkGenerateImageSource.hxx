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

#ifndef __itkGenerateImageSource_hxx
#define __itkGenerateImageSource_hxx

#include "itkGenerateImageSource.h"

namespace itk
{

template< typename TOutputImage >
GenerateImageSource< TOutputImage >
::GenerateImageSource()
  : m_Spacing( 1.0 ),
    m_Origin( 0.0 )
{
  this->m_Size.Fill( 64 ); // abitrary default size
  this->m_Direction.SetIdentity();
}


template< typename TOutputImage >
void
GenerateImageSource< TOutputImage >
::GenerateOutputInformation()
{
  OutputImageType *output = this->GetOutput(0);

  typename OutputImageType::IndexType index;
  index.Fill( 0 );

  typename OutputImageType::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize(this->m_Size);
  largestPossibleRegion.SetIndex(index);
  output->SetLargestPossibleRegion(largestPossibleRegion);

  output->SetSpacing(this->m_Spacing);
  output->SetOrigin(this->m_Origin);
  output->SetDirection(this->m_Direction);
}


template< typename TOutputImage >
void
GenerateImageSource< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Size: " << this->GetSize() << std::endl;
  os << indent << "Origin: " << this->GetOrigin() << std::endl;
  os << indent << "Spacing: " << this->GetSpacing() << std::endl;
  os << indent << "Direction: " << this->GetDirection() << std::endl;

}

} // end namespace itk

#endif // __itkGenerateImageSour_hxx
