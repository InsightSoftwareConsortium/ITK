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
#ifndef itkImageFunction_hxx
#define itkImageFunction_hxx

#include "itkImageFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutput, typename TCoordRep >
ImageFunction< TInputImage, TOutput, TCoordRep >
::ImageFunction()
{
  m_Image = ITK_NULLPTR;
  m_StartIndex.Fill(0);
  m_EndIndex.Fill(0);
  m_StartContinuousIndex.Fill(0.0f);
  m_EndContinuousIndex.Fill(0.0f);
}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage, typename TOutput, typename TCoordRep >
void
ImageFunction< TInputImage, TOutput, TCoordRep >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "InputImage: " << m_Image.GetPointer() << std::endl;
  os << indent << "StartIndex: " << m_StartIndex << std::endl;
  os << indent << "EndIndex: " << m_EndIndex << std::endl;
  os << indent << "StartContinuousIndex: " << m_StartContinuousIndex << std::endl;
  os << indent << "EndContinuousIndex: " << m_EndContinuousIndex << std::endl;
}

/**
 * Initialize by setting the input image
 */
template< typename TInputImage, typename TOutput, typename TCoordRep >
void
ImageFunction< TInputImage, TOutput, TCoordRep >
::SetInputImage(
  const InputImageType *ptr)
{
  // set the input image
  m_Image = ptr;

  if ( ptr )
    {
    typename InputImageType::SizeType size = ptr->GetBufferedRegion().GetSize();
    m_StartIndex = ptr->GetBufferedRegion().GetIndex();

    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      m_EndIndex[j] = m_StartIndex[j] + static_cast< IndexValueType >( size[j] ) - 1;
      m_StartContinuousIndex[j] = static_cast< CoordRepType >( m_StartIndex[j] - 0.5 );
      m_EndContinuousIndex[j]   = static_cast< CoordRepType >( m_EndIndex[j] + 0.5 );
      }
    }
}
} // end namespace itk

#endif
