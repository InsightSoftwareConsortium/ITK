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
#ifndef itkLabelMapToAttributeImageFilter_hxx
#define itkLabelMapToAttributeImageFilter_hxx

#include "itkLabelMapToAttributeImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk {

template <typename TInputImage, typename TOutputImage, typename TAttributeAccessor>
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::LabelMapToAttributeImageFilter()
{
  m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
}

template <typename TInputImage, typename TOutputImage, typename TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if ( !input )
    { return; }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <typename TInputImage, typename TOutputImage, typename TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<typename TInputImage, typename TOutputImage, typename TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  OutputImageType * output = this->GetOutput();
  const InputImageType * input = this->GetInput();
  ProgressReporter progress( this, 0, output->GetRequestedRegion().GetNumberOfPixels() );

  AttributeAccessorType accessor;

  output->FillBuffer( m_BackgroundValue );

  for( typename InputImageType::ConstIterator loit( input );
       ! loit.IsAtEnd();
       ++loit )
    {
    typedef typename InputImageType::LabelObjectType  LabelObjectType;
    const LabelObjectType * labelObject = loit.GetLabelObject();
    const AttributeValueType & attribute = accessor( labelObject );

    typename LabelObjectType::ConstIndexIterator it( labelObject );
    while( ! it.IsAtEnd() )
      {
      const IndexType idx = it.GetIndex();
      output->SetPixel( idx, static_cast<OutputImagePixelType>( attribute ) );
      ++it;
      progress.CompletedPixel();
      }
    }
}


template<typename TInputImage, typename TOutputImage, typename TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BackgroundValue: "  << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue) << std::endl;
}

}// end namespace itk
#endif
