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
#ifndef __itkLabelMapToAttributeImageFilter_hxx
#define __itkLabelMapToAttributeImageFilter_hxx

#include "itkLabelMapToAttributeImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk {

template <class TInputImage, class TOutputImage, class TAttributeAccessor>
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::LabelMapToAttributeImageFilter()
{
  m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
}

template <class TInputImage, class TOutputImage, class TAttributeAccessor>
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


template <class TInputImage, class TOutputImage, class TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage, class TAttributeAccessor>
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

  typename InputImageType::LabelObjectContainerType::const_iterator it;
  const typename InputImageType::LabelObjectContainerType & labelObjectContainer = input->GetLabelObjectContainer();
  for( it = labelObjectContainer.begin(); it != labelObjectContainer.end(); it++ )
    {
    typedef typename InputImageType::LabelObjectType  LabelObjectType;
    const LabelObjectType * labeObject = it->second;
    const AttributeValueType & attribute = accessor( labeObject );

    typename LabelObjectType::LineContainerType::const_iterator lit;
    const typename LabelObjectType::LineContainerType & lineContainer = labeObject->GetLineContainer();

    typedef typename LabelObjectType::LengthType LengthType;

    for( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
      {
      IndexType idx = lit->GetIndex();
      LengthType length = lit->GetLength();
      for( LengthType i=0; i<length; i++)
        {
        output->SetPixel( idx, static_cast<OutputImagePixelType>( attribute ) );
        idx[0]++;
        progress.CompletedPixel();
        }
      }
    }
}


template<class TInputImage, class TOutputImage, class TAttributeAccessor>
void
LabelMapToAttributeImageFilter<TInputImage, TOutputImage, TAttributeAccessor>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BackgroundValue: "  << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue) << std::endl;
}

}// end namespace itk
#endif
