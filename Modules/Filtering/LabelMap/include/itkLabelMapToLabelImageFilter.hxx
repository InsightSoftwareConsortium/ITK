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
#ifndef itkLabelMapToLabelImageFilter_hxx
#define itkLabelMapToLabelImageFilter_hxx

#include "itkLabelMapToLabelImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
LabelMapToLabelImageFilter< TInputImage, TOutputImage >
::LabelMapToLabelImageFilter()
{
  m_OutputImage = ITK_NULLPTR;
}


template< typename TInputImage, typename TOutputImage >
void
LabelMapToLabelImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  OutputImageType * output = this->GetOutput();
  const InputImageType *input = this->GetInput();

  output->FillBuffer( input->GetBackgroundValue() );
  Superclass::BeforeThreadedGenerateData();
  this->m_OutputImage = this->GetOutput();
}


template< typename TInputImage, typename TOutputImage >
void
LabelMapToLabelImageFilter< TInputImage, TOutputImage >
::ThreadedProcessLabelObject(LabelObjectType *labelObject)
{
  const typename LabelObjectType::LabelType & label = labelObject->GetLabel();
  typename LabelObjectType::ConstIndexIterator it( labelObject );

  while( !it.IsAtEnd() )
    {
    this->m_OutputImage->SetPixel( it.GetIndex(), label );
    ++it;
    }
}

} // end namespace itk

#endif
