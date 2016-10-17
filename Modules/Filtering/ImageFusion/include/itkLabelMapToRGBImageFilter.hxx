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
#ifndef itkLabelMapToRGBImageFilter_hxx
#define itkLabelMapToRGBImageFilter_hxx

#include "itkLabelMapToRGBImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk {

template <typename TInputImage, typename TOutputImage>
LabelMapToRGBImageFilter<TInputImage, TOutputImage>
::LabelMapToRGBImageFilter()
{
}


template<typename TInputImage, typename TOutputImage>
void
LabelMapToRGBImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  OutputImageType * output = this->GetOutput();
  const InputImageType * input = this->GetInput();

  FunctorType function( m_Functor );
  function.SetBackgroundValue( input->GetBackgroundValue() );
  output->FillBuffer( function( input->GetBackgroundValue() ) );

  Superclass::BeforeThreadedGenerateData();

}


template<typename TInputImage, typename TOutputImage>
void
LabelMapToRGBImageFilter<TInputImage, TOutputImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  const typename LabelObjectType::LabelType & label = labelObject->GetLabel();
  const InputImageType * input = this->GetInput();

  FunctorType function(m_Functor);
  function.SetBackgroundValue( input->GetBackgroundValue() );

  typename LabelObjectType::ConstIndexIterator it( labelObject );
  TOutputImage *outputImage = this->GetOutput();
  while( ! it.IsAtEnd() )
    {
    const IndexType idx = it.GetIndex();
    outputImage->SetPixel( idx, function( label ) );
    ++it;
    }
}

template<typename TInputImage, typename TOutputImage>
void
LabelMapToRGBImageFilter<TInputImage, TOutputImage>
::GenerateOutputInformation()
{
  // this methods is overloaded so that if the output image is a
  // VectorImage then the correct number of components are set.

  Superclass::GenerateOutputInformation();
  OutputImageType* output = this->GetOutput();

  if ( !output )
    {
    return;
    }
  if ( output->GetNumberOfComponentsPerPixel() != 3 )
    {
    output->SetNumberOfComponentsPerPixel( 3 );
    }
}


}// end namespace itk
#endif
