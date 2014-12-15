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
#ifndef itkImageToVectorImageFilter_hxx
#define itkImageToVectorImageFilter_hxx

#include "itkImageToVectorImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk
{
//----------------------------------------------------------------------------
template< typename TInputImage >
ImageToVectorImageFilter< TInputImage >
::ImageToVectorImageFilter()
{
  // At least 1 inputs is necessary for a vector image.
  this->SetNumberOfRequiredInputs(1);
}

//----------------------------------------------------------------------------
template< typename TInputImage >
void
ImageToVectorImageFilter< TInputImage >
::GenerateOutputInformation(void)
{
  // Override the method in itkImageSource, so we can set the vector length of
  // the output itk::VectorImage

  this->Superclass::GenerateOutputInformation();

  OutputImageType *output = this->GetOutput();
  output->SetVectorLength( this->GetNumberOfInputs() );
}

//----------------------------------------------------------------------------
template< typename TInputImage >
void
ImageToVectorImageFilter< TInputImage >
::BeforeThreadedGenerateData()
{
  // Check to verify all inputs are specified and have the same metadata,
  // spacing etc...
  const unsigned int numberOfInputs = this->GetNumberOfInputs();
  RegionType         region;

  for ( unsigned int i = 0; i < numberOfInputs; i++ )
    {
    const InputImageType * input = this->Superclass::GetInput(i);
    if ( !input )
      {
      itkExceptionMacro(<< "Input " << i << " not set!");
      }
    if ( i == 0 )
      {
      region = input->GetLargestPossibleRegion();
      }
    else if ( input->GetLargestPossibleRegion() != region )
      {
      itkExceptionMacro(<< "All Inputs must have the same dimensions.");
      }
    }
}

//----------------------------------------------------------------------------
template< typename TInputImage >
void
ImageToVectorImageFilter< TInputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType)
{
  typename OutputImageType::Pointer outputImage =
    static_cast< OutputImageType * >( this->ProcessObject::GetOutput(0) );

  ImageRegionIterator< OutputImageType > oit(outputImage, outputRegionForThread);
  oit.GoToBegin();

  typedef ImageRegionConstIterator< InputImageType > InputIteratorType;
  std::vector< InputIteratorType * > inputItContainer;

  for ( unsigned int i = 0; i < this->GetNumberOfInputs(); i++ )
    {
    typename InputImageType::Pointer inputImagePointer =
      static_cast< InputImageType * >( this->ProcessObject::GetInput(i) );

    InputIteratorType *iit = new InputIteratorType(
      inputImagePointer, outputRegionForThread);
    iit->GoToBegin();
    inputItContainer.push_back(iit);
    }

  typename OutputImageType::PixelType pix( this->GetNumberOfInputs() );
  while ( !oit.IsAtEnd() )
    {
    for ( unsigned int i = 0; i < this->GetNumberOfInputs(); i++ )
      {
      pix[i] = inputItContainer[i]->Get();
      ++( *inputItContainer[i] );
      }
    oit.Set(pix);
    ++oit;
    }

  for ( unsigned int i = 0; i < this->GetNumberOfInputs(); i++ )
    {
    delete inputItContainer[i];
    }
}
} // end namespace itk

#endif
