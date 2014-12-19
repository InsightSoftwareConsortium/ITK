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
#ifndef itkClosingByReconstructionImageFilter_hxx
#define itkClosingByReconstructionImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkClosingByReconstructionImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
ClosingByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::ClosingByReconstructionImageFilter():
  m_Kernel()
{
  m_FullyConnected = false;
  m_PreserveIntensities = false;
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ClosingByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ClosingByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ClosingByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to a dilate filter.
  typename GrayscaleDilateImageFilter< TInputImage, TInputImage, TKernel >::Pointer
  dilate = GrayscaleDilateImageFilter< TInputImage, TInputImage, TKernel >::New();

  dilate->SetInput( this->GetInput() );
  dilate->SetKernel(this->m_Kernel);

  progress->RegisterInternalFilter(dilate, .5);

  // Delegate to a dilate filter.
  typename ReconstructionByErosionImageFilter< TInputImage, TInputImage >::Pointer
  erode = ReconstructionByErosionImageFilter< TInputImage, TInputImage >::New();

  erode->SetMarkerImage( dilate->GetOutput() );
  erode->SetMaskImage( this->GetInput() );
  erode->SetFullyConnected(m_FullyConnected);

  if ( m_PreserveIntensities )
    {
    progress->RegisterInternalFilter(erode, .25);
    erode->Update();
    typename TInputImage::Pointer tempImage = TInputImage::New();
    tempImage->SetRegions ( dilate->GetOutput()->GetBufferedRegion() );
    tempImage->CopyInformation( this->GetInput() );

    tempImage->Allocate();

    ImageRegionConstIterator< TInputImage > inputIt( this->GetInput(),
                                                     dilate->GetOutput()->GetBufferedRegion() );
    ImageRegionConstIterator< TInputImage > dilateIt( dilate->GetOutput(),
                                                      erode->GetOutput()->GetBufferedRegion() );
    ImageRegionConstIterator< TInputImage > erodeIt( erode->GetOutput(),
                                                     erode->GetOutput()->GetBufferedRegion() );
    ImageRegionIterator< TInputImage > tempIt( tempImage,
                                               dilate->GetOutput()->GetBufferedRegion() );
    while ( !dilateIt.IsAtEnd() )
      {
      if ( dilateIt.Get() == erodeIt.Get() )
        {
        tempIt.Set( inputIt.Get() );
        }
      else
        {
        tempIt.Set( NumericTraits< InputImagePixelType >::max() );
        }
      ++dilateIt;
      ++erodeIt;
      ++tempIt;
      ++inputIt;
      }

    typename ReconstructionByErosionImageFilter< TInputImage, TInputImage >::Pointer
    erodeAgain = ReconstructionByErosionImageFilter< TInputImage, TInputImage >::New();
    erodeAgain->SetMaskImage ( this->GetInput() );
    erodeAgain->SetMarkerImage (tempImage);
    erodeAgain->SetFullyConnected(m_FullyConnected);
    erodeAgain->GraftOutput( this->GetOutput() );
    progress->RegisterInternalFilter(erodeAgain, 0.25f);
    erodeAgain->Update();
    this->GraftOutput( erodeAgain->GetOutput() );
    }
  else
    {
    progress->RegisterInternalFilter(erode, .5);
    erode->GraftOutput( this->GetOutput() );
    erode->Update();
    this->GraftOutput( erode->GetOutput() );
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ClosingByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "PreserveIntensities: "  << m_PreserveIntensities << std::endl;
}
} // end namespace itk
#endif
