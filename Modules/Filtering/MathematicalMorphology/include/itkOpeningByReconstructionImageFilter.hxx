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
#ifndef __itkOpeningByReconstructionImageFilter_hxx
#define __itkOpeningByReconstructionImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkOpeningByReconstructionImageFilter.h"
#include "itkGrayscaleErodeImageFilter.h"
#include "itkReconstructionByDilationImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
OpeningByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::OpeningByReconstructionImageFilter():
  m_Kernel()
{
  m_FullyConnected = false;
  m_PreserveIntensities = false;
}

template< class TInputImage, class TOutputImage, class TKernel >
void
OpeningByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
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

template< class TInputImage, class TOutputImage, class TKernel >
void
OpeningByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage, class TKernel >
void
OpeningByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to an erode filter.
  typename GrayscaleErodeImageFilter< TInputImage, TInputImage, TKernel >::Pointer
  erode = GrayscaleErodeImageFilter< TInputImage, TInputImage, TKernel >::New();

  erode->SetInput( this->GetInput() );
  erode->SetKernel(this->m_Kernel);

  // Delegate to a dilate filter.
  typename ReconstructionByDilationImageFilter< TInputImage, TInputImage >::Pointer
  dilate = ReconstructionByDilationImageFilter< TInputImage, TInputImage >::New();

  dilate->SetMarkerImage( erode->GetOutput() );
  dilate->SetMaskImage( this->GetInput() );
  dilate->SetFullyConnected(m_FullyConnected);

  progress->RegisterInternalFilter(erode, 0.5f);
  progress->RegisterInternalFilter(dilate, 0.25f);

  if ( m_PreserveIntensities )
    {
    dilate->Update();
    typename TInputImage::Pointer tempImage = TInputImage::New();
    tempImage->SetRegions ( erode->GetOutput()->GetBufferedRegion() );
    tempImage->CopyInformation( this->GetInput() );

    tempImage->Allocate();

    ImageRegionConstIterator< TInputImage > inputIt( this->GetInput(),
                                                     erode->GetOutput()->GetBufferedRegion() );
    ImageRegionConstIterator< TInputImage > erodeIt( erode->GetOutput(),
                                                     erode->GetOutput()->GetBufferedRegion() );
    ImageRegionConstIterator< TInputImage > dilateIt( dilate->GetOutput(),
                                                      erode->GetOutput()->GetBufferedRegion() );
    ImageRegionIterator< TInputImage > tempIt( tempImage,
                                               erode->GetOutput()->GetBufferedRegion() );
    while ( !erodeIt.IsAtEnd() )
      {
      if ( erodeIt.Get() == dilateIt.Get() )
        {
        tempIt.Set( inputIt.Get() );
        }
      else
        {
        tempIt.Set( NumericTraits< InputImagePixelType >::NonpositiveMin() );
        }
      ++erodeIt;
      ++dilateIt;
      ++tempIt;
      ++inputIt;
      }

    typename ReconstructionByDilationImageFilter< TInputImage, TInputImage >::Pointer
    dilateAgain = ReconstructionByDilationImageFilter< TInputImage, TInputImage >::New();
    dilateAgain->SetMaskImage ( this->GetInput() );
    dilateAgain->SetMarkerImage (tempImage);
    dilateAgain->SetFullyConnected(m_FullyConnected);
    dilateAgain->GraftOutput( this->GetOutput() );
    progress->RegisterInternalFilter(dilateAgain, 0.25f);
    dilateAgain->Update();
    this->GraftOutput( dilateAgain->GetOutput() );
    }
  else
    {
    dilate->GraftOutput( this->GetOutput() );
    dilate->Update();
    this->GraftOutput( dilate->GetOutput() );
    }
}

template< class TInputImage, class TOutputImage, class TKernel >
void
OpeningByReconstructionImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "PreserveIntensities: "  << m_PreserveIntensities << std::endl;
}
} // end namespace itk
#endif
