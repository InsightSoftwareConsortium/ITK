/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkClosingByReconstructionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkClosingByReconstructionImageFilter_txx
#define __itkClosingByReconstructionImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkClosingByReconstructionImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkProgressAccumulator.h"


namespace itk {

template <class TInputImage, class TOutputImage, class TKernel>
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::ClosingByReconstructionImageFilter()
  : m_Kernel()
{
  m_FullyConnected = false;
  m_PreserveIntensities = false;
}

template <class TInputImage, class TOutputImage, class TKernel>
void 
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage, class TKernel>
void 
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage, class TKernel>
void 
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();
  
  // Delegate to a dilate filter.
  typename GrayscaleDilateImageFilter<TInputImage, TInputImage, TKernel>::Pointer
    dilate = GrayscaleDilateImageFilter<TInputImage, TInputImage, TKernel>::New();

  dilate->SetInput( this->GetInput() );
  dilate->SetKernel( this->m_Kernel );

  // Delegate to a dilate filter.
  typename ReconstructionByErosionImageFilter<TInputImage, TInputImage>::Pointer
    erode = ReconstructionByErosionImageFilter<TInputImage, TInputImage>::New();

  erode->SetMarkerImage( dilate->GetOutput() );
  erode->SetMaskImage( this->GetInput() );
  erode->SetFullyConnected( m_FullyConnected );

  if (m_PreserveIntensities)
    {
    erode->Update();
    typename TInputImage::Pointer tempImage = TInputImage::New();
    tempImage->SetRegions (dilate->GetOutput()->GetBufferedRegion());
    tempImage->Allocate();

    ImageRegionConstIterator<TInputImage> inputIt(this->GetInput(),
                                                  dilate->GetOutput()->GetBufferedRegion());
    ImageRegionConstIterator<TInputImage> dilateIt(dilate->GetOutput(),
                                                   erode->GetOutput()->GetBufferedRegion());
    ImageRegionConstIterator<TInputImage> erodeIt(erode->GetOutput(),
                                                  erode->GetOutput()->GetBufferedRegion());
    ImageRegionIterator<TInputImage> tempIt(tempImage,
                                            dilate->GetOutput()->GetBufferedRegion());
    while (!dilateIt.IsAtEnd())
      {
      if (dilateIt.Get() == erodeIt.Get())
        {
        tempIt.Set(inputIt.Get());
        }
      else
        {
        tempIt.Set(NumericTraits<InputImagePixelType>::max());
        }
      ++dilateIt;
      ++erodeIt;
      ++tempIt;
      ++inputIt;
      }

    typename ReconstructionByErosionImageFilter<TInputImage, TInputImage>::Pointer
      erodeAgain = ReconstructionByErosionImageFilter<TInputImage, TInputImage>::New();
    erodeAgain->SetMaskImage (this->GetInput());
    erodeAgain->SetMarkerImage (tempImage);
    erodeAgain->SetFullyConnected( m_FullyConnected );
    erodeAgain->GraftOutput( this->GetOutput() );
    progress->RegisterInternalFilter(erodeAgain, 0.25f);
    erodeAgain->Update();
    this->GraftOutput( erodeAgain->GetOutput() );
    }
  else
    {
    erode->GraftOutput( this->GetOutput() );
    erode->Update();
    this->GraftOutput( erode->GetOutput() );
    }
}

template<class TInputImage, class TOutputImage, class TKernel>
void
ClosingByReconstructionImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "PreserveIntensities: "  << m_PreserveIntensities << std::endl;
}

}// end namespace itk
#endif
