/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHMaximaImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHMaximaImageFilter_txx
#define __itkHMaximaImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkHMaximaImageFilter.h"
#include "itkGrayscaleGeodesicDilateImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template <class TInputImage, class TOutputImage>
HMaximaImageFilter<TInputImage, TOutputImage>
::HMaximaImageFilter()
  : m_Height ( 2 ),
    m_NumberOfIterationsUsed( 0 )

{
}

template <class TInputImage, class TOutputImage>
void 
HMaximaImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template <class TInputImage, class TOutputImage>
void 
HMaximaImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
HMaximaImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  
  // construct a marker image to manipulate using reconstruction by
  // dilation. the marker image is the input image minus the height
  // parameter.
  typedef ShiftScaleImageFilter<TInputImage, TInputImage>
    ShiftFilterType;
  typename ShiftFilterType::Pointer shift = ShiftFilterType::New();
  shift->SetInput( this->GetInput() );
  shift->SetShift( -1.0 * static_cast<typename ShiftFilterType::RealType>(m_Height) );

  // Delegate to a geodesic dilation filter.
  //
  //
  typename GrayscaleGeodesicDilateImageFilter<TInputImage, TInputImage>::Pointer
    dilate
    = GrayscaleGeodesicDilateImageFilter<TInputImage, TInputImage>::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(dilate,1.0f);

  // set up the dilate filter
  dilate->RunOneIterationOff();             // run to convergence
  dilate->SetMarkerImage( shift->GetOutput() );
  dilate->SetMaskImage( this->GetInput() );

  // graft our output to the dilate filter to force the proper regions
  // to be generated
  dilate->GraftOutput( this->GetOutput() );

  // reconstruction by dilation
  dilate->Update();

  // graft the output of the dilate filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( dilate->GetOutput() );

  // copy the number of iterations used
  m_NumberOfIterationsUsed = dilate->GetNumberOfIterationsUsed();
}


template<class TInputImage, class TOutputImage>
void
HMaximaImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Height of local maxima (contrast): "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_Height)
     << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
}
  
}// end namespace itk
#endif
