/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleGrindPeakImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleGrindPeakImageFilter_txx
#define __itkGrayscaleGrindPeakImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkGrayscaleGrindPeakImageFilter.h"
#include "itkGrayscaleGeodesicDilateImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkImageRegionExclusionConstIteratorWithIndex.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"

namespace itk {

template <class TInputImage, class TOutputImage>
GrayscaleGrindPeakImageFilter<TInputImage, TOutputImage>
::GrayscaleGrindPeakImageFilter()
  : m_NumberOfIterationsUsed( 0 )
{
}

template <class TInputImage, class TOutputImage>
void 
GrayscaleGrindPeakImageFilter<TInputImage, TOutputImage>
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
GrayscaleGrindPeakImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
GrayscaleGrindPeakImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  
  // construct a marker image to manipulate using reconstruction by
  // dilation. the marker image will have the same pixel values as the
  // input image on the boundary of the image and will have the
  // minimum pixel value from the input image for all the pixels in
  // the interior
  //

  // compute the minimum pixel value in the input
  typename MinimumMaximumImageCalculator<TInputImage>::Pointer calculator
    = MinimumMaximumImageCalculator<TInputImage>::New();
  calculator->SetImage( this->GetInput() );
  calculator->ComputeMinimum();

  InputImagePixelType minValue;
  minValue = calculator->GetMinimum();
  
  // allocate a marker image
  InputImagePointer markerPtr = InputImageType::New();
  markerPtr->SetRequestedRegion( this->GetInput()->GetRequestedRegion() );
  markerPtr->SetBufferedRegion( this->GetInput()->GetBufferedRegion() );
  markerPtr
    ->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  markerPtr->Allocate();

  // fill the marker image with the maximum value from the input
  markerPtr->FillBuffer( minValue );

  // copy the borders of the input image to the marker image
  //
  ImageRegionExclusionConstIteratorWithIndex<TInputImage>
    inputBoundaryIt( this->GetInput(), this->GetInput()->GetRequestedRegion());
  ImageRegionExclusionIteratorWithIndex<TInputImage>
    markerBoundaryIt( markerPtr, this->GetInput()->GetRequestedRegion() );

  // build the exclusion region
  InputImageRegionType exclusionRegion;
  exclusionRegion = this->GetInput()->GetRequestedRegion();
  for (unsigned int i=0; i < InputImageType::ImageDimension; ++i)
    {
    exclusionRegion.SetSize( i, exclusionRegion.GetSize()[i] - 2);
    exclusionRegion.SetIndex( i, exclusionRegion.GetIndex()[i] + 1);
    }

  // set the exclusion region
  inputBoundaryIt.SetExclusionRegion( exclusionRegion );
  markerBoundaryIt.SetExclusionRegion( exclusionRegion );

  // copy the boundary pixels
  inputBoundaryIt.GoToBegin();
  markerBoundaryIt.GoToBegin();
  while ( !inputBoundaryIt.IsAtEnd() )
    {
    markerBoundaryIt.Set( inputBoundaryIt.Get() );
    ++markerBoundaryIt;
    ++inputBoundaryIt;
    }
    
  
  // Delegate to a geodesic dilation filter.
  //
  //
  typename GrayscaleGeodesicDilateImageFilter<TInputImage, TInputImage>::Pointer
    dilate
       = GrayscaleGeodesicDilateImageFilter<TInputImage, TInputImage>::New();

  // set up the dilate filter
  dilate->RunOneIterationOff();             // run to convergence
  dilate->SetMarkerImage( markerPtr );
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
GrayscaleGrindPeakImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
}
  
}// end namespace itk
#endif
