/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleFillholeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleFillholeImageFilter_txx
#define __itkGrayscaleFillholeImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkGrayscaleFillholeImageFilter.h"
#include "itkGrayscaleGeodesicErodeImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkImageRegionExclusionConstIteratorWithIndex.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"

namespace itk {

template <class TInputImage, class TOutputImage>
GrayscaleFillholeImageFilter<TInputImage, TOutputImage>
::GrayscaleFillholeImageFilter()
  : m_NumberOfIterationsUsed( 0 )
{
}

template <class TInputImage, class TOutputImage>
void 
GrayscaleFillholeImageFilter<TInputImage, TOutputImage>
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
GrayscaleFillholeImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
GrayscaleFillholeImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  
  // construct a marker image to manipulate using reconstruction by
  // erosion. the marker image will have the same pixel values as the
  // input image on the boundary of the image and will have the
  // maximum pixel value from the input image for all the pixels in
  // the interior
  //

  // compute the maximum pixel value in the input
  MinimumMaximumImageCalculator<TInputImage>::Pointer calculator
    = MinimumMaximumImageCalculator<TInputImage>::New();
  calculator->SetImage( this->GetInput() );
  calculator->ComputeMaximum();

  InputImagePixelType maxValue;
  maxValue = calculator->GetMaximum();

  // allocate a marker image
  InputImagePointer markerPtr = InputImageType::New();
  markerPtr->SetRequestedRegion( this->GetInput()->GetRequestedRegion() );
  markerPtr->SetBufferedRegion( this->GetInput()->GetBufferedRegion() );
  markerPtr
    ->SetLargestPossibleRegion( this->GetInput()->GetLargestPossibleRegion() );
  markerPtr->Allocate();

  // fill the marker image with the maximum value from the input
  markerPtr->FillBuffer( maxValue );

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
    
  
  // Delegate to a geodesic erosion filter.
  //
  //
  GrayscaleGeodesicErodeImageFilter<TInputImage, TInputImage>::Pointer
    erode
       = GrayscaleGeodesicErodeImageFilter<TInputImage, TInputImage>::New();

  // set up the erode filter
  erode->RunOneIterationOff();             // run to convergence
  erode->SetMarkerImage( markerPtr );
  erode->SetMaskImage( this->GetInput() );

  // graft our output to the erode filter to force the proper regions
  // to be generated
  erode->GraftOutput( this->GetOutput() );

  // reconstruction by erosion
  erode->Update();

  // graft the output of the erode filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( erode->GetOutput() );

  // copy the number of iterations used
  m_NumberOfIterationsUsed = erode->GetNumberOfIterationsUsed();
}


template<class TInputImage, class TOutputImage>
void
GrayscaleFillholeImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
}
  
}// end namespace itk
#endif
