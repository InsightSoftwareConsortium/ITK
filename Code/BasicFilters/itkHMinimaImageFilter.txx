/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHMinimaImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHMinimaImageFilter_txx
#define __itkHMinimaImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkHMinimaImageFilter.h"
#include "itkGrayscaleGeodesicErodeImageFilter.h"
#include "itkShiftScaleImageFilter.h"

namespace itk {

template <class TInputImage, class TOutputImage>
HMinimaImageFilter<TInputImage, TOutputImage>
::HMinimaImageFilter()
  : m_NumberOfIterationsUsed( 0 ),
    m_Height ( 2 )
{
}

template <class TInputImage, class TOutputImage>
void 
HMinimaImageFilter<TInputImage, TOutputImage>
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
HMinimaImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
HMinimaImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  
  // construct a marker image to manipulate using reconstruction by
  // erosion. the marker image is the input image minus the height
  // parameter.
  typedef typename ShiftScaleImageFilter<TInputImage, TInputImage>
    ShiftFilterType;
  typename ShiftFilterType::Pointer shift = ShiftFilterType::New();
  shift->SetInput( this->GetInput() );
  shift->SetShift( static_cast<ShiftFilterType::RealType>(m_Height) );

  // Delegate to a geodesic erosion filter.
  //
  //
  typename GrayscaleGeodesicErodeImageFilter<TInputImage, TInputImage>::Pointer
    erode
       = GrayscaleGeodesicErodeImageFilter<TInputImage, TInputImage>::New();

  // set up the erode filter
  erode->RunOneIterationOff();             // run to convergence
  erode->SetMarkerImage( shift->GetOutput() );
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
HMinimaImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Depth of local maxima (contrast): "
     << static_cast<NumericTraits<InputImagePixelType>::PrintType>(m_Height)
     << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
}
  
}// end namespace itk
#endif
