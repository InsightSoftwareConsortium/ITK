/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHConvexImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHConvexImageFilter_txx
#define __itkHConvexImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkHConvexImageFilter.h"
#include "itkHMaximaImageFilter.h"
#include "itkSubtractImageFilter.h"

namespace itk {

template <class TInputImage, class TOutputImage>
HConvexImageFilter<TInputImage, TOutputImage>
::HConvexImageFilter()
  : m_Height ( 2 ),
    m_NumberOfIterationsUsed( 0 )

{
}

template <class TInputImage, class TOutputImage>
void 
HConvexImageFilter<TInputImage, TOutputImage>
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
HConvexImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<class TInputImage, class TOutputImage>
void
HConvexImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();
  
  // Delegate to a H-Maxima filter.
  //
  //
  typename HMaximaImageFilter<TInputImage, TInputImage>::Pointer
    hmax = HMaximaImageFilter<TInputImage, TInputImage>::New();

  hmax->SetInput( this->GetInput() );
  hmax->SetHeight( m_Height );

  // Need to subtract the H-Maxima image from the input
  typename SubtractImageFilter<TInputImage, TInputImage, TOutputImage>::Pointer
    subtract=SubtractImageFilter<TInputImage,TInputImage,TOutputImage>::New();

  subtract->SetInput1( this->GetInput() );
  subtract->SetInput2( hmax->GetOutput() );

  // graft our output to the subtract filter to force the proper regions
  // to be generated
  subtract->GraftOutput( this->GetOutput() );

  // run the algorithm
  subtract->Update();

  // graft the output of the subtract filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( subtract->GetOutput() );

  // copy the number of iterations used
  m_NumberOfIterationsUsed = hmax->GetNumberOfIterationsUsed();
}


template<class TInputImage, class TOutputImage>
void
HConvexImageFilter<TInputImage, TOutputImage>
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
