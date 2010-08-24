/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptBlackTopHatImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBlackTopHatImageFilter_txx
#define __itkBlackTopHatImageFilter_txx

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkBlackTopHatImageFilter.h"
#include "itkGrayscaleMorphologicalClosingImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
BlackTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::BlackTopHatImageFilter()
{
  m_SafeBorder = true;
  m_Algorithm = HISTO;
  m_ForceAlgorithm = false;
}

template< class TInputImage, class TOutputImage, class TKernel >
void
BlackTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to a closing filter.
  typename GrayscaleMorphologicalClosingImageFilter< TInputImage, TInputImage, TKernel >::Pointer
  close = GrayscaleMorphologicalClosingImageFilter< TInputImage, TInputImage, TKernel >::New();

  close->SetInput( this->GetInput() );
  close->SetKernel( this->GetKernel() );
  close->SetSafeBorder(m_SafeBorder);
  if ( m_ForceAlgorithm )
    {
    close->SetAlgorithm(m_Algorithm);
    }
  else
    {
    m_Algorithm = close->GetAlgorithm();
    }

  // Need to subtract the input from the closed image
  typename SubtractImageFilter< TInputImage, TInputImage, TOutputImage >::Pointer
  subtract = SubtractImageFilter< TInputImage, TInputImage, TOutputImage >::New();

  subtract->SetInput1( close->GetOutput() );
  subtract->SetInput2( this->GetInput() );

  // graft our output to the subtract filter to force the proper regions
  // to be generated
  subtract->GraftOutput( this->GetOutput() );

  // run the algorithm
  progress->RegisterInternalFilter(close, .9f);
  progress->RegisterInternalFilter(subtract, .1f);

  subtract->Update();

  // graft the output of the subtract filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( subtract->GetOutput() );
}

template< class TInputImage, class TOutputImage, class TKernel >
void
BlackTopHatImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Algorithm: " << m_Algorithm << std::endl;
  os << indent << "SafeBorder: " << m_SafeBorder << std::endl;
  os << indent << "ForceAlgorithm: " << m_ForceAlgorithm << std::endl;
}
} // end namespace itk
#endif
