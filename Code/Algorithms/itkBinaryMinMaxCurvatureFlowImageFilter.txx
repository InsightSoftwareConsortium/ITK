/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMinMaxCurvatureFlowImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryMinMaxCurvatureFlowImageFilter_txx
#define __itkBinaryMinMaxCurvatureFlowImageFilter_txx
#include "itkBinaryMinMaxCurvatureFlowImageFilter.h"

#include "itkExceptionObject.h"

namespace itk
{

/*
 * Constructor
 */
template <class TInputImage, class TOutputImage>
BinaryMinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
::BinaryMinMaxCurvatureFlowImageFilter()
{

  m_Threshold = 0.0;

  typename BinaryMinMaxCurvatureFlowFunctionType::Pointer cffp;
  cffp = BinaryMinMaxCurvatureFlowFunctionType::New();

  this->SetDifferenceFunction( static_cast<FiniteDifferenceFunctionType *>( 
                                 cffp.GetPointer() ) );

}


/*
 * Standard PrintSelf method.
 */
template <class TInputImage, class TOutputImage>
void
BinaryMinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Threshold: " << m_Threshold << std::endl;
}


/*
 * Initialize the state of filter and equation before each iteration.
 */
template <class TInputImage, class TOutputImage>
void
BinaryMinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
::InitializeIteration()
{

  // update variables in the equation object
 
  BinaryMinMaxCurvatureFlowFunctionType *f = 
    dynamic_cast<BinaryMinMaxCurvatureFlowFunctionType *>
    (this->GetDifferenceFunction().GetPointer());

  if ( !f )
    {
    itkExceptionMacro(<<"DifferenceFunction not of type BinaryMinMaxCurvatureFlowFunction");
    }

  f->SetThreshold( m_Threshold );

  this->Superclass::InitializeIteration();           

  
}

} // end namespace itk

#endif
