/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinMaxCurvatureFlowImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMinMaxCurvatureFlowImageFilter_txx
#define __itkMinMaxCurvatureFlowImageFilter_txx

#include "itkMinMaxCurvatureFlowImageFilter.h"

#include "itkExceptionObject.h"

namespace itk
{

/*
 * Constructor
 */
template <class TInputImage, class TOutputImage>
MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
::MinMaxCurvatureFlowImageFilter()
{

  m_StencilRadius = 2;

  typename MinMaxCurvatureFlowFunctionType::Pointer cffp;
  cffp = MinMaxCurvatureFlowFunctionType::New();

  this->SetDifferenceFunction( static_cast<FiniteDifferenceFunctionType *>( 
                                 cffp.GetPointer() ) );

}


/*
 * Standard PrintSelf method.
 */
template <class TInputImage, class TOutputImage>
void
MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "StencilRadius: " << m_StencilRadius << std::endl;
}


/*
 * Initialize the state of filter and equation before each iteration.
 */
template <class TInputImage, class TOutputImage>
void
MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>
::InitializeIteration()
{

  // update variables in the equation object
  MinMaxCurvatureFlowFunctionType *f = 
    dynamic_cast<MinMaxCurvatureFlowFunctionType *>
    (this->GetDifferenceFunction().GetPointer());

  if ( !f )
    {
    itkExceptionMacro(<<"DifferenceFunction not of type MinMaxCurvatureFlowFunction");
    }

  f->SetStencilRadius( m_StencilRadius );

  this->Superclass::InitializeIteration();           
  
}

} // end namespace itk

#endif
