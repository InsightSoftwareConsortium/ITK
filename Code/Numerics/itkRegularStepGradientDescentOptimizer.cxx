/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularStepGradientDescentOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegularStepGradientDescentOptimizer_txx
#define _itkRegularStepGradientDescentOptimizer_txx

#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkCommand.h"
#include "itkEventObject.h"

namespace itk
{



/**
 * Advance one Step following the gradient direction
 * This method will be overrided in non-vector spaces
 */
void
RegularStepGradientDescentOptimizer
::StepAlongGradient( double factor, 
                     const DerivativeType & transformedGradient )
{ 

  itkDebugMacro(<<"factor = " << factor << "  transformedGradient= " << transformedGradient );

  const unsigned int spaceDimension =
                        m_CostFunction->GetNumberOfParameters();

  ParametersType newPosition( spaceDimension );
  ParametersType currentPosition = this->GetCurrentPosition();

  for(unsigned int j=0; j<spaceDimension; j++)
  {
    newPosition[j] = currentPosition[j] + transformedGradient[j] * factor;
  }

  itkDebugMacro(<<"new position = " << newPosition );

  this->SetCurrentPosition( newPosition );

}



} // end namespace itk

#endif
