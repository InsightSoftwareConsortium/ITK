/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonallyCorrected2DParametricPath.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkOrthogonallyCorrected2DParametricPath.h"
#include <math.h>

// not all versions of math.h seem to define M_PI:
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif



namespace itk
{

OrthogonallyCorrected2DParametricPath::OutputType
OrthogonallyCorrected2DParametricPath
::Evaluate( const InputType & input ) const
{
  InputType   theta;
  OutputType  output;
  /*
  int         numHarmonics;
  
  numHarmonics = m_CosCoefficients->Size(); 
  output.Fill(0);
  
  if( numHarmonics > 0 ) { output += m_CosCoefficients->ElementAt(0); }
  
  for(int n=1; n<numHarmonics; n++)
    {
    // input defined over [0,1] maps to theta defined over [0,2pi * n]
    theta = M_PI*2.0*n*input;
    output += ( m_CosCoefficients->ElementAt(n) * cos(theta) +
                m_SinCoefficients->ElementAt(n) * sin(theta) ) * 2.0;
    }
  */output.Fill(0);theta=0;
  
  return output;
}



OrthogonallyCorrected2DParametricPath::VectorType
OrthogonallyCorrected2DParametricPath
::EvaluateDerivative(const InputType & input) const
{
  InputType   theta;
  VectorType  output;
  /*
  int         numHarmonics;
  
  numHarmonics = m_CosCoefficients->Size(); 
  output.Fill(0);
  
  for(int n=1; n<numHarmonics; n++)
    {
    // input defined over [0,1] maps to theta defined over [0,2pi * n]
    theta = M_PI*2.0*n*input;
    output += ( m_SinCoefficients->ElementAt(n) * cos(theta) -
                m_CosCoefficients->ElementAt(n) * sin(theta) ) * (2.0 * n);
    }
  */output.Fill(0);theta=0;

  return output;
}



void
OrthogonallyCorrected2DParametricPath
::SetOrthogonalCorrectionTable( const OrthogonalCorrectionTablePointer
                                      orthogonalCorrectionTable )
{  
  m_OrthogonalCorrectionTable = orthogonalCorrectionTable;
  this->Modified();
}



/**
 * Constructor
 */
OrthogonallyCorrected2DParametricPath
::OrthogonallyCorrected2DParametricPath()
{
  m_OrthogonalCorrectionTable = OrthogonalCorrectionTableType::New();
}



/**
 * Standard "PrintSelf" method
 */
void
OrthogonallyCorrected2DParametricPath
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Correction Table:  " << m_OrthogonalCorrectionTable << std::endl;
}



} // end namespaceitk
