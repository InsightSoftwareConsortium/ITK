/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFourierSeriesPath.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkFourierSeriesPath_txx
#define _itkFourierSeriesPath_txx

#include "itkFourierSeriesPath.h"
#include <math.h>

// not all versions of math.h seem to define M_PI:
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif



namespace itk
{

template<unsigned int VDimension>
typename FourierSeriesPath<VDimension>::OutputType
FourierSeriesPath<VDimension>
::Evaluate( const InputType & input ) const
{
  InputType   theta;
  OutputType  output;
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
  
  return output;
}



template<unsigned int VDimension>
typename FourierSeriesPath<VDimension>::VectorType
FourierSeriesPath<VDimension>
::EvaluateDerivative(const InputType & input) const
{
  InputType   theta;
  VectorType  output;
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
  
  return output;
}



template<unsigned int VDimension>
void
FourierSeriesPath<VDimension>
::AddHarmonic( const VectorType & CosCoefficients,
               const VectorType & SinCoefficients )
{
  unsigned int numHarmonics = m_CosCoefficients->Size();
  
  m_CosCoefficients->InsertElement(numHarmonics, CosCoefficients);
  m_SinCoefficients->InsertElement(numHarmonics, SinCoefficients);
  this->Modified();
}



/**
 * Constructor
 */
template <unsigned int VDimension>
FourierSeriesPath<VDimension>
::FourierSeriesPath()
{
  m_DefaultInputStepSize = 1.0/50.0;
  m_CosCoefficients = CoefficientsType::New();
  m_SinCoefficients = CoefficientsType::New();
}



/**
 * Standard "PrintSelf" method
 */
template <unsigned int VDimension>
void
FourierSeriesPath<VDimension>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Cos Coefficients:  " << m_CosCoefficients << std::endl;
  os << indent << "Sin Coefficients:  " << m_SinCoefficients << std::endl;
}



} // end namespaceitk

#endif
