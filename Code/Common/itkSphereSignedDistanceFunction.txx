/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereSignedDistanceFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSphereSignedDistanceFunction_txx
#define __itkSphereSignedDistanceFunction_txx

#include "itkSphereSignedDistanceFunction.h"

namespace itk
{

// Constructor with default arguments
template<typename TCoordRep, unsigned int VSpaceDimension>
SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::SphereSignedDistanceFunction()
{
  this->GetParameters().SetSize( SpaceDimension + 1 );
  this->GetParameters().Fill( 0.0 );
  this->GetParameters()[0] = 1.0;
  m_Translation.Fill( 0.0 );
  m_Radius = 1.0;
}
    

// Set the parameters
template<typename TCoordRep, unsigned int VSpaceDimension>
void
SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::SetParameters( const ParametersType & parameters )
{
  if ( parameters != this->GetParameters() )
    {
    this->m_Parameters = parameters;

    m_Radius = parameters[0];

    for( unsigned int i=0; i<SpaceDimension; i++ )
      {
      m_Translation[i] = parameters[i+1];
      }

    this->Modified();
    }
}

// Print self
template<typename TCoordRep, unsigned int VSpaceDimension>
void
SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Translation: " << m_Translation << std::endl;
  os << indent << "Radius: " << m_Radius << std::endl;
}

// Evaluate the signed distance
template<typename TCoordRep, unsigned int VSpaceDimension>
typename SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::OutputType
SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::Evaluate( const PointType& point ) const
{
  typedef typename NumericTraits<OutputType>::RealType RealType;
  RealType output = 0.0;

  for( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    output += vnl_math_sqr( ( point[j] - m_Translation[j] ) );
    }
  
  output = vcl_sqrt(output) - m_Radius;

  return output;

}
  
} // end namespace itk

#endif
