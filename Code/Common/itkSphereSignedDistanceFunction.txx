/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereSignedDistanceFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSphereSignedDistanceFunction_txx
#define _itkSphereSignedDistanceFunction_txx

#include "itkSphereSignedDistanceFunction.h"


namespace itk
{

// Constructor with default arguments
template<typename TCoordRep, unsigned int VSpaceDimension>
SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::SphereSignedDistanceFunction()
{
  m_Parameters = ParametersType( SpaceDimension + 1 );
  m_Parameters.Fill( 0.0 );
  m_Parameters[0] = 1.0;
  m_Translation.Fill( 0.0 );
  m_Radius = 1.0;
}
    

// Set the parameters
template<typename TCoordRep, unsigned int VSpaceDimension>
void
SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::SetParameters( const ParametersType & parameters )
{
  if ( parameters != m_Parameters )
    {
    m_Parameters = parameters;

    m_Radius = parameters[0];

    for( unsigned int i=0; i<SpaceDimension; i++ )
      {
      m_Translation[i] = parameters[i+1];
      }

    this->Modified();
    }
}

// Get the parameters
template<typename TCoordRep, unsigned int VSpaceDimension>
const typename SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::ParametersType &
SphereSignedDistanceFunction<TCoordRep, VSpaceDimension>
::GetParameters( void ) const
{
  return m_Parameters;
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
  
} // namespace

#endif
