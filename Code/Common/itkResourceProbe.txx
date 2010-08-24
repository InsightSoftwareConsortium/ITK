/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkResourceProbe.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkResourceProbe_txx
#define __itkResourceProbe_txx

#include "itkResourceProbe.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template< class ValueType, class MeanType >
ResourceProbe< ValueType, MeanType >
::ResourceProbe(const std::string & type, const std::string & unit):
  m_TypeString(type), m_UnitString(unit)
{
  this->m_TotalValue      = NumericTraits< ValueType >::ZeroValue();
  this->m_StartValue      = NumericTraits< ValueType >::ZeroValue();
  this->m_NumberOfStarts  = NumericTraits< CountType >::ZeroValue();
  this->m_NumberOfStops   = NumericTraits< CountType >::ZeroValue();
}

/** Destructor */
template< class ValueType, class MeanType >
ResourceProbe< ValueType, MeanType >
::~ResourceProbe()
{}

/** Returns the type probed value */
template< class ValueType, class MeanType >
std::string
ResourceProbe< ValueType, MeanType >
::GetType(void) const
{
  return this->m_TypeString;
}

/** Returns the unit probed value */
template< class ValueType, class MeanType >
std::string
ResourceProbe< ValueType, MeanType >
::GetUnit(void) const
{
  return this->m_UnitString;
}

/** Start counting */
template< class ValueType, class MeanType >
void
ResourceProbe< ValueType, MeanType >
::Start(void)
{
  this->m_NumberOfStarts++;
  this->m_StartValue = this->GetInstantValue();
}

/** Stop the probe */
template< class ValueType, class MeanType >
void
ResourceProbe< ValueType, MeanType >
::Stop(void)
{
  if ( this->m_NumberOfStops == this->m_NumberOfStarts )
    {
    itkGenericExceptionMacro(<< "Can't stop a probe that has not been started.");
    }
  this->m_TotalValue += this->GetInstantValue() - this->m_StartValue;
  this->m_NumberOfStops++;
}

/** Get Number of Starts */
template< class ValueType, class MeanType >
typename ResourceProbe< ValueType, MeanType >::CountType
ResourceProbe< ValueType, MeanType >
::GetNumberOfStarts(void) const
{
  return this->m_NumberOfStarts;
}

/** Get Number of Stops */
template< class ValueType, class MeanType >
typename ResourceProbe< ValueType, MeanType >::CountType
ResourceProbe< ValueType, MeanType >
::GetNumberOfStops(void) const
{
  return this->m_NumberOfStops;
}

/** Get Total */
template< class ValueType, class MeanType >
ValueType
ResourceProbe< ValueType, MeanType >
::GetTotal(void) const
{
  return this->m_TotalValue;
}

/** Get Mean */
template< class ValueType, class MeanType >
MeanType
ResourceProbe< ValueType, MeanType >
::GetMean(void) const
{
  MeanType meanValue = NumericTraits< MeanType >::ZeroValue();

  if ( this->m_NumberOfStops )
    {
    meanValue = static_cast< MeanType >( this->m_TotalValue ) / static_cast< MeanType >( this->m_NumberOfStops );
    }

  return meanValue;
}
} // end namespace itk

#endif
