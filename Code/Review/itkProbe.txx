/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProbe.txx
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
#ifndef __itkProbe_txx
#define __itkProbe_txx

#include "itkProbe.h"
#include "itkNumericTraits.h"

namespace itk
{

/** Constructor */
template<class ValueType, class MeanType>
Probe<ValueType,MeanType>
::Probe(const std::string & type, const std::string unit)
:m_TypeString(type),m_UnitString(unit)
{
  m_TotalValue      = NumericTraits< ValueType >::ZeroValue();
  m_StartValue      = NumericTraits< ValueType >::ZeroValue();
  m_NumberOfStarts  = NumericTraits< CountType >::ZeroValue();
  m_NumberOfStops   = NumericTraits< CountType >::ZeroValue();
}

/** Destructor */
template<class ValueType, class MeanType>
Probe<ValueType,MeanType>
::~Probe()
{
}

/** Returns the type probed value */
template<class ValueType, class MeanType>
std::string 
Probe<ValueType,MeanType>
::GetType(void)const
{
  return m_TypeString;
}

/** Returns the unit probed value */
template<class ValueType, class MeanType>
std::string 
Probe<ValueType,MeanType>
::GetUnit(void)const
{
  return m_UnitString;
}

/** Start counting */
template<class ValueType, class MeanType>
void 
Probe<ValueType,MeanType>
::Start(void)
{
  ++m_NumberOfStarts;
  m_StartValue = this->GetInstantValue();
}
 
/** Stop the probe */
template<class ValueType, class MeanType>
void 
Probe<ValueType,MeanType>
::Stop(void)
{
  if( m_NumberOfStops == m_NumberOfStarts )
    {
    itkGenericExceptionMacro( << "Can't stop a probe that has not been started." );
    }
  m_TotalValue += this->GetInstantValue() - m_StartValue;
  ++m_NumberOfStops;
}

/** Get Number of Starts */
template<class ValueType, class MeanType>
typename Probe<ValueType,MeanType>::CountType 
Probe<ValueType,MeanType>
::GetNumberOfStarts(void) const
{
  return m_NumberOfStarts;
}

/** Get Number of Stops */
template<class ValueType, class MeanType>
typename Probe<ValueType,MeanType>::CountType 
Probe<ValueType,MeanType>
::GetNumberOfStops(void) const
{
  return m_NumberOfStops;
}

/** Get Total */
template<class ValueType, class MeanType>
ValueType 
Probe<ValueType,MeanType>
::GetTotal(void) const
{
  return m_TotalValue;
}

/** Get Mean */
template<class ValueType, class MeanType>
MeanType 
Probe<ValueType,MeanType>
::GetMean(void) const
{
  MeanType meanValue = NumericTraits< MeanType >::ZeroValue();
  
  if( m_NumberOfStops)
    {
    meanValue = static_cast<MeanType>(m_TotalValue) / static_cast<MeanType>(m_NumberOfStops);
    }

  return meanValue;

}


} // end namespace itk

#endif
