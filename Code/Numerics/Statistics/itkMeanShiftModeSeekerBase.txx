/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanShiftModeSeekerBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanShiftModeSeekerBase_txx
#define __itkMeanShiftModeSeekerBase_txx

namespace itk{ 
namespace Statistics{

template< class TSample >
MeanShiftModeSeekerBase< TSample >
::MeanShiftModeSeekerBase()
{
  m_MaximumIteration = 0 ;
  m_CacheMethod = 0 ;
}


template< class TSample >
MeanShiftModeSeekerBase< TSample >
::~MeanShiftModeSeekerBase()
{
}

template< class TSample >
void
MeanShiftModeSeekerBase< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "InputSample: " ;
  if ( m_InputSample != 0 )
    {
    os << m_InputSample << std::endl;
    }
  else
    {
    os << "not set." << std::endl ;
    }

  os << indent << "CacheMethod: " ;
  if ( m_CacheMethod != 0 )
    {
    os << m_CacheMethod << std::endl;
    }
  else
    {
    os << "not set." << std::endl ;
    }

  os << indent << "Maximum iterations: " << m_MaximumIteration << std::endl ;
}

template< class TSample >
void
MeanShiftModeSeekerBase< TSample >
::SetInputSample(TSample* sample)
{
  if ( m_InputSample != sample )
    {
    m_InputSample = sample ;
    this->Modified() ;
    }
}

template< class TSample >
void
MeanShiftModeSeekerBase< TSample >
::SetCacheMethod(CacheMethodType* method)
{
  if ( m_CacheMethod != method )
    {
    m_CacheMethod = method ;
    this->Modified() ;
    }
}

template< class TSample >
typename MeanShiftModeSeekerBase< TSample >::MeasurementVectorType
MeanShiftModeSeekerBase< TSample >
::Evolve(MeasurementVectorType instance)
{
  MeasurementVectorType queryPoint = instance ;
  MeasurementVectorType newPoint ;
  MeasurementVectorType previousPoint = queryPoint ;

  unsigned int currentIteration = 0 ;
  
  while ( true )
    {
    if ( m_MaximumIteration > 0 && currentIteration > m_MaximumIteration )
      {
      if ( this->GetDebug() )
        {
        std::cout << "DEBUG: max exceeded." << std::endl ;
        }

      return queryPoint ;
      }

    if ( m_CacheMethod != 0 )
      {
      if ( !m_CacheMethod->GetMeasurementVector(queryPoint, newPoint) )
        {
        this->ComputeMode( queryPoint, newPoint ) ;
        m_CacheMethod->SetMeasurementVector( queryPoint, newPoint ) ;
        }
      }
    else
      {
      this->ComputeMode( queryPoint, newPoint ) ;
      }
    
    if ( queryPoint != newPoint && newPoint != previousPoint )
      {
      previousPoint = queryPoint ;
      queryPoint = newPoint ;
      }
    else
      {
      return queryPoint ;
      }
    ++currentIteration ;
    } // end of while
}

} // end of namespace Statistics 
} // end of namespace itk

#endif

