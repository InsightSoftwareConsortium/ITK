/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodSampler.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNeighborhoodSampler_txx
#define __itkNeighborhoodSampler_txx

#include "itkNeighborhoodSampler.h"
#include "vnl/vnl_math.h"

namespace itk{ 
namespace Statistics{

template< class TSample >
NeighborhoodSampler< TSample >
::NeighborhoodSampler()
{
  m_Center = 0 ;
  m_Radius = 0 ;
  m_Subsample = SubsampleType::New() ;
}

template< class TSample >
void
NeighborhoodSampler< TSample >
::PrintSelf(std::ostream& os, Indent indent) const 
{
  Superclass::PrintSelf(os,indent) ;

  os << indent << "Center      " << (*m_Center) << std::endl;
  os << indent << "Radius      " << (*m_Radius) << std::endl;
  os << indent << "Output      " << m_Subsample << std::endl;
  os << indent << "Output Size " << m_Subsample->Size() << std::endl ;
}

template< class TSample >
NeighborhoodSampler< TSample >::OutputType*
NeighborhoodSampler< TSample >
::GetOutput()
{
  return m_Subsample.GetPointer() ;
}

template< class TSample >
void
NeighborhoodSampler< TSample >
::GenerateData()
{
  if (m_Radius == 0 || m_Center == 0 || this->GetInputSample() == 0)
    {
      itkExceptionMacro("Member variables have not been properly set.") ;
    }

  m_Subsample->SetSample(this->GetInputSample()) ;

  int j ;
  double squaredRadius ;
  double distance ;
  double coordinateDistance ;
  MeasurementVectorType tempVector ;

  squaredRadius = (*m_Radius) * (*m_Radius) ;

  m_Subsample->Clear() ;
  typename TSample::Iterator iter = this->GetInputSample()->Begin() ;
  typename TSample::Iterator last = this->GetInputSample()->End() ;
  while (iter != last)
    {
      distance = 0.0 ;
      tempVector = iter.GetMeasurementVector() ;
      for (j = 0 ; j < MeasurementVectorSize && distance < squaredRadius ; j++)
        {
          coordinateDistance = (double)tempVector[j] - (*m_Center)[j] ;
          if (vnl_math_abs(coordinateDistance) > (*m_Radius) )
            {
              distance = squaredRadius ;
            }
        }
      
      for (j = 0 ; j < MeasurementVectorSize && distance < squaredRadius ; j++)
        {
          coordinateDistance = (double)tempVector[j] - (*m_Center)[j] ;
          distance += coordinateDistance * coordinateDistance ;
        }
      
      if (distance < squaredRadius)
        {
          m_Subsample->AddInstance(iter.GetInstanceIdentifier()) ;
        }
      ++iter ;
    }
}

} // end of namespace Statistics 
} // end of namespace itk

#endif
