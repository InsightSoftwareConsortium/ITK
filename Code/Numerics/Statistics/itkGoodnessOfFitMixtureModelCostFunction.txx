/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGoodnessOfFitMixtureModelCostFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGoodnessOfFitMixtureModelCostFunction_txx
#define __itkGoodnessOfFitMixtureModelCostFunction_txx

#include "itkGoodnessOfFitMixtureModelCostFunction.h"

namespace itk{ 
namespace Statistics{

template< class TInputSample >
GoodnessOfFitMixtureModelCostFunction< TInputSample >
::GoodnessOfFitMixtureModelCostFunction()
{
  m_Function = 0 ;
}

template< class TInputSample >
GoodnessOfFitMixtureModelCostFunction< TInputSample >
::~GoodnessOfFitMixtureModelCostFunction()
{
}

template< class TInputSample >
void
GoodnessOfFitMixtureModelCostFunction< TInputSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Function  " << m_Function << std::endl;
  for ( unsigned int i = 0 ; i < m_Components.size() ; i++)
    {
      os << indent << "Components["<< i <<"]  "  << m_Components[i]  << std::endl;
    }
}

template< class TInputSample >
void
GoodnessOfFitMixtureModelCostFunction< TInputSample >
::AddComponent(ComponentType* component)
{
  m_Components.push_back(component) ;
}

template< class TInputSample >
void
GoodnessOfFitMixtureModelCostFunction< TInputSample >
::SetFunction(FunctionType* core)
{
  if ( m_Function != core )
    {
      m_Function = core ;
      this->Modified() ;
    }
}

template< class TInputSample >
unsigned int
GoodnessOfFitMixtureModelCostFunction< TInputSample >
::GetNumberOfParameters() const 
{
  unsigned int size = 0 ;
  ComponentType* component ;
  for ( unsigned int componentIndex = 0 ; componentIndex < m_Components.size() ;
        componentIndex++ )
    {
      component = m_Components[componentIndex] ;
      size += component->GetNumberOfParameters() ;
    }

  return size ;
}

template< class TInputSample >
typename GoodnessOfFitMixtureModelCostFunction< TInputSample >::MeasureType
GoodnessOfFitMixtureModelCostFunction< TInputSample >
::GetValue(const ParametersType &parameters) const
{
  unsigned int i;

  double value = 0.0 ;
 
  unsigned int index = 0 ;
  unsigned int paramSize;
  ComponentType* component ;
  for ( unsigned int  componentIndex = 0 ; componentIndex < m_Components.size() ;
        componentIndex++ )
    {
      component = m_Components[componentIndex] ;
      paramSize = component->GetNumberOfParameters() ;
      ParametersType params(paramSize) ;
      for ( i = 0 ; i < paramSize ; i++)
        {
          params[i] = parameters[index] ;
          index++ ;
        }

      component->SetParameters(params) ;
      component->
        SetUseExpectedHistogram(m_Function->GetUseExpectedHistogram()) ;
      if ( component->GetObservedHistogram() == 0 )
        {
          component->CreateHistograms() ;
        }

      component->Resample() ;

      if ( component->GetResampledSample()->GetTotalFrequency() == 0 ) 
        {
          return NumericTraits< double >::max() ;
        }

      component->CalculateProjectionAxes() ;
      
      m_Function->
        SetTotalObservedScale(component->GetTotalObservedScale()) ;
      
      m_Function->SetObservedHistogram(component->GetObservedHistogram()) ;
      
      if ( m_Function->GetUseExpectedHistogram() )
        {
          m_Function->
            SetExpectedHistogram(component->GetExpectedHistogram()) ;
        }
      
      for (i = 0 ; i < MeasurementVectorSize ; i++)
        {
          component->Project(i) ;
          if ( m_Function->GetUseExpectedHistogram() )
            {
              component->UpdateExpectedHistogram() ;
            }
          
          m_Function->Update() ;
          value += m_Function->GetOutput() ;
        }
    } // end of while ( iter ...

  return value ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif
