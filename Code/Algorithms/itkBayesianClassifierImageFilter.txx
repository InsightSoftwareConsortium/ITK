/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBayesianClassifierImageFilter.txx
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
#ifndef _itkBayesianClassifierImageFilter_txx
#define _itkBayesianClassifierImageFilter_txx

#include "itkBayesianClassifierImageFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
BayesianClassifierImageFilter<TInputImage, TOutputImage>
::BayesianClassifierImageFilter()
{

}


/**
 * Add a membership function
 */
template <class TInputImage, class TOutputImage>
void
BayesianClassifierImageFilter<TInputImage, TOutputImage>
::AddMembershipFunction( const MembershipFunctionType * newFunction )
{
   MembershipFunctionConstPointer functionSmartPointer = newFunction;
   m_MembershipFunctions.push_back( functionSmartPointer );
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
BayesianClassifierImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Number of membership functions = " << m_MembershipFunctions.size() << std::endl;
  for( unsigned int i=0; i< m_MembershipFunctions.size(); i++)
    {
    os << indent << m_MembershipFunctions[i].GetPointer() << std::endl;
    }

}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
BayesianClassifierImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  DecisionRuleType::Pointer decisionRule = DecisionRuleType::New();  

  if( m_MembershipFunctions.size() == 0 )
    {
    itkExceptionMacro("No membership functions have been set up. Please call AddMembershipFunction() first");
    return;
    }

  typedef ImageRegionConstIterator< InputImageType >  ItertatorType;

  ItertatorType it( this->GetInput(), 
                    this->GetInput()->GetRequestedRegion() );

  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Get();
    ++it;
    }


}


} // end namespace itk

#endif
