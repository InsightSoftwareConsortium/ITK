/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkClassifierBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkClassifierBase_txx
#define _itkClassifierBase_txx

#include "itkClassifierBase.h"
#include "itkCommand.h"

namespace itk
{

template <class TDataContainer>
ClassifierBase<TDataContainer>
::ClassifierBase(void):
  m_NumberOfClasses( 0 ),
  m_DecisionRulePointer( NULL )
{
  m_MembershipFunctions.resize(0);
}

template <class TDataContainer>
ClassifierBase<TDataContainer>
::~ClassifierBase()
{

}


template <class TDataContainer>
void
ClassifierBase<TDataContainer>
::Update()
{
  GenerateData() ;
}


template <class TDataContainer>
void
ClassifierBase<TDataContainer>
::GenerateData()
{
  if( ( m_NumberOfClasses == 0 ) ||
      ( m_MembershipFunctions.size() == 0 ) ||
      ( m_NumberOfClasses != m_MembershipFunctions.size() ) )
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }

}

/*
 * PrintSelf
 */
template <class TDataContainer>
void
ClassifierBase<TDataContainer>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Number of classes: " << m_NumberOfClasses << std::endl;

}// end PrintSelf

//------------------------------------------------------------------
// Add a membership function corresponding to the class index
//------------------------------------------------------------------

template<class TDataContainer>
unsigned int 
ClassifierBase< TDataContainer >
::AddMembershipFunction(MembershipFunctionType * function)
{
  m_MembershipFunctions.push_back(function) ;
  return static_cast<unsigned int>( m_MembershipFunctions.size() );
}



} // namespace itk






















#endif
