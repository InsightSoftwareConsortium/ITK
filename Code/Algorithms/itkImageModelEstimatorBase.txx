/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageModelEstimatorBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageModelEstimatorBase_txx
#define _itkImageModelEstimatorBase_txx

#include "itkImageModelEstimatorBase.h"
#include "itkCommand.h"

namespace itk
{

template <class TInputImage, 
          class TMembershipFunction>
ImageModelEstimatorBase<TInputImage, TMembershipFunction>
::ImageModelEstimatorBase(void):
  m_NumberOfModels( 0 )
{

}

template <class TInputImage, 
          class TMembershipFunction>
ImageModelEstimatorBase<TInputImage, TMembershipFunction>
::~ImageModelEstimatorBase()
{

}
template <class TInputImage, 
          class TMembershipFunction>
void
ImageModelEstimatorBase<TInputImage, TMembershipFunction>
::Update()
{
  GenerateData() ;
}

template <class TInputImage, 
          class TMembershipFunction>
void
ImageModelEstimatorBase<TInputImage, TMembershipFunction>
::GenerateData()
{

/*
  if( ( m_NumberOfModels == 0 ) ||
      ( m_MembershipFunctions.size() == 0 ) ||
      ( m_NumberOfClasses != m_MembershipFunctions.size() ) )
    {
      throw ExceptionObject(__FILE__, __LINE__);
    }
*/

  this->EstimateModels();
}

/*
 * PrintSelf
 */
template <class TInputImage, 
          class TMembershipFunction>
void
ImageModelEstimatorBase<TInputImage, TMembershipFunction>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Number of models: " << m_NumberOfModels << std::endl;
  os << indent << "                   " << std::endl;

  os << indent <<"Results of the model estimator."<<std::endl;
  os << indent <<"===================================="<<std::endl;

  for (unsigned int classIndex = 0 ; classIndex < m_NumberOfModels ; classIndex++)
    {    
    os << indent << "Statistics for " << classIndex << std::endl;
    (m_MembershipFunctions[classIndex])->Print(std::cout);    

    os << indent <<"===================================="<<std::endl;
    }

  os << indent << "                   " << std::endl;

  os << indent << "InputImage: ";
  os << m_InputImage.GetPointer() << std::endl;


}// end PrintSelf

//------------------------------------------------------------------
// Add a membership function corresponding to the class index
//------------------------------------------------------------------

template <class TInputImage, 
          class TMembershipFunction>
unsigned int 
ImageModelEstimatorBase<TInputImage, TMembershipFunction>
::AddMembershipFunction(MembershipFunctionPointer function)
{
  m_MembershipFunctions.push_back(function) ;
  return static_cast<unsigned int>( m_MembershipFunctions.size() );
}

} // namespace itk






















#endif
