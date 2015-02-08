/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageModelEstimatorBase_hxx
#define itkImageModelEstimatorBase_hxx

#include "itkImageModelEstimatorBase.h"
#include "itkCommand.h"

namespace itk
{
template< typename TInputImage,
          typename TMembershipFunction >
ImageModelEstimatorBase< TInputImage, TMembershipFunction >
::ImageModelEstimatorBase(void):
  m_NumberOfModels(0)
{}

template< typename TInputImage,
          typename TMembershipFunction >
ImageModelEstimatorBase< TInputImage, TMembershipFunction >
::~ImageModelEstimatorBase()
{}

template< typename TInputImage,
          typename TMembershipFunction >
void
ImageModelEstimatorBase< TInputImage, TMembershipFunction >
::Update()
{
  GenerateData();
}

template< typename TInputImage,
          typename TMembershipFunction >
void
ImageModelEstimatorBase< TInputImage, TMembershipFunction >
::GenerateData()
{
  this->EstimateModels();
}

/**
 * PrintSelf
 */
template< typename TInputImage,
          typename TMembershipFunction >
void
ImageModelEstimatorBase< TInputImage, TMembershipFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number of models: " << m_NumberOfModels << std::endl;
  os << indent << "                   " << std::endl;

  os << indent << "Results of the model estimator." << std::endl;
  os << indent << "====================================" << std::endl;

  for ( unsigned int classIndex = 0; classIndex < m_NumberOfModels; classIndex++ )
    {
    os << indent << "Statistics for " << classIndex << std::endl;
    ( m_MembershipFunctions[classIndex] )->Print(os);

    os << indent << "====================================" << std::endl;
    }

  os << indent << "                   " << std::endl;

  os << indent << "InputImage: ";
  os << m_InputImage.GetPointer() << std::endl;
} // end PrintSelf

//------------------------------------------------------------------
// Add a membership function corresponding to the class index
//------------------------------------------------------------------

template< typename TInputImage,
          typename TMembershipFunction >
unsigned int
ImageModelEstimatorBase< TInputImage, TMembershipFunction >
::AddMembershipFunction(MembershipFunctionPointer function)
{
  m_MembershipFunctions.push_back(function);
  return static_cast< unsigned int >( m_MembershipFunctions.size() );
}
} // namespace itk

#endif
