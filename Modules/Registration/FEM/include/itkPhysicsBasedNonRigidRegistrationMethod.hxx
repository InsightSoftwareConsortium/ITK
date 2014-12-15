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
#ifndef itkPhysicsBasedNonRigidRegistrationMethod_hxx
#define itkPhysicsBasedNonRigidRegistrationMethod_hxx

#include <iostream>
#include "itkTimeProbe.h"
#include "itkPhysicsBasedNonRigidRegistrationMethod.h"


namespace itk
{

namespace fem
{

template <typename TFixedImage, typename TMovingImage, typename TMaskImage, typename TMesh, typename TDeformationField>
PhysicsBasedNonRigidRegistrationMethod<TFixedImage, TMovingImage, TMaskImage, TMesh, TDeformationField>
::PhysicsBasedNonRigidRegistrationMethod()
{
  // defaults
  this->m_NonConnectivity = 0; // VERTEX_CONNECTIVITY
  this->m_SelectFraction = 0.1;
  this->m_BlockRadius.Fill( 2 );
  this->m_SearchRadius.Fill( 5 );
  this->m_ApproximationSteps = 10;
  this->m_OutlierRejectionSteps = 10;

  // setup internal pipeline
  this->m_FeatureSelectionFilter = FeatureSelectionFilterType::New();
  this->m_FeatureSelectionFilter->ComputeStructureTensorsOn();
  this->m_BlockMatchingFilter = BlockMatchingFilterType::New();
  this->m_BlockMatchingFilter->SetFeaturePoints( this->m_FeatureSelectionFilter->GetOutput() );
  this->m_FEMFilter = FEMFilterType::New();
  this->m_FEMFilter->SetConfidencePointSet( this->m_BlockMatchingFilter->GetSimilarities() );
  this->m_FEMFilter->SetTensorPointSet( this->m_FeatureSelectionFilter->GetOutput() );

  // all inputs are required
  this->SetPrimaryInputName("FixedImage");
  this->AddRequiredInputName("FixedImage");
  this->AddRequiredInputName("MovingImage");
  this->AddRequiredInputName("MaskImage");
  this->AddRequiredInputName("Mesh");
}


template <typename TFixedImage, typename TMovingImage, typename TMaskImage, typename TMesh, typename TDeformationField>
PhysicsBasedNonRigidRegistrationMethod<TFixedImage, TMovingImage, TMaskImage, TMesh, TDeformationField>
::~PhysicsBasedNonRigidRegistrationMethod()
{
}


template <typename TFixedImage, typename TMovingImage, typename TMaskImage, typename TMesh, typename TDeformationField>
void
PhysicsBasedNonRigidRegistrationMethod<TFixedImage, TMovingImage, TMaskImage, TMesh, TDeformationField>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "m_BlockRadius: " << m_BlockRadius << std::endl
     << indent << "m_SearchRadius: " << m_SearchRadius << std::endl
     << indent << "m_SelectFraction: " << m_SelectFraction << std::endl
     << indent << "m_NonConnectivity: " << m_NonConnectivity << std::endl
     << indent << "m_ApproximationSteps: " << m_ApproximationSteps << std::endl
     << indent << "m_OutlierRejectionSteps: " << m_OutlierRejectionSteps << std::endl;
}

template <typename TFixedImage, typename TMovingImage, typename TMaskImage, typename TMesh, typename TDeformationField>
void
PhysicsBasedNonRigidRegistrationMethod<TFixedImage, TMovingImage, TMaskImage, TMesh, TDeformationField>
::GenerateData()
{
  // feature selection
  this->m_FeatureSelectionFilter->SetInput( this->GetMovingImage() );
  this->m_FeatureSelectionFilter->SetMaskImage( this->GetMaskImage() );
  this->m_FeatureSelectionFilter->SetSelectFraction( this->m_SelectFraction );
  this->m_FeatureSelectionFilter->SetNonConnectivity( this->m_NonConnectivity );
  this->m_FeatureSelectionFilter->SetBlockRadius( this->m_BlockRadius );

  // block matching
  this->m_BlockMatchingFilter->SetFixedImage( this->GetFixedImage() );
  this->m_BlockMatchingFilter->SetMovingImage( this->GetMovingImage() );
  this->m_BlockMatchingFilter->SetBlockRadius( this->m_BlockRadius );
  this->m_BlockMatchingFilter->SetSearchRadius( this->m_SearchRadius );

  // assembly and solver
  typename BlockMatchingFilterType::DisplacementsType * displacements = this->m_BlockMatchingFilter->GetDisplacements();
  this->m_FEMFilter->SetInput( displacements );
  this->m_FEMFilter->SetMesh( const_cast< MeshType * >( this->GetMesh() ) );
  const FixedImageType * fixedImage = this->GetFixedImage();
  this->m_FEMFilter->SetSpacing( fixedImage->GetSpacing() );
  this->m_FEMFilter->SetOrigin( fixedImage->GetOrigin() );
  this->m_FEMFilter->SetSize( fixedImage->GetLargestPossibleRegion().GetSize() );

  typename FEMFilterType::FEMSolverType * femSolver = this->m_FEMFilter->GetModifiableFEMSolver();
  femSolver->SetApproximationSteps( this->m_ApproximationSteps );
  femSolver->SetOutlierRejectionSteps( this->m_OutlierRejectionSteps );

  // graft our output to the filter to force the proper regions to be generated
  this->m_FEMFilter->GraftOutput( this->GetOutput() );

  this->m_FEMFilter->Update();

  // graft the output of the subtract filter back onto this filter's output
  // this is needed to get the appropriate regions passed back
  this->GraftOutput( this->m_FEMFilter->GetOutput() );
}

}
}  // end namespace itk::fem

#endif
