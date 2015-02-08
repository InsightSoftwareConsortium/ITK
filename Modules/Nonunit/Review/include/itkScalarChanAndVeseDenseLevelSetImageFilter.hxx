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
#ifndef itkScalarChanAndVeseDenseLevelSetImageFilter_hxx
#define itkScalarChanAndVeseDenseLevelSetImageFilter_hxx

#include "itkScalarChanAndVeseDenseLevelSetImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction,
          typename TSharedData >
void
ScalarChanAndVeseDenseLevelSetImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                           TFunction, TSharedData >::Initialize()
{
  // Set the feature image for the individual level-set functions
  for ( unsigned int fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    InputImagePointer input = this->m_LevelSet[fId];
    InputPointType    origin = input->GetOrigin();

    // In the context of the global coordinates
    FeatureIndexType start;
    this->GetInput()->TransformPhysicalPointToIndex(origin, start);

    // Defining roi region
    FeatureRegionType region;
    region.SetSize( input->GetLargestPossibleRegion().GetSize() );
    region.SetIndex(start);

    // Initialize the ROI filter with the feature image
    ROIFilterPointer roi = ROIFilterType::New();
    roi->SetInput( this->GetInput() );
    roi->SetRegionOfInterest(region);
    roi->Update();

    // Assign roi output
    FeatureImagePointer feature = roi->GetOutput();
    this->m_DifferenceFunctions[fId]->SetFeatureImage(feature);
    this->m_DifferenceFunctions[fId]->CalculateAdvectionImage();
    }

  // Initialize the function count in shared data
  this->m_SharedData->SetFunctionCount (this->m_FunctionCount);

  // Set the KdTree pointer
  if ( this->m_KdTree )
    {
    this->m_SharedData->SetKdTree(this->m_KdTree);
    }

  for ( unsigned int fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_DifferenceFunctions[fId]->SetFunctionId(fId);

    this->m_SharedData->CreateHeavisideFunctionOfLevelSetImage (fId, this->m_LevelSet[fId]);

    // Share the this->m_SharedData structure
    this->m_DifferenceFunctions[fId]->SetSharedData(this->m_SharedData);
    }

  this->m_SharedData->AllocateListImage( this->GetInput() );

  this->m_SharedData->PopulateListImage();

  this->Superclass::Initialize();
}

/** Overrides parent implementation */
// This function is called at the end of each iteration
template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction,
          typename TSharedData >
void
ScalarChanAndVeseDenseLevelSetImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                           TFunction, TSharedData >
::InitializeIteration()
{
  this->Superclass::InitializeIteration();

  for ( unsigned int fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_DifferenceFunctions[fId]->SetInitialImage(this->m_LevelSet[fId]);
    this->m_DifferenceFunctions[fId]->UpdateSharedData (true);
    }

  for ( unsigned int fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_DifferenceFunctions[fId]->UpdateSharedData (false);
    }
}
} /* end namespace itk */

#endif
