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
#ifndef itkScalarChanAndVeseSparseLevelSetImageFilter_hxx
#define itkScalarChanAndVeseSparseLevelSetImageFilter_hxx

#include "itkScalarChanAndVeseSparseLevelSetImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction,
          typename TSharedData, typename TIdCell >
void
ScalarChanAndVeseSparseLevelSetImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                            TFunction, TSharedData, TIdCell >::Initialize()
{
  // Set the feature image for the individual level-set functions
  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
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
    this->m_DifferenceFunctions[fId]->SetInitialImage(input);
    this->m_DifferenceFunctions[fId]->CalculateAdvectionImage();
    }

  // Initialize the function count in m_SharedData
  this->m_SharedData->SetFunctionCount (this->m_FunctionCount);

  // Set the KdTree pointer
  if ( this->m_KdTree )
    {
    this->m_SharedData->SetKdTree(this->m_KdTree);
    }

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    FunctionPtr typedPointer = this->m_DifferenceFunctions[fId];

    typedPointer->SetFunctionId(fId);

    this->m_SharedData->CreateHeavisideFunctionOfLevelSetImage (fId, this->m_LevelSet[fId]);

    // Share the m_SharedData structure
    typedPointer->SetSharedData(this->m_SharedData);
    }

  this->m_SharedData->AllocateListImage( this->GetInput() );

  this->m_SharedData->PopulateListImage();

  Superclass::Initialize();

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_DifferenceFunctions[fId]->UpdateSharedData(true);
    }

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_DifferenceFunctions[fId]->UpdateSharedData(false);
    }
}

/** Overrides parent implementation */
// This function is called at the end of each iteration
template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction,
          typename TSharedData, typename TIdCell >
void
ScalarChanAndVeseSparseLevelSetImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                            TFunction, TSharedData, TIdCell >::InitializeIteration()
{
  Superclass::InitializeIteration();

  for ( IdCellType fId = 0; fId < this->m_FunctionCount; ++fId )
    {
    this->m_DifferenceFunctions[fId]->UpdateSharedData(false);
    }

  // Estimate the progress of the filter
  this->UpdateProgress( ( (float)this->m_ElapsedIterations
                       / (float)this->m_NumberOfIterations ) );
}

template< typename TInputImage, typename TFeatureImage, typename TOutputImage, typename TFunction,
          typename TSharedData, typename TIdCell >
void
ScalarChanAndVeseSparseLevelSetImageFilter< TInputImage, TFeatureImage, TOutputImage,
                                            TFunction, TSharedData, TIdCell >::UpdatePixel(
  unsigned int fId,
  unsigned int idx,
  NeighborhoodIterator<
    InputImageType > & iterator,
  ValueType & newValue,
  bool & status)
{
  FunctionPtr typedPointer = this->m_DifferenceFunctions[fId];

  typedPointer->UpdatePixel(idx, iterator, newValue, status);

  iterator.SetPixel(idx, newValue, status);
}
} /* end namespace itk */

#endif
