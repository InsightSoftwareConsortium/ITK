/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseDenseLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScalarChanAndVeseDenseLevelSetImageFilter_txx
#define __itkScalarChanAndVeseDenseLevelSetImageFilter_txx

#include "itkScalarChanAndVeseDenseLevelSetImageFilter.h"

namespace itk
{
template< class TInputImage, class TFeatureImage, class TOutputImage, class TFunction,
          class TSharedData >
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
template< class TInputImage, class TFeatureImage, class TOutputImage, class TFunction,
          class TSharedData >
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
