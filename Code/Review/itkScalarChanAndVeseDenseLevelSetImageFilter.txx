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
#include "itkScalarChanAndVeseLevelSetFunctionSharedData.h"

namespace itk
{
template < class TInput,
  class TFeature,
  class TFunction,
  class TOutputPixel,
  class TSharedData >
void
ScalarChanAndVeseDenseLevelSetImageFilter< TInput, TFeature,
  TFunction, TOutputPixel, TSharedData >::
Initialize()
{
  // Set the feature image for the individual level-set functions
  for( unsigned int i = 0; i < this->m_FunctionCount; i++)
    {
    InputImagePointer input = this->m_LevelSet[i];
    InputPointType origin = input->GetOrigin();
    InputSpacingType spacing = input->GetSpacing();

    // In the context of the global coordinates
    FeatureIndexType start;

    // FIXME: This is suspicious code. It looks like we should
    // have used first IndexToPhysicalPoint and then PhysicalPointToIndex
    // as it was done in the ShrinkImagefilter...
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      start[j] = static_cast<FeatureIndexValueType>( origin[j]/spacing[j] );
      }

    // Defining roi region
    FeatureRegionType region;
    region.SetSize( input->GetLargestPossibleRegion().GetSize() );
    region.SetIndex( start );

    // Initialize the ROI filter with the feature image
    ROIFilterPointer roi = ROIFilterType::New();
    roi->SetInput( this->GetFeatureImage() );
    roi->SetRegionOfInterest( region );
    roi->Update();

    // Assign roi output
    FeatureImagePointer feature = roi->GetOutput();
    this->m_DifferenceFunctions[i]->SetFeatureImage( feature );
    this->m_DifferenceFunctions[i]->SetInitialImage( input );
    }

  // Initialize the function count in shared data
  this->m_SharedData->SetFunctionCount ( this->m_FunctionCount );

  // Set the KdTree pointer
  if ( this->m_KdTree )
    {
    this->m_SharedData->SetKdTree( this->m_KdTree );
    }

  for ( unsigned int i = 0; i < this->m_FunctionCount; i++ )
    {
    this->m_DifferenceFunctions[i]->SetFunctionId( i );

    this->m_SharedData->CreateHeavisideFunctionOfLevelSetImage ( i, this->m_LevelSet[i] );

    // Share the this->m_SharedData structure
    this->m_DifferenceFunctions[i]->SetSharedData( this->m_SharedData );
    }

  this->m_SharedData->AllocateListImage( this->GetFeatureImage() );

  this->m_SharedData->PopulateListImage();

  this->Superclass::Initialize();

  for (unsigned int i = 0; i < this->m_FunctionCount; i++)
    {
    this->m_DifferenceFunctions[i]->UpdateSharedData(true);
    }

  for ( unsigned int i = 0; i < this->m_FunctionCount; i++ )
    {
    this->m_DifferenceFunctions[i]->UpdateSharedData( false );
    }
}


/** Overrides parent implementation */
// This function is called at the end of each iteration
template < class TInput,
  class TFeature,
  class TFunction,
  class TOutputPixel,
  class TSharedData >
void
ScalarChanAndVeseDenseLevelSetImageFilter< TInput, TFeature, TFunction, TOutputPixel, TSharedData >
::InitializeIteration()
{
  this->Superclass::InitializeIteration();

  for( unsigned int i = 0; i < this->m_FunctionCount; i++ )
    {
    this->m_DifferenceFunctions[i]->SetInitialImage( this->m_LevelSet[i] );
    this->m_DifferenceFunctions[i]->UpdateSharedData ( true );
    }

  for( unsigned int i = 0; i < this->m_FunctionCount; i++ )
    {
    this->m_DifferenceFunctions[i]->UpdateSharedData ( false );
    }

  // Estimate the progress of the filter
  this->SetProgress ( ( ( float ) this->m_ElapsedIterations / this->m_NumberOfIterations ) );
}

template < class TInput,
  class TFeature,
  class TFunction,
  class TOutputPixel,
  class TSharedData >
void
ScalarChanAndVeseDenseLevelSetImageFilter< TInput, TFeature, TFunction, TOutputPixel, TSharedData >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Class Name: " << GetNameOfClass( ) << std::endl;
}


} /* end namespace itk */

#endif
