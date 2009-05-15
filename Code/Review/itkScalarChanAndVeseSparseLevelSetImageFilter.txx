/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseSparseLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScalarChanAndVeseSparseLevelSetImageFilter_txx
#define __itkScalarChanAndVeseSparseLevelSetImageFilter_txx

#include "itkScalarChanAndVeseSparseLevelSetImageFilter.h"

namespace itk
{
template < class TInput, class TFeature, class TFunction,
class TOutputPixel, class TSharedData, typename TIdCell >
void
ScalarChanAndVeseSparseLevelSetImageFilter< TInput, TFeature, TFunction,
TOutputPixel, TSharedData, TIdCell >::
Initialize()
{
  // Set the feature image for the individual level-set functions
  for( IdCellType i = 0; i < this->m_FunctionCount; i++)
    {
    InputImagePointer input = this->m_LevelSet[i];
    InputPointType origin = input->GetOrigin();
    InputSpacingType spacing = input->GetSpacing();

    // In the context of the global coordinates
    FeatureIndexType start;
    for( unsigned int j = 0; j < ImageDimension; j++ )
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
    FeatureImagePtr feature = roi->GetOutput();
    this->m_DifferenceFunctions[i]->SetFeatureImage( feature );
    this->m_DifferenceFunctions[i]->SetInitialImage( input );
    }

  // Initialize the function count in m_SharedData
  this->m_SharedData->SetFunctionCount ( this->m_FunctionCount );

  // Set the KdTree pointer
  if ( this->m_KdTree )
    {
    this->m_SharedData->SetKdTree( this->m_KdTree );
    }

  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    FunctionPtr typedPointer = this->m_DifferenceFunctions[i];

    typedPointer->SetFunctionId( i );

    this->m_SharedData->CreateHeavisideFunctionOfLevelSetImage ( i, this->m_LevelSet[i] );

    // Share the m_SharedData structure
    typedPointer->SetSharedData( this->m_SharedData );
    }

  this->m_SharedData->AllocateListImage( this->GetFeatureImage() );

  this->m_SharedData->PopulateListImage();

  Superclass::Initialize();

  for (IdCellType i = 0; i < this->m_FunctionCount; i++)
    {
    this->m_DifferenceFunctions[i]->UpdateSharedData(true);
    }

  for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
    {
    this->m_DifferenceFunctions[i]->UpdateSharedData( false );
    }
}

/** Overrides parent implementation */
// This function is called at the end of each iteration
template < class TInput, class TFeature, class TFunction,
class TOutputPixel, class TSharedData, typename TIdCell >
void
ScalarChanAndVeseSparseLevelSetImageFilter< TInput, TFeature,
TFunction, TOutputPixel, TSharedData, TIdCell > ::
InitializeIteration()
{
  Superclass::InitializeIteration();

  for (IdCellType i = 0; i < this->m_FunctionCount; i++)
    {
    this->m_DifferenceFunctions[i]->UpdateSharedData( false );
    }

  // Estimate the progress of the filter
  this->SetProgress( ( ( float ) this->m_ElapsedIterations
    / ( float ) this->m_NumberOfIterations ) );
}

template < class TInput, class TFeature, class TFunction,
class TOutputPixel, class TSharedData, typename TIdCell >
void
ScalarChanAndVeseSparseLevelSetImageFilter< TInput, TFeature,
TFunction, TOutputPixel, TSharedData, TIdCell > ::
UpdatePixel ( unsigned int functionIndex, unsigned int idx,
NeighborhoodIterator< OutputImageType > &iterator, ValueType &newValue,
bool &status )
{
  FunctionPtr typedPointer = this->m_DifferenceFunctions[functionIndex];
  typedPointer->UpdatePixel( idx, iterator, newValue, status );

  iterator.SetPixel(idx, newValue, status);
}

template < class TInput, class TFeature, class TFunction,
class TOutputPixel, class TSharedData, typename TIdCell >
void
ScalarChanAndVeseSparseLevelSetImageFilter< TInput, TFeature,
TFunction, TOutputPixel, TSharedData, TIdCell > ::
PrintSelf( std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Name Of Class: " << std::endl;
}


} /* end namespace itk */

#endif
