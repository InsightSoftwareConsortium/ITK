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

#ifndef itkLevelSetTovtkImageData_hxx
#define itkLevelSetTovtkImageData_hxx

#include "itkLevelSetTovtkImageData.h"

#include "itkLevelSetDenseImage.h"
#include "itkWhitakerSparseLevelSetImage.h"
#include "itkShiSparseLevelSetImage.h"
#include "itkMalcolmSparseLevelSetImage.h"

#include "itkImage.h"
#include "itkImageToVTKImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

#include "itkLabelMapToLabelImageFilter.h"

namespace itk
{
template< typename TImage >
LevelSetTovtkImageData< LevelSetDenseImage< TImage > >
::LevelSetTovtkImageData()
{
  this->m_Converter = ConverterType::New();
}

template< typename TImage >
LevelSetTovtkImageData< LevelSetDenseImage< TImage > >
::~LevelSetTovtkImageData()
{}

template< typename TImage >
vtkImageData*
LevelSetTovtkImageData< LevelSetDenseImage< TImage > >
::GetOutput() const
  {
  return this->m_Converter->GetOutput();
  }

template< typename TImage >
void
LevelSetTovtkImageData< LevelSetDenseImage< TImage > >
::GenerateData()
  {
  if( !this->m_LevelSet->GetImage() )
    {
    itkGenericExceptionMacro( <<"this->m_LevelSet->GetImage() is ITK_NULLPTR" );
    }
  this->m_Converter->SetInput( this->m_LevelSet->GetImage() );
  this->m_Converter->Update();
  }

// -----------------------------------------------------------------------------
template< typename TOutput, unsigned int VDimension >
LevelSetTovtkImageData< WhitakerSparseLevelSetImage< TOutput, VDimension > >
::LevelSetTovtkImageData()
{
  m_InternalImage = ImageType::New();
  m_Converter = ConverterType::New();
}

template< typename TOutput, unsigned int VDimension >
LevelSetTovtkImageData< WhitakerSparseLevelSetImage< TOutput, VDimension > >
::~LevelSetTovtkImageData()
{}

template< typename TOutput, unsigned int VDimension >
vtkImageData*
LevelSetTovtkImageData< WhitakerSparseLevelSetImage< TOutput, VDimension > >
::GetOutput() const
  {
  return m_Converter->GetOutput();
  }

template< typename TOutput, unsigned int VDimension >
void
LevelSetTovtkImageData< WhitakerSparseLevelSetImage< TOutput, VDimension > >
::GenerateData()
  {
  if( this->m_LevelSet->GetLabelMap() == ITK_NULLPTR )
    {
    itkGenericExceptionMacro( <<"this->m_LevelSet->GetLabelMap() is ITK_NULLPTR" );
    }

  typename LevelSetType::LabelMapPointer labelmap = this->m_LevelSet->GetLabelMap();

  this->m_InternalImage->SetRegions( labelmap->GetLargestPossibleRegion() );
  this->m_InternalImage->SetSpacing( labelmap->GetSpacing() );
  this->m_InternalImage->SetOrigin( labelmap->GetOrigin() );
  this->m_InternalImage->SetDirection( labelmap->GetDirection() );
  this->m_InternalImage->SetNumberOfComponentsPerPixel( 1 );
  this->m_InternalImage->Allocate();

  typedef ImageRegionIteratorWithIndex< ImageType > IteratorType;

  IteratorType it( this->m_InternalImage, this->m_InternalImage->GetLargestPossibleRegion() );
  it.GoToBegin();

  typename ImageType::IndexType idx;

  while( !it.IsAtEnd() )
    {
    idx = it.GetIndex();
    it.Set( this->m_LevelSet->Evaluate( idx ) );
    ++it;
    }

  this->m_Converter->SetInput( this->m_InternalImage );
  this->m_Converter->Update();
  }


// -----------------------------------------------------------------------------
template< unsigned int VDimension >
LevelSetTovtkImageData< ShiSparseLevelSetImage< VDimension > >
::LevelSetTovtkImageData()
{
  m_LabelMapToLabelImageFilter = LabelMapToLabelImageFilterType::New();
  m_Converter = ConverterType::New();
}

template< unsigned int VDimension >
LevelSetTovtkImageData< ShiSparseLevelSetImage< VDimension > >
::~LevelSetTovtkImageData()
  {}

template< unsigned int VDimension >
vtkImageData*
LevelSetTovtkImageData< ShiSparseLevelSetImage< VDimension > >
::GetOutput() const
  {
  return this->m_Converter->GetOutput();
  }

template< unsigned int VDimension >
void
LevelSetTovtkImageData< ShiSparseLevelSetImage< VDimension > >
::GenerateData()
  {
  if( this->m_LevelSet->GetLabelMap() == ITK_NULLPTR )
    {
    itkGenericExceptionMacro( <<"this->m_LevelSet->GetLabelMap() is ITK_NULLPTR" );
    }

  LabelMapPointer labelmap = this->m_LevelSet->GetLabelMap();

  this->m_LabelMapToLabelImageFilter->SetInput( labelmap );
  this->m_LabelMapToLabelImageFilter->Modified();
  this->m_LabelMapToLabelImageFilter->Update();

  this->m_Converter->SetInput( m_LabelMapToLabelImageFilter->GetOutput() );
  this->m_Converter->Modified();
  this->m_Converter->Update();
  }

// -----------------------------------------------------------------------------
template< unsigned int VDimension >
LevelSetTovtkImageData< MalcolmSparseLevelSetImage< VDimension > >
::LevelSetTovtkImageData()
  {
  m_LabelMapToLabelImageFilter = LabelMapToLabelImageFilterType::New();
  m_Converter = ConverterType::New();
  }

template< unsigned int VDimension >
LevelSetTovtkImageData< MalcolmSparseLevelSetImage< VDimension > >
::~LevelSetTovtkImageData()
  {}

template< unsigned int VDimension >
vtkImageData*
LevelSetTovtkImageData< MalcolmSparseLevelSetImage< VDimension > >
::GetOutput() const
  {
  return this->m_Converter->GetOutput();
  }

template< unsigned int VDimension >
void
LevelSetTovtkImageData< MalcolmSparseLevelSetImage< VDimension > >
::GenerateData()
  {
  if( this->m_LevelSet->GetLabelMap() == ITK_NULLPTR )
    {
    itkGenericExceptionMacro( <<"this->m_LevelSet->GetLabelMap() is ITK_NULLPTR" );
    }

  LabelMapPointer labelmap = this->m_LevelSet->GetLabelMap();

  this->m_LabelMapToLabelImageFilter->SetInput( labelmap );
  this->m_LabelMapToLabelImageFilter->Modified();
  this->m_LabelMapToLabelImageFilter->Update();

  this->m_Converter->SetInput( m_LabelMapToLabelImageFilter->GetOutput() );
  this->m_Converter->Modified();
  this->m_Converter->Update();
  }
}

#endif // itkLevelSetTovtkImageData_h
