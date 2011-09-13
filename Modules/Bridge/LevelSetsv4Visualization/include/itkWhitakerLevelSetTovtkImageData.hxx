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

#ifndef __itkWhitakerLevelSetTovtkImageData_hxx
#define __itkWhitakerLevelSetTovtkImageData_hxx

#include "vtkImageData.h"
#include "itkWhitakerLevelSetTovtkImageData.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
template< typename TOutput, unsigned int VDimension >
WhitakerLevelSetTovtkImageData< TOutput, VDimension >
::WhitakerLevelSetTovtkImageData()
{
  m_InternalImage = ImageType::New();
  m_Converter = ConverterType::New();
}

template< typename TOutput, unsigned int VDimension >
void
WhitakerLevelSetTovtkImageData< TOutput, VDimension >
::SetInput( LevelSetType* iLevelSet )
{
  if( iLevelSet == NULL )
    {
    itkGenericExceptionMacro( <<"iLevelSet is NULL" );
    }
  if( iLevelSet->GetLabelMap() == NULL )
    {
    itkGenericExceptionMacro( <<"iLevelSet->GetLabelMap() is NULL" );
    }
  m_LevelSet = iLevelSet;

  typename LevelSetType::LabelMapPointer labelmap = m_LevelSet->GetLabelMap();

  m_InternalImage->SetRegions( labelmap->GetLargestPossibleRegion() );
  m_InternalImage->SetSpacing( labelmap->GetSpacing() );
  m_InternalImage->SetOrigin( labelmap->GetOrigin() );
  m_InternalImage->SetDirection( labelmap->GetDirection() );
  m_InternalImage->SetNumberOfComponentsPerPixel( 1 );
  m_InternalImage->Allocate();

}

template< typename TOutput, unsigned int VDimension >
vtkImageData*
WhitakerLevelSetTovtkImageData< TOutput, VDimension >
::GetOutput() const
{
  return m_Converter->GetOutput();
}

template< typename TOutput, unsigned int VDimension >
void
WhitakerLevelSetTovtkImageData< TOutput, VDimension >
::Update()
{
  typedef ImageRegionIteratorWithIndex< ImageType > IteratorType;

  IteratorType it( m_InternalImage, m_InternalImage->GetLargestPossibleRegion() );
  it.GoToBegin();

  typename ImageType::IndexType idx;

  while( !it.IsAtEnd() )
    {
    idx = it.GetIndex();
    it.Set( m_LevelSet->Evaluate( idx ) );
    ++it;
    }
  m_Converter->SetInput( m_InternalImage );
  m_Converter->Update();
}
}
#endif // __itkWhitakerLevelSetTovtkImageData_hxx
