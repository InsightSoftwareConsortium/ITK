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

#ifndef itkVTKVisualize2DSparseLevelSetLayersBase_hxx
#define itkVTKVisualize2DSparseLevelSetLayersBase_hxx

#include "vtkVersion.h"

#include "itkVTKVisualize2DSparseLevelSetLayersBase.h"
#include "vtkImageMapper3D.h"

namespace itk
{
template< typename TInputImage, typename TLevelSet >
VTKVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::VTKVisualize2DSparseLevelSetLayersBase(): Superclass()
{
  this->m_VTKImageActor = vtkSmartPointer< vtkImageActor >::New();
  this->m_VTKImageActor->InterpolateOff();

  this->m_Renderer->AddActor2D( this->m_VTKImageActor );
}

template< typename TInputImage, typename TLevelSet >
VTKVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::~VTKVisualize2DSparseLevelSetLayersBase()
{
}

template< typename TInputImage, typename TLevelSet >
void
VTKVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::SetInputImage( const InputImageType* iImage )
{
  this->m_InputImageConverter->SetInput( iImage );
  try
    {
    this->m_InputImageConverter->Update();
    }
  catch( itk::ExceptionObject& e )
    {
    std::cout << e << std::endl;
    return;
    }
}

template< typename TInputImage, typename TLevelSet >
void
VTKVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::SetLevelSet( LevelSetType* levelSet )
{
  this->m_LevelSet = levelSet;
}

template< typename TInputImage, typename TLevelSet >
void
VTKVisualize2DSparseLevelSetLayersBase< TInputImage, TLevelSet >
::PrepareVTKPipeline()
{
  this->m_InputImageConverter->Update();
  this->m_VTKImage = this->m_InputImageConverter->GetOutput();

  this->m_Renderer->RemoveActor2D( m_VTKImageActor );

#if VTK_MAJOR_VERSION <= 5
    this->m_VTKImageActor->SetInput( this->m_VTKImage );
#else
    this->m_VTKImageActor->GetMapper()->SetInputData( this->m_VTKImage );
#endif

  this->AddLayers();

  this->m_Renderer->AddActor2D( this->m_VTKImageActor );
}
}
#endif
