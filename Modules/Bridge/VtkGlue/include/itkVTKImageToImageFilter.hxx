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

#ifndef itkVTKImageToImageFilter_hxx
#define itkVTKImageToImageFilter_hxx

#include "itkVTKImageToImageFilter.h"

#include "vtkVersion.h"

namespace itk
{

/**
 * Constructor
 */
template <typename TOutputImage>
VTKImageToImageFilter<TOutputImage>
::VTKImageToImageFilter()
{

  m_Exporter = vtkImageExport::New();

  this->SetUpdateInformationCallback( m_Exporter->GetUpdateInformationCallback());
  this->SetPipelineModifiedCallback( m_Exporter->GetPipelineModifiedCallback());
  this->SetWholeExtentCallback( m_Exporter->GetWholeExtentCallback());
  this->SetSpacingCallback( m_Exporter->GetSpacingCallback());
  this->SetOriginCallback( m_Exporter->GetOriginCallback());
  this->SetScalarTypeCallback( m_Exporter->GetScalarTypeCallback());
  this->SetNumberOfComponentsCallback( m_Exporter->GetNumberOfComponentsCallback());
  this->SetPropagateUpdateExtentCallback( m_Exporter->GetPropagateUpdateExtentCallback());
  this->SetUpdateDataCallback( m_Exporter->GetUpdateDataCallback());
  this->SetDataExtentCallback( m_Exporter->GetDataExtentCallback());
  this->SetBufferPointerCallback( m_Exporter->GetBufferPointerCallback());
  this->SetCallbackUserData( m_Exporter->GetCallbackUserData());

}

/**
 * Destructor
 */
template <typename TOutputImage>
VTKImageToImageFilter<TOutputImage>
::~VTKImageToImageFilter()
{
  if( m_Exporter )
    {
    m_Exporter->Delete();
    m_Exporter = 0;
    }
}

/**
 * Set a vtkImageData as input
 */
template <typename TOutputImage>
void
VTKImageToImageFilter<TOutputImage>
::SetInput( vtkImageData * inputImage )
{
#if VTK_MAJOR_VERSION <= 5
  m_Exporter->SetInput( inputImage );
#else
  m_Exporter->SetInputData( inputImage );
#endif
}

/**
 * Get the exporter filter
 */
template <typename TOutputImage>
vtkImageExport *
VTKImageToImageFilter<TOutputImage>
::GetExporter() const
{
  return m_Exporter;
}

/**
 * Get the importer filter
 */
template <typename TOutputImage>
const typename VTKImageToImageFilter<TOutputImage>::Superclass *
VTKImageToImageFilter<TOutputImage>
::GetImporter() const
{
  return this;
}

} // end namespace itk

#endif
