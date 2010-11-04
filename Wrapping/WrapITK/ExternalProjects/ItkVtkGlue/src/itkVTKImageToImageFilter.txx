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
#ifndef _itkVTKImageToImageFilter_txx
#define _itkVTKImageToImageFilter_txx

#include "itkVTKImageToImageFilter.h"

namespace itk
{



/**
 * Constructor
 */
template <class TOutputImage>
VTKImageToImageFilter<TOutputImage>
::VTKImageToImageFilter()
{

  m_Exporter = vtkImageExport::New();

  m_Importer = ImporterFilterType::New();

  m_Importer->SetUpdateInformationCallback( m_Exporter->GetUpdateInformationCallback());
  m_Importer->SetPipelineModifiedCallback( m_Exporter->GetPipelineModifiedCallback());
  m_Importer->SetWholeExtentCallback( m_Exporter->GetWholeExtentCallback());
  m_Importer->SetSpacingCallback( m_Exporter->GetSpacingCallback());
  m_Importer->SetOriginCallback( m_Exporter->GetOriginCallback());
  m_Importer->SetScalarTypeCallback( m_Exporter->GetScalarTypeCallback());
  m_Importer->SetNumberOfComponentsCallback( m_Exporter->GetNumberOfComponentsCallback());
  m_Importer->SetPropagateUpdateExtentCallback( m_Exporter->GetPropagateUpdateExtentCallback());
  m_Importer->SetUpdateDataCallback( m_Exporter->GetUpdateDataCallback());
  m_Importer->SetDataExtentCallback( m_Exporter->GetDataExtentCallback());
  m_Importer->SetBufferPointerCallback( m_Exporter->GetBufferPointerCallback());
  m_Importer->SetCallbackUserData( m_Exporter->GetCallbackUserData());

}




/**
 * Destructor
 */
template <class TOutputImage>
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
template <class TOutputImage>
void
VTKImageToImageFilter<TOutputImage>
::SetInput( vtkImageData * inputImage )
{
  m_Exporter->SetInput( inputImage );
}



/**
 * Get an itk::Image as output
 */
template <class TOutputImage>
const typename VTKImageToImageFilter<TOutputImage>::OutputImageType *
VTKImageToImageFilter<TOutputImage>
::GetOutput() const
{
  return m_Importer->GetOutput();
}




/**
 * Get the exporter filter
 */
template <class TOutputImage>
vtkImageExport *
VTKImageToImageFilter<TOutputImage>
::GetExporter() const
{
  return m_Exporter;
}



/**
 * Get the importer filter
 */
template <class TOutputImage>
typename VTKImageToImageFilter<TOutputImage>::ImporterFilterType *
VTKImageToImageFilter<TOutputImage>
::GetImporter() const
{
  return m_Importer;
}




/**
 * Delegate the Update to the importer
 */
template <class TOutputImage>
void
VTKImageToImageFilter<TOutputImage>
::Update()
{
  m_Importer->Update();
}




} // end namespace itk

#endif

