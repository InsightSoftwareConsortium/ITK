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
#ifndef itkImageToVTKImageFilter_hxx
#define itkImageToVTKImageFilter_hxx

#include "itkImageToVTKImageFilter.h"

namespace itk
{

/**
 * Constructor
 */
template <typename TInputImage>
ImageToVTKImageFilter<TInputImage>
::ImageToVTKImageFilter()
{
  m_Importer = vtkImageImport::New();
  m_Exporter = ExporterFilterType::New();

  m_Importer->SetUpdateInformationCallback(m_Exporter->GetUpdateInformationCallback());
  m_Importer->SetPipelineModifiedCallback(m_Exporter->GetPipelineModifiedCallback());
  m_Importer->SetWholeExtentCallback(m_Exporter->GetWholeExtentCallback());
  m_Importer->SetSpacingCallback(m_Exporter->GetSpacingCallback());
  m_Importer->SetOriginCallback(m_Exporter->GetOriginCallback());
  m_Importer->SetScalarTypeCallback(m_Exporter->GetScalarTypeCallback());
  m_Importer->SetNumberOfComponentsCallback(m_Exporter->GetNumberOfComponentsCallback());
  m_Importer->SetPropagateUpdateExtentCallback(m_Exporter->GetPropagateUpdateExtentCallback());
  m_Importer->SetUpdateDataCallback(m_Exporter->GetUpdateDataCallback());
  m_Importer->SetDataExtentCallback(m_Exporter->GetDataExtentCallback());
  m_Importer->SetBufferPointerCallback(m_Exporter->GetBufferPointerCallback());
  m_Importer->SetCallbackUserData(m_Exporter->GetCallbackUserData());

}

/**
 * Destructor
 */
template <typename TInputImage>
ImageToVTKImageFilter<TInputImage>
::~ImageToVTKImageFilter()
{
  if( m_Importer )
    {
    m_Importer->Delete();
    m_Importer = 0;
    }
}

/**
 * Set an itk::Image as input
 */
template <typename TInputImage>
void
ImageToVTKImageFilter<TInputImage>
::SetInput( const InputImageType * inputImage )
{
  m_Exporter->SetInput( inputImage );
}

template <typename TInputImage>
typename ImageToVTKImageFilter<TInputImage>::InputImageType *
ImageToVTKImageFilter<TInputImage>
::GetInput()
{
  return m_Exporter->GetInput();
}

/**
 * Get a vtkImage as output
 */
template <typename TInputImage>
vtkImageData *
ImageToVTKImageFilter<TInputImage>
::GetOutput() const
{
  return m_Importer->GetOutput();
}

/**
 * Get the importer filter
 */
template <typename TInputImage>
vtkImageImport *
ImageToVTKImageFilter<TInputImage>
::GetImporter() const
{
  return m_Importer;
}

/**
 * Get the exporter filter
 */
template <typename TInputImage>
typename ImageToVTKImageFilter<TInputImage>::ExporterFilterType *
ImageToVTKImageFilter<TInputImage>
::GetExporter() const
{
  return m_Exporter.GetPointer();
}

/**
 * Delegate the Update to the importer
 */
template <typename TInputImage>
void
ImageToVTKImageFilter<TInputImage>
::Update()
{
  m_Importer->Update();
}

/**
 * Delegate the UpdateLargestPossibleRegion to the importer
 */
template <typename TInputImage>
void
ImageToVTKImageFilter<TInputImage>
::UpdateLargestPossibleRegion()
{
  m_Importer->UpdateWholeExtent();
}

} // end namespace itk

#endif
