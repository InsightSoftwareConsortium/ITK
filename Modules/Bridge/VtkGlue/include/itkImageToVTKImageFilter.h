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
#ifndef itkImageToVTKImageFilter_h
#define itkImageToVTKImageFilter_h

#include "itkVTKImageExport.h"
#include "vtkImageImport.h"
#include "vtkImageData.h"

namespace itk
{

/** \class ImageToVTKImageFilter
 * \brief Converts an ITK image into a VTK image and plugs a
 *  itk data pipeline to a VTK datapipeline.
 *
 *  This class puts together an itkVTKImageExporter and a vtkImageImporter.
 *  It takes care of the details related to the connection of ITK and VTK
 *  pipelines. The User will perceive this filter as an adaptor to which
 *  an itk::Image can be plugged as input and a vtkImage is produced as
 *  output.
 *
 * \ingroup   ITKVtkGlue
 *
 * \wiki
 * \wikiexample{IO/ImageToVTKImageFilter,Display an ITK image}
 * \wikiexample{IO/itkVtkImageConvertDICOM,Uses a custom user matrix to align the image with DICOM physical space}
 * \endwiki
 */
template <typename TInputImage >
class ITK_TEMPLATE_EXPORT ImageToVTKImageFilter : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToVTKImageFilter);

  /** Standard class type aliases. */
  using Self = ImageToVTKImageFilter;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToVTKImageFilter, ProcessObject);

  /** Some type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::ConstPointer;

  using ExporterFilterType = VTKImageExport< InputImageType>;
  using ExporterFilterPointer = typename ExporterFilterType::Pointer;

  /** Get the output in the form of a vtkImage.
      This call is delegated to the internal vtkImageImporter filter  */
  vtkImageData *  GetOutput() const;

  /** Set the input in the form of an itk::Image */
  using Superclass::SetInput;
  void SetInput( const InputImageType * );
  InputImageType * GetInput();

  /** Return the internal VTK image importer filter.
      This is intended to facilitate users the access
      to methods in the importer */
  vtkImageImport * GetImporter() const;

  /** Return the internal ITK image exporter filter.
      This is intended to facilitate users the access
      to methods in the exporter */
  ExporterFilterType * GetExporter() const;

  /** This call delegates the update to the importer */
  void Update() override;

  /** This call delegates the update to the importer */
  void UpdateLargestPossibleRegion() override;

protected:
  ImageToVTKImageFilter();
  ~ImageToVTKImageFilter() override;

private:
  ExporterFilterPointer       m_Exporter;
  vtkImageImport *            m_Importer;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToVTKImageFilter.hxx"
#endif

#endif
