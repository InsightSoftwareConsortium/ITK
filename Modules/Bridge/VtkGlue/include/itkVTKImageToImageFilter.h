/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkVTKImageToImageFilter_h
#define itkVTKImageToImageFilter_h

#include "itkVTKImageImport.h"
#include "vtkImageExport.h"
#include "vtkImageData.h"
#include "vtkSmartPointer.h"

#ifndef vtkFloatingPointType
#  define vtkFloatingPointType float
#endif

namespace itk
{

/**
 *\class VTKImageToImageFilter
 * \brief Converts a VTK image into an ITK image and plugs a
 *  VTK data pipeline to an ITK datapipeline.
 *
 *  This class puts together an itk::VTKImageImport and a vtk::ImageExport.
 *  It takes care of the details related to the connection of ITK and VTK
 *  pipelines. The User will perceive this filter as an adaptor to which
 *  a vtkImageData can be plugged as input and an itk::Image is produced as
 *  output.
 *
 * \ingroup ITKVtkGlue
 * \sphinx
 * \sphinxexample{Bridge/VtkGlue/VTKImageToITKImage,VTK Image To ITK Image}
 * \endsphinx
 */
template <typename TOutputImage>
class ITK_TEMPLATE_EXPORT VTKImageToImageFilter : public VTKImageImport<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VTKImageToImageFilter);

  /** Standard class type aliases. */
  using Self = VTKImageToImageFilter;
  using Superclass = VTKImageImport<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKImageToImageFilter, VTKImageImport);

  /** Some type alias. */
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::ConstPointer;

  /** Set the input in the form of a vtkImageData */
  void
  SetInput(vtkImageData *);
  using Superclass::SetInput;

  /** Return the internal VTK image exporter filter.
      This is intended to facilitate users the access
      to methods in the exporter */
  vtkImageExport *
  GetExporter() const;

  /** Return the internal ITK image importer filter.
      This is intended to facilitate users the access
      to methods in the importer.
      */
  const Superclass *
  GetImporter() const;

protected:
  VTKImageToImageFilter();
  ~VTKImageToImageFilter() override;

private:
  using ImageExportPointer = vtkSmartPointer<vtkImageExport>;
  ImageExportPointer m_Exporter;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVTKImageToImageFilter.hxx"
#endif

#endif
