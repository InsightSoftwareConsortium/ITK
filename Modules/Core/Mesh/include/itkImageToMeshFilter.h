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
#ifndef itkImageToMeshFilter_h
#define itkImageToMeshFilter_h

#include "itkMeshSource.h"

namespace itk
{
/** \class ImageToMeshFilter
 * \brief
 *
 * ImageToMeshFilter is the base class for all process objects that output
 * Mesh data and require image data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * \ingroup ImageFilters
 * \ingroup ITKMesh
 */
template <typename TInputImage, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT ImageToMeshFilter : public MeshSource<TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToMeshFilter);

  /** Standard class type aliases. */
  using Self = ImageToMeshFilter;
  using Superclass = MeshSource<TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToMeshFilter, MeshSource);

  /** Create a valid output. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObject::Pointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

  /** Some Image related type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Some Mesh related type alias. */
  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  /** Set the input image of this process object.  */
  using Superclass::SetInput;
  void
  SetInput(unsigned int idx, const InputImageType * input);
  void
  SetInput(const InputImageType * input)
  {
    this->SetInput(0, input);
  }

  /** Get the input image of this process object.  */
  const InputImageType *
  GetInput(unsigned int idx);
  const InputImageType *
  GetInput()
  {
    return this->GetInput(0);
  }

  /** Get the output Mesh of this process object.  */
  OutputMeshType *
  GetOutput();

  /** Prepare the output */
  void
  GenerateOutputInformation() override;

protected:
  ImageToMeshFilter();
  ~ImageToMeshFilter() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToMeshFilter.hxx"
#endif

#endif
