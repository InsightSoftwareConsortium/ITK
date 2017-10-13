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
template< typename TInputImage, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT ImageToMeshFilter:public MeshSource< TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef ImageToMeshFilter          Self;
  typedef MeshSource< TOutputMesh >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToMeshFilter, MeshSource);

  /** Create a valid output. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObject::Pointer  MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  /** Some Image related typedefs. */
  typedef   TInputImage                           InputImageType;
  typedef   typename InputImageType::Pointer      InputImagePointer;
  typedef   typename InputImageType::ConstPointer InputImageConstPointer;
  typedef   typename InputImageType::RegionType   InputImageRegionType;
  typedef   typename InputImageType::PixelType    InputImagePixelType;

  /** Some Mesh related typedefs. */
  typedef   TOutputMesh                      OutputMeshType;
  typedef   typename OutputMeshType::Pointer OutputMeshPointer;

  /** Set the input image of this process object.  */
  using Superclass::SetInput;
  void SetInput(unsigned int idx, const InputImageType *input);
  void SetInput(const InputImageType *input)
    {
    this->SetInput(0, input);
    }

  /** Get the input image of this process object.  */
  const InputImageType * GetInput(unsigned int idx);
  const InputImageType * GetInput()
    {
    return this->GetInput(0);
    }

  /** Get the output Mesh of this process object.  */
  OutputMeshType * GetOutput();

  /** Prepare the output */
  void GenerateOutputInformation() ITK_OVERRIDE;

protected:
  ImageToMeshFilter();
  ~ImageToMeshFilter() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToMeshFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToMeshFilter.hxx"
#endif

#endif
