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

#ifndef itkImageToRGBVTKImageFilter_h
#define itkImageToRGBVTKImageFilter_h

#include "itkProcessObject.h"
#include "vtkSmartPointer.h"
#include "vtkImageData.h"

namespace itk
{
/** \class ImageToRGBVTKImageFilter
 * \brief Converts an ITK image into a VTK image.
 *
 * \ingroup ITKLevelSetsv4Visualization
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT ImageToRGBVTKImageFilter:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageToRGBVTKImageFilter    Self;
  typedef ProcessObject               Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToRGBVTKImageFilter, ProcessObject);

  /** Some typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType   InputRegionType;
  typedef typename InputImageType::SpacingType  InputSpacingType;
  typedef typename InputImageType::SizeType     InputSizeType;
  typedef typename InputImageType::PixelType    InputPixelType;
  typedef typename InputImageType::IndexType    InputIndexType;

  /** Get the output in the form of a vtkImage.
      This call is delegated to the internal vtkImageImporter filter  */
  vtkSmartPointer< vtkImageData >  GetOutput() const;

  /** Set the input in the form of an itk::Image */
  using Superclass::SetInput;
  void SetInput(const InputImageType *);

  /** This call delegate the update to the importer */
  void Update();

protected:
  ImageToRGBVTKImageFilter();
  virtual ~ImageToRGBVTKImageFilter();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToRGBVTKImageFilter);

  InputImagePointer               m_Input;
  vtkSmartPointer< vtkImageData > m_Output;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToRGBVTKImageFilter.hxx"
#endif

#endif
