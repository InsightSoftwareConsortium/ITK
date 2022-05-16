/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkPyImageFilter_h
#define itkPyImageFilter_h

#include "itkImageToImageFilter.h"

// The python header defines _POSIX_C_SOURCE without a preceding #undef
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
#include "Python.h"

namespace itk
{

/** \class PyImageFilter
 * \brief ImageToImageFilter subclass that calls a Python callable object, e.g.
 * a Python function or a class with a __call__ method.
 *
 * For more information on ITK filters, the GenerateData() method, and other filter pipeline methods,
 * see the ITK Software Guide.
 */
template <class TInputImage, class TOutputImage>
class ITK_TEMPLATE_EXPORT PyImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PyImageFilter);

  /** Standard class type aliases. */
  using Self = PyImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PyImageFilter, ImageToImageFilter);

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension enumeration */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Python callable called during the filter's GenerateInputRequestedRegion.
   *
   *  The callable takes one argument: the PyImageFilter Python instance. */
  void
  SetPyGenerateInputRequestedRegion(PyObject * obj);

  /** Python callable called during the filter's GenerateOutputInformation.
   *
   *  The callable takes one argument: the PyImageFilter Python instance. */
  void
  SetPyGenerateOutputInformation(PyObject * obj);

  /** Python callable called during the filter's EnlargeOutputRequestedRegion.
   *
   *  The callable takes two arguments: the PyImageFilter Python instance and the output image instance.*/
  void
  SetPyEnlargeOutputRequestedRegion(PyObject * obj);

  /** Python callable called during the filter's GenerateData.
   *
   * The callable takes one argument: the PyImageFilter Python instance. */
  void
  SetPyGenerateData(PyObject * obj);

  /** Python internal method to pass a pointer to the wrapping Python object. */
  void
  _SetSelf(PyObject * self)
  {
    this->m_Self = self;
  }

protected:
  PyImageFilter();
  virtual ~PyImageFilter();

  void
  GenerateInputRequestedRegion() override;

  void
  GenerateOutputInformation() override;

  void
  EnlargeOutputRequestedRegion(DataObject * dataObject) override;

  void
  GenerateData() override;

private:
  PyObject * m_Self;
  PyObject * m_GenerateInputRequestedRegionCallable{ nullptr };
  PyObject * m_GenerateOutputInformationCallable{ nullptr };
  PyObject * m_EnlargeOutputRequestedRegionCallable{ nullptr };
  PyObject * m_GenerateDataCallable{ nullptr };
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPyImageFilter.hxx"
#endif

#endif // _itkPyImageFilter_h
