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

#ifndef itkPyImageFilter_h
#define itkPyImageFilter_h

#include "itkImageToImageFilter.h"

// The python header defines _POSIX_C_SOURCE without a preceding #undef
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
// For Python 2.7 hypot bug, see https://bugs.python.org/issue11566
#include "PatchedPython27pyconfig.h"
#include <Python.h>

namespace itk
{

/** \class PyImageFilter
 *  \brief ImageToImageFilter subclass that calls a Python callable object, e.g.
 *  a Python function.
 */


template <class TInputImage, class TOutputImage>
class PyImageFilter : public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PyImageFilter                                 Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PyImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef typename    InputImageType::Pointer      InputImagePointer;
  typedef typename    InputImageType::RegionType   InputImageRegionType;
  typedef typename    InputImageType::PixelType    InputImagePixelType;
  typedef TOutputImage                             OutputImageType;
  typedef typename     OutputImageType::Pointer    OutputImagePointer;
  typedef typename     OutputImageType::RegionType OutputImageRegionType;
  typedef typename     OutputImageType::PixelType  OutputImagePixelType;

  /** ImageDimension enumeration */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);


  void SetPyGenerateData(PyObject *obj);

protected:
  PyImageFilter();
  virtual ~PyImageFilter();
  virtual void GenerateData();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PyImageFilter);
  PyObject *m_Object;

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPyImageFilter.hxx"
#endif

#endif // _itkPyImageFilter_h
