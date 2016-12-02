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
#ifndef itkImageToPathFilter_h
#define itkImageToPathFilter_h

#include "itkImage.h"
#include "itkPathSource.h"

namespace itk
{
/** \class ImageToPathFilter
 * \brief Base class for filters that take an image as input and produce an path as output.
 *
 * ImageToPathFilter is the base class for all process objects that output
 * path data and require image data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * This code was contributed in the Insight Journal paper:
 * "ContourExtractor2DImageFilter: A subpixel-precision image isocontour extraction filter."
 * by Pincus Z.
 * https://hdl.handle.net/1926/165
 * http://www.insight-journal.org/browse/publication/72
 *
 * \ingroup ImageFilters
 * \ingroup ITKPath
 */
template< typename TInputImage, typename TOutputPath >
class ITK_TEMPLATE_EXPORT ImageToPathFilter:public PathSource< TOutputPath >
{
public:
  /** Standard class typedefs. */
  typedef ImageToPathFilter          Self;
  typedef PathSource< TOutputPath >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToPathFilter, PathSource);

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Set/Get the image input of this process object.  */
  using Superclass::SetInput;
  virtual void SetInput(const InputImageType *image);

  virtual void SetInput(unsigned int, const TInputImage *image);

  const InputImageType * GetInput();

  const InputImageType * GetInput(unsigned int idx);

protected:
  ImageToPathFilter();
  virtual ~ImageToPathFilter();

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToPathFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToPathFilter.hxx"
#endif

#endif
