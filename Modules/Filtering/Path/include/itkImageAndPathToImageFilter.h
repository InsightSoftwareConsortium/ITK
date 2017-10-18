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
#ifndef itkImageAndPathToImageFilter_h
#define itkImageAndPathToImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class ImageAndPathToImageFilter
 * \brief Base class for filters that take both a path and an image as input and produce a path as output.
 *
 * This class is the base class for filters that take both an image and a path
 * as input and produce an image as output.  Specifically, this class defines
 * the methods SetPathInput() and SetImageInput().  (It also establishes the
 * precedent of having image inputs precede path inputs for functions producing
 * images as outputs, according to the underlying DataObject implementation.)
 *
 * \ingroup ImageFilters
 * \ingroup PathFilters
 * \ingroup ITKPath
 */
template< typename TInputImage, typename TInputPath, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ImageAndPathToImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageAndPathToImageFilter                       Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageAndPathToImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef          TInputImage                  InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef          TInputPath                   InputPathType;
  typedef typename InputPathType::Pointer       InputPathPointer;
  typedef typename InputPathType::ConstPointer  InputPathConstPointer;
  typedef typename InputPathType::InputType     InputPathInputType;
  typedef typename InputPathType::OutputType    InputPathOutputType;
  typedef typename InputPathType::IndexType     InputPathIndexType;
  typedef typename InputPathType::OffsetType    InputPathOffsetType;
  typedef          TOutputImage                 OutputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  OutputImageRegionType;
  typedef typename OutputImageType::PixelType   OutputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Set/Get the image input of this process object. */
  virtual void SetImageInput(const TInputImage *image);


  /** Set/Get the path input of this process object. */
  virtual void SetPathInput(const TInputPath *path);

  const InputImageType * GetImageInput();
  const InputPathType * GetPathInput();

protected:
  InputImageType * GetNonConstImageInput();
  InputPathType * GetNonConstPathInput();
  ImageAndPathToImageFilter();
  virtual ~ImageAndPathToImageFilter() ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageAndPathToImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageAndPathToImageFilter.hxx"
#endif

#endif
