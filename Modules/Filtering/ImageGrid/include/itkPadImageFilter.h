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
#ifndef itkPadImageFilter_h
#define itkPadImageFilter_h

#include "itkPadImageFilterBase.h"

#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class PadImageFilter
 * \brief Increase the image size by padding. Superclass for filters that fill
 * in extra pixels.
 *
 *
 * \image html PadImageFilter.png "Visual explanation of padding regions."
 *
 * PadImageFilter changes the image boundary of an image by padding each
 * dimension with subclass defined algorithms.  The number of pixels to pad
 * for the upper and lower bounds of each dimension must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransform
 * \sa WrapPadImageFilter, MirrorPadImageFilter, ConstantPadImageFilter
 *
 * \ingroup ITKImageGrid
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT PadImageFilter:
  public PadImageFilterBase< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef PadImageFilter                                  Self;
  typedef PadImageFilterBase< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef to describe the output and input image region types. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::RegionType  InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  typedef typename TInputImage::PixelType  InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename TOutputImage::IndexType    OutputImageIndexType;
  typedef typename TInputImage::IndexType     InputImageIndexType;
  typedef typename TOutputImage::SizeType     OutputImageSizeType;
  typedef typename TInputImage::SizeType      InputImageSizeType;
  typedef typename TInputImage::SizeType      SizeType;
  typedef typename TInputImage::SizeValueType SizeValueType;

  /** Typedef to describe the boundary condition. */
  typedef ImageBoundaryCondition< TInputImage, TOutputImage > BoundaryConditionType;
  typedef BoundaryConditionType *                             BoundaryConditionPointerType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PadImageFilter, PadImageFilterBase);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Set/Get the output image padding.  Default is no padding
   *  (same as input). */
  itkSetMacro(PadLowerBound, SizeType);
  itkSetMacro(PadUpperBound, SizeType);
  itkGetConstReferenceMacro(PadLowerBound, SizeType);
  itkGetConstReferenceMacro(PadUpperBound, SizeType);
  itkSetVectorMacro(PadLowerBound, const SizeValueType, ImageDimension);
  itkSetVectorMacro(PadUpperBound, const SizeValueType, ImageDimension);

  void SetPadBound(const InputImageSizeType & bound)
  {
    this->SetPadLowerBound(bound);
    this->SetPadUpperBound(bound);
  }

protected:
  PadImageFilter();
  ~PadImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** PadImageFilter produces an image which is a different resolution
   * than its input image.  As such, PadImageFilter needs to
   * provide an implementation for GenerateOutputInformation() in order
   * to inform the pipeline execution model.  The original
   * documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void GenerateOutputInformation() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PadImageFilter);

  SizeType m_PadLowerBound;
  SizeType m_PadUpperBound;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPadImageFilter.hxx"
#endif

#endif
