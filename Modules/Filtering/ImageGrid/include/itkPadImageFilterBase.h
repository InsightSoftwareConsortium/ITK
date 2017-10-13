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
#ifndef itkPadImageFilterBase_h
#define itkPadImageFilterBase_h

#include "itkImageToImageFilter.h"

#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class PadImageFilterBase
 * \brief Increase the image size by padding. Superclass for filters that fill
 * in extra pixels.
 *
 *
 * \image html PadImageFilter.png "Visual explanation of padding regions."
 *
 * PadImageFilterBase changes the image boundary of an image by padding each
 * dimension with subclass defined algorithms.  The padded region must be
 * specified by the subclasses.
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransform
 * \sa WrapPadImageFilterBase, MirrorPadImageFilterBase, ConstantPadImageFilterBase
 *
 * \ingroup ITKImageGrid
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT PadImageFilterBase:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef PadImageFilterBase                              Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
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
  itkTypeMacro(PadImageFilterBase, ImageToImageFilter);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Set/get the boundary condition. */
  itkSetMacro(BoundaryCondition, BoundaryConditionPointerType);
  itkGetConstMacro(BoundaryCondition, BoundaryConditionPointerType);

protected:
  PadImageFilterBase();
  ~PadImageFilterBase() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** PadImageFilterBase needs a smaller input requested region than
   * output requested region.  As such, PadImageFilterBase needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** This class can be multithreaded. */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  /** Method for subclasses to set the boundary condition. */
  void InternalSetBoundaryCondition( const BoundaryConditionPointerType boundaryCondition );

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PadImageFilterBase);

  BoundaryConditionPointerType m_BoundaryCondition;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPadImageFilterBase.hxx"
#endif

#endif
