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
#ifndef itkMiniPipelineSeparableImageFilter_h
#define itkMiniPipelineSeparableImageFilter_h

#include "itkBoxImageFilter.h"

namespace itk
{
/**
 * \class MiniPipelineSeparableImageFilter
 * \brief A separable filter for filter which are using radius
 *
 * This filter takes a non separable implementation of a neighborhood
 * filter, and run it several times (one per dimension) to implement
 * the same separable transform.
 * This filter can be used with the filter for which the neighborhood is
 * defined by the SetRadius() method, like the BoxImageFilter and its
 * subcalsses.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 *
 * \author Gaetan Lehmann
 * \author Richard Beare
 * \ingroup ITKReview
 */

template< typename TInputImage, typename TOutputImage, typename TFilter >
class ITK_TEMPLATE_EXPORT MiniPipelineSeparableImageFilter:
  public BoxImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef MiniPipelineSeparableImageFilter            Self;
  typedef BoxImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MiniPipelineSeparableImageFilter,
               BoxImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::PixelType  PixelType;
  typedef typename TInputImage::OffsetType OffsetType;

  typedef TOutputImage                     OutputImageType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  typedef TFilter                                            FilterType;
  typedef CastImageFilter< InputImageType, OutputImageType > CastType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** n-dimensional Kernel radius. */
  typedef typename TInputImage::SizeType RadiusType;

  virtual void SetRadius(const RadiusType &) ITK_OVERRIDE;

  virtual void SetRadius(const SizeValueType & radius) ITK_OVERRIDE
  {
    // needed because of the overloading of the method
    Superclass::SetRadius(radius);
  }

  virtual void Modified() const ITK_OVERRIDE;

  virtual void SetNumberOfThreads(ThreadIdType nb) ITK_OVERRIDE;

protected:
  MiniPipelineSeparableImageFilter();
  ~MiniPipelineSeparableImageFilter() {}

  void GenerateData() ITK_OVERRIDE;

  typename FilterType::Pointer m_Filters[ImageDimension];
  typename CastType::Pointer m_Cast;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MiniPipelineSeparableImageFilter);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMiniPipelineSeparableImageFilter.hxx"
#endif

#endif
