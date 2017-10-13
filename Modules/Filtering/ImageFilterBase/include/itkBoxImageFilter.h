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
#ifndef itkBoxImageFilter_h
#define itkBoxImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkCastImageFilter.h"

namespace itk
{
/**
 * \class BoxImageFilter
 * \brief A base class for all the filters working on a box neighborhood
 *
 * This filter provides the code to store the radius information about the
 * neighborhood used in the subclasses.
 * It also conveniently reimplement the GenerateInputRequestedRegion() so
 * that region is well defined for the provided radius.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \ingroup ITKImageFilterBase
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BoxImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BoxImageFilter                                  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BoxImageFilter,
               ImageToImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::OffsetType OffsetType;

  typedef typename TInputImage::PixelType InputPixelType;

  typedef TOutputImage                     OutputImageType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  /** n-dimensional Kernel radius. */
  typedef typename TInputImage::SizeType      RadiusType;
  typedef typename TInputImage::SizeValueType RadiusValueType;

  virtual void SetRadius(const RadiusType & radius);

  virtual void SetRadius(const RadiusValueType & radius);

  itkGetConstReferenceMacro(Radius, RadiusType);

protected:
  BoxImageFilter();
  ~BoxImageFilter() ITK_OVERRIDE {}

  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BoxImageFilter);

  RadiusType m_Radius;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoxImageFilter.hxx"
#endif

#endif
