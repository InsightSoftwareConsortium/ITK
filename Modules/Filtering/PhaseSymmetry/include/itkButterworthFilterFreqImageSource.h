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
#ifndef __itkButterworthFilterFreqImageSource_h
#define __itkButterworthFilterFreqImageSource_h

#include "itkImageSource.h"
#include "itkFixedArray.h"
#include "itkSize.h"
#include "itkArray2D.h"


namespace itk
{

/** \class ButterworthFilterFreqImageSource
 *
 * \ingroup PhaseSymmetry
 */
template <typename TOutputImage>
class ButterworthFilterFreqImageSource : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ButterworthFilterFreqImageSource Self;
  typedef ImageSource<TOutputImage>        Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ButterworthFilterFreqImageSource, ImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Dimensionality of the output image. */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  typedef TOutputImage OutputImageType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  typedef typename TOutputImage::SpacingType SpacingType;

  /** Origin typedef support.  The origin is the geometric coordinates
   * of the index (0,0). */
  typedef typename TOutputImage::PointType PointType;

  /** Direction typedef support.  The direction is the direction
   * cosines of the image. */
  typedef typename TOutputImage::DirectionType DirectionType;

  /** Size type matches that used for images */
  typedef typename TOutputImage::SizeType SizeType;

  /** Specify the size of the output image. */
  virtual void
  SetSize(const SizeType & size);
  /** Get the size of the output image. */
  itkGetConstReferenceMacro(Size, SizeType);

  /** Specify the spacing of the output image. */
  virtual void
  SetSpacing(const SpacingType & spacing);
  /** Get the spacing of the output image. */
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Specify the origin of the output image. */
  virtual void
  SetOrigin(const PointType & origin);
  /** Get the origin of the output image. */
  itkGetConstReferenceMacro(Origin, PointType);

  /** Specify the direction of the output image. */
  virtual void
  SetDirection(const DirectionType & direction);
  itkGetConstReferenceMacro(Direction, DirectionType);

  /** Set/Get the cutoff frequency */
  itkSetMacro(Cutoff, double);
  itkGetConstMacro(Cutoff, double);

  /** Set/Get the filter order */
  itkSetMacro(Order, double);
  itkGetConstMacro(Order, double);

protected:
  ButterworthFilterFreqImageSource();
  virtual ~ButterworthFilterFreqImageSource();

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  typedef FixedArray<double, ImageDimension> DoubleArrayType;

  virtual void
  PrintSelf(std::ostream & os, Indent indent) const;
  virtual void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType tid) ITK_OVERRIDE;
  virtual void
  GenerateOutputInformation() ITK_OVERRIDE;

private:
  ButterworthFilterFreqImageSource(const ButterworthFilterFreqImageSource &); // purposely not implemented
  void
  operator=(const ButterworthFilterFreqImageSource &); // purposely not implemented

  SizeType      m_Size;      // size of the output image
  SpacingType   m_Spacing;   // spacing
  PointType     m_Origin;    // origin
  DirectionType m_Direction; // direction

  double m_Cutoff;
  double m_Order;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkButterworthFilterFreqImageSource.hxx"
#endif

#endif
