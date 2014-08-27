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
#ifndef __itkSteerableFilterFreqImageSource_h
#define __itkSteerableFilterFreqImageSource_h

#include "itkImageSource.h"
#include "itkFixedArray.h"
#include "itkSize.h"
#include "itkArray2D.h"

#include <vector>
#include <complex>


namespace itk
{

template <typename TOutputImage>
class SteerableFilterFreqImageSource : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef SteerableFilterFreqImageSource Self;
  typedef ImageSource<TOutputImage>      Superclass;
  typedef SmartPointer<Self>             Pointer;
  typedef SmartPointer<const Self>       ConstPointer;

  /** Typedef for the output image PixelType. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Spacing typedef support.  Spacing holds the size of a pixel.  The
   * spacing is the geometric distance between image samples. */
  typedef typename TOutputImage::SpacingType SpacingType;

  /** Origin typedef support.  The origin is the geometric coordinates
   * of the index (0,0). */
  typedef typename TOutputImage::PointType PointType;

  /** Direction typedef support.  The direction is the direction
   * cosines of the image. */
  typedef typename TOutputImage::DirectionType DirectionType;

  typedef std::vector<std::vector<double>> RangeType;

  /** Dimensionality of the output image */
  itkStaticConstMacro(NDimensions, unsigned int, TOutputImage::ImageDimension);

  /** Type used to store gaussian parameters. */


  // Type used to store the range for each axis

  /** Size type matches that used for images */
  typedef typename TOutputImage::SizeType      SizeType;
  typedef typename TOutputImage::SizeValueType SizeValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SteerableFilterFreqImageSource, ImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Specify the size of the output image. */
  virtual void
  SetSize(const SizeValueType * values);

  /** Specify the size of the output image. */
  virtual void
  SetSize(const SizeType values);

  /** Get the size of the output image. */
  itkGetVectorMacro(Size, const SizeValueType, NDimensions);

  /** Specify the spacing of the output image. */
  itkSetMacro(Spacing, SpacingType);
  virtual void
  SetSpacing(const float * values);
  virtual void
  SetSpacing(const double * values);

  /** Get the spacing of the output image. */
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Specify the origin of the output image. */
  itkSetMacro(Origin, PointType);
  virtual void
  SetOrigin(const float * values);
  virtual void
  SetOrigin(const double * values);

  /** Get the origin of the output image. */
  itkGetConstReferenceMacro(Origin, PointType);

  /** Specify the direction of the output image. */
  itkSetMacro(Direction, DirectionType);

  itkGetConstReferenceMacro(Direction, DirectionType);

  // itkSetMacro(Ranges, RangeType);

  typedef FixedArray<double, TOutputImage::ImageDimension - 1> DimMinusOneDoubleArrayType;
  typedef FixedArray<double, TOutputImage::ImageDimension>     DoubleArrayType;

  itkSetMacro(Orientation, DoubleArrayType);
  itkGetConstReferenceMacro(Orientation, DoubleArrayType);

  itkSetMacro(AngularBandwidth, double);
  itkGetConstReferenceMacro(AngularBandwidth, double);

protected:
  SteerableFilterFreqImageSource();
  ~SteerableFilterFreqImageSource();
  void
  PrintSelf(std::ostream & os, Indent indent) const;
  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType tid);
  virtual void
  GenerateOutputInformation();

private:
  SteerableFilterFreqImageSource(const SteerableFilterFreqImageSource &); // purposely not implemented
  void
  operator=(const SteerableFilterFreqImageSource &); // purposely not implemented

  SizeValueType m_Size[NDimensions]; // size of the output image
  SpacingType   m_Spacing;           // spacing
  PointType     m_Origin;            // origin
  DirectionType m_Direction;         // direction

  DoubleArrayType m_Orientation;
  double          m_AngularBandwidth;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSteerableFilterFreqImageSource.hxx"
#endif

#endif
