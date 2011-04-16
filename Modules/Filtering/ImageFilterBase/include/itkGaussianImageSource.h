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
#ifndef __itkGaussianImageSource_h
#define __itkGaussianImageSource_h

#include "itkImageSource.h"
#include "itkFixedArray.h"
#include "itkSize.h"

namespace itk
{
/** \class GaussianImageSource
 * \brief Generate an n-dimensional image of a Gaussian.
 *
 * GaussianImageSource generates an image of a Gaussian.
 * m_Normalized determines whether or not the Gaussian is normalized
 * (whether or not the sum over infinite space is 1.0)
 * When creating an image, it is preferable to _not_ normalize the Gaussian
 * m_Scale scales the output of the Gaussian to span a range
 * larger than 0->1, and is typically set to the maximum value
 * of the output data type (for instance, 255 for uchars)
 *
 * The output image may be of any dimension.
 *
 * \ingroup DataSources
 * \ingroup ITK-ImageFilterBase
 */
template< typename TOutputImage >
class ITK_EXPORT GaussianImageSource:public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef GaussianImageSource         Self;
  typedef ImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

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

  /** Dimensionality of the output image */
  itkStaticConstMacro(NDimensions, unsigned int, TOutputImage::ImageDimension);

  /** Type used to store gaussian parameters. */
  typedef FixedArray< double, itkGetStaticConstMacro(NDimensions) > ArrayType;

  /** Size type matches that used for images */
  typedef typename TOutputImage::SizeType      SizeType;
  typedef typename TOutputImage::SizeValueType SizeValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianImageSource, ImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Specify the size of the output image. */
  itkSetMacro(Size, SizeType);
  itkSetVectorMacro(Size, SizeValueType, NDimensions);

  /** Get the size of the output image. */
  itkGetConstReferenceMacro(Size, SizeType);

  /** Specify the spacing of the output image. */
  itkSetMacro(Spacing, SpacingType);
  itkSetVectorMacro(Spacing, const float, NDimensions);

  /** Get the spacing of the output image. */
  itkGetConstReferenceMacro(Spacing, SpacingType);

  /** Specify the origin of the output image. */
  itkSetMacro(Origin, PointType);
  itkSetVectorMacro(Origin, const float, NDimensions);

  /** Get the origin of the output image. */
  itkGetConstReferenceMacro(Origin, PointType);

  /** Specify the direction of the output image. */
  itkSetMacro(Direction, DirectionType);
  itkGetConstReferenceMacro(Direction, DirectionType);

  /** Gets and sets for gaussian parameters */
  itkSetMacro(Scale, double);
  itkGetConstReferenceMacro(Scale, double);
  itkSetMacro(Normalized, bool);
  itkGetConstReferenceMacro(Normalized, bool);
  itkSetMacro(Sigma, ArrayType);
  itkGetConstReferenceMacro(Sigma, ArrayType);
  itkSetMacro(Mean, ArrayType);
  itkGetConstReferenceMacro(Mean, ArrayType);
protected:
  GaussianImageSource();
  ~GaussianImageSource();
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

  virtual void GenerateOutputInformation();

private:
  GaussianImageSource(const GaussianImageSource &); //purposely not implemented
  void operator=(const GaussianImageSource &);      //purposely not implemented

  SizeType      m_Size;              //size of the output image
  SpacingType   m_Spacing;           //spacing
  PointType     m_Origin;            //origin
  DirectionType m_Direction;         // direction

  /** Parameters for the Gaussian. */

  /** The standard deviation in each direction. */
  ArrayType m_Sigma;

  /** The mean in each direction. */
  ArrayType m_Mean;

  /** A scale factor multiplied by the true value of the Gaussian. */
  double m_Scale;

  /** Whether or not to normalize the Gaussian. */
  bool m_Normalized;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianImageSource.txx"
#endif

#endif
