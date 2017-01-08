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
#ifndef itkGaussianImageSource_h
#define itkGaussianImageSource_h

#include "itkParametricImageSource.h"
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
 * \ingroup ITKImageSources
 */
template< typename TOutputImage >
class ITK_TEMPLATE_EXPORT GaussianImageSource :
  public ParametricImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef GaussianImageSource                   Self;
  typedef ParametricImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** Typedef for the output image type. */
  typedef TOutputImage                     OutputImageType;

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

  /** Type used to store Gaussian parameters. */
  typedef FixedArray< double, itkGetStaticConstMacro(NDimensions) > ArrayType;

  /** Size type matches that used for images */
  typedef typename TOutputImage::SizeType      SizeType;
  typedef typename TOutputImage::SizeValueType SizeValueType;

  /** Types for parameters. */
  typedef typename Superclass::ParametersValueType ParametersValueType;
  typedef typename Superclass::ParametersType      ParametersType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianImageSource, ParametricImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Gets and sets for Gaussian parameters */

  /** Set/Get the scale factor to multiply the true value of the Gaussian. */
  itkSetMacro(Scale, double);
  itkGetConstReferenceMacro(Scale, double);

  /** Set/Get whether or not to normalize the Gaussian. Default is false. */
  itkSetMacro(Normalized, bool);
  itkGetConstReferenceMacro(Normalized, bool);
  itkBooleanMacro(Normalized);

  /** Set/Get the standard deviation in each direction. */
  itkSetMacro(Sigma, ArrayType);
  itkGetConstReferenceMacro(Sigma, ArrayType);

  /** Set/Get the mean in each direction. */
  itkSetMacro(Mean, ArrayType);
  itkGetConstReferenceMacro(Mean, ArrayType);

  /** Set/Get the parameters for this source. When this source is
   * templated over an N-dimensional output image type, the first N
   * values in the parameter array are the Sigma parameters in each
   * dimension, the next N values are the Mean parameters in each
   * dimension, and the last value is the Scale. */
  virtual void SetParameters(const ParametersType & parameters) ITK_OVERRIDE;
  virtual ParametersType GetParameters() const ITK_OVERRIDE;

  /** Get the number of parameters for this image source. When this
   * source is templated over an N-dimensional output image type, the
   * number of parameters is 2*N+1. */
  virtual unsigned int GetNumberOfParameters() const ITK_OVERRIDE;

protected:
  GaussianImageSource();
  // ~GaussianImageSource(); default implementation ok
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianImageSource);

  ArrayType m_Sigma;

  ArrayType m_Mean;

  double m_Scale;

  bool m_Normalized;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianImageSource.hxx"
#endif

#endif
