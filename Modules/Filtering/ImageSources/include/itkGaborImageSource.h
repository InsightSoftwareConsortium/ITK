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
#ifndef itkGaborImageSource_h
#define itkGaborImageSource_h

#include "itkGenerateImageSource.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class GaborImageSource
 * \brief Generate an n-dimensional image of a Gabor filter.
 *
 * GaborImageSource generates an image of either the real
 * (i.e. symmetric) or complex (i.e. antisymmetric) part
 * of the Gabor filter with the orientation directed along
 * the x-axis.  The GaborKernelFunction is used to evaluate
 * the contribution along the x-axis whereas a non-normalized
 * 1-D Gaussian envelope provides the contribution in each of
 * the remaining N dimensions.  Orientation can be manipulated
 * via the Transform classes of the toolkit.
 *
 * The output image may be of any dimension.
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://hdl.handle.net/1926/500
 *
 * \ingroup DataSources
 * \ingroup ITKImageSources
 */
template< typename TOutputImage >
class GaborImageSource:
  public GenerateImageSource< TOutputImage >
{
public:

  /** Standard class typedefs. */
  typedef GaborImageSource                    Self;
  typedef GenerateImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Output image typedefs */
  typedef TOutputImage                            OutputImageType;
  typedef typename OutputImageType::PixelType     PixelType;
  typedef typename OutputImageType::RegionType    RegionType;
  typedef typename OutputImageType::SpacingType   SpacingType;
  typedef typename OutputImageType::PointType     PointType;
  typedef typename OutputImageType::DirectionType DirectionType;

  typedef typename RegionType::SizeType SizeType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaborImageSource, GenerateImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Dimensionality of the output image */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      OutputImageType::ImageDimension);

  /** Type used to store gabor parameters. */
  typedef FixedArray< double,
                      itkGetStaticConstMacro(ImageDimension) >    ArrayType;

  itkSetMacro(Sigma, ArrayType);
  itkGetConstReferenceMacro(Sigma, ArrayType);

  itkSetMacro(Mean, ArrayType);
  itkGetConstReferenceMacro(Mean, ArrayType);

  itkSetMacro(Frequency, double);
  itkGetConstReferenceMacro(Frequency, double);

  itkSetMacro(CalculateImaginaryPart, bool);
  itkGetConstReferenceMacro(CalculateImaginaryPart, bool);
  itkBooleanMacro(CalculateImaginaryPart);

protected:
  GaborImageSource();
  // ~GaborImageSource(); default implementation ok
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

private:
  GaborImageSource(const GaborImageSource &); //purposely not implemented
  void operator=(const GaborImageSource &);   //purposely not implemented

  /** Parameters for the Gabor. */

  /** Evaluate using the complex part */
  bool m_CalculateImaginaryPart;

  /** Modulation frequency of the sine or cosine component */
  double m_Frequency;

  /** Evaluate using a stretched gabor filter (ensure zero dc response) */
  double m_PhaseOffset;

  /** The standard deviation in each direction. */
  ArrayType m_Sigma;

  /** The mean in each direction. */
  ArrayType m_Mean;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaborImageSource.hxx"
#endif

#endif
