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
#ifndef __itkLogGaborFreqImageSource_h
#define __itkLogGaborFreqImageSource_h

#include "itkGenerateImageSource.h"

namespace itk
{

/** \class LogGaborFreqImageSource
 *
 * \ingroup PhaseSymmetry
 */
template <typename TOutputImage>
class LogGaborFreqImageSource : public GenerateImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef LogGaborFreqImageSource           Self;
  typedef GenerateImageSource<TOutputImage> Superclass;
  typedef SmartPointer<Self>                Pointer;
  typedef SmartPointer<const Self>          ConstPointer;

  /** Dimensionality of the output image */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  typedef typename TOutputImage::RegionType    OutputImageRegionType;
  typedef typename TOutputImage::SizeType      SizeType;
  typedef typename TOutputImage::SpacingType   SpacingType;
  typedef typename TOutputImage::PointType     PointType;
  typedef typename TOutputImage::DirectionType DirectionType;

  typedef FixedArray<double, ImageDimension> ArrayType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LogGaborFreqImageSource, GenerateImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get the Gaussian width parameter. */
  itkSetMacro(Sigma, double);
  itkGetConstMacro(Sigma, double);

  /** Set/Get the wavelengths in each direction. */
  itkSetMacro(Wavelengths, ArrayType);
  itkGetConstReferenceMacro(Wavelengths, ArrayType);

protected:
  LogGaborFreqImageSource();
  virtual ~LogGaborFreqImageSource();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
  virtual void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  // Ratio of k/wo in each direction
  double m_Sigma;

  // The wavelengths in each direction
  ArrayType m_Wavelengths;

  LogGaborFreqImageSource(const LogGaborFreqImageSource &); // purposely not implemented
  void
  operator=(const LogGaborFreqImageSource &); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLogGaborFreqImageSource.hxx"
#endif

#endif
