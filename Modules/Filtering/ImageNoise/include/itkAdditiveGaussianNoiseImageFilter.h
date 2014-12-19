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
#ifndef itkAdditiveGaussianNoiseImageFilter_h
#define itkAdditiveGaussianNoiseImageFilter_h

#include "itkNoiseBaseImageFilter.h"

namespace itk
{

/** \class AdditiveGaussianNoiseImageFilter
 *
 * \brief Alter an image with additive gaussian white noise.
 *
 * \author Gaetan Lehmann
 *
 * This code was contributed in the Insight Journal paper "Noise
 * Simulation". http://hdl.handle.net/10380/3158
 *
 * \ingroup ITKImageNoise
 */
template <class TInputImage, class TOutputImage=TInputImage>
class AdditiveGaussianNoiseImageFilter :
  public NoiseBaseImageFilter<TInputImage,TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef AdditiveGaussianNoiseImageFilter                 Self;
  typedef NoiseBaseImageFilter< TInputImage,TOutputImage > Superclass;
  typedef SmartPointer<Self>                               Pointer;
  typedef SmartPointer<const Self>                         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AdditiveGaussianNoiseImageFilter, NoiseBaseImageFilter);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageType       OutputImageType;
  typedef typename Superclass::OutputImagePointer    OutputImagePointer;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType  OutputImagePixelType;

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  itkGetConstMacro(Mean, double);
  itkSetMacro(Mean, double);

  itkGetConstMacro(StandardDeviation, double);
  itkSetMacro(StandardDeviation, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType,
                                        typename TOutputImage::PixelType>) );
  /** End concept checking */
#endif

protected:
  AdditiveGaussianNoiseImageFilter();
  virtual ~AdditiveGaussianNoiseImageFilter() {
  }

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType threadId ) ITK_OVERRIDE;

private:
  AdditiveGaussianNoiseImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&);                   //purposely not implemented

  double m_Mean;
  double m_StandardDeviation;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAdditiveGaussianNoiseImageFilter.hxx"
#endif

#endif
