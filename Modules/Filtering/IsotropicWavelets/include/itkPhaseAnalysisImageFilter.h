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
#ifndef itkPhaseAnalysisImageFilter_h
#define itkPhaseAnalysisImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkVectorImage.h>
#include "itkImageScanlineIterator.h"
#include "itkImageScanlineConstIterator.h"
#include <itkImage.h>
#include <itkFixedArray.h>
namespace itk
{
/** \class PhaseAnalysisImageFilter
 * Base class for phase analysis filters.
 * The output represents the phase of some image.
 * The input is a VectorImage where first component is the original.
 * The phase value can be defined in multiple ways but it always represents local structural
 * information.
 * Other outputs might be the amplitude or the orientation.
 * Doing phase analysis usually involves having an original image \f$I\f$, and
 * a vector \f$\mathbf{F} = (f_1, ..., f_N)\f$ storing the results of applying some
 * directional or feature filters to \f$I\f$
 * For example, F can be directional derivatives, Riesz filter, a feature filter at
 * different angles, etc.
 * There is some freedom to define the phase of a pixel, but usually involves a ratio between
 * the amplitudes of \f$I\f$ and the vector \f$F\f$.
 * This class uses:
 * Amplitude:
\f[
 A(\mathbf{x_0})= \sqrt{I(\mathbf{x_0})^2 + A_F(\mathbf{x_0})^2 }
\f]
\f[
 A_F(\mathbf{x_0})=  \sqrt{\sum_{i=1}^N f_i(\mathbf{x_0})^2}
\f]
Phase:
\f[
 P(\mathbf{x_0})= \text{atan2}(A_F(\mathbf{x_0}),A(\mathbf{x_0}))
\f]
Orientation:
\begin{align*}
 O_j(\mathbf{x_0})&= \text{atan2}(\hat{f_j}(\mathbf{x_0}),\hat{f_1}(\mathbf{x_0}))\\
&\text{where } \hat{f_i} = f_i / A_F
\end{align*}
 *
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage,
          typename TOutputImage = Image<typename TInputImage::PixelType::ComponentType, TInputImage::ImageDimension>>
class PhaseAnalysisImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PhaseAnalysisImageFilter                      Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(PhaseAnalysisImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType;
  typedef typename InputImageType::PixelType      InputImagePixelType;
  typedef typename InputImageType::SpacingType    SpacingType;
  typedef typename InputImageRegionType::SizeType SizeType;

  typedef SpacingType                               DirectionType;
  typedef typename InputImageType::SpacingValueType FloatType;

  typedef typename OutputImageType::Pointer                        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer                   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType                     OutputImageRegionType;
  typedef typename itk::ImageScanlineIterator<OutputImageType>     OutputImageRegionIterator;
  typedef typename OutputImageType::PixelType                      OutputImagePixelType;
  typedef typename itk::ImageScanlineConstIterator<InputImageType> InputImageRegionConstIterator;

#ifdef ITK_USE_CONCEPT_CHECKING
  /// This ensure that PixelType is float||double, and not complex.
  itkConceptMacro(OutputPixelTypeIsFloatCheck, (Concept::IsFloatingPoint<typename TOutputImage::PixelType>));
#endif
  const OutputImageType *
  GetOutputPhase() const
  {
    return this->GetOutput(0);
  }

  OutputImageType *
  GetOutputPhase()
  {
    return this->GetOutput(0);
  }

  const OutputImageType *
  GetOutputAmplitude() const
  {
    return this->GetOutput(1);
  }

  OutputImageType *
  GetOutputAmplitude()
  {
    return this->GetOutput(1);
  }

protected:
  PhaseAnalysisImageFilter();
  ~PhaseAnalysisImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  inline OutputImagePixelType
  ComputeFeatureVectorNormSquare(const InputImagePixelType & inputPixel) const
  {
    const unsigned int & nC = this->GetInput()->GetNumberOfComponentsPerPixel();
    OutputImagePixelType out(0);

    for (unsigned int r = 1; r < nC; r++)
    {
      out += inputPixel[r] * inputPixel[r];
    }
    return out;
  }

  /**************** Helpers requiring the square norm of Riesz *******************/
  inline OutputImagePixelType
  ComputeAmplitude(const InputImagePixelType & inputPixel, const OutputImagePixelType & featureAmpSquare) const
  {
    return sqrt(inputPixel[0] * inputPixel[0] + featureAmpSquare);
  }

  inline OutputImagePixelType
  ComputePhase(const InputImagePixelType & inputPixel, const OutputImagePixelType & featureAmpSquare) const
  {
    return atan2(sqrt(featureAmpSquare), inputPixel[0]);
  }

  itk::FixedArray<OutputImagePixelType, ImageDimension - 1>
  ComputePhaseOrientation(const InputImagePixelType & inputPixel, const OutputImagePixelType & featureAmpSquare) const
  {
    // the angles of the polar coordinates of the normed vector:
    // V = (R1*f, ..., Rn*f) / FeatureNorm
    FixedArray<OutputImagePixelType, ImageDimension - 1> out;
    out.Fill(NumericTraits<OutputImagePixelType>::ZeroValue());
    OutputImagePixelType fNorm = sqrt(featureAmpSquare);
    OutputImagePixelType f1Unitary = inputPixel[1] / fNorm;
    for (unsigned int i = 0; i < ImageDimension - 1; i++)
    {
      out[i] = atan2(inputPixel[i + 2] / fNorm, f1Unitary) + ((inputPixel[i + 2] >= 0) ? 0 : itk::Math::pi);
    }
    return out;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhaseAnalysisImageFilter);
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhaseAnalysisImageFilter.hxx"
#endif

#endif
