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

#ifndef itkKrcahEigenToScalarParameterEstimationImageFilter_h
#define itkKrcahEigenToScalarParameterEstimationImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkArray.h"

namespace itk {
/** \class KrcahEigenToScalarParameterEstimationImageFilter
 * \brief Automatic parameter estimation for the Krcah method
 *
 * This class takes an image of eigenvalues and estimates the parameters for the Krcah
 * bone enhanced filter. According to the original paper,
 *    \f{eqnarray*}{
 *      \alpha &=& 0.5 \\
 *      \beta &=& 0.5 \\
 *      \gamma &=& 0.25
 *    \f}
 * However, this does not include scaling \f$ R_noise \f$ by the trace of the matrix.
 * The trace of the matrix can be computed directly from the eigenvalues as the sum
 * of the eigenvalues.
 * 
 * In this implementation, the average of the trace is moved into the constant 
 * \f$ \gamma \f$. This is done to seperate parameter estimation from the unary
 * functor. The modification is very simple. If the average of the trace is
 * denoted \f$ T \f$ the new parameter becomes:
 *  \f{
 *      \gamma &=& 0.25 \cdot T
 *  \f}
 * 
 * However, the code was implemented different than described in the original
 * paper. In the implementation, \f$ R_noise \f$ is scaled by the sum
 * of the absolute value of the eigenvalues NOT the sum of the eigenvalues.
 * Furthermore, all parameters were scaled by a factor of 2 in the implementation
 * and \f$ \gamma \f$ was set to 0.5 of the average sum of absolute value of the
 * eigenvalues. To account for these discrepancies, the flag KrcahImplementationType
 * can be set which enables the implementation parameters:
 *    \f{eqnarray*}{
 *      \alpha &=& \sqrt{2} \cdot 0.5 \\
 *      \beta &=& \sqrt{2} \cdot 0.5 \\
 *      \gamma &=& \sqrt{2} \cdot 0.5 \cdot T
 *    \f}
 * Default is to use parameters from the implementation, not the paper.
 * 
 * Finally, a mask can be provided. Metal or non-standard objects in the field
 * of view may throw off the parameter estimation. In those cases, a mask can
 * be provided so those voxels are not factored into the calculation of \f$ T \f$.
 * 
 * If the input image and mask have different regions over which they
 * are defined, parameters are estimated only in the intersection of
 * the two image regions. However, the mask region must be a proper sub
 * subset (contained) in the image region.
 * 
 * \sa KrcahEigenToScalarImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TMaskImage >
class ITK_TEMPLATE_EXPORT KrcahEigenToScalarParameterEstimationImageFilter:
public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(KrcahEigenToScalarParameterEstimationImageFilter);

  /** Standard Self type alias */
  using Self = KrcahEigenToScalarParameterEstimationImageFilter;
  using Superclass = ImageToImageFilter< TInputImage, TInputImage >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KrcahEigenToScalarParameterEstimationImageFilter, ImageToImageFilter);

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;
  using InputRegionType = typename TInputImage::RegionType;
  using InputSizeType = typename TInputImage::SizeType;
  using InputIndexType = typename TInputImage::IndexType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputPixelValueType = typename InputPixelType::ValueType;

  /** Output region definitions */
  using OutputRegionType = InputRegionType;

  /** Mask related type alias. */
  using MaskImagePointer = typename TMaskImage::Pointer;
  using MaskImageConstPointer = typename TMaskImage::ConstPointer;
  using MaskPixelType = typename TMaskImage::PixelType;
  using MaskRegionType = typename TMaskImage::RegionType;

  /** Methods to set/get the mask image */
  itkSetInputMacro(MaskImage, TMaskImage);
  itkGetInputMacro(MaskImage, TMaskImage);

  /** Iterator types */
  itkSetMacro(BackgroundValue, MaskPixelType);
  itkGetConstMacro(BackgroundValue, MaskPixelType);

  typedef enum {
    UseImplementationParameters = 1,
    UseJournalParameters
  } KrcahImplementationType;
  itkSetMacro(ParameterSet, KrcahImplementationType);
  itkGetConstMacro(ParameterSet, KrcahImplementationType);

  /* Set parameter set */
  void SetParameterSetToImplementation() {
    this->SetParameterSet(UseImplementationParameters);
  }

  void SetParameterSetToJournalArticle() {
    this->SetParameterSet(UseJournalParameters);
  }

  /** Parameters */
  using RealType = typename NumericTraits< InputPixelValueType >::RealType;
  using RealTypeDecoratedType = SimpleDataObjectDecorator< RealType >;

  /** Decorators for parameters so they can be passed as a process object */
  RealTypeDecoratedType * GetAlphaOutput();
  const RealTypeDecoratedType * GetAlphaOutput() const;
  RealTypeDecoratedType * GetBetaOutput();
  const RealTypeDecoratedType * GetBetaOutput() const;
  RealTypeDecoratedType * GetGammaOutput();
  const RealTypeDecoratedType * GetGammaOutput() const;

  /** Standard getters for the output parameters */
  RealType GetAlpha() const
  {
    return this->GetAlphaOutput()->Get();
  }
  RealType GetBeta() const
  {
    return this->GetBetaOutput()->Get();
  }
  RealType GetGamma() const
  {
    return this->GetGammaOutput()->Get();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHaveDimension3Check,
                   ( Concept::SameDimension< TInputImage::ImageDimension, 3u >) );
  // End concept checking
#endif
protected:
  KrcahEigenToScalarParameterEstimationImageFilter();
  virtual ~KrcahEigenToScalarParameterEstimationImageFilter() {}

  /** Pass the input through unmodified. Do this by Grafting in the AllocateOutputs method. */
  void AllocateOutputs() override;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() override;

  /** Do final mean and variance computation from data accumulated in threads. */
  void AfterThreadedGenerateData() override;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputRegionType &
                             outputRegionForThread,
                             ThreadIdType threadId) override;

  /** Override since the filter needs all the data for the algorithm */
  void GenerateInputRequestedRegion() override;

  /** Override since the filter produces all of its output */
  void EnlargeOutputRequestedRegion(DataObject *data) override;

  void PrintSelf(std::ostream & os, Indent indent) const override;

  /** Calculation of \f$ T \f$ changes depending on the implementation */
  inline RealType CalculateTraceAccordingToImplementation(InputPixelType pixel);
  inline RealType CalculateTraceAccordingToJournalArticle(InputPixelType pixel);

private:
  /* Parameters */
  KrcahImplementationType m_ParameterSet{UseImplementationParameters};

  /* Inputs */
  MaskPixelType m_BackgroundValue{NumericTraits< MaskPixelType >::Zero};

  /* Arrays for threads */
  Array< RealType >       m_AccumulatedAverageTrace{1};
  Array< SizeValueType >  m_NumVoxels{1};
}; //end class
} // end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKrcahEigenToScalarParameterEstimationImageFilter.hxx"
#endif

#endif // itkKrcahEigenToScalarParameterEstimationImageFilter_h
