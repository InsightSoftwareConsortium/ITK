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
  /** Standard Self typedef */
  typedef KrcahEigenToScalarParameterEstimationImageFilter      Self;
  typedef ImageToImageFilter< TInputImage, TInputImage >        Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KrcahEigenToScalarParameterEstimationImageFilter, ImageToImageFilter);

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer       InputImagePointer;
  typedef typename TInputImage::ConstPointer  InputImageConstPointer;
  typedef typename TInputImage::RegionType    InputRegionType;
  typedef typename TInputImage::SizeType      InputSizeType;
  typedef typename TInputImage::IndexType     InputIndexType;
  typedef typename TInputImage::PixelType     InputPixelType;
  typedef typename InputPixelType::ValueType  InputPixelValueType;

  /** Output region definitions */
  typedef InputRegionType OutputRegionType;

  /** Mask related typedefs. */
  typedef typename TMaskImage::Pointer      MaskImagePointer;
  typedef typename TMaskImage::ConstPointer MaskImageConstPointer;
  typedef typename TMaskImage::PixelType    MaskPixelType;
  typedef typename TMaskImage::RegionType   MaskRegionType;

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
  typedef typename NumericTraits< InputPixelValueType >::RealType RealType;
  typedef SimpleDataObjectDecorator< RealType >                   RealTypeDecoratedType;

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
  void AllocateOutputs() ITK_OVERRIDE;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Do final mean and variance computation from data accumulated in threads. */
  void AfterThreadedGenerateData() ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputRegionType &
                             outputRegionForThread,
                             ThreadIdType threadId) ITK_OVERRIDE;

  /** Override since the filter needs all the data for the algorithm */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Override since the filter produces all of its output */
  void EnlargeOutputRequestedRegion(DataObject *data) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Calculation of \f$ T \f$ changes depending on the implementation */
  inline RealType CalculateTraceAccordingToImplementation(InputPixelType pixel);
  inline RealType CalculateTraceAccordingToJournalArticle(InputPixelType pixel);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(KrcahEigenToScalarParameterEstimationImageFilter);

  /* Parameters */
  KrcahImplementationType m_ParameterSet;

  /* Inputs */
  MaskPixelType m_BackgroundValue;

  /* Arrays for threads */
  Array< RealType >       m_AccumulatedAverageTrace;
  Array< SizeValueType >  m_NumVoxels;
}; //end class
} // end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKrcahEigenToScalarParameterEstimationImageFilter.hxx"
#endif

#endif // itkKrcahEigenToScalarParameterEstimationImageFilter_h
