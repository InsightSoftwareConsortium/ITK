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

#ifndef itkKrcahEigenToMeasureParameterEstimationFilter_h
#define itkKrcahEigenToMeasureParameterEstimationFilter_h

#include "itkMath.h"
#include "itkEigenToMeasureParameterEstimationFilter.h"
#include "itkSimpleFastMutexLock.h"
#include "itkCompensatedSummation.h"

namespace itk {
/** \class KrcahEigenToMeasureParameterEstimationFilter
 * \brief Automatic parameter estimation as defined by Krcah et al.
 * 
 * This class takes an image of eigenvalues and estimates the parameters for the Krcah
 * bone enhanced filter. According to the original paper,
 *    \f{eqnarray*}{
 *      \alpha &=& 0.5 \\
 *      \beta &=& 0.5 \\
 *      \gamma &=& 0.25
 *    \f}
 * 
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
 * The parameters are estimated over the whole volume unless a mask is given.
 * If a mask is given, parameters are evaluated only where IsInside returns
 * true.
 * 
 * \sa KrcahEigenToMeasureImageFilter
 * \sa EigenToMeasureParameterEstimationFilter
 * \sa MultiScaleHessianEnhancementImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template<typename TInputImage, typename TInputSpatialObject >
class KrcahEigenToMeasureParameterEstimationFilter :
        public EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject > {
public:
  /** Standard Self typedef */
  typedef KrcahEigenToMeasureParameterEstimationFilter      Self;
  typedef EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject >
                                                            Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;
  
  /** Input typedefs */
  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;
  typedef typename Superclass::InputImagePixelType    InputImagePixelType;
  typedef typename Superclass::PixelValueType         PixelValueType;

  /** Output typedefs */
  typedef typename Superclass::OutputImageType        OutputImageType;
  typedef typename Superclass::OutputImageRegionType  OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType   OutputImagePixelType;

  /** Input SpatialObject typedefs. */
  typedef typename Superclass::SpatialObjectType          SpatialObjectType;
  typedef typename Superclass::SpatialObjectConstPointer  SpatialObjectConstPointer;

  /** Parameter typedefs */
  typedef typename Superclass::RealType               RealType;
  typedef typename Superclass::ParameterArrayType     ParameterArrayType;
  typedef typename Superclass::ParameterDecoratedType ParameterDecoratedType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(KrcahEigenToMeasureParameterEstimationFilter, EigenToMeasureParameterEstimationFilter);

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

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHaveDimension3Check,
                   ( Concept::SameDimension< TInputImage::ImageDimension, 3u >) );
  itkConceptMacro( InputFixedArrayHasDimension3Check,
                   ( Concept::SameDimension< TInputImage::PixelType::Dimension, 3u >) );
  // End concept checking
#endif
protected:
  KrcahEigenToMeasureParameterEstimationFilter();
  virtual ~KrcahEigenToMeasureParameterEstimationFilter() {}

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Do final mean and variance computation from data accumulated in threads. */
  void AfterThreadedGenerateData() ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
  void DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) ITK_OVERRIDE;

  /** Calculation of \f$ T \f$ changes depending on the implementation */
  inline RealType CalculateTraceAccordingToImplementation(InputImagePixelType pixel);
  inline RealType CalculateTraceAccordingToJournalArticle(InputImagePixelType pixel);

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
private:
  ITK_DISALLOW_COPY_AND_ASSIGN(KrcahEigenToMeasureParameterEstimationFilter);

  /* Member variables */
  KrcahImplementationType         m_ParameterSet;
  CompensatedSummation<RealType>  m_ThreadCount;
  CompensatedSummation<RealType>  m_ThreadAccumulatedTrace;

  SimpleFastMutexLock m_Mutex;
}; // end class
} /* end namespace */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKrcahEigenToMeasureParameterEstimationFilter.hxx"
#endif

#endif /* itkKrcahEigenToMeasureParameterEstimationFilter_h */
