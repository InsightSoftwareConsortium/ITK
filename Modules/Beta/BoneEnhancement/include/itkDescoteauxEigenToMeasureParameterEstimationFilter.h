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

#ifndef itkDescoteauxEigenToMeasureParameterEstimationFilter_h
#define itkDescoteauxEigenToMeasureParameterEstimationFilter_h

#include "itkMath.h"
#include "itkEigenToMeasureParameterEstimationFilter.h"

namespace itk {
/** \class DescoteauxEigenToMeasureParameterEstimationFilter
 * \brief Automatic parameter estimation as defined by Descoteaux et al.
 * 
 * The default parameters are:
 *   \f{eqnarray*}{
 *      \alpha &=& 0.5 \\
 *      \beta &=& 0.5 \\
 *      \gamma &=& 0.5 max\( Frobenius norm \)
 *   \f}
 * 
 * Where the Frobenius norm for a real, symmetric matrix is given by
 * the square root of the sum of squares of the eigenvalues.
 * 
 * The parameters are estimated over the whole volume unless a mask is given.
 * If a mask is given, parameters are evaluated only where IsInside returns
 * true.
 * 
 * \sa DescoteauxEigenToMeasureImageFilter
 * \sa EigenToMeasureParameterEstimationFilter
 * \sa MultiScaleHessianEnhancementImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template<typename TInputImage, typename TInputSpatialObject >
class DescoteauxEigenToMeasureParameterEstimationFilter :
        public EigenToMeasureParameterEstimationFilter< TInputImage, TInputSpatialObject > {
public:
  /** Standard Self typedef */
  typedef DescoteauxEigenToMeasureParameterEstimationFilter Self;
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
  itkTypeMacro(DescoteauxEigenToMeasureParameterEstimationFilter, EigenToMeasureParameterEstimationFilter);

  /** Setter/Getter methods for setting FrobeniusNormWeight */
  itkSetMacro(FrobeniusNormWeight, RealType);
  itkGetConstMacro(FrobeniusNormWeight, RealType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHaveDimension3Check,
                   ( Concept::SameDimension< TInputImage::ImageDimension, 3u >) );
  itkConceptMacro( InputFixedArrayHasDimension3Check,
                   ( Concept::SameDimension< TInputImage::PixelType::Dimension, 3u >) );
  // End concept checking
#endif
protected:
  DescoteauxEigenToMeasureParameterEstimationFilter();
  virtual ~DescoteauxEigenToMeasureParameterEstimationFilter() {}

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Do final mean and variance computation from data accumulated in threads. */
  void AfterThreadedGenerateData() ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  inline RealType CalculateFrobeniusNorm(const InputImagePixelType& pixel) const;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DescoteauxEigenToMeasureParameterEstimationFilter);

  /* Member variables */
  RealType          m_FrobeniusNormWeight;
  Array< RealType > m_MaxFrobeniusNorm;
}; // end class
} /* end namespace */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDescoteauxEigenToMeasureParameterEstimationFilter.hxx"
#endif

#endif /* itkDescoteauxEigenToMeasureParameterEstimationFilter_h */
