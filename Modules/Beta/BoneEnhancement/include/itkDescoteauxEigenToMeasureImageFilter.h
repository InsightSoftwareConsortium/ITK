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

#ifndef itkDescoteauxEigenToMeasureImageFilter_h
#define itkDescoteauxEigenToMeasureImageFilter_h

#include "itkEigenToMeasureImageFilter.h"
#include "itkMath.h"

namespace itk {
/** \class DescoteauxEigenToMeasureImageFilter
 * \brief Convert eigenvalues into a measure of sheetness according to the method of Descoteaux et al.
 * 
 * Converts a 3D fixed array of eigenvalues into a measure of sheetness according to the method
 * of Descoteaux et al. The parameters of the filter should be set using DescoteauxEigentoScalarParameterEstimationImageFilter.
 * 
 * Computes the following equation for eigenvalues in a three dimensional fixed array:
 *  \f{eqnarray*}{
 *      R_{sheet} &=&  \frac{|\lambda_2|}{\lambda_3|} \\
 *      R_{blob} &=&  \frac{|2 |\lambda_3| - |\lambda_2| - |\lambda_1| |}{|\lambda_3|} \\
 *      R_{noise} &=&  \sqrt{|\lambda_1|^2 + |\lambda_2|^2 + \lambda_3|^2} \\
 *      s &=& \exp\left(- \frac{R_{sheet}^2}{\alpha^2} \right) \left(1 - \exp\left(- \frac{R_{blob}^2}{\beta^2} \right) \right) \left(1 - \exp\left(- \frac{R_{noise}^2}{c^2} \right) \right)
 *  \f}
 * 
 * Note that if \f$ \lambda_3 > 0 \f$, \f$ s = 0 \f$.
 * 
 * \sa DescoteauxEigenToMeasureParameterEstimationFilter
 * \sa EigenToMeasureImageFilter
 * \sa MultiScaleHessianEnhancementImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
class DescoteauxEigenToMeasureImageFilter :
        public EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject > {
public:
  /** Standard Self typedef */
  typedef DescoteauxEigenToMeasureImageFilter       Self;
  typedef EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
                                                    Superclass;
  typedef SmartPointer<Self>                        Pointer;
  typedef SmartPointer<const Self>                  ConstPointer;

  /** Input typedefs */
  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::InputImagePixelType    InputImagePixelType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  typedef typename Superclass::InputImageRegionType   InputImageRegionType;

  /** Output typedefs */
  typedef typename Superclass::OutputImageType        OutputImageType;
  typedef typename Superclass::OutputImagePointer     OutputImagePointer;
  typedef typename Superclass::OutputImageRegionType  OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType   OutputImagePixelType;

  /** Parameter typedefs */
  typedef typename Superclass::RealType               RealType;
  typedef typename Superclass::ParameterArrayType     ParameterArrayType;
  typedef typename Superclass::ParameterDecoratedType ParameterDecoratedType;

  /** Input SpatialObject typedefs. */
  typedef typename Superclass::SpatialObjectType          SpatialObjectType;
  typedef typename Superclass::SpatialObjectConstPointer  SpatialObjectConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DescoteauxEigenToMeasureImageFilter, EigenToMeasureImageFilter);

  /** Explicitely state the eigenvalues are ordered by magnitude for this filter */
  typename Superclass::EigenValueOrderType GetEigenValueOrder() const ITK_OVERRIDE
  {
    return Superclass::OrderByMagnitude;
  }

  /** Setter/Getter methods for setting Direction */
  itkSetMacro(EnhanceType, RealType);
  itkGetConstMacro(EnhanceType, RealType);
  void SetEnhanceBrightObjects()
  {
    SetEnhanceType(-1.0);
  }
  void SetEnhanceDarkObjects()
  {
    SetEnhanceType(1.0);
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHaveDimension3Check,
                   ( Concept::SameDimension< TInputImage::ImageDimension, 3u >) );
  itkConceptMacro( OutputHaveDimension3Check,
                   ( Concept::SameDimension< TOutputImage::ImageDimension, 3u >) );
  itkConceptMacro( InputFixedArrayHasDimension3Check,
                   ( Concept::SameDimension< TInputImage::PixelType::Dimension, 3u >) );
  // End concept checking
#endif
protected:
  DescoteauxEigenToMeasureImageFilter();
  virtual ~DescoteauxEigenToMeasureImageFilter() {}

  inline OutputImagePixelType ProcessPixel(const InputImagePixelType& pixel, RealType alpha, RealType beta, RealType c);

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DescoteauxEigenToMeasureImageFilter);

  /* Member variables */
  RealType m_EnhanceType;
}; // end class
} /* end namespace itk */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDescoteauxEigenToMeasureImageFilter.hxx"
#endif

#endif /* itkDescoteauxEigenToMeasureImageFilter_h */
