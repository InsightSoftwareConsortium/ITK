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

#ifndef itkKrcahEigenToMeasureImageFilter_h
#define itkKrcahEigenToMeasureImageFilter_h

#include "itkEigenToMeasureImageFilter.h"
#include "itkMath.h"

namespace itk {
namespace Functor {
/** \class KrcahEigenToMeasureFunctor
 * \brief Eigenvalue to measure functor as defined by Krcah et al.
 * 
 * Computes the following equation for eigenvalues in a three
 * dimensional fixed array:
 *  \f{eqnarray*}{
 *      R_{sheet} &=&  \frac{|\lambda_2|}{\lambda_3|} \\
 *      R_{tube} &=&  \frac{\lambda_1|}{|\lambda_2| \cdot \lambda_3|} \\
 *      R_{noise} &=&  \lambda_1| + |\lambda_2| + \lambda_3| \\
 *      s &=& sign(\lambda_3) \exp\left(- \frac{R_{sheet}^2}{\alpha^2} \right) \exp\left(- \frac{R_{tube}^2}{\beta^2} \right) \left(1 - \exp\left(- \frac{R_{noise}^2}{\gamma^2} \right) \right)
 *  \f}
 * 
 * The scaling by the average trace of the Hessian matrix is implicit in \f$ \gamma \f$.
 * 
 * \sa KrcahEigenToMeasureImageFilter
 * 
 * \author: Thomas Fitze
 * \ingroup BoneEnhancement
 */
template<class TInputPixel, class TOutputPixel>
class KrcahEigenToMeasureFunctor {
public:
  /* Basic type definitions */
  using RealType = typename NumericTraits< TOutputPixel >::RealType;

  KrcahEigenToMeasureFunctor() :
    m_Direction(-1.0)
  {}

  inline TOutputPixel operator()(const TInputPixel &A) {
    double sheetness = 0.0;
    double a1 = static_cast<double>( A[0] );
    double a2 = static_cast<double>( A[1] );
    double a3 = static_cast<double>( A[2] );
    double l1 = Math::abs(a1);
    double l2 = Math::abs(a2);
    double l3 = Math::abs(a3);

    /* Avoid divisions by zero (or close to zero) */
    if (static_cast<double>( l3 ) < Math::eps || static_cast<double>( l2 ) < Math::eps) {
        return static_cast<TOutputPixel>( sheetness );
    }

    /**
     * Compute sheet, noise, and tube like measures. Note that the average trace of the
     * Hessian matrix is implicitly included in \f$ \gamma \f$ here.
     */
    const double Rsheet = l2 / l3;
    const double Rnoise = (l1 + l2 + l3); // T implicite in m_Gamma
    const double Rtube = l1 / (l2 * l3);

    /* Multiply together to get sheetness */
    sheetness = (m_Direction*a3/l3);
    sheetness *= vcl_exp(-(Rsheet * Rsheet) / (m_Alpha * m_Alpha));
    sheetness *= vcl_exp(-(Rtube * Rtube) / (m_Beta * m_Beta));
    sheetness *= (1.0 - vcl_exp(-(Rnoise * Rnoise) / (m_Gamma * m_Gamma)));

    return static_cast<TOutputPixel>( sheetness );
  }

  /** Macro definition for set/get of parameters */
  virtual void SetAlpha(const RealType alpha)
  {
    this->m_Alpha = alpha;
  }
  virtual void SetBeta(const RealType beta)
  {
    this->m_Beta = beta;
  }
  virtual void SetGamma(const RealType gamma)
  {
    this->m_Gamma= gamma;
  }
  RealType GetAlpha() const
  {
    return this->m_Alpha;
  }
  RealType GetBeta() const
  {
    return this->m_Beta;
  }
  RealType GetGamma() const
  {
    return this->m_Gamma;
  }

  /** Set/get the type to enhance */
  void SetEnhanceBrightObjects()
  {
    m_Direction = -1.0;
  }
  void SetEnhanceDarkObjects()
  {
    m_Direction = 1.0;
  }
  RealType GetEnhanceType() const
  {
    return m_Direction;
  }

private:
  /* Private member variables */
  RealType m_Alpha;
  RealType m_Beta;
  RealType m_Gamma;
  RealType m_Direction;
}; // end class
} // end Functor

/** \class KrcahEigenToMeasureImageFilter
 * \brief Convert eigenvalues into a measure of sheetness according to the method of Krcah et al.
 * 
 * Converts a 3D fixed array of eigenvalues into a measure of sheetness according to the method
 * of Krcah et al. The parameters of the filter should be set using KrcahEigentoScalarParameterEstimationImageFilter.
 * 
 * The mathematics are defined in KrcahEigenToMeasureFunctor
 * 
 * \sa KrcahEigenToMeasureFunctor
 * \sa KrcahEigenToMeasureParameterEstimationFilter
 * \sa EigenToMeasureImageFilter
 * \sa MultiScaleHessianEnhancementImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
class KrcahEigenToMeasureImageFilter
  : public EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
{
  ITK_DISALLOW_COPY_AND_ASSIGN(KrcahEigenToMeasureImageFilter);

  /** Standard Self typedef */
  using Self = KrcahEigenToMeasureImageFilter;
  using Superclass = EigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Input typedefs */
  using InputImageType          = typename Superclass::InputImageType;
  using InputImagePixelType     = typename Superclass::InputImagePixelType;
  using InputImagePointer       = typename Superclass::InputImagePointer;
  using InputImageConstPointer  = typename Superclass::InputImageConstPointer;
  using InputImageRegionType    = typename Superclass::InputImageRegionType;

  /** Output typedefs */
  using OutputImageType       = typename Superclass::OutputImageType;
  using OutputImagePointer    = typename Superclass::OutputImagePointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputImagePixelType  = typename Superclass::OutputImagePixelType;

  /** Parameter typedefs */
  using RealType                = typename Superclass::RealType;
  using ParameterArrayType      = typename Superclass::ParameterArrayType;
  using ParameterDecoratedType  = typename Superclass::ParameterDecoratedType;

  /** Input SpatialObject typedefs. */
  using SpatialObjectType         = typename Superclass::SpatialObjectType;
  using SpatialObjectConstPointer = typename Superclass::SpatialObjectConstPointer;

  /** Functor typedefs. */
  using FunctorType = typename Functor::KrcahEigenToMeasureFunctor< InputImagePixelType, OutputImagePixelType >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(KrcahEigenToMeasureImageFilter, EigenToMeasureImageFilter);

  /** Explicitely state the eigenvalues are ordered by magnitude for this filter */
  typename Superclass::EigenValueOrderType GetEigenValueOrder() const override
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
  KrcahEigenToMeasureImageFilter();
  virtual ~KrcahEigenToMeasureImageFilter() {}

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

  void PrintSelf(std::ostream & os, Indent indent) const override;
private:
  /* Member variables */
  RealType    m_EnhanceType;
  FunctorType m_Functor;
}; // end class
} /* end namespace itk */

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKrcahEigenToMeasureImageFilter.hxx"
#endif

#endif /* itkKrcahEigenToMeasureImageFilter_h */
