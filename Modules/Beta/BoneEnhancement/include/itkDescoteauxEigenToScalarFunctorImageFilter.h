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

#ifndef itkDescoteauxEigenToScalarFunctorImageFilter_h
#define itkDescoteauxEigenToScalarFunctorImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk {
namespace Functor {
/** \class DescoteauxEigenToScalarFunctor
 * \brief Eigenvalue to scalar functor as defined by Descoteaux et al.
 * 
 * Computes the following equation for eigenvalues in a three
 * dimensional fixed array:
 *  \f{eqnarray*}{
 *      R_{sheet} &=&  \frac{|\lambda_2|}{\lambda_3|} \\
 *      R_{blob} &=&  \frac{|2 |\lambda_3| - |\lambda_2| - |\lambda_1| |}{|\lambda_3|} \\
 *      R_{noise} &=&  \sqrt{|\lambda_1|^2 + |\lambda_2|^2 + \lambda_3|^2} \\
 *      s &=& \exp\left(- \frac{R_{sheet}^2}{\alpha^2} \right) \left(1 - \exp\left(- \frac{R_{blob}^2}{\beta^2} \right) \right) \left(1 - \exp\left(- \frac{R_{noise}^2}{c^2} \right) \right)
 *  \f}
 * 
 * Note that if \f$ \lambda_3 > 0 \f$, \f$ s = 0 \f$.
 * 
 * The parameter selection is done by DescoteauxEigentoScalarParameterEstimationImageFilter
 * where the parameter \f$ c \f$ is scaled by the maximum of the Frobenius norm.
 * 
 * \sa DescoteauxEigentoScalarParameterEstimationImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template<typename TInputPixel, typename TOutputPixel>
class DescoteauxEigenToScalarFunctor {
public:
  /* Basic type definitions */
  using RealType = typename NumericTraits< TOutputPixel >::RealType;

  DescoteauxEigenToScalarFunctor() :
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

    /* Deal with l3 > 0 */
    if ( m_Direction * a3 < 0 ) {
        return static_cast<TOutputPixel>( 0.0 );
    }

    /* Avoid divisions by zero (or close to zero) */
    if ( l3 < Math::eps) {
        return static_cast<TOutputPixel>( 0.0 );
    }

    /* Compute measures */
    const double Rsheet = l2 / l3;
    const double Rblob = Math::abs(2*l3 - l2 - l1) / l3;
    const double Rnoise = sqrt(l1*l1 + l2*l2 + l3*l3);

    /* Multiply together to get sheetness */
    sheetness = 1.0;
    sheetness *= std::exp(-(Rsheet * Rsheet) / (2 * m_Alpha * m_Alpha));
    sheetness *= (1.0 - std::exp(-(Rblob * Rblob) / (2 * m_Beta * m_Beta)));
    sheetness *= (1.0 - std::exp(-(Rnoise * Rnoise) / (2 * m_C * m_C)));

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
  virtual void SetC(const RealType c)
  {
    this->m_C= c;
  }
  RealType GetAlpha() const
  {
    return this->m_Alpha;
  }
  RealType GetBeta() const
  {
    return this->m_Beta;
  }
  RealType GetC() const
  {
    return this->m_C;
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
  RealType m_C;
  RealType m_Direction;
}; // end class
} // end Functor

/** \class DescoteauxEigenToScalarFunctorImageFilter
 * \brief Convert eigenvalues into a measure of sheetness according to the method of Descoteaux et al.
 * 
 * Converts a 3D fixed array of eigenvalues into a measure of sheetness according to the method
 * of Descoteaux et al. The parameters of the filter should be set using DescoteauxEigentoScalarParameterEstimationImageFilter.
 * 
 * See functor DescoteauxEigenToScalarFunctor for mathematics.
 * 
 * \sa DescoteauxEigenToScalarFunctor
 * \sa DescoteauxEigenToScalarImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template<typename TInputImage, typename TOutputImage >
class DescoteauxEigenToScalarFunctorImageFilter :
        public UnaryFunctorImageFilter<TInputImage, TOutputImage,
                Functor::DescoteauxEigenToScalarFunctor<typename TInputImage::PixelType, typename TOutputImage::PixelType > > {
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DescoteauxEigenToScalarFunctorImageFilter);

  /** Standard Self type alias */
  using Self = DescoteauxEigenToScalarFunctorImageFilter;
  using Superclass = UnaryFunctorImageFilter<TInputImage, TOutputImage,
          Functor::DescoteauxEigenToScalarFunctor<typename TInputImage::PixelType, typename TOutputImage::PixelType > >;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Useful type alias for numerics */
  typedef typename Functor::DescoteauxEigenToScalarFunctor<typename TInputImage::PixelType, typename TOutputImage::PixelType > 
                    DescoteauxFunctorType;
  typedef typename DescoteauxFunctorType::RealType
                    RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DescoteauxEigenToScalarFunctorImageFilter, UnaryFunctorImageFilter);

  /** Define decorator types */
  using InputParameterDecoratorType = SimpleDataObjectDecorator< RealType >;

  /** Process object */
  itkSetGetDecoratedInputMacro(Alpha, RealType);
  itkSetGetDecoratedInputMacro(Beta, RealType);
  itkSetGetDecoratedInputMacro(C, RealType);

  /** Need to access the input parameters at execution time */
  void BeforeThreadedGenerateData() override
  {
    /* Set functor parameters after a call to Update() to make sure the input parameters resolve */
    this->GetFunctor().SetAlpha( this->GetAlphaInput()->Get() );
    this->GetFunctor().SetBeta( this->GetBetaInput()->Get() );
    this->GetFunctor().SetC( this->GetCInput()->Get() );
  }

  /** setter/getter methods for setting type of object to enhance */
  void SetEnhanceBrightObjects()
  {
    this->GetFunctor().SetEnhanceBrightObjects();
  }
  void SetEnhanceDarkObjects()
  {
    this->GetFunctor().SetEnhanceDarkObjects();
  }
  RealType GetEnhanceType() const
  {
    return this->GetFunctor().GetEnhanceType();
  }

protected:
  DescoteauxEigenToScalarFunctorImageFilter() {}
  virtual ~DescoteauxEigenToScalarFunctorImageFilter() {}

private:
  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Alpha: " << GetAlpha() << std::endl;
    os << indent << "Beta: " << GetBeta() << std::endl;
    os << indent << "C: " << GetC() << std::endl;
    os << indent << "EnhanceType: " << GetEnhanceType() << std::endl;
  }
}; // end class
} // end namespace

#endif // itkDescoteauxEigenToScalarFunctorImageFilter_h
