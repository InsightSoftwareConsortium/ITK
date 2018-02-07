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
namespace Functor {
/** \class DescoteauxEigenToMeasureFunctor
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
template<class TInputPixel, class TOutputPixel>
class DescoteauxEigenToMeasureFunctor {
public:
  /* Basic type definitions */
  itkStaticConstMacro(NumberOfParameters, unsigned int, 3);
  typedef typename TInputPixel::ValueType                     PixelValueType;
  typedef typename NumericTraits< PixelValueType >::RealType  RealType;
  typedef FixedArray< RealType, NumberOfParameters >          ParameterType;

  DescoteauxEigenToMeasureFunctor() :
    m_Direction(-1.0),
    m_Alpha(0.0),
    m_Beta(0.0),
    m_C(0.0)
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
    sheetness *= vcl_exp(-(Rsheet * Rsheet) / (2 * m_Alpha * m_Alpha));
    sheetness *= (1.0 - vcl_exp(-(Rblob * Rblob) / (2 * m_Beta * m_Beta)));
    sheetness *= (1.0 - vcl_exp(-(Rnoise * Rnoise) / (2 * m_C * m_C)));

    return static_cast<TOutputPixel>( sheetness );
  }

  /** Macro definition for set/get of parameters */
  virtual void SetParameters(const ParameterType parameters)
  {
    this->m_Alpha = parameters[0];
    this->m_Beta = parameters[1];
    this->m_C = parameters[2];
  }

  virtual ParameterType GetParameters() const
  {
    ParameterType parameters;
    parameters[0] = this->m_Alpha;
    parameters[1] = this->m_Beta;
    parameters[2] = this->m_C;

    return parameters;
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

/** \class DescoteauxEigenToMeasureImageFilter
 * \brief Convert eigenvalues into a measure of sheetness according to the method of Descoteaux et al.
 * 
 * Converts a 3D fixed array of eigenvalues into a measure of sheetness according to the method
 * of Descoteaux et al. The parameters of the filter should be set using DescoteauxEigentoScalarParameterEstimationImageFilter.
 * 
 * See functor DescoteauxEigenToScalarFunctor for mathematics.
 * 
 * \sa DescoteauxEigenToMeasureFunctor
 * \sa DescoteauxEigenToScalarImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
class DescoteauxEigenToMeasureImageFilter :
        public EigenToMeasureImageFilter<TInputImage, TOutputImage, TInputSpatialObject,
                Functor::DescoteauxEigenToMeasureFunctor<typename TInputImage::PixelType, typename TOutputImage::PixelType > > {
public:
  /** Standard Self typedef */
  typedef DescoteauxEigenToMeasureImageFilter       Self;
  typedef EigenToMeasureImageFilter<TInputImage, TOutputImage, TInputSpatialObject,
          Functor::DescoteauxEigenToMeasureFunctor<typename TInputImage::PixelType, typename TOutputImage::PixelType > >
                                                    Superclass;
  typedef SmartPointer<Self>                        Pointer;
  typedef SmartPointer<const Self>                  ConstPointer;

  /** Functor typedef */
  typedef typename Superclass::FunctorType    FunctorType;
  typedef typename FunctorType::RealType      RealType;
  typedef typename FunctorType::ParameterType ParameterType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DescoteauxEigenToMeasureImageFilter, EigenToMeasureImageFilter);

  /** setter/getter methods for setting type of object to enhance */
  void SetEnhanceBrightObjects()
  {
    this->GetFunctor().SetEnhanceBrightObjects();
    this->Modified();
  }
  void SetEnhanceDarkObjects()
  {
    this->GetFunctor().SetEnhanceDarkObjects();
    this->Modified();
  }
  RealType GetEnhanceType() const
  {
    return this->GetFunctor().GetEnhanceType();
  }

  /** Explicitely state the eigenvalues are ordered by magnitude for this filter */
  typename Superclass::EigenValueOrderType GetEigenValueOrder() const ITK_OVERRIDE
  {
    return Superclass::OrderByMagnitude;
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
  DescoteauxEigenToMeasureImageFilter() {}
  virtual ~DescoteauxEigenToMeasureImageFilter() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DescoteauxEigenToMeasureImageFilter);
}; // end class
} /* end namespace itk */

#endif /* itkDescoteauxEigenToMeasureImageFilter_h */
