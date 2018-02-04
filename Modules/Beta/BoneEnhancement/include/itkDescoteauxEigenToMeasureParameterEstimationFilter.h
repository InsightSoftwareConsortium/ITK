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

#include "itkFixedArray.h"
#include "itkMath.h"
#include "itkEigenToMeasureParameterEstimationFilter.h"

namespace itk {
namespace Functor {
/** \class DescoteauxEigenParameterFunctor
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
 * One can set FrobeniusNormWeight to control the weighting on the
 * Frobenius norm.
 * 
 * \sa DescoteauxEigentoScalarParameterEstimationImageFilter
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */
template<class TInputPixel>
class DescoteauxEigenParameterFunctor {
public:
  /* Basic type definitions */
  itkStaticConstMacro(NumberOfParameters, unsigned int, 3);
  typedef typename TInputPixel::ValueType                     PixelValueType;
  typedef typename NumericTraits< PixelValueType >::RealType  RealType;
  typedef FixedArray< RealType, NumberOfParameters >          ParameterType;

  DescoteauxEigenParameterFunctor() :
    m_FrobeniusNormWeight(0.5)
  {
    m_MaxFrobeniusNormArray.SetSize(0);
  }

  void Initialize(unsigned int numberOfCalls)
  {
    m_MaxFrobeniusNormArray.SetSize(numberOfCalls);
    m_MaxFrobeniusNormArray.Fill(NumericTraits< RealType >::ZeroValue());
  }

  void ProcessPixel(const TInputPixel& inputPixel, unsigned int callNumber)
  {
    RealType norm = CalculateFrobeniusNorm(inputPixel);
    if (norm > m_MaxFrobeniusNormArray[callNumber]){
      m_MaxFrobeniusNormArray[callNumber] = norm;
    }
  }

  ParameterType GetComputedParameters()
  {
    /* Alpha is 0.5 */
    m_Parameters[0] = 0.5;

    /* Beta is 0.5 */
    m_Parameters[1] = 0.5;

    /* C is m_FrobeniusNormWeight * max(frobenius norm) */
     m_Parameters[2] = 0;
    for (unsigned int i = 0; i < m_MaxFrobeniusNormArray.GetSize(); ++i)
    {
      if (m_MaxFrobeniusNormArray[i] > m_Parameters[2])
      {
        m_Parameters[2] = m_MaxFrobeniusNormArray[i];
      }
    }
    
    /* Weight by the selection parmaeter */
    m_Parameters[2] = m_FrobeniusNormWeight*m_Parameters[2];

    /* Finally, return the parameters we found */
    return m_Parameters;
  }

  void SetFrobeniusNormWeight(RealType weight)
  {
    m_FrobeniusNormWeight = weight;
  }

  RealType GetFrobeniusNormWeight() const
  {
    return m_FrobeniusNormWeight;
  }

private:
  inline RealType CalculateFrobeniusNorm(const TInputPixel& pixel) {
    /* Forbenius norm is given by the square root of the sum of squares 
    * of the eigenvalues for real, symmetric matricies
    */
    RealType norm = 0;
    for( unsigned int i = 0; i < pixel.Length; ++i) {
      norm += pixel[i]*pixel[i];
    }
    return sqrt(norm);
  }

  /* Private member variables */
  RealType            m_FrobeniusNormWeight;
  Array< RealType >   m_MaxFrobeniusNormArray;
  ParameterType       m_Parameters;
}; // end class
} // end Functor

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
template<typename TInputImage, typename TInputSpatialObject >
class DescoteauxEigenToMeasureParameterEstimationFilter :
        public EigenToMeasureParameterEstimationFilter<TInputImage, TInputSpatialObject,
                Functor::DescoteauxEigenParameterFunctor<typename TInputImage::PixelType > > {
public:
  /** Standard Self typedef */
  typedef DescoteauxEigenToMeasureParameterEstimationFilter Self;
  typedef EigenToMeasureParameterEstimationFilter<TInputImage, TInputSpatialObject,
          Functor::DescoteauxEigenParameterFunctor<typename TInputImage::PixelType > >
                                                            Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;
  
  /** Functor typedef */
  typedef typename Superclass::FunctorType    FunctorType;
  typedef typename FunctorType::RealType      RealType;
  typedef typename FunctorType::ParameterType ParameterType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DescoteauxEigenToMeasureParameterEstimationFilter, EigenToMeasureParameterEstimationFilter);

  /** Define decorator types */
  typedef SimpleDataObjectDecorator< RealType > InputParameterDecoratorType;

  /** setter/getter methods for setting type of object to enhance */
  void SetFrobeniusNormWeight(RealType weight)
  {
    this->GetFunctor().SetFrobeniusNormWeight(weight);
  }
  RealType GetFrobeniusNormWeight() const
  {
    return this->GetFunctor().GetFrobeniusNormWeight();
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
  DescoteauxEigenToMeasureParameterEstimationFilter() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DescoteauxEigenToMeasureParameterEstimationFilter);

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "FrobeniusNormWeight: " << GetFrobeniusNormWeight() << std::endl;
  }
}; // end class
} /* end namespace */

#endif /* itkDescoteauxEigenToMeasureParameterEstimationFilter_h */
