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
//
//  Created by Jean-Marie Mirebeau on 06/03/2014.
//
//

#ifndef itkCoherenceEnhancingDiffusionImageFilter_h
#define itkCoherenceEnhancingDiffusionImageFilter_h

#include "itkAnisotropicDiffusionLBRImageFilter.h"


namespace itk
{
/**
 * \class CoherenceEnhancingDiffusionImageFilter
 *
 * \brief Coherence enhanging diffusion and edge enhancing diffusion.
 *
 * Implementation of Coherence Enhancing Diffusion (CED), and
 * Edge Enhancing Diffusion (EED), as described by Weickert.
 *
 * CED heuristically smoothes everywhere except accross image contours,
 * while EED smoothes nowhere but tangentially to image contours.
 *
 * The non-linear diffusion tensor is defined in terms of the structure tensor.
 *
 * Denote by \f$\mu_i\f$ the structure tensor eigenvalues, at a given point \f$x\f$,
 * with \f$0\leq i < d\f$. Let also \f$\mu_{\rm min}\f$ and \f$\mu_{\rm max}\f$,
 * be the smallest and largest eigenvalues respectively. The diffusion tensor is
 * defined by the same eigenvectors, but with modified with eigenvalues \f$\lambda_i\f$.
 *
 * Coherence Enhancing Diffusion:
 *
 *   \f$\lambda_i := g(\mu_i - \mu_{\rm min})\f$, where \f$g(s) = 1 - (1-\alpha)*exp(-(\lambda/s)^m)\f$
 *
 * Note the limit values \f$g(0) = 1\f$, \f$g(\infty) = \alpha\f$.
 *
 * Edge enhancing diffusion:
 *
 *   \f$\lambda_i := g(\mu_{\rm max} - \mu_i)\f$, where  \f$g(s) = \alpha + (1-\alpha)*exp(-(\lambda/s)^m)\f$
 *
 * Note the limit values \f$g(0) = \alpha\f$, \f$g(\infty) = 1\f$.
 *
 * \ingroup AnisotropicDiffusionLBR
 */
template< typename TImage, typename TScalar = typename NumericTraits< typename TImage::PixelType >::RealType >
class CoherenceEnhancingDiffusionImageFilter:
  public AnisotropicDiffusionLBRImageFilter< TImage, TScalar >
{
public:
  typedef CoherenceEnhancingDiffusionImageFilter                Self;
  typedef AnisotropicDiffusionLBRImageFilter< TImage, TScalar > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CoherenceEnhancingDiffusionImageFilter, Superclass);

  static const unsigned int Dimension = Superclass::Dimension;

  typedef typename Superclass::EigenValuesArrayType EigenValuesArrayType;
  EigenValuesArrayType EigenValuesTransform(const EigenValuesArrayType &) const override;

  typedef typename Superclass::ScalarType ScalarType;
  /** Exponent m involved in the function g defining eigenvalues. */
  itkSetMacro(Exponent, ScalarType);
  itkSetMacro(Lambda, ScalarType);
  itkSetMacro(Alpha, ScalarType);

  itkGetMacro(Exponent, ScalarType);
  itkGetMacro(Lambda, ScalarType);
  itkGetMacro(Alpha, ScalarType);

  enum EnhancementType {CED, cCED, EED, cEED, Isotropic};
  /// Switch between CED, EED, and variants.
  itkSetEnumMacro(Enhancement, EnhancementType);
  itkGetEnumMacro(Enhancement, EnhancementType);

protected:
  ScalarType      m_Lambda;
  ScalarType      m_Exponent;
  ScalarType      m_Alpha;
  EnhancementType m_Enhancement;

  ScalarType g_CED(ScalarType s) const {return s<=0 ? m_Alpha : m_Alpha + (1-m_Alpha)*exp(-pow(m_Lambda/s,m_Exponent));}
  ScalarType g_EED(ScalarType s) const {return s<=0 ? 1 : 1 - (1-m_Alpha)*exp(-pow(m_Lambda/s,m_Exponent));}

  CoherenceEnhancingDiffusionImageFilter();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCoherenceEnhancingDiffusionImageFilter.hxx"
#endif

#endif
