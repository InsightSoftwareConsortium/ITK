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

#ifndef itkLevelSetEquationChanAndVeseExternalTerm_h
#define itkLevelSetEquationChanAndVeseExternalTerm_h

#include "itkLevelSetEquationChanAndVeseInternalTerm.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 *  \class LevelSetEquationChanAndVeseExternalTerm
 *  \brief Class to represent the external energy Chan And Vese term
 *
 *  \f[
 *    \delta_{\epsilon}\left( \phi_{k}\left( p \right) \right) \cdot
 *    \prod_{i = 1, i \neq k}^{N} \left( 1 - H_{\epsilon} \left( \phi_i\left( p \right) \right) \right) \cdot
      \left\| I(p) - \mu_{out} \right\|^2
 *  \f]
 *
 *  \li \f$ \delta_{\epsilon} \f$ is a regularized dirac function,
 *  \li \f$ k \f$ is the current level-set id,
 *  \li \f$  I\left( p \right) \f$ is the pixel value at the given location \f$ p \f$,
 *  \li \f$ H_{\epsilon}  \f$ is a regularized Heaviside function,
 *  \li \f$ \mu_{out} \f$ is the external mean intensity
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInput, // Input image or mesh
          typename TLevelSetContainer >
class ITK_TEMPLATE_EXPORT LevelSetEquationChanAndVeseExternalTerm :
    public LevelSetEquationChanAndVeseInternalTerm< TInput, TLevelSetContainer >
{
public:
  typedef LevelSetEquationChanAndVeseExternalTerm         Self;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;
  typedef LevelSetEquationChanAndVeseInternalTerm< TInput,
                                    TLevelSetContainer >  Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationChanAndVeseExternalTerm,
                LevelSetEquationChanAndVeseInternalTerm );

  typedef typename Superclass::InputImageType     InputImageType;
  typedef typename Superclass::InputImagePointer  InputImagePointer;
  typedef typename Superclass::InputPixelType     InputPixelType;
  typedef typename Superclass::InputPixelRealType InputPixelRealType;

  typedef typename Superclass::LevelSetContainerType      LevelSetContainerType;
  typedef typename Superclass::LevelSetContainerPointer   LevelSetContainerPointer;
  typedef typename Superclass::LevelSetType               LevelSetType;
  typedef typename Superclass::LevelSetPointer            LevelSetPointer;
  typedef typename Superclass::LevelSetOutputPixelType    LevelSetOutputPixelType;
  typedef typename Superclass::LevelSetOutputRealType     LevelSetOutputRealType;
  typedef typename Superclass::LevelSetInputIndexType     LevelSetInputIndexType;
  typedef typename Superclass::LevelSetGradientType       LevelSetGradientType;
  typedef typename Superclass::LevelSetHessianType        LevelSetHessianType;
  typedef typename Superclass::LevelSetIdentifierType     LevelSetIdentifierType;

  typedef typename Superclass::DomainMapImageFilterType   DomainMapImageFilterType;
  typedef typename Superclass::CacheImageType             CacheImageType;

  typedef typename DomainMapImageFilterType::DomainMapType::const_iterator  DomainIteratorType;

  typedef typename LevelSetContainerType::IdListType          IdListType;
  typedef typename LevelSetContainerType::IdListIterator      IdListIterator;
  typedef typename LevelSetContainerType::IdListConstIterator IdListConstIterator;

  typedef typename Superclass::HeavisideType         HeavisideType;
  typedef typename Superclass::HeavisideConstPointer HeavisideConstPointer;

  /** Compute the product of Heaviside functions in the multi-levelset cases */
  virtual void ComputeProduct( const LevelSetInputIndexType& iP,
                               LevelSetOutputRealType& prod ) ITK_OVERRIDE;

  /** Compute the product of Heaviside functions in the multi-levelset cases
   *  except the current levelset */
  virtual void ComputeProductTerm( const LevelSetInputIndexType& iP,
                                  LevelSetOutputRealType& prod ) ITK_OVERRIDE;

  /** Supply updates at pixels to keep the term parameters always updated */
  virtual void UpdatePixel( const LevelSetInputIndexType& iP,
                           const LevelSetOutputRealType & oldValue,
                           const LevelSetOutputRealType & newValue ) ITK_OVERRIDE;

protected:
  LevelSetEquationChanAndVeseExternalTerm();
  virtual ~LevelSetEquationChanAndVeseExternalTerm() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationChanAndVeseExternalTerm);

  DomainMapImageFilterType *m_DomainMapImageFilter;
  CacheImageType           *m_CacheImage;
};

}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationChanAndVeseExternalTerm.hxx"
#endif

#endif
