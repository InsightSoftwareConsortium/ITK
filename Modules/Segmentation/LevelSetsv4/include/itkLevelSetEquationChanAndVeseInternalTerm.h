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

#ifndef itkLevelSetEquationChanAndVeseInternalTerm_h
#define itkLevelSetEquationChanAndVeseInternalTerm_h

#include "itkLevelSetEquationTermBase.h"

namespace itk
{
/**
 *  \class LevelSetEquationChanAndVeseInternalTerm
 *  \brief Class to represent the internal energy Chan And Vese term
 *
 *  \f[
 *    \delta_{\epsilon}\left( \phi_{k} \left( p \right) \right) \cdot
      \left\| I\left( p \right) - \mu_{in} \right\|^2
 *  \cdot
 *  \f]
 *
 *  \li \f$ \delta_{epsilon}  \f$ is a regularized dirac function,
 *  \li \f$ k \f$ is the current level-set id,
 *  \li \f$ I\left( p \right) \f$ is the pixel value at the given location \f$ p \f$,
 *  \li \f$ \mu_{in}  \f$ is the internal mean intensity.
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInput, // Input image or mesh
          typename TLevelSetContainer >
class ITK_TEMPLATE_EXPORT LevelSetEquationChanAndVeseInternalTerm :
    public LevelSetEquationTermBase< TInput, TLevelSetContainer >
{
public:
  typedef LevelSetEquationChanAndVeseInternalTerm         Self;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;
  typedef LevelSetEquationTermBase< TInput,
                                    TLevelSetContainer >  Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationChanAndVeseInternalTerm,
                LevelSetEquationTermBase );

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

  typedef typename Superclass::HeavisideType              HeavisideType;
  typedef typename Superclass::HeavisideConstPointer      HeavisideConstPointer;

  typedef typename Superclass::LevelSetDataType LevelSetDataType;

  typedef typename Superclass::DomainMapImageFilterType   DomainMapImageFilterType;
  typedef typename Superclass::CacheImageType             CacheImageType;

  itkSetMacro( Mean, InputPixelRealType );
  itkGetMacro( Mean, InputPixelRealType );

  /** Update the term parameter values at end of iteration */
  virtual void Update() ITK_OVERRIDE;

  /** Initialize parameters in the terms prior to an iteration */
  virtual void InitializeParameters() ITK_OVERRIDE;

  /** Initialize term parameters in the dense case by computing for each pixel location */
  virtual void Initialize( const LevelSetInputIndexType& inputIndex ) ITK_OVERRIDE;

  /** Compute the product of Heaviside functions in the multi-levelset cases */
  virtual void ComputeProduct( const LevelSetInputIndexType& inputPixel,
                              LevelSetOutputRealType& prod );

  /** Compute the product of Heaviside functions in the multi-levelset cases
   *  except the current levelset */
  virtual void ComputeProductTerm( const LevelSetInputIndexType& ,
                                  LevelSetOutputRealType& )
  {}

  /** Supply updates at pixels to keep the term parameters always updated */
  virtual void UpdatePixel( const LevelSetInputIndexType& inputPixel,
                           const LevelSetOutputRealType & oldValue,
                           const LevelSetOutputRealType & newValue ) ITK_OVERRIDE;

protected:
  LevelSetEquationChanAndVeseInternalTerm();

  virtual ~LevelSetEquationChanAndVeseInternalTerm() ITK_OVERRIDE;

  /** Returns the term contribution for a given location inputPixel, i.e.
   *  \f$ \omega_i( p ) \f$. */
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& inputPixel ) ITK_OVERRIDE;

  /** Returns the term contribution for a given location inputPixel, i.e.
   *  \f$ \omega_i( p ) \f$. */
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& inputPixel,
                                        const LevelSetDataType& data ) ITK_OVERRIDE;

  /** Accumulate contribution to term parameters from a given pixel */
  void Accumulate( const InputPixelType& inputPixel,
                   const LevelSetOutputRealType& heavisideValue );

  InputPixelRealType      m_Mean;
  InputPixelRealType      m_TotalValue;
  LevelSetOutputRealType  m_TotalH;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationChanAndVeseInternalTerm);
};

}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationChanAndVeseInternalTerm.hxx"
#endif

#endif
