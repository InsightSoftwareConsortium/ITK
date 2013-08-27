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

#ifndef __itkLevelSetEquationBinaryMaskTerm_h
#define __itkLevelSetEquationBinaryMaskTerm_h

#include "itkLevelSetEquationTermBase.h"

namespace itk
{
/**
 *  \class LevelSetEquationBinaryMaskTerm
 *  \brief Class to represent the mask boundary as a negative energy
 *
 *  \f[
 *    M\left( p \right)
 *  \cdot
 *  \f]
 *
 *  \li \f$ M \f$ is a mask with 0 representing a boundary,
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  \ingroup ITKLevelSetsv4
 */
template< class TInput, // Input image or mesh
          class TLevelSetContainer >
class LevelSetEquationBinaryMaskTerm :
    public LevelSetEquationTermBase< TInput, TLevelSetContainer >
{
public:
  typedef LevelSetEquationBinaryMaskTerm         Self;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;
  typedef LevelSetEquationTermBase< TInput,
                                    TLevelSetContainer >  Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationBinaryMaskTerm,
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

  itkSetObjectMacro( Mask, InputImageType );

  /** Update the term parameter values at end of iteration */
  virtual void Update();

  /** Initialize parameters in the terms prior to an iteration */
  virtual void InitializeParameters();

  /** Initialize term parameters in the dense case by computing for each pixel location */
  virtual void Initialize( const LevelSetInputIndexType& iP );

  /** Supply updates at pixels to keep the term parameters always updated */
  virtual void UpdatePixel( const LevelSetInputIndexType& iP,
                           const LevelSetOutputRealType & oldValue,
                           const LevelSetOutputRealType & newValue );

protected:
  LevelSetEquationBinaryMaskTerm();

  virtual ~LevelSetEquationBinaryMaskTerm();

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& iP );

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& iP,
                                        const LevelSetDataType& iData );


  InputImagePointer m_Mask;

private:
  LevelSetEquationBinaryMaskTerm( const Self& ); // purposely not implemented
  void operator = ( const Self& ); // purposely not implemented
};

}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationBinaryMaskTerm.hxx"
#endif

#endif
