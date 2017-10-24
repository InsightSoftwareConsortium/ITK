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

#ifndef itkLevelSetEquationOverlapPenaltyTerm_h
#define itkLevelSetEquationOverlapPenaltyTerm_h

#include "itkLevelSetEquationTermBase.h"
#include "itkCompensatedSummation.h"

namespace itk
{
/**
 *  \class LevelSetEquationOverlapPenaltyTerm
 *  \brief Class to represent the overlap penalty among many level-sets
 *
 *  \f[
 *    \sum_{i \neq j } \left(H_{\epsilon}\left(\phi_j \left( p \right) \right)\right)
 *  \f]
 *
 *  \li \f$ i  \f$ is the current level-set id,
 *  \li \f$ j  \f$ is any level-set id,
 *  \li \f$ p  \f$ is the given location,
 *  \li \f$ H_{\epsilon}  \f$ is regularized Heaviside function.
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInput, // Input image or mesh
          typename TLevelSetContainer >
class ITK_TEMPLATE_EXPORT LevelSetEquationOverlapPenaltyTerm :
    public LevelSetEquationTermBase< TInput, TLevelSetContainer >
{
public:
  typedef LevelSetEquationOverlapPenaltyTerm                     Self;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;
  typedef LevelSetEquationTermBase< TInput, TLevelSetContainer > Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationOverlapPenaltyTerm, LevelSetEquationTermBase );

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

  typedef typename LevelSetContainerType::IdListType          IdListType;
  typedef typename LevelSetContainerType::IdListIterator      IdListIterator;
  typedef typename LevelSetContainerType::IdListConstIterator IdListConstIterator;

  typedef CompensatedSummation<LevelSetOutputRealType> CompensatedSummationType;


  /** Update the term parameter values at end of iteration */
  virtual void Update() ITK_OVERRIDE;

  /** Initialize parameters in the terms prior to an iteration */
  virtual void InitializeParameters() ITK_OVERRIDE;

  /** Initialize term parameters in the dense case by computing for each pixel location */
  virtual void Initialize( const LevelSetInputIndexType& index ) ITK_OVERRIDE;

  /** Compute the sum of Heaviside functions in the multi-levelset cases
   *  except the current levelset */
  virtual void ComputeSumTerm( const LevelSetInputIndexType& index,
                                  LevelSetOutputRealType& sum );

  /** Supply updates at pixels to keep the term parameters always updated */
  virtual void UpdatePixel( const LevelSetInputIndexType& index,
                            const LevelSetOutputRealType& oldValue,
                            const LevelSetOutputRealType& newValue ) ITK_OVERRIDE;


protected:
  LevelSetEquationOverlapPenaltyTerm();

  virtual ~LevelSetEquationOverlapPenaltyTerm() ITK_OVERRIDE;

  /** Returns the term contribution for a given location index */
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& index ) ITK_OVERRIDE;

  /** Returns the term contribution for a given location index */
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& index,
                                        const LevelSetDataType& data ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationOverlapPenaltyTerm);

  DomainMapImageFilterType *m_DomainMapImageFilter;
  CacheImageType           *m_CacheImage;
};

}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationOverlapPenaltyTerm.hxx"
#endif

#endif
