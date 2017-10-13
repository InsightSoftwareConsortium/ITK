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

#ifndef itkLevelSetEquationTermBase_h
#define itkLevelSetEquationTermBase_h

#include "itkObject.h"
#include "itkHeavisideStepFunctionBase.h"
#include "itksys/hash_set.hxx"

namespace itk
{
/**
 *  \class LevelSetEquationTermBase
 *  \brief Abstract class to represents a term in the level-set evolution PDE
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *
 *  Evolving single level-set function \f$ \phi \f$ can be expressed as follows:
 *  \f[
 *  \frac{\partial \phi(p)}{\partial \tau} = \sum\limits_{i=1}^{N} \alpha_{i} \cdot \omega_i(p)
 *  \f]
 *  where \f$\omega_i\f$ is a term which could depend on the level-set function,
 *  the input image; and \f$\alpha_i\f$ is a weight to balance the contribution of
 *  each term in the PDE.
 *
 *  \sa LevelSetEquationContainer
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInputImage, // Input image
          typename TLevelSetContainer >
class ITK_TEMPLATE_EXPORT LevelSetEquationTermBase : public Object
{
public:
  typedef LevelSetEquationTermBase   Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef Object                     Superclass;

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationTermBase, Object );

  typedef TInputImage                                     InputImageType;
  typedef typename InputImageType::Pointer                InputImagePointer;
  typedef typename InputImageType::PixelType              InputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType
                                                          InputPixelRealType;

  /** Level-set function container type */
  typedef TLevelSetContainer                              LevelSetContainerType;
  typedef typename LevelSetContainerType::LevelSetIdentifierType
                                                          LevelSetIdentifierType;
  typedef typename LevelSetContainerType::Pointer         LevelSetContainerPointer;
  typedef typename LevelSetContainerType::LevelSetType    LevelSetType;
  typedef typename LevelSetContainerType::LevelSetPointer LevelSetPointer;
  typedef typename LevelSetContainerType::OutputType      LevelSetOutputPixelType;
  typedef typename LevelSetContainerType::OutputRealType  LevelSetOutputRealType;
  typedef typename LevelSetContainerType::InputIndexType  LevelSetInputIndexType;
  typedef typename LevelSetContainerType::GradientType    LevelSetGradientType;
  typedef typename LevelSetContainerType::HessianType     LevelSetHessianType;
  typedef typename LevelSetContainerType::LevelSetDataType
                                                          LevelSetDataType;

  typedef typename LevelSetContainerType::DomainMapImageFilterType  DomainMapImageFilterType;
  typedef typename LevelSetContainerType::CacheImageType            CacheImageType;

  typedef HeavisideStepFunctionBase< LevelSetOutputRealType,
                                     LevelSetOutputRealType >
                                          HeavisideType;
//  typedef typename HeavisideType::Pointer HeavisidePointer;
  typedef typename HeavisideType::ConstPointer HeavisideConstPointer;

  /** Set/Get the image to be segmented */
  itkSetObjectMacro( Input, InputImageType );
  itkGetModifiableObjectMacro(Input, InputImageType );

  itkSetMacro( Coefficient, LevelSetOutputRealType );
  itkGetMacro( Coefficient, LevelSetOutputRealType );

  itkSetMacro( CurrentLevelSetId, LevelSetIdentifierType );
  itkGetMacro( CurrentLevelSetId, LevelSetIdentifierType );

  itkGetModifiableObjectMacro(CurrentLevelSetPointer, LevelSetType );

  virtual void SetLevelSetContainer( LevelSetContainerType*ptr );
  itkGetModifiableObjectMacro(LevelSetContainer, LevelSetContainerType );

  /** Returns the weighted term contribution at the given location iP, i.e.
   *  \f$ \alpha_i \cdot \omega_i( p ) \f$
   */
  virtual LevelSetOutputRealType Evaluate( const LevelSetInputIndexType& iP );

  virtual LevelSetOutputRealType Evaluate( const LevelSetInputIndexType& iP,
                                           const LevelSetDataType& iData );

  /** \todo to be documented. */
  virtual void Initialize( const LevelSetInputIndexType& iP ) = 0;

  /** Initialize the parameters in the terms prior to an iteration */
  virtual void InitializeParameters() = 0;

  /** Supply updates at pixels to keep the term parameters always updated */
  virtual void UpdatePixel( const LevelSetInputIndexType& iP,
                           const LevelSetOutputRealType & oldValue,
                           const LevelSetOutputRealType & newValue ) = 0;

  /** Get the CFL contribution for the given term */
  itkGetConstMacro( CFLContribution, LevelSetOutputRealType );

  /** Set/Get the term name */
  itkSetStringMacro( TermName );
  itkGetStringMacro( TermName );

  /** Update the term parameter values at end of iteration */
  virtual void Update() = 0;

  typedef itksys::hash_set< std::string > RequiredDataType;

  const RequiredDataType & GetRequiredData() const;

protected:
  /** Default Constructor */
  LevelSetEquationTermBase();

  /** Destructor */
  virtual ~LevelSetEquationTermBase() ITK_OVERRIDE;

  void SetUp();

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. This method must be implemented in all
   *  class which inherits from this class.
   */
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& iP ) = 0;

  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& iP,
                                        const LevelSetDataType& iData ) = 0;

  /** Input image */
  InputImagePointer        m_Input;

  /** Container of level-set function */
  LevelSetContainerPointer m_LevelSetContainer;

  /** Id of the current level-set function */
  LevelSetIdentifierType   m_CurrentLevelSetId;

  LevelSetPointer          m_CurrentLevelSetPointer;

  /** Coefficient \f$ \alpha_i \f$ */
  LevelSetOutputRealType   m_Coefficient;

  /** Contribution to the CFL condition (which will be used to compute the
   *  the time step at the next iteration
   */
  LevelSetOutputRealType   m_CFLContribution;

  /** Heaviside function to be used. Depending on the term expression,
   *  this one may need to be provided
   */
  HeavisideConstPointer         m_Heaviside;

  /** Name to be given to the term. Note by default, one name is provided,
   *  but end-users may rename differently each term.
   */
  std::string               m_TermName;

  RequiredDataType          m_RequiredData;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationTermBase);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationTermBase.hxx"
#endif

#endif
