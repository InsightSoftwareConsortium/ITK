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

#ifndef itkLevelSetEquationPropagationTerm_h
#define itkLevelSetEquationPropagationTerm_h

#include "itkLevelSetEquationTermBase.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkVector.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
/**
 *  \class LevelSetEquationPropagationTerm
 *  \brief Derived class to represents a propagation term in the level-set evolution PDE
 *
 *  \f[
 *  PropagationImage( p ) \cdot \left\| \nabla \phi\left( p \right) \right\|
 *  \f]
 *
 *  \li PropagationImage is the propagation image set by the user.
 *  \li \f$ \| \cdot  \| \f$ denotes the usual \f$L_2\f$-norm
 *  \li \f$ \nabla \phi \f$ denotes the gradient of the level-set function \f$ \phi \f$.
 *
 *  \tparam TInput Input Image Type
 *  \tparam TLevelSetContainer Level set function container type
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInput, // Input image or mesh
          typename TLevelSetContainer,
          typename TPropagationImage = TInput >
class ITK_TEMPLATE_EXPORT LevelSetEquationPropagationTerm :
    public LevelSetEquationTermBase< TInput, TLevelSetContainer >
{
public:
  typedef LevelSetEquationPropagationTerm       Self;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;
  typedef LevelSetEquationTermBase< TInput, TLevelSetContainer >
                                                Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationPropagationTerm,
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
  typedef typename Superclass::LevelSetDataType           LevelSetDataType;

  typedef typename Superclass::HeavisideType         HeavisideType;
  typedef typename Superclass::HeavisideConstPointer HeavisideConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int, InputImageType::ImageDimension);

  typedef TPropagationImage                       PropagationImageType;
  typedef typename PropagationImageType::Pointer  PropagationImagePointer;

  /** Neighborhood radius type */
  typedef ZeroFluxNeumannBoundaryCondition< InputImageType > DefaultBoundaryConditionType;
  typedef typename ConstNeighborhoodIterator< InputImageType >::RadiusType RadiusType;
  typedef ConstNeighborhoodIterator< InputImageType, DefaultBoundaryConditionType > NeighborhoodType;

  typedef Vector< LevelSetOutputRealType, itkGetStaticConstMacro(ImageDimension) > NeighborhoodScalesType;

  /** Set/Get the propagation image. By default, if no PropagationImage has
  been set, it casts the input image and uses it in the term contribution
  calculation. */
  itkSetObjectMacro( PropagationImage, PropagationImageType );
  itkGetModifiableObjectMacro(PropagationImage, PropagationImageType );

  /** \todo to be documented. */
  virtual void Update() ITK_OVERRIDE;

  /** Initialize the parameters in the terms prior to an iteration */
  virtual void InitializeParameters() ITK_OVERRIDE;

  /** \todo to be documented. */
  virtual void Initialize( const LevelSetInputIndexType& ) ITK_OVERRIDE;

  /** Supply updates at pixels to keep the term parameters always updated */
  virtual void UpdatePixel( const LevelSetInputIndexType& iP,
                            const LevelSetOutputRealType& oldValue,
                            const LevelSetOutputRealType& newValue ) ITK_OVERRIDE;

protected:
  LevelSetEquationPropagationTerm();

  virtual ~LevelSetEquationPropagationTerm() ITK_OVERRIDE;

  PropagationImagePointer m_PropagationImage;

  /** Return the spatial speed dependence a given pixel location
   * Usually, it is constant across the image domain */
  LevelSetOutputRealType PropagationSpeed( const LevelSetInputIndexType& iP ) const;

  /** Returns the term contribution for a given location iP, i.e.
   *  \f$ \omega_i( p ) \f$. */
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& iP ) ITK_OVERRIDE;
  virtual LevelSetOutputRealType Value( const LevelSetInputIndexType& iP,
                                        const LevelSetDataType& iData ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationPropagationTerm);
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationPropagationTerm.hxx"
#endif

#endif
