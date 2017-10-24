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

#ifndef itkLevelSetEvolutionNumberOfIterationsStoppingCriterion_h
#define itkLevelSetEvolutionNumberOfIterationsStoppingCriterion_h

#include "itkObjectFactory.h"
#include "itkLevelSetEvolutionStoppingCriterion.h"

namespace itk
{
/** \class LevelSetEvolutionStoppingCriterion
\ingroup ITKLevelSetsv4
*/
template< typename TLevelSetContainer >
class ITK_TEMPLATE_EXPORT LevelSetEvolutionNumberOfIterationsStoppingCriterion :
  public LevelSetEvolutionStoppingCriterion< TLevelSetContainer >
{
public:
  typedef LevelSetEvolutionNumberOfIterationsStoppingCriterion     Self;
  typedef LevelSetEvolutionStoppingCriterion< TLevelSetContainer > Superclass;
  typedef SmartPointer< Self >                                     Pointer;
  typedef SmartPointer< const Self >                               ConstPointer;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevelSetEvolutionNumberOfIterationsStoppingCriterion,
                LevelSetEvolutionStoppingCriterion );

  typedef TLevelSetContainer                               LevelSetContainerType;
  typedef typename LevelSetContainerType::Pointer          LevelSetContainerPointer;

  typedef typename LevelSetContainerType::LevelSetIdentifierType  LevelSetIdentifierType;
  typedef typename LevelSetContainerType::LevelSetType            LevelSetType;
  typedef typename LevelSetContainerType::LevelSetPointer         LevelSetPointer;

  typedef typename LevelSetContainerType::InputIndexType   InputIndexType;
  typedef typename LevelSetContainerType::OutputType       OutputType;
  typedef typename LevelSetContainerType::OutputRealType   OutputRealType;
  typedef typename LevelSetContainerType::GradientType     GradientType;
  typedef typename LevelSetContainerType::HessianType      HessianType;

  typedef typename LevelSetContainerType::HeavisideType    HeavisideType;
  typedef typename LevelSetContainerType::HeavisideType    HeavisidePointer;

  virtual bool IsSatisfied() const ITK_OVERRIDE;

  virtual std::string GetDescription() const ITK_OVERRIDE;

protected:
  /** Constructor */
  LevelSetEvolutionNumberOfIterationsStoppingCriterion();

  /** Destructor */
  virtual ~LevelSetEvolutionNumberOfIterationsStoppingCriterion() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEvolutionNumberOfIterationsStoppingCriterion);
 };
 }
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.hxx"
#endif
#endif
