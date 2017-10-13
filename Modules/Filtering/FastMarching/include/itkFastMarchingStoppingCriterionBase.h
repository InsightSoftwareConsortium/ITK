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

#ifndef itkFastMarchingStoppingCriterionBase_h
#define itkFastMarchingStoppingCriterionBase_h

#include "itkStoppingCriterionBase.h"
#include "itkNumericTraits.h"
#include "itkFastMarchingTraits.h"

namespace itk
{

/** \class FastMarchingStoppingCriterionBase
  \brief Abstract Stopping Criterion dedicated for Fast Marching Methods

  \ingroup ITKFastMarching
  */
template< typename TInput, typename TOutput >
class FastMarchingStoppingCriterionBase : public StoppingCriterionBase
{
public:
  typedef FastMarchingStoppingCriterionBase      Self;
  typedef StoppingCriterionBase                  Superclass;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;
  typedef FastMarchingTraits< TInput, TOutput >  Traits;

  typedef typename Traits::NodeType            NodeType;
  typedef typename Traits::OutputPixelType     OutputPixelType;
  typedef typename Traits::NodePairType        NodePairType;
  typedef typename Traits::OutputDomainType    OutputDomainType;
  typedef typename Traits::OutputDomainPointer OutputDomainPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingStoppingCriterionBase, StoppingCriterionBase);

  /** Reinitialize internal values. */
  void Reinitialize()
    {
    m_CurrentValue  = NumericTraits< OutputPixelType >::ZeroValue();
    m_PreviousValue = NumericTraits< OutputPixelType >::ZeroValue();

    this->Reset();
    }

  void SetCurrentNodePair( const NodePairType& iNodePair )
    {
    this->SetCurrentNode( iNodePair.GetNode() );
    this->SetCurrentValue( iNodePair.GetValue() );
    }

  itkSetObjectMacro( Domain, OutputDomainType );
  itkGetModifiableObjectMacro(Domain, OutputDomainType );

 protected:
  /** Constructor */
  FastMarchingStoppingCriterionBase() : Superclass(), m_Domain( ITK_NULLPTR )
  {
    m_CurrentValue = NumericTraits< OutputPixelType >::ZeroValue();
    m_PreviousValue = NumericTraits< OutputPixelType >::ZeroValue();
  }

  /** Destructor */
  virtual ~FastMarchingStoppingCriterionBase() ITK_OVERRIDE {}

  OutputDomainPointer m_Domain;

  OutputPixelType m_PreviousValue;
  OutputPixelType m_CurrentValue;

  /** Inherited classes must implement this method and make sure member variables
  got reinitialized. */
  virtual void Reset() = 0;

  /** Set the Current Node */
  virtual void SetCurrentNode( const NodeType& iNode ) = 0;

  /** Set the Current Value */
  virtual void SetCurrentValue( const OutputPixelType& iValue )
    {
    m_PreviousValue = m_CurrentValue;
    m_CurrentValue = iValue;
    }

private:
  FastMarchingStoppingCriterionBase( const Self& );
  void operator = ( const Self& );
};
}
#endif
