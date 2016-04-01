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

#ifndef itkFastMarchingBase_hxx
#define itkFastMarchingBase_hxx

#include "itkFastMarchingBase.h"

#include "itkProgressReporter.h"
#include "itkMath.h"
#include "itkMath.h"

namespace itk
{
// -----------------------------------------------------------------------------
template< typename TInput, typename TOutput >
FastMarchingBase< TInput, TOutput >::
FastMarchingBase()
  {
  this->ProcessObject::SetNumberOfRequiredInputs(0);

  m_TrialPoints = ITK_NULLPTR;
  m_AlivePoints = ITK_NULLPTR;
  m_ProcessedPoints = ITK_NULLPTR;
  m_ForbiddenPoints = ITK_NULLPTR;

  //m_Heap = PriorityQueueType::New();
  m_SpeedConstant = 1.;
  m_InverseSpeed = -1.;
  m_NormalizationFactor = 1.;
  m_TargetReachedValue = NumericTraits< OutputPixelType >::ZeroValue();
  m_TopologyCheck = Nothing;
  m_LargeValue = NumericTraits< OutputPixelType >::max();
  m_TopologyValue = m_LargeValue;
  m_CollectPoints = false;
  }
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TInput, typename TOutput >
FastMarchingBase< TInput, TOutput >::
~FastMarchingBase()
  {
  }
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TInput, typename TOutput >
void
FastMarchingBase< TInput, TOutput >::
PrintSelf( std::ostream & os, Indent indent ) const
  {
  Superclass::PrintSelf( os, indent );
  os << indent << "Speed constant: " << m_SpeedConstant << std::endl;
  os << indent << "Topology check: " << m_TopologyCheck << std::endl;
  os << indent << "Normalization Factor: " << m_NormalizationFactor << std::endl;
  }

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TInput, typename TOutput >
void
FastMarchingBase< TInput, TOutput >::
Initialize( OutputDomainType* oDomain )
  {
  if( m_TrialPoints.IsNull() )
    {
    itkExceptionMacro( <<"No Trial Nodes" );
    }
  if( m_StoppingCriterion.IsNull() )
    {
    itkExceptionMacro( <<"No Stopping Criterion Set" );
    }
  if( m_NormalizationFactor < itk::Math::eps )
    {
    itkExceptionMacro( <<"Normalization Factor is null or negative" );
    }
  if( m_SpeedConstant < itk::Math::eps )
    {
    itkExceptionMacro( <<"SpeedConstant is null or negative" );
    }
  if( m_CollectPoints )
    {
    if( m_ProcessedPoints.IsNull() )
      {
      m_ProcessedPoints = NodePairContainerType::New();
      }
    }

  // make sure the heap is empty
  while ( !m_Heap.empty() )
    {
    m_Heap.pop();
    }
  /*
  while ( !m_Heap->Empty() )
    {
    m_Heap->Pop();
    }
  */

  this->InitializeOutput( oDomain );

  // By setting the output domain to the stopping criterion, we enable funky
  // criterion based on informations extracted from it
  m_StoppingCriterion->SetDomain( oDomain );
  }
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
template< typename TInput, typename TOutput >
void
FastMarchingBase< TInput, TOutput >::
GenerateData()
  {
  OutputDomainType* output = this->GetOutput();

  Initialize( output );

  OutputPixelType current_value = 0.;

  ProgressReporter progress( this, 0, this->GetTotalNumberOfNodes() );

  m_StoppingCriterion->Reinitialize();

  try
    {
    //while( !m_Heap->Empty() )
    while( !m_Heap.empty() )
      {
      //PriorityQueueElementType element = m_Heap->Peek();
      //m_Heap->Pop();
      //
      //NodeType current_node = element.m_Element;
      //OutputPixelType current_value = element.m_Priority;


      NodePairType current_node_pair = m_Heap.top();
      m_Heap.pop();

      NodeType current_node = current_node_pair.GetNode();
      current_value = this->GetOutputValue( output, current_node );

      if( Math::ExactlyEquals(current_value, current_node_pair.GetValue()) )
        {
        // is this node already alive ?
        if( this->GetLabelValueForGivenNode( current_node ) != Traits::Alive )
          {
          m_StoppingCriterion->SetCurrentNodePair( current_node_pair );

          if( m_StoppingCriterion->IsSatisfied() )
            {
            break;
            }

          if( this->CheckTopology( output, current_node ) )
            {
            if ( m_CollectPoints )
              {
              m_ProcessedPoints->push_back( current_node_pair );
              }

              // set this node as alive
            this->SetLabelValueForGivenNode( current_node, Traits::Alive );

            // update its neighbors
            this->UpdateNeighbors( output, current_node );
            }
          }
        progress.CompletedPixel();
        }
      }
    }
  catch ( ProcessAborted & )
    {
    // User aborted filter execution Here we catch an exception thrown by the
    // progress reporter and rethrow it with the correct line number and file
    // name. We also invoke AbortEvent in case some observer was interested on
    // it.
    //
    // RELEASE MEMORY!!!
    while( !m_Heap.empty() )
      {
      m_Heap.pop();
      }
    /*while( !m_Heap->Empty() )
      {
      m_Heap->Pop();
      }*/

    throw ProcessAborted(__FILE__, __LINE__);
    }

  m_TargetReachedValue = current_value;

  // let's release some useless memory...
  while( !m_Heap.empty() )
    {
    m_Heap.pop();
    }
  /*while( !m_Heap->Empty() )
    {
    m_Heap->Pop();
    }*/
  }
// -----------------------------------------------------------------------------

} // end of namespace itk

#endif
