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
#ifndef itkFastMarchingExtensionImageFilterBase_hxx
#define itkFastMarchingExtensionImageFilterBase_hxx

#include "itkFastMarchingExtensionImageFilterBase.h"

namespace itk
{
template< typename TInput, typename TOutput,
         typename TAuxValue,
         unsigned int VAuxDimension >
FastMarchingExtensionImageFilterBase< TInput, TOutput, TAuxValue, VAuxDimension >
::FastMarchingExtensionImageFilterBase()
{
  m_AuxiliaryAliveValues = ITK_NULLPTR;
  m_AuxiliaryTrialValues = ITK_NULLPTR;

  this->ProcessObject::SetNumberOfRequiredOutputs(1 + AuxDimension);

  AuxImagePointer ptr;
  for ( unsigned int k = 0; k < AuxDimension; k++ )
    {
    ptr = AuxImageType::New();
    this->ProcessObject::SetNthOutput( k + 1, ptr.GetPointer() );
    this->m_AuxImages[k] = ITK_NULLPTR;
    }
}

template< typename TInput, typename TOutput,
         typename TAuxValue,
         unsigned int VAuxDimension >
void
FastMarchingExtensionImageFilterBase< TInput, TOutput, TAuxValue, VAuxDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Aux alive values: ";
  //os << m_AuxiliaryAliveValues.GetPointer() << std::endl;
  os << indent << "Aux trail values: ";
  //os << m_AuxiliaryTrialValues.GetPointer() << std::endl;
}


 template< typename TInput, typename TOutput,
         typename TAuxValue,
         unsigned int VAuxDimension >
typename FastMarchingExtensionImageFilterBase< TInput, TOutput, TAuxValue, VAuxDimension >
::AuxImageType *
FastMarchingExtensionImageFilterBase< TInput, TOutput, TAuxValue, VAuxDimension >
::GetAuxiliaryImage(const unsigned int& idx)
{
  if ( idx >= AuxDimension || this->GetNumberOfIndexedOutputs() < idx + 2 )
    {
    return ITK_NULLPTR;
    }

  return static_cast< AuxImageType * >( this->ProcessObject::GetOutput(idx + 1) );
}

/*
 *
 */
template< typename TInput, typename TOutput,
         typename TAuxValue,
         unsigned int VAuxDimension >
void
FastMarchingExtensionImageFilterBase< TInput, TOutput, TAuxValue, VAuxDimension >
::GenerateOutputInformation()
{
  // call the superclass implementation of this function
  this->Superclass::GenerateOutputInformation();

  // set the size of all the auxiliary outputs
  // to be the same as the primary output
  OutputImageType* primaryOutput = this->GetOutput();
  for ( unsigned int k = 0; k < AuxDimension; k++ )
    {
    AuxImageType *ptr = this->GetAuxiliaryImage(k);
    ptr->CopyInformation(primaryOutput);
    }
}

/*
 *
 */
template< typename TInput, typename TOutput,
         typename TAuxValue,
         unsigned int VAuxDimension >
void
FastMarchingExtensionImageFilterBase< TInput, TOutput, TAuxValue, VAuxDimension >
::EnlargeOutputRequestedRegion(
  DataObject *itkNotUsed(output) )
{
  // This filter requires all of the output images in the buffer.
  for ( unsigned int j = 0; j < this->GetNumberOfIndexedOutputs(); j++ )
    {
    if ( this->ProcessObject::GetOutput(j) )
      {
      this->ProcessObject::GetOutput(j)->SetRequestedRegionToLargestPossibleRegion();
      }
    }
}

/*
 *
 */
template< typename TInput, typename TOutput,
         typename TAuxValue,
         unsigned int VAuxDimension >
void
FastMarchingExtensionImageFilterBase< TInput, TOutput, TAuxValue, VAuxDimension >
::InitializeOutput(OutputImageType* oImage)
{
  this->Superclass::InitializeOutput( oImage );

  if ( !m_AuxiliaryAliveValues )
    {
    itkExceptionMacro(<< "in Initialize(): Null pointer for AuxAliveValues");
    }
  if ( m_AuxiliaryAliveValues->size() != ( this->m_AlivePoints->size() ) )
    {
    itkExceptionMacro(<< "in Initialize(): AuxAliveValues is the wrong size");
    }

  if( !m_AuxiliaryTrialValues )
    {
    itkExceptionMacro(<< "in Initialize(): Null pointer for AuxTrialValues");
    }
  if ( m_AuxiliaryTrialValues->size() != this->m_TrialPoints->size() )
    {
    itkExceptionMacro(<< "in Initialize(): AuxTrialValues is the wrong size");
    }

  // allocate memory for the auxiliary outputs
  for ( unsigned int k = 0; k < AuxDimension; k++ )
    {
    AuxImageType *ptr = this->GetAuxiliaryImage(k);
    ptr->SetBufferedRegion( ptr->GetRequestedRegion() );
    ptr->Allocate();
    this->m_AuxImages[k] = ptr;
    }

  // set all alive points to alive
  typename Superclass::NodeType node;

  AuxValueVectorType auxVec;

  if ( m_AuxiliaryAliveValues )
    {
    AuxValueContainerConstIterator auxIter = m_AuxiliaryAliveValues->Begin();
    NodePairContainerConstIterator pointsIter =  this->m_AlivePoints->Begin();
    NodePairContainerConstIterator pointsEnd =  this->m_AlivePoints->Begin();

    while( pointsIter != pointsEnd )
      {
      node = pointsIter->Value().GetNode();
      auxVec = auxIter->Value();

      // check if node index is within the output level set
      if ( this->m_BufferedRegion.IsInside( node ) )
        {
        for ( unsigned int k = 0; k < AuxDimension; k++ )
          {
          this->m_AuxImages[k]->SetPixel( node, auxVec[k] );
          }
        }
      ++pointsIter;
      ++auxIter;
      } // end container while
    }   // if AuxAliveValues set

  if ( m_AuxiliaryTrialValues )
    {
    AuxValueContainerConstIterator auxIter = m_AuxiliaryTrialValues->Begin();
    NodePairContainerConstIterator pointsIter =  this->m_TrialPoints->Begin();
    NodePairContainerConstIterator pointsEnd =  this->m_TrialPoints->End();

    while ( pointsIter != pointsEnd )
      {
      node = pointsIter->Value().GetNode();
      auxVec = auxIter->Value();

      // check if node index is within the output level set
      if ( this->m_BufferedRegion.IsInside( node ) )
        {
        for ( unsigned int k = 0; k < AuxDimension; k++ )
          {
          this->m_AuxImages[k]->SetPixel( node, auxVec[k]);
          }
        }
      ++pointsIter;
      ++auxIter;
      } // end container loop
    }   // if AuxTrialValues set
}

template< typename TInput, typename TOutput,
         typename TAuxValue,
         unsigned int VAuxDimension >
void
FastMarchingExtensionImageFilterBase< TInput, TOutput, TAuxValue, VAuxDimension >
::UpdateValue( OutputImageType* oImage, const NodeType& iNode )
{
  // A extension value at node is chosen such that
  // grad(F) dot_product grad(Phi) = 0
  // where F is the extended speed function and Phi is
  // the level set function.
  //
  // The extension value can approximated as a weighted
  // sum of the values from nodes used in the calculation
  // of the distance by the superclass.
  //
  // For more detail see Chapter 11 of
  // "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  // Cambridge Press, Second edition, 1999.

  typename Superclass::InternalNodeStructureArray NodesUsed;

  this->GetInternalNodesUsed( oImage, iNode, NodesUsed );

  InternalNodeStructure temp_node;

  OutputPixelType outputPixel =
      static_cast< OutputPixelType >( this->Solve( oImage, iNode, NodesUsed ) );

  if ( outputPixel < this->m_LargeValue )
    {
    oImage->SetPixel(iNode, outputPixel);

    // insert point into trial heap
    this->m_LabelImage->SetPixel( iNode, Traits::Trial );

    //node.SetValue( outputPixel );
    //node.SetIndex( index );
    //m_TrialHeap.push(node);
    this->m_Heap.push( NodePairType( iNode, outputPixel ) );

    // update auxiliary values
    for ( unsigned int k = 0; k < AuxDimension; k++ )
      {
      double       numer = 0.;
      double       denom = 0.;
      AuxValueType auxVal;

      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        temp_node = NodesUsed[j];

        if ( outputPixel < temp_node.m_Value )
          {
          break;
          }

        auxVal = this->m_AuxImages[k]->GetPixel( temp_node.m_Node );
        numer += auxVal * ( outputPixel - temp_node.m_Value );
        denom += outputPixel - temp_node.m_Value;
        }

      if ( denom > itk::Math::eps )
        {
        auxVal = static_cast< AuxValueType >( numer / denom );
        }
      else
        {
        auxVal = NumericTraits< AuxValueType >::ZeroValue();
        }

      this->m_AuxImages[k]->SetPixel(iNode, auxVal);
      }
    }
}
} // namespace itk

#endif
