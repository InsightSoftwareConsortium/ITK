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
#ifndef itkPolyLineParametricPath_hxx
#define itkPolyLineParametricPath_hxx

#include "itkPolyLineParametricPath.h"
#include "itkMacro.h"
#include <cmath>
#include "itkMath.h"

namespace itk
{
template< unsigned int VDimension >
typename PolyLineParametricPath< VDimension >::OutputType
PolyLineParametricPath< VDimension >
::Evaluate(const InputType & input) const
{
  // Handle the endpoint carefully, since there is no following vertex
  const InputType endPoint = static_cast< InputType >( m_VertexList->Size() - 1 );
  if ( input > endPoint || itk::Math::FloatAlmostEqual( input, endPoint ) )
    {
    return static_cast<const VertexListType*>(this->m_VertexList)->ElementAt(m_VertexList->Size() - 1); // the last vertex
    }

  const VertexType vertex0 = static_cast<const VertexListType*>(this->m_VertexList)->ElementAt( int(input) );
  const VertexType vertex1 = static_cast<const VertexListType*>(this->m_VertexList)->ElementAt( int(input) + 1 );

  const double fractionOfLineSegment = input - int(input);

  const PointType outputPoint = vertex0 + ( vertex1 - vertex0 ) * fractionOfLineSegment;

  // For some stupid reason, there is no easy way to cast from a point to a
  // continuous index.
  OutputType output;
  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    output[i] = outputPoint[i];
    }

  return output;
}

template<unsigned int VDimension>
typename PolyLineParametricPath<VDimension>::VectorType
PolyLineParametricPath<VDimension>
::EvaluateDerivative(const InputType & input) const
{
  //Get next integral time-point
  const InputType nextTimepoint = std::min(std::floor(input + 1.0), this->EndOfInput());

  //Get previous integral time-point
  const InputType previousTimepoint = nextTimepoint - 1.0;

  //Calculate the continuous index for both points
  const ContinuousIndexType nextIndex = this->Evaluate(nextTimepoint);
  const ContinuousIndexType prevIndex = this->Evaluate(previousTimepoint);

  //For some reason, there's no way to convert ContinuousIndexType to VectorType
  VectorType partialDerivatives;
  for (unsigned int i = 0; i < VDimension; ++i)
    {
    partialDerivatives[i] = nextIndex[i] - prevIndex[i];
    }

  return partialDerivatives;
}

template<unsigned int VDimension>
typename PolyLineParametricPath<VDimension>::OffsetType
PolyLineParametricPath<VDimension>
::IncrementInput(InputType & input) const
{
  //Save this input index since we will use it to calculate the offset
  const IndexType originalIndex = this->EvaluateToIndex(input);

  InputType potentialTimestep = itk::NumericTraits< InputType >::ZeroValue();
  bool timeStepSmallEnough = false;
  while (!timeStepSmallEnough)
    {
    if (Math::ExactlyEquals(input, this->EndOfInput()))
      {
      const IndexType finalIndex = this->EvaluateToIndex(this->EndOfInput());
      OffsetType finalOffset;
      for (unsigned int i = 0; i < VDimension; ++i)
        {
        finalOffset[i] = finalIndex[i] - originalIndex[i];
        }
      return finalOffset;
      }

    //Check to make sure we aren't already at a place with an offset of 1 pixel
    const IndexType potentialIndex = this->EvaluateToIndex(input);
    //For some reason, there's no way to convert OutputType to OffsetType
    OffsetType offset;
    for (unsigned int i = 0; i < VDimension; ++i)
      {
      offset[i] = potentialIndex[i] - originalIndex[i];
      }

    if (offset != this->GetZeroOffset())
      {
      return offset;
      }
    //Get the derivative at the current input
    VectorType partialDerivatives = this->EvaluateDerivative(input);

    //Find the largest of all the partial derivatives
    unsigned int maxPartialDerivativeIndex = 0;
    for (unsigned int i = 1; i < VDimension; ++i)
      {
      if (std::abs(partialDerivatives[i]) > std::abs(partialDerivatives[maxPartialDerivativeIndex]))
        {
        maxPartialDerivativeIndex = i;
        }
      }

    //Calculate the timestep required to effect a 1 pixel change
    potentialTimestep = 1.0/std::abs(partialDerivatives[maxPartialDerivativeIndex]);

    //Check to make sure the timestep doesn't put the input past the next integral timestep
    //(since the derivatives can change)
    if (input + potentialTimestep > std::floor(input + 1.0))
      {
      input = std::floor(input + 1.0); //Set the input to the next integral time-step
      }
    else
      {
      timeStepSmallEnough = true;
      }
    }

  //Finalize the potential timestep into THE timestep
  const InputType timestep = potentialTimestep;

  //Get the index at the next timestep so we can calculate the offset
  const IndexType nextIndex = this->EvaluateToIndex(input + timestep);

  //For some reason, there's no way to convert OutputType to OffsetType
  OffsetType offset;
  for (unsigned int i = 0; i < VDimension; ++i)
    {
    offset[i] = nextIndex[i] -  originalIndex[i];
    }

  //Update input timestep
  input += timestep;

  //Return the offset
  return offset;
}

template< unsigned int VDimension >
PolyLineParametricPath< VDimension >
::PolyLineParametricPath()
{
  this->SetDefaultInputStepSize(0.3);
  this->m_VertexList = VertexListType::New();
}

template< unsigned int VDimension >
void
PolyLineParametricPath< VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Verticies:  " << m_VertexList << std::endl;
}
} // end namespace itk

#endif
