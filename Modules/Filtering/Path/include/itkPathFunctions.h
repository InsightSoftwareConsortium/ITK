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
#ifndef itkPathFunctions_h
#define itkPathFunctions_h

#include "itkChainCodePath.h"
#include "itkFourierSeriesPath.h"
#include <cmath>

namespace itk
{
/** Make a chain code trace another path of same dimensionality.
 * If restrictMovement is true, then individual steps are allowed to move
 * through only one dimension at a time; for 2D paths this results in an
 * 8-connected chain code. */
template< typename TChainCodePath, typename TPathInput >
void MakeChainCodeTracePath(TChainCodePath & chainPath,
                            const TPathInput & inPath,
                            bool restrictMovement = false)
{
  typedef typename TChainCodePath::OffsetType OffsetType;
  typedef typename TChainCodePath::InputType  ChainInputType;
  typedef typename TPathInput::InputType      InPathInputType;

  OffsetType      offset, tempOffset, zeroOffset;
  InPathInputType inPathInput;
  int             dimension = OffsetType::GetOffsetDimension();

  zeroOffset.Fill(0);

  chainPath.Clear();
  inPathInput = inPath.StartOfInput();
  chainPath.SetStart( inPath.EvaluateToIndex(inPathInput) );

  for ( ChainInputType chainInput = 0;; )
    {
    offset  = inPath.IncrementInput(inPathInput);
    if ( zeroOffset == offset ) { break; }

    if ( !restrictMovement )
      {
      chainPath.InsertStep(chainInput++, offset);
      }
    else
      {
      for ( int d = 0; d < dimension; d++ )
        {
        tempOffset.Fill(0);
        tempOffset[d] = offset[d];
        chainPath.InsertStep(chainInput++, tempOffset);
        }
      }
    }
}

/** Make a Fourier series path trace a chain code path of same dimensionality.
 * numHarmonics is the number of harmonics (frequency coefficients, which
 * include the "DC" term) to compute.  If chainPath has too few steps to
 * calculate numHarmonics (due to the Nyquist criterion), then as many harmonics
 * as possible (chainPath->NumberOfSteps()/2) will be calculated.  No fewer than
 * 2 harmonics will be calcualted. */
template< typename TFourierSeriesPath, typename TChainCodePath >
void MakeFourierSeriesPathTraceChainCode(TFourierSeriesPath & FSPath,
                                         const TChainCodePath & chainPath,
                                         unsigned int numHarmonics = 8)
{
  typedef typename TFourierSeriesPath::IndexType  IndexType;
  typedef typename TFourierSeriesPath::OffsetType OffsetType;
  typedef typename TFourierSeriesPath::VectorType VectorType;

  typedef typename TFourierSeriesPath::InputType  FSInputType;
  typedef typename TChainCodePath::InputType      ChainInputType;

  IndexType   index;
  VectorType  indexVector;
  VectorType  cosCoefficient;
  VectorType  sinCoefficient;
  FSInputType theta;
  int         dimension =     OffsetType::GetOffsetDimension();
  size_t      numSteps  =     chainPath.NumberOfSteps();

  const double PI = 4.0 * std::atan(1.0);

  FSPath.Clear();

  // Adjust our private copy of numHarmonics if necessary
  if ( numHarmonics <= 1 )
    {
    numHarmonics = 2;
    }
  else if ( numHarmonics * 2 > numSteps )
    {
    numHarmonics = numSteps / 2;
    }

  for ( unsigned n = 0; n < numHarmonics; n++ )
    {
    index = chainPath.GetStart();
    cosCoefficient.Fill(0.0);
    sinCoefficient.Fill(0.0);

    for ( ChainInputType step = 0; step < numSteps; step++ )
      {
      index += chainPath.Evaluate(step);
      theta = 2 * n * PI * ( double(step + 1) ) / numSteps;

      // turn the current index into a vector
      for ( int d = 0; d < dimension; d++ )
        {
        indexVector[d] = index[d];
        }
      cosCoefficient += indexVector * ( std::cos(theta) / numSteps );
      sinCoefficient += indexVector * ( std::sin(theta) / numSteps );
      }

    FSPath.AddHarmonic(cosCoefficient, sinCoefficient);
    }
}
} // end namespace itk

#endif
