/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathFunctions.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkPathFunctions_h
#define _itkPathFunctions_h


#include "itkPath.h"
#include "itkChainCodePath.h"
#include "itkFourierSeriesPath.h"
#include "itkOffset.h"
#include <math.h>

// not all versions of math.h seem to define M_PI:
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

namespace itk
{



/** Make a chain code trace another path of same dimensionality.
 * If restrictMovement is true, then individual steps are allowed to move
 * through only one dimension at a time; for 2D paths this results in an
 * 8-connected chain code. */
template <class TChainCodePath, class TPathInput>
void MakeChainCodeTracePath( TChainCodePath & chainPath,
                             const TPathInput & inPath,
                             bool restrictMovement = false )
{
  typedef typename TChainCodePath::OffsetType OffsetType;
  typedef typename TChainCodePath::InputType  ChainInputType;
  typedef typename TChainCodePath::OutputType ChainOutputType;
  typedef typename TPathInput::InputType      InPathInputType;
  typedef typename TPathInput::OutputType     InPathOutputType;
  
  OffsetType offset, tempOffset, zeroOffset;
  InPathInputType inPathInput;
  int dimension = OffsetType::GetOffsetDimension();
  
  zeroOffset.Fill(0);
  
  chainPath.Clear();
  inPathInput = inPath.StartOfInput();
  chainPath.SetStart(  inPath.EvaluateToIndex( inPathInput )  );
  
  for(ChainInputType chainInput=0;;)
    {
    offset  = inPath.IncrementInput(inPathInput);
    if( zeroOffset == offset ) { break; }
    
    if( ! restrictMovement )
      {
      chainPath.InsertStep( chainInput++, offset );
      }
    else
      {
      for( int d=0; d<dimension; d++ )
        {
        tempOffset.Fill(0);
        tempOffset[d] = offset[d];
        chainPath.InsertStep( chainInput++, tempOffset );
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
template <class TFourierSeriesPath, class TChainCodePath>
void MakeFourierSeriesPathTraceChainCode( TFourierSeriesPath & FSPath,
                             const TChainCodePath & chainPath,
                             unsigned int numHarmonics = 8 )
{
  typedef typename TFourierSeriesPath::IndexType  IndexType;
  typedef typename TFourierSeriesPath::OffsetType OffsetType;
  typedef typename TFourierSeriesPath::VectorType VectorType;
  
  typedef typename TFourierSeriesPath::InputType  FSInputType;
  typedef typename TFourierSeriesPath::OutputType FSOutputType;
  typedef typename TChainCodePath::InputType      ChainInputType;
  typedef typename TChainCodePath::OutputType     ChainOutputType;
  
  IndexType   index;
  VectorType  indexVector;
  VectorType  cosCoefficient;
  VectorType  sinCoefficient;
  FSInputType theta;
  int         dimension =     OffsetType::GetOffsetDimension();
  unsigned    numSteps  =     chainPath.NumberOfSteps();
  
  FSPath.Clear();
  
  // Adjust our private copy of numHarmonics if necessary
  if( numHarmonics <= 1 )
    numHarmonics = 2;
  else if( numHarmonics*2 > numSteps )
    numHarmonics = numSteps / 2;
  
  for( unsigned n=0; n<numHarmonics; n++ )
    {
    index = chainPath.GetStart();
    cosCoefficient.Fill(0.0);
    sinCoefficient.Fill(0.0);
    
    for( ChainInputType step=0; step<numSteps; step++ )
      {
      index += chainPath.Evaluate( step );
      theta = 2*n*M_PI*(double(step+1))/numSteps;
      
      // turn the current index into a vector
      for( int d=0; d<dimension; d++ )
        indexVector[d] = index[d];
      
      cosCoefficient += indexVector * (cos(theta)/numSteps);
      sinCoefficient += indexVector * (sin(theta)/numSteps);
      }
    
    FSPath.AddHarmonic( cosCoefficient, sinCoefficient );
    }
}



} // namespace itk

#endif
