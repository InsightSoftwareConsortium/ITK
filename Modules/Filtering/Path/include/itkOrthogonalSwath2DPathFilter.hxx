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
#ifndef itkOrthogonalSwath2DPathFilter_hxx
#define itkOrthogonalSwath2DPathFilter_hxx

#include "itkOrthogonalSwath2DPathFilter.h"
#include "itkMath.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TParametricPath, typename TSwathMeritImage >
OrthogonalSwath2DPathFilter< TParametricPath, TSwathMeritImage >
::OrthogonalSwath2DPathFilter()
{
  SizeType size;

  // Initialize the member variables
  size[0] = 0;
  size[1] = 0;
  m_SwathSize = size;
  m_StepValues  = ITK_NULLPTR;
  m_MeritValues = ITK_NULLPTR;
  m_OptimumStepsValues = ITK_NULLPTR;
  m_FinalOffsetValues = OrthogonalCorrectionTableType::New();
}

/**
 * Destructor
 */
template< typename TParametricPath, typename TSwathMeritImage >
OrthogonalSwath2DPathFilter< TParametricPath, TSwathMeritImage >
::~OrthogonalSwath2DPathFilter()
{
  delete[] m_StepValues;
  delete[] m_MeritValues;
  delete[] m_OptimumStepsValues;
}

/**
 * GenerateData Performs the reflection
 */
template< typename TParametricPath, typename TSwathMeritImage >
void
OrthogonalSwath2DPathFilter< TParametricPath, TSwathMeritImage >
::GenerateData(void)
{
  // Get a convenience pointer
  ImageConstPointer swathMeritImage = this->GetImageInput();

  // Re-initialize the member variables
  m_SwathSize = swathMeritImage->GetLargestPossibleRegion().GetSize();
  delete[] m_StepValues;
  delete[] m_MeritValues;
  delete[] m_OptimumStepsValues;
  m_StepValues = new int[m_SwathSize[0] * m_SwathSize[1] * m_SwathSize[1]];
  m_MeritValues = new double[m_SwathSize[0] * m_SwathSize[1] * m_SwathSize[1]];
  m_OptimumStepsValues = new int[m_SwathSize[0]];
  m_FinalOffsetValues->Initialize();

  // Perform the remaining calculations; use dynamic programming

  // current swath column (all previous columns have been fully processed)
  unsigned int x;
  // current first row and last row of the swath.
  unsigned int F, L;
  // index used to access the processed swath image; filled in with x, F, & L
  IndexType index;

  // CalcFirstStep (x=0)
  // Enter the initial merit values
  index[0] = 0;
  for ( F = 0; F < m_SwathSize[1]; F++ )
    {
    for ( L = 0; L < m_SwathSize[1]; L++ )
      {
      if ( F == L )
        {
        index[1] = F;
        MeritValue(F, L, 0) = (double)swathMeritImage->GetPixel(index);
        StepValue(F, L, 0) = F;
        }
      else
        {
        MeritValue(F, L, 0) = NumericTraits< double >::NonpositiveMin();
        StepValue(F, L, 0) = F;
        }
      }
    }
  // end of double for-loop covering F & L

  // PrepForRemainingSteps
  for ( F = 0; F < m_SwathSize[1]; F++ )
    {
    for ( L = 0; L < m_SwathSize[1]; L++ )
      {
      // find merit for x=1
      if ( itk::Math::abs(F - L) <= 1 )
        {
        IndexType index2; // we need a second index here
        index[0] = 0;
        index[1] = F;
        index2[0] = 1;
        index2[1] = L;
        // Here we know in advance that Pixel(0,F) =
        // Max(l=L-1..L+1){Merit(F,l,0)}
        MeritValue(F, L, 1) = double( swathMeritImage->GetPixel(index)
                                      + swathMeritImage->GetPixel(index2) );
        }
      else
        {
        MeritValue(F, L, 1) = NumericTraits< double >::NonpositiveMin();
        }
      // Enter the final step values (x=SWATH_COLUMNS-1)
      StepValue(F, L, m_SwathSize[0] - 1) = L;
      }
    }
  // end of double for-loop covering F & L

  // CalcRestPath
  for ( x = 1; x < m_SwathSize[0] - 1; x++ )
    {
    for ( F = 0; F < m_SwathSize[1]; F++ )
      {
      for ( L = 0; L < m_SwathSize[1]; L++ )
        {
        int bestL = FindAndStoreBestErrorStep(x, F, L);
        index[0] = x + 1;
        index[1] = L;
        MeritValue(F, L, x + 1) = MeritValue(F, bestL, x)
                                  + double( swathMeritImage->GetPixel(index) );
        }
      }
    }
  // end of tripple for-loop covering x & F & L

  // Find the best starting and ending points (F & L) for the path
  int    bestF = 0, bestL = 0;
  double meritTemp, meritMax = NumericTraits< double >::NonpositiveMin();
  for ( F = 0; F < m_SwathSize[1]; F++ )
    {
    for ( L = 0; L < m_SwathSize[1]; L++ )
      {
      if ( itk::Math::abs(F - L) <= 1 ) // only accept closed paths
        {
        meritTemp = MeritValue(F, L, m_SwathSize[0] - 1);
        if ( meritTemp > meritMax )
          {
          meritMax = meritTemp;
          bestF = F;
          bestL = L;
          }
        }
      }
    }
  // end of double for-loop covering F & L

  // Fill in the optimum path error-step (orthogonal correction) values
  m_OptimumStepsValues[m_SwathSize[0] - 1] = bestL;
  for ( x = m_SwathSize[0] - 2;; x-- )
    {
    m_OptimumStepsValues[x] = StepValue(bestF, m_OptimumStepsValues[x + 1], x);
    if ( 0 == x ) { break; }
    }

  // Convert from absolute indices to +/- orthogonal offset values
  for ( x = 0; x < m_SwathSize[0]; x++ )
    {
    m_FinalOffsetValues->InsertElement(
      x,  double( m_OptimumStepsValues[x] - int(m_SwathSize[1] / 2) ) );
    }

  // setup the output path
  OutputPathPointer outputPtr = this->GetOutput(0);
  outputPtr->SetOriginalPath( this->GetPathInput() );
  outputPtr->SetOrthogonalCorrectionTable(m_FinalOffsetValues);
}

template< typename TParametricPath, typename TSwathMeritImage >
void
OrthogonalSwath2DPathFilter< TParametricPath, TSwathMeritImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "StepValues:  " << m_StepValues << std::endl;
  os << indent << "MeritValues:  " << m_MeritValues << std::endl;
  os << indent << "OptimumStepsValues:  " << m_OptimumStepsValues << std::endl;
  os << indent << "FinalOffsetValues:  " << m_FinalOffsetValues << std::endl;
}

// The next three functions are private helper functions
template< typename TParametricPath, typename TSwathMeritImage >
unsigned int
OrthogonalSwath2DPathFilter< TParametricPath, TSwathMeritImage >
::FindAndStoreBestErrorStep(unsigned int x, unsigned int F, unsigned int L)
{
  unsigned int bestL; // L with largest merit of L and its 2 neighbors L-1 & L+1

  // Handle perimeter boundaries of the vert. gradient image
  if ( L == 0 )
    {
    if ( MeritValue(F, L + 1, x) > MeritValue(F, L, x) )
      {
      bestL = L + 1;
      }
    else
      {
      bestL = L;
      }
    }
  else if ( L == m_SwathSize[1] - 1 )
    {
    if ( MeritValue(F, L - 1, x) > MeritValue(F, L, x) )
      {
      bestL = L - 1;
      }
    else
      {
      bestL = L;
      }
    }
  else
    {
    // We are now free to consider all 3 cases for bestL
    if ( MeritValue(F, L + 1, x) > MeritValue(F, L, x)
         && MeritValue(F, L + 1, x) > MeritValue(F, L - 1, x) )
      {
      bestL = L + 1;
      }
    else if ( MeritValue(F, L - 1, x) > MeritValue(F, L, x)
              && MeritValue(F, L - 1, x) > MeritValue(F, L + 1, x) )
      {
      bestL = L - 1;
      }
    else
      {
      bestL = L;
      }
    }
  StepValue(F, L, x) = bestL;
  return bestL;
}
} // end namespace itk

#endif
