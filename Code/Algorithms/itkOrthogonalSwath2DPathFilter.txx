/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonalSwath2DPathFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkOrthogonalSwath2DPathFilter_txx
#define _itkOrthogonalSwath2DPathFilter_txx

#include "itkOrthogonalSwath2DPathFilter.h"
#include "vnl/vnl_math.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * Constructor
 */
template <class TParametricPath, class TImage>
OrthogonalSwath2DPathFilter<TParametricPath, TImage>
::OrthogonalSwath2DPathFilter()
{
  SizeType size;
  
  // Initialize the member variables
  size[0]=0;
  size[1]=0;
  m_SwathSize = size;
  m_StepValues  = 0;
  m_MeritValues = 0;
  m_OptimumStepsValues = 0;
  m_FinalOffsetValues = OrthogonalCorrectionTableType::New();
  
  // Setup internal pipeline, which is used for preprocessing the image data
  
  // Extract the swath image
  m_Filter1 = Filter1Type::New();
  m_Filter1->SetImageInput( this->GetImageInput() );
  m_Filter1->SetPathInput( this->GetPathInput() );
  size[0]=512;
  size[1]=16*2+1; // the top 1 and bottom 1 rows are dropped when smoothing
  m_Filter1->SetSize(size);
  
  // wrap-pad the image so that the smoothing of the next step treats the swath
  // as a closed strip
  m_Filter2 = Filter2Type::New();
  m_Filter2->SetInput( m_Filter1->GetOutput() );
  unsigned long lowerfactors[2] = { 1, 0};  // pad 1 columns, 0 rows to the left
  unsigned long upperfactors[2] = { 1,  0}; // pad 1 columns, 0 rows to the right
  m_Filter2->SetPadLowerBound(lowerfactors);
  m_Filter2->SetPadUpperBound(upperfactors);
  m_Filter2->UpdateLargestPossibleRegion();
  
  // Smooth the padded swath image
  m_Filter3 = Filter3Type::New();
  m_Filter3->SetInput( m_Filter2->GetOutput() );
  double gaussianVariance = 1.0;
  // We want a 3x3 kernel.  Gausian Operator will not truncate its kernel width
  // to any less than a 5x5 kernel (kernel width of 3 for 1 center pixel + 2 edge
  // pixels).  However, GausianOperator always uses at least a 3x3 kernel, and so
  // setting the maximum error to 1.0 (no limit) will make it stop growing the
  // kernel at the desired 3x3 size.
  double maxError = 1.0;  
  m_Filter3->SetUseImageSpacingOff();
  m_Filter3->SetVariance( gaussianVariance );
  m_Filter3->SetMaximumError( maxError );
  
  // Find the vertical gradient of the smoothed & padded swath image
  m_Filter4 = Filter4Type::New();
  m_Filter4->SetInput( m_Filter3->GetOutput() );
  m_Filter4->SetOrder( 1 ); // first partial derivative
  m_Filter4->SetDirection( 1 ); // d/dy
}


/**
 * Destructor
 */
template <class TParametricPath, class TImage>
OrthogonalSwath2DPathFilter<TParametricPath, TImage>
::~OrthogonalSwath2DPathFilter()
{
  if(m_StepValues)          delete [] m_StepValues;
  if(m_MeritValues)         delete [] m_MeritValues;
  if(m_OptimumStepsValues)  delete [] m_OptimumStepsValues;
}


/**
 * GenerateData Performs the reflection
 */
template <class TParametricPath, class TImage>
void
OrthogonalSwath2DPathFilter<TParametricPath, TImage>
::GenerateData( void )
{
  // Run the internal pipeline
  FloatImagePointer outImage;
  outImage=m_Filter4->GetOutput();
  outImage->Update();
  
  // Re-initialize the member variables
  m_SwathSize = outImage->GetLargestPossibleRegion().GetSize();
  if(m_StepValues)          delete [] m_StepValues;
  if(m_MeritValues)         delete [] m_MeritValues;
  if(m_OptimumStepsValues)  delete [] m_OptimumStepsValues;
  m_StepValues  = new int[ m_SwathSize[0] * m_SwathSize[1] * m_SwathSize[1] ];
  m_MeritValues = new double[ m_SwathSize[0] * m_SwathSize[1] * m_SwathSize[1] ];
  m_OptimumStepsValues = new int[ m_SwathSize[0] ];
  m_FinalOffsetValues->Initialize();
  
  
  // Perform the remaining calculations; use dynamic programming
  
  // current column of the swath (all previous columns have been fully processed)
  unsigned int x;
  // current first row and last row of the swath.
  unsigned int F,L;
  // index used to access the processed swath image; filled in with x, F, & L
  IndexType index;
  
  // CalcFirstStep (x=0)
  // Enter the initial merit values
  index[0]=0;
  for(F=0;F<m_SwathSize[1];F++) for(L=0;L<m_SwathSize[1];L++)
    {
    if(F==L)
      {
      index[1]=F;
      MeritValue(F,L,0) = (double) outImage->GetPixel(index);
      StepValue( F,L,0) = F;
      }
    else
      {
      MeritValue(F,L,0) = NumericTraits<double>::NonpositiveMin();
      StepValue( F,L,0) = F;
      }
    }
  // end of double for-loop covering F & L
  
  

  // PrepForRemainingSteps
  for(F=0;F<m_SwathSize[1];F++) for(L=0;L<m_SwathSize[1];L++)
    {
    // find merit for x=1
    if( vnl_math_abs(F-L) <= 1 )
      {
      IndexType index2; // we need a second index here
      index[0]=0;
      index[1]=F;
      index2[0]=1;
      index2[1]=L;
      // Here we know in advance that Pixel(0,F) = Max(l=L-1..L+1){Merit(F,l,0)}
      MeritValue(F,L,1) = double( outImage->GetPixel(index)
                                + outImage->GetPixel(index2) );
      }
    else
      {
      MeritValue(F,L,1) = NumericTraits<double>::NonpositiveMin();
      }
    // Enter the final step values (x=SWATH_COLUMNS-1)
    StepValue(F,L,m_SwathSize[0]-1) = L;
    }
  // end of double for-loop covering F & L
  
  // CalcRestPath
  for(x=1;x<m_SwathSize[0]-1;x++)
  for(F=0;F<m_SwathSize[1];  F++)
  for(L=0;L<m_SwathSize[1];  L++)
    {
    int bestL = FindAndStoreBestErrorStep(x,F,L);
    index[0]=x+1;
    index[1]=L;
    MeritValue(F,L,x+1) = MeritValue(F,bestL,x) +
                          double( outImage->GetPixel(index) );
    }
  // end of tripple for-loop covering x & F & L
  
  
  // Find the best starting and ending points (F & L) for the path
  int bestF, bestL;
  double meritTemp, meritMax=NumericTraits<double>::NonpositiveMin();
  for(F=0;F<m_SwathSize[1];F++) for(L=0;L<m_SwathSize[1];L++)
    {
    if( vnl_math_abs(F-L) <= 1 ) // only accept closed paths
      {
      meritTemp = MeritValue( F, L, m_SwathSize[0]-1 );
      if( meritTemp > meritMax )
        {
        meritMax = meritTemp;
        bestF = F;
        bestL = L;
        }
      }
    }
  // end of double for-loop covering F & L
  
  // Fill in the optimum path error-step (orthogonal correction) values
  m_OptimumStepsValues[ m_SwathSize[0]-1 ] = bestL;
  for(x=m_SwathSize[0]-2; x>=0; x--)
    {
    m_OptimumStepsValues[x] = StepValue(bestF, m_OptimumStepsValues[x+1], x);
    }
  
  // Convert from absolute indicies to +/- orthogonal offset values
  for(x=0;x<m_SwathSize[0];x++)
    {
    m_FinalOffsetValues->InsertElement( 
        x,  double( m_OptimumStepsValues[x] - m_SwathSize[1]/2 )  );
    }
  
  // setup the output path
  OutputPathPointer outputPtr = this->GetOutput(0);
  outputPtr->SetOriginalPath(this->GetPathInput());
  outputPtr->SetOrthogonalCorrectionTable(m_FinalOffsetValues);
}

template <class TParametricPath, class TImage>
void
OrthogonalSwath2DPathFilter<TParametricPath, TImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

// The next three functions are private helper functions
template <class TParametricPath, class TImage>
unsigned int
OrthogonalSwath2DPathFilter<TParametricPath, TImage>
::FindAndStoreBestErrorStep(unsigned int x, unsigned int F, unsigned int L)
{
  unsigned int bestL; // L with largest merit of L and its 2 neighbors L-1 & L+1
  
  // Handle perimeter boundaries of the vert. gradient image
  if(L==0)
    {
    if( MeritValue(F,L+1,x) > MeritValue(F,L,x) )
      bestL = L+1;
    else
      bestL = L;
    }
  else if(L==m_SwathSize[1]-1)
    {
    if( MeritValue(F,L-1,x) > MeritValue(F,L,x) )
      bestL = L-1;
    else
      bestL = L;
    }
  else
    // We are now free to consider all 3 cases for bestL
    {
    if(      MeritValue(F,L+1,x) > MeritValue(F,L,x)
          && MeritValue(F,L+1,x) > MeritValue(F,L-1,x) )
      {
      bestL = L+1;
      }
    else if( MeritValue(F,L-1,x) > MeritValue(F,L,x)
          && MeritValue(F,L-1,x) > MeritValue(F,L+1,x) )
      {
      bestL = L-1;
      }
    else
      {
      bestL = L;
      }
    }
  StepValue(F,L,x) = bestL;
  return bestL;
}
template <class TParametricPath, class TImage>
inline int &
OrthogonalSwath2DPathFilter<TParametricPath, TImage>
::StepValue(int f, int l, int x)
{
  int rows=m_SwathSize[1];
  return m_StepValues[ (x*rows*rows) + (f*rows) + (l) ];
}
template <class TParametricPath, class TImage>
inline double &
OrthogonalSwath2DPathFilter<TParametricPath, TImage>
::MeritValue(int f, int l, int x)
{
  int rows=m_SwathSize[1];
  return m_MeritValues[ (x*rows*rows) + (f*rows) + (l) ];
}

} // end namespace itk

#endif
