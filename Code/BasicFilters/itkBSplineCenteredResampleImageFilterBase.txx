/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineCenteredResampleImageFilterBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBSplineCenteredResampleImageFilterBase_txx
#define _itkBSplineCenteredResampleImageFilterBase_txx
#include "itkBSplineCenteredResampleImageFilterBase.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkProgressReporter.h"
namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
BSplineCenteredResampleImageFilterBase<TInputImage, TOutputImage>
::BSplineCenteredResampleImageFilterBase()
{

}

/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutputImage>
void
BSplineCenteredResampleImageFilterBase<TInputImage, TOutputImage>
::PrintSelf(
  std::ostream& os, 
  Indent indent) const
{
  Superclass::PrintSelf( os, indent );
}

template <class TInputImage, class TOutputImage>
void BSplineCenteredResampleImageFilterBase<TInputImage, TOutputImage>
::InitializePyramidSplineFilter(int SplineOrder)
{
  switch (SplineOrder) 
    {   
    case 1 :
      m_gSize = 21;
      m_hSize = 2;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  1.; 
      m_g[1]  =  0.333333; 
      m_g[2]  = -0.333333; 
      m_g[3]  = -0.111111; 
      m_g[4]  =  0.111111;
      m_g[5]  =  0.037037; 
      m_g[6]  = -0.037037; 
      m_g[7]  = -0.0123457; 
      m_g[8]  =  0.0123457;
      m_g[9]  =  0.00411523; 
      m_g[10] = -0.00411523; 
      m_g[11] = -0.00137174; 
      m_g[12] =  0.00137174;
      m_g[13] =  0.000457247; 
      m_g[14] = -0.000457247; 
      m_g[15] = -0.000152416;
      m_g[16] =  0.000152416; 
      m_g[17] =  0.0000508053; 
      m_g[18] = -0.0000508053;
      m_g[19] = -0.0000169351; 
      m_g[20] =  0.0000169351;
      m_h[0] =  1; 
      m_h[1] =  0.5;
      break;
    
    case 2 :
      m_gSize = 21;
      m_hSize = 11;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.738417; 
      m_g[1]  =  0.307916; 
      m_g[2]  = -0.171064; 
      m_g[3]  = -0.0799199; 
      m_g[4]  =  0.0735791;
      m_g[5]  =  0.03108; 
      m_g[6]  = -0.0307862; 
      m_g[7]  = -0.0128561; 
      m_g[8]  =  0.0128425;
      m_g[9]  =  0.00535611; 
      m_g[10] = -0.00535548; 
      m_g[11] = -0.00223325; 
      m_g[12] =  0.00223322;
      m_g[13] =  0.000931242; 
      m_g[14] = -0.00093124; 
      m_g[15] = -0.000388322; 
      m_g[16] =  0.000388322;
      m_g[17] =  0.000161928; 
      m_g[18] = -0.000161928; 
      m_g[19] = -0.0000675233;
      m_g[20] =  0.0000675233;
      m_h[0]  =  1.20711; 
      m_h[1]  =  0.585786; 
      m_h[2]  = -0.12132; 
      m_h[3]  = -0.100505; 
      m_h[4]  =  0.0208153;
      m_h[5]  =  0.0172439; 
      m_h[6]  = -0.00357134; 
      m_h[7]  = -0.00295859; 
      m_h[8]  =  0.000612745;
      m_h[9]  =  0.000507614; 
      m_h[10] = -0.00010513;
      break;
      
    case 3 :
      m_gSize = 21;
      m_hSize = 16;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.708792; 
      m_g[1]  =  0.328616; 
      m_g[2]  = -0.165157; 
      m_g[3]  = -0.114448; 
      m_g[4]  =  0.0944036;
      m_g[5]  =  0.0543881; 
      m_g[6]  = -0.05193; 
      m_g[7]  = -0.0284868; 
      m_g[8]  =  0.0281854;
      m_g[9]  =  0.0152877; 
      m_g[10] = -0.0152508; 
      m_g[11] = -0.00825077; 
      m_g[12] =  0.00824629;
      m_g[13] =  0.00445865; 
      m_g[14] = -0.0044582; 
      m_g[15] = -0.00241009; 
      m_g[16] =  0.00241022;
      m_g[17] =  0.00130278; 
      m_g[18] = -0.00130313; 
      m_g[19] = -0.000704109; 
      m_g[20] =  0.000704784;
      m_h[0]  =  1.13726; 
      m_h[1]  =  0.625601; 
      m_h[2]  = -0.0870191; 
      m_h[3]  = -0.159256; 
      m_h[4]  =  0.0233167;
      m_h[5]  =  0.0426725; 
      m_h[6]  = -0.00624769; 
      m_h[7]  = -0.0114341; 
      m_h[8]  =  0.00167406;
      m_h[9]  =  0.00306375; 
      m_h[10] = -0.000448564; 
      m_h[11] = -0.000820929; 
      m_h[12] =  0.000120192;
      m_h[13] =  0.000219967; 
      m_h[14] = -0.0000322054; 
      m_h[15] = -0.00005894; 
      break;
    
    case 4 :
      m_gSize = 21;
      m_hSize = 20;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.673072; 
      m_g[1]  =  0.331218; 
      m_g[2]  = -0.139359; 
      m_g[3]  = -0.12051; 
      m_g[4]  =  0.086389;
      m_g[5]  =  0.0611801; 
      m_g[6]  = -0.0542989; 
      m_g[7]  = -0.034777; 
      m_g[8]  =  0.033388;
      m_g[9]  =  0.0206275; 
      m_g[10] = -0.0203475; 
      m_g[11] = -0.0124183; 
      m_g[12] =  0.0123625;
      m_g[13] =  0.00751369; 
      m_g[14] = -0.00750374; 
      m_g[15] = -0.00455348; 
      m_g[16] =  0.00455363;
      m_g[17] =  0.00276047; 
      m_g[18] = -0.00276406; 
      m_g[19] = -0.00167279; 
      m_g[20] =  0.00167938;
      m_h[0]  =  1.14324; 
      m_h[1]  =  0.643609; 
      m_h[2]  = -0.0937888; 
      m_h[3]  = -0.194993; 
      m_h[4]  =  0.030127;
      m_h[5]  =  0.0699433; 
      m_h[6]  = -0.0108345; 
      m_h[7]  = -0.0252663; 
      m_h[8]  =  0.00391424;
      m_h[9]  =  0.00912967; 
      m_h[10] = -0.00141437; 
      m_h[11] = -0.00329892; 
      m_h[12] =  0.000511068;
      m_h[13] =  0.00119204; 
      m_h[14] = -0.00018467; 
      m_h[15] = -0.000430732; 
      m_h[16] =  0.0000667289;
      m_h[17] =  0.000155641; 
      m_h[18] = -0.0000241119; 
      m_h[19] = -0.0000562395;
      break;
    default :
      // Throw an execption for unsupported splines.
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( "BSplineCenteredResampleImageFilterBase" );
      err.SetDescription( "SplineOrder for Centered pyramid filter must be 0 through 4. Requested spline order has not been implemented." );
      throw err;
      break;
    }
}

/** Reduce1DImage - reduces the vector of data (in) by a 
 *     factor of 2 and writes the results to the location specified
 *     by the Iterator (out).  inTraverseSize is the size of the in vector.
 */
template <class TInputImage, class TOutputImage>
void BSplineCenteredResampleImageFilterBase<TInputImage, TOutputImage>
::Reduce1DImage( const std::vector<double> & in, OutputImageIterator & out, 
                 unsigned int inTraverseSize, ProgressReporter &progress )
{
  long i1, i2;

  unsigned long outK, inK;
  unsigned long outTraverseSize = inTraverseSize/2;
  inTraverseSize = outTraverseSize*2;  // ensures that an even number is used.
  unsigned long inModK; // number for modulus math of in
  inModK = 2L * inTraverseSize;


  // TODO:  Need to allocate this once as a scratch variable instead of each time through.
  std::vector<double> temp;
  temp.resize(inTraverseSize);
  
  for (inK = 0; inK < inTraverseSize; inK++) 
    {
    temp[inK] = in[inK] * m_g[0];
  
    for (int i = 1; i < m_gSize; i++) 
      {
      // Calculate indicies for left and right of symmetrical filter.
      i1 = inK - i;
      i2 = inK + i;
      // reflect at boundaries if necessary
      if (i1 < 0) 
        {
        i1 = (2L * inTraverseSize - 1L - i1) % inModK;
        if (i1 >= (long) inTraverseSize)
          {
          i1 = inModK - i1 - 1L;
          }
        }
      if (i2 >= (long) inTraverseSize ) // originally (i2 > (inTraverseSize - 1) ) 
        {
        i2 = i2 % inModK;
        if (i2 >= (long) inTraverseSize)
          {
          i2 = inModK - i2 - 1L;
          }
        }
      temp[inK] += m_g[i]*(in[i1] + in[i2]);
      }
    }

  for(outK = 0; outK < outTraverseSize; outK++)
    {
    i1 = 2 * outK;
    double outVal = ( temp[i1] + temp[i1 + 1])/2.0;
    out.Set( static_cast<OutputImagePixelType> (outVal)  );
    ++out;
    progress.CompletedPixel();
    }
  
  
}

/* Expand1DImage - expands the vector of data (in) by a 
 *     factor of 2 and writes the results to the location specified
 *     by the Iterator (out).  inTraverseSize is the size of the in vector.
 */
template <class TInputImage, class TOutputImage>
void BSplineCenteredResampleImageFilterBase<TInputImage, TOutputImage>
::Expand1DImage( const std::vector<double> & in, OutputImageIterator & out, 
                 unsigned int inTraverseSize, ProgressReporter &progress )
{
  long i1, i2;

  long inK;
  unsigned long outTraverseSize = inTraverseSize * 2;
  //inTraverseSize = outTraverseSize/2;  // ensures that an even number is used.
  long inModK; // number for modulus math of in
  inModK = outTraverseSize;
  long k0 = (m_hSize/2) * 2 - 1L;


  double outVal, outVal2;
  
  
  for (inK = 0; inK < (long) inTraverseSize; inK++) 
    {
    //outK = inK * 2L;
    outVal = in[inK] * m_h[0];
    for (int k = 2; k < m_hSize; k += 2)
      {
      i1 = inK - k / 2L;
      i2 = inK + k / 2L;
      if ( i1 < 0 )
        {                                            // provide correct border condition
        i1 = (2L * (long) inTraverseSize - 1L -i1 ) % inModK; // pseudo mirror image
        if (i1 >= (long) inTraverseSize)
          {
          i1 = outTraverseSize -i1 - 1L;
          }
        }
      if ( i2 >= (long) inTraverseSize )
        {
        i2 = i2 % inModK;
        if ( i2 >= (long) inTraverseSize )
          {
          i2 = outTraverseSize- i2 - 1L;
          }
        }
      outVal += m_h[k] * (in[i1] + in[i2]);
      }
    out.Set( static_cast<OutputImagePixelType> (outVal) );
    ++out;
    outVal2 = 0;
    for (long k = -k0; k < m_hSize; k += 2L )
      {
      long kk = abs(static_cast<int>(k));
      i1 = inK + ( k + 1L )/2L;
      if (i1 < 0L )
        {
        i1 = ( 2 * inTraverseSize - 1 - i1) % inModK;
        if (i1 > (long) inTraverseSize -1L )
          {
          i1 = outTraverseSize - i1 - 1L;
          }
        }
      if (i1 >= (long) inTraverseSize )
        {
        i1 = i1 % inModK;
        if ( i1 >= (long) inTraverseSize )
          {
          i1 = outTraverseSize - i1 - 1;
          }
        }
      outVal2 += m_h[kk] * in[i1];
      }
    out.Set( static_cast<OutputImagePixelType> (outVal2) );
    ++out;
    }

  // Now apply the Haar[-x] 
  --out;
  for (long j = outTraverseSize - 1; j > 0L ; j-- )
    {
    // out[j] = (out[j] + out[j-1]/2.0;
    outVal = out.Get();
    --out;
    outVal2 = out.Get();
    outVal = ( outVal + outVal2 ) / 2.0; 
    ++out;
    out.Set( static_cast<OutputImagePixelType> (outVal) );
    --out;
    progress.CompletedPixel();
    
    }
  // out[0] /= 2.0;
  out.Set( static_cast<OutputImagePixelType> ( out.Get() / 2.0 ) );
   
  // TODO: Temporary fix for itkImageLinearIteratorWithIndex::NextLine() not setting m_Position correctly
  out.GoToEndOfLine();
    
  
}

} // namespace itk

#endif
