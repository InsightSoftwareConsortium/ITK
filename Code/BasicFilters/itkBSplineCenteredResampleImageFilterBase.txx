/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineCenteredResampleImageFilterBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
      this->m_gSize = 21;
      this->m_hSize = 2;
      this->m_g.resize(this->m_gSize);
      this->m_h.resize(this->m_hSize);
      this->m_g[0]  =  1.; 
      this->m_g[1]  =  0.333333; 
      this->m_g[2]  = -0.333333; 
      this->m_g[3]  = -0.111111; 
      this->m_g[4]  =  0.111111;
      this->m_g[5]  =  0.037037; 
      this->m_g[6]  = -0.037037; 
      this->m_g[7]  = -0.0123457; 
      this->m_g[8]  =  0.0123457;
      this->m_g[9]  =  0.00411523; 
      this->m_g[10] = -0.00411523; 
      this->m_g[11] = -0.00137174; 
      this->m_g[12] =  0.00137174;
      this->m_g[13] =  0.000457247; 
      this->m_g[14] = -0.000457247; 
      this->m_g[15] = -0.000152416;
      this->m_g[16] =  0.000152416; 
      this->m_g[17] =  0.0000508053; 
      this->m_g[18] = -0.0000508053;
      this->m_g[19] = -0.0000169351; 
      this->m_g[20] =  0.0000169351;
      this->m_h[0] =  1; 
      this->m_h[1] =  0.5;
      break;
    
    case 2 :
      this->m_gSize = 21;
      this->m_hSize = 11;
      this->m_g.resize(this->m_gSize);
      this->m_h.resize(this->m_hSize);
      this->m_g[0]  =  0.738417; 
      this->m_g[1]  =  0.307916; 
      this->m_g[2]  = -0.171064; 
      this->m_g[3]  = -0.0799199; 
      this->m_g[4]  =  0.0735791;
      this->m_g[5]  =  0.03108; 
      this->m_g[6]  = -0.0307862; 
      this->m_g[7]  = -0.0128561; 
      this->m_g[8]  =  0.0128425;
      this->m_g[9]  =  0.00535611; 
      this->m_g[10] = -0.00535548; 
      this->m_g[11] = -0.00223325; 
      this->m_g[12] =  0.00223322;
      this->m_g[13] =  0.000931242; 
      this->m_g[14] = -0.00093124; 
      this->m_g[15] = -0.000388322; 
      this->m_g[16] =  0.000388322;
      this->m_g[17] =  0.000161928; 
      this->m_g[18] = -0.000161928; 
      this->m_g[19] = -0.0000675233;
      this->m_g[20] =  0.0000675233;
      this->m_h[0]  =  1.20711; 
      this->m_h[1]  =  0.585786; 
      this->m_h[2]  = -0.12132; 
      this->m_h[3]  = -0.100505; 
      this->m_h[4]  =  0.0208153;
      this->m_h[5]  =  0.0172439; 
      this->m_h[6]  = -0.00357134; 
      this->m_h[7]  = -0.00295859; 
      this->m_h[8]  =  0.000612745;
      this->m_h[9]  =  0.000507614; 
      this->m_h[10] = -0.00010513;
      break;
      
    case 3 :
      this->m_gSize = 21;
      this->m_hSize = 16;
      this->m_g.resize(this->m_gSize);
      this->m_h.resize(this->m_hSize);
      this->m_g[0]  =  0.708792; 
      this->m_g[1]  =  0.328616; 
      this->m_g[2]  = -0.165157; 
      this->m_g[3]  = -0.114448; 
      this->m_g[4]  =  0.0944036;
      this->m_g[5]  =  0.0543881; 
      this->m_g[6]  = -0.05193; 
      this->m_g[7]  = -0.0284868; 
      this->m_g[8]  =  0.0281854;
      this->m_g[9]  =  0.0152877; 
      this->m_g[10] = -0.0152508; 
      this->m_g[11] = -0.00825077; 
      this->m_g[12] =  0.00824629;
      this->m_g[13] =  0.00445865; 
      this->m_g[14] = -0.0044582; 
      this->m_g[15] = -0.00241009; 
      this->m_g[16] =  0.00241022;
      this->m_g[17] =  0.00130278; 
      this->m_g[18] = -0.00130313; 
      this->m_g[19] = -0.000704109; 
      this->m_g[20] =  0.000704784;
      this->m_h[0]  =  1.13726; 
      this->m_h[1]  =  0.625601; 
      this->m_h[2]  = -0.0870191; 
      this->m_h[3]  = -0.159256; 
      this->m_h[4]  =  0.0233167;
      this->m_h[5]  =  0.0426725; 
      this->m_h[6]  = -0.00624769; 
      this->m_h[7]  = -0.0114341; 
      this->m_h[8]  =  0.00167406;
      this->m_h[9]  =  0.00306375; 
      this->m_h[10] = -0.000448564; 
      this->m_h[11] = -0.000820929; 
      this->m_h[12] =  0.000120192;
      this->m_h[13] =  0.000219967; 
      this->m_h[14] = -0.0000322054; 
      this->m_h[15] = -0.00005894; 
      break;
    
    case 4 :
      this->m_gSize = 21;
      this->m_hSize = 20;
      this->m_g.resize(this->m_gSize);
      this->m_h.resize(this->m_hSize);
      this->m_g[0]  =  0.673072; 
      this->m_g[1]  =  0.331218; 
      this->m_g[2]  = -0.139359; 
      this->m_g[3]  = -0.12051; 
      this->m_g[4]  =  0.086389;
      this->m_g[5]  =  0.0611801; 
      this->m_g[6]  = -0.0542989; 
      this->m_g[7]  = -0.034777; 
      this->m_g[8]  =  0.033388;
      this->m_g[9]  =  0.0206275; 
      this->m_g[10] = -0.0203475; 
      this->m_g[11] = -0.0124183; 
      this->m_g[12] =  0.0123625;
      this->m_g[13] =  0.00751369; 
      this->m_g[14] = -0.00750374; 
      this->m_g[15] = -0.00455348; 
      this->m_g[16] =  0.00455363;
      this->m_g[17] =  0.00276047; 
      this->m_g[18] = -0.00276406; 
      this->m_g[19] = -0.00167279; 
      this->m_g[20] =  0.00167938;
      this->m_h[0]  =  1.14324; 
      this->m_h[1]  =  0.643609; 
      this->m_h[2]  = -0.0937888; 
      this->m_h[3]  = -0.194993; 
      this->m_h[4]  =  0.030127;
      this->m_h[5]  =  0.0699433; 
      this->m_h[6]  = -0.0108345; 
      this->m_h[7]  = -0.0252663; 
      this->m_h[8]  =  0.00391424;
      this->m_h[9]  =  0.00912967; 
      this->m_h[10] = -0.00141437; 
      this->m_h[11] = -0.00329892; 
      this->m_h[12] =  0.000511068;
      this->m_h[13] =  0.00119204; 
      this->m_h[14] = -0.00018467; 
      this->m_h[15] = -0.000430732; 
      this->m_h[16] =  0.0000667289;
      this->m_h[17] =  0.000155641; 
      this->m_h[18] = -0.0000241119; 
      this->m_h[19] = -0.0000562395;
      break;
    default :
      // Throw an execption for unsupported splines.
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( ITK_LOCATION );
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
    temp[inK] = in[inK] * this->m_g[0];
  
    for (int i = 1; i < this->m_gSize; i++) 
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
      temp[inK] += this->m_g[i]*(in[i1] + in[i2]);
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
  long k0 = (this->m_hSize/2) * 2 - 1L;


  double outVal, outVal2;
  
  
  for (inK = 0; inK < (long) inTraverseSize; inK++) 
    {
    //outK = inK * 2L;
    outVal = in[inK] * this->m_h[0];
    for (int k = 2; k < this->m_hSize; k += 2)
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
      outVal += this->m_h[k] * (in[i1] + in[i2]);
      }
    out.Set( static_cast<OutputImagePixelType> (outVal) );
    ++out;
    outVal2 = 0;
    for (long k = -k0; k < this->m_hSize; k += 2L )
      {
      long kk = vcl_abs(static_cast<int>(k));
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
      outVal2 += this->m_h[kk] * in[i1];
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
