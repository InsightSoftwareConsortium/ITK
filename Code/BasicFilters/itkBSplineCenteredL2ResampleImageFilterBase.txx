/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineCenteredL2ResampleImageFilterBase.txx
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

#ifndef _itkBSplineCenteredL2ResampleImageFilterBase_txx
#define _itkBSplineCenteredL2ResampleImageFilterBase_txx
#include "itkBSplineCenteredL2ResampleImageFilterBase.h"

#include "itkImageLinearIteratorWithIndex.h"
namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
BSplineCenteredL2ResampleImageFilterBase<TInputImage, TOutputImage>
::BSplineCenteredL2ResampleImageFilterBase()
{

}

/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutputImage>
void
BSplineCenteredL2ResampleImageFilterBase<TInputImage, TOutputImage>
::PrintSelf(
  std::ostream& os, 
  Indent indent) const
{
  Superclass::PrintSelf( os, indent );
}

template <class TInputImage, class TOutputImage>
void BSplineCenteredL2ResampleImageFilterBase<TInputImage, TOutputImage>
::InitializePyramidSplineFilter(int SplineOrder)
{
  switch (SplineOrder) 
    {   
    case 1 :
      m_hSize = 9;
      m_gSize = 17;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.820272; 
      m_g[1]  =  0.316987; 
      m_g[2]  = -0.203044; 
      m_g[3]  = -0.0849365;
      m_g[4]  =  0.0544056; 
      m_g[5]  =  0.0227587; 
      m_g[6]  = -0.0145779;
      m_g[7]  = -0.00609817; 
      m_g[8]  =  0.00390615; 
      m_g[9]  =  0.001634;
      m_g[10] = -0.00104665; 
      m_g[11] = -0.000437829; 
      m_g[12] =  0.000280449;
      m_g[13] =  0.000117316; 
      m_g[14] = -0.000075146; 
      m_g[15] = -0.0000314347;
      m_g[16] =  0.0000201353; 
      m_h[0] =  1.20096; 
      m_h[1] =  0.473076; 
      m_h[2] = -0.0932667;
      m_h[3] =  0.0249907; 
      m_h[4] = -0.00669625; 
      m_h[5] =  0.00179425;
      m_h[6] = -0.000480769; 
      m_h[7] =  0.000128822; 
      m_h[8] = -0.0000345177;
      break;
      
    case 2 :
      m_hSize = 11;
      m_gSize = 21;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.727973; 
      m_g[1]  =  0.314545; 
      m_g[2]  = -0.167695;
      m_g[3]  = -0.0893693; 
      m_g[4]  =  0.0768426; 
      m_g[5]  =  0.0354175;
      m_g[6]  = -0.0331015; 
      m_g[7]  = -0.0151496; 
      m_g[8]  =  0.0142588;
      m_g[9]  =  0.00651781; 
      m_g[10] = -0.00613959; 
      m_g[11] = -0.00280621;
      m_g[12] =  0.00264356; 
      m_g[13] =  0.00120827; 
      m_g[14] = -0.00113825;
      m_g[15] = -0.000520253; 
      m_g[16] =  0.000490105; 
      m_g[17] =  0.000224007;
      m_g[18] = -0.000211028; 
      m_g[19] = -0.0000964507;
      m_g[20] =  0.0000908666;
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
      m_hSize = 15;
      m_gSize = 21;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.70222; 
      m_g[1]  =  0.328033; 
      m_g[2]  = -0.159368; 
      m_g[3]  = -0.113142;
      m_g[4]  =  0.0902447; 
      m_g[5]  =  0.0530861; 
      m_g[6]  = -0.0492084;
      m_g[7]  = -0.0274987; 
      m_g[8]  =  0.0264529; 
      m_g[9]  =  0.0146073;
      m_g[10] = -0.0141736; 
      m_g[11] = -0.0078052; 
      m_g[12] =  0.00758856;
      m_g[13] =  0.00417626; 
      m_g[14] = -0.00406225; 
      m_g[15] = -0.00223523;
      m_g[16] =  0.00217454; 
      m_g[17] =  0.00119638; 
      m_g[18] = -0.00116412;
      m_g[19] = -0.000640258; 
      m_g[20] =  0.000623379;
      m_h[0]  =  1.15089; 
      m_h[1]  =  0.623278; 
      m_h[2]  = -0.0961988;
      m_h[3]  = -0.155743; 
      m_h[4]  =  0.0259827; 
      m_h[5]  =  0.041346;
      m_h[6]  = -0.0067263; 
      m_h[7]  = -0.0112084; 
      m_h[8]  =  0.00187221;
      m_h[9]  =  0.00296581; 
      m_h[10] = -0.000481593; 
      m_h[11] = -0.000805427;
      m_h[12] =  0.000134792; 
      m_h[13] =  0.000212736; 
      m_h[14] = -0.00003447;
      break;
      
    case 4:
      m_hSize = 20; 
      m_gSize = 21;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.672101; 
      m_g[1]  =  0.331667; 
      m_g[2]  = -0.138779;
      m_g[3]  = -0.121385; 
      m_g[4]  =  0.0864024; 
      m_g[5]  =  0.0618776;
      m_g[6]  = -0.0545165; 
      m_g[7]  = -0.0352403; 
      m_g[8]  =  0.0335951;
      m_g[9]  =  0.0209537; 
      m_g[10] = -0.0205211; 
      m_g[11] = -0.0126439;
      m_g[12] =  0.0124959; 
      m_g[13] =  0.0076682; 
      m_g[14] = -0.00760135;
      m_g[15] = -0.00465835; 
      m_g[16] =  0.00462238; 
      m_g[17] =  0.00283148;
      m_g[18] = -0.00281055; 
      m_g[19] = -0.00172137; 
      m_g[20] =  0.00170884;
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
      m_h[19] = -0.0000562396;
      break;       
    default :
      // Throw an exception for the unsupported splines.
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( "BSplineCenteredL2ResampleImageFilterBase" );
      err.SetDescription( "SplineOrder for Centered L2 pyramid filter must be 1 through 4. Requested spline order has not been implemented." );
      throw err;
      break;
    }
}


} // namespace itk

#endif
