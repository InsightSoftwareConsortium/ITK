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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBSplineCenteredL2ResampleImageFilterBase_hxx
#define itkBSplineCenteredL2ResampleImageFilterBase_hxx
#include "itkBSplineCenteredL2ResampleImageFilterBase.h"

#include "itkImageLinearIteratorWithIndex.h"
namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
BSplineCenteredL2ResampleImageFilterBase< TInputImage, TOutputImage >
::BSplineCenteredL2ResampleImageFilterBase()
{}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage, typename TOutputImage >
void
BSplineCenteredL2ResampleImageFilterBase< TInputImage, TOutputImage >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TInputImage, typename TOutputImage >
void BSplineCenteredL2ResampleImageFilterBase< TInputImage, TOutputImage >
::InitializePyramidSplineFilter(int SplineOrder)
{
  switch ( SplineOrder )
    {
    case 1:
      this->m_HSize = 9;
      this->m_GSize = 17;
      this->m_G.resize(this->m_GSize);
      this->m_H.resize(this->m_HSize);
      this->m_G[0]  =  0.820272;
      this->m_G[1]  =  0.316987;
      this->m_G[2]  = -0.203044;
      this->m_G[3]  = -0.0849365;
      this->m_G[4]  =  0.0544056;
      this->m_G[5]  =  0.0227587;
      this->m_G[6]  = -0.0145779;
      this->m_G[7]  = -0.00609817;
      this->m_G[8]  =  0.00390615;
      this->m_G[9]  =  0.001634;
      this->m_G[10] = -0.00104665;
      this->m_G[11] = -0.000437829;
      this->m_G[12] =  0.000280449;
      this->m_G[13] =  0.000117316;
      this->m_G[14] = -0.000075146;
      this->m_G[15] = -0.0000314347;
      this->m_G[16] =  0.0000201353;
      this->m_H[0] =  1.20096;
      this->m_H[1] =  0.473076;
      this->m_H[2] = -0.0932667;
      this->m_H[3] =  0.0249907;
      this->m_H[4] = -0.00669625;
      this->m_H[5] =  0.00179425;
      this->m_H[6] = -0.000480769;
      this->m_H[7] =  0.000128822;
      this->m_H[8] = -0.0000345177;
      break;

    case 2:
      this->m_HSize = 11;
      this->m_GSize = 21;
      this->m_G.resize(this->m_GSize);
      this->m_H.resize(this->m_HSize);
      this->m_G[0]  =  0.727973;
      this->m_G[1]  =  0.314545;
      this->m_G[2]  = -0.167695;
      this->m_G[3]  = -0.0893693;
      this->m_G[4]  =  0.0768426;
      this->m_G[5]  =  0.0354175;
      this->m_G[6]  = -0.0331015;
      this->m_G[7]  = -0.0151496;
      this->m_G[8]  =  0.0142588;
      this->m_G[9]  =  0.00651781;
      this->m_G[10] = -0.00613959;
      this->m_G[11] = -0.00280621;
      this->m_G[12] =  0.00264356;
      this->m_G[13] =  0.00120827;
      this->m_G[14] = -0.00113825;
      this->m_G[15] = -0.000520253;
      this->m_G[16] =  0.000490105;
      this->m_G[17] =  0.000224007;
      this->m_G[18] = -0.000211028;
      this->m_G[19] = -0.0000964507;
      this->m_G[20] =  0.0000908666;
      this->m_H[0]  =  1.20711;
      this->m_H[1]  =  0.585786;
      this->m_H[2]  = -0.12132;
      this->m_H[3]  = -0.100505;
      this->m_H[4]  =  0.0208153;
      this->m_H[5]  =  0.0172439;
      this->m_H[6]  = -0.00357134;
      this->m_H[7]  = -0.00295859;
      this->m_H[8]  =  0.000612745;
      this->m_H[9]  =  0.000507614;
      this->m_H[10] = -0.00010513;
      break;

    case 3:
      this->m_HSize = 15;
      this->m_GSize = 21;
      this->m_G.resize(this->m_GSize);
      this->m_H.resize(this->m_HSize);
      this->m_G[0]  =  0.70222;
      this->m_G[1]  =  0.328033;
      this->m_G[2]  = -0.159368;
      this->m_G[3]  = -0.113142;
      this->m_G[4]  =  0.0902447;
      this->m_G[5]  =  0.0530861;
      this->m_G[6]  = -0.0492084;
      this->m_G[7]  = -0.0274987;
      this->m_G[8]  =  0.0264529;
      this->m_G[9]  =  0.0146073;
      this->m_G[10] = -0.0141736;
      this->m_G[11] = -0.0078052;
      this->m_G[12] =  0.00758856;
      this->m_G[13] =  0.00417626;
      this->m_G[14] = -0.00406225;
      this->m_G[15] = -0.00223523;
      this->m_G[16] =  0.00217454;
      this->m_G[17] =  0.00119638;
      this->m_G[18] = -0.00116412;
      this->m_G[19] = -0.000640258;
      this->m_G[20] =  0.000623379;
      this->m_H[0]  =  1.15089;
      this->m_H[1]  =  0.623278;
      this->m_H[2]  = -0.0961988;
      this->m_H[3]  = -0.155743;
      this->m_H[4]  =  0.0259827;
      this->m_H[5]  =  0.041346;
      this->m_H[6]  = -0.0067263;
      this->m_H[7]  = -0.0112084;
      this->m_H[8]  =  0.00187221;
      this->m_H[9]  =  0.00296581;
      this->m_H[10] = -0.000481593;
      this->m_H[11] = -0.000805427;
      this->m_H[12] =  0.000134792;
      this->m_H[13] =  0.000212736;
      this->m_H[14] = -0.00003447;
      break;

    case 4:
      this->m_HSize = 20;
      this->m_GSize = 21;
      this->m_G.resize(this->m_GSize);
      this->m_H.resize(this->m_HSize);
      this->m_G[0]  =  0.672101;
      this->m_G[1]  =  0.331667;
      this->m_G[2]  = -0.138779;
      this->m_G[3]  = -0.121385;
      this->m_G[4]  =  0.0864024;
      this->m_G[5]  =  0.0618776;
      this->m_G[6]  = -0.0545165;
      this->m_G[7]  = -0.0352403;
      this->m_G[8]  =  0.0335951;
      this->m_G[9]  =  0.0209537;
      this->m_G[10] = -0.0205211;
      this->m_G[11] = -0.0126439;
      this->m_G[12] =  0.0124959;
      this->m_G[13] =  0.0076682;
      this->m_G[14] = -0.00760135;
      this->m_G[15] = -0.00465835;
      this->m_G[16] =  0.00462238;
      this->m_G[17] =  0.00283148;
      this->m_G[18] = -0.00281055;
      this->m_G[19] = -0.00172137;
      this->m_G[20] =  0.00170884;
      this->m_H[0]  =  1.14324;
      this->m_H[1]  =  0.643609;
      this->m_H[2]  = -0.0937888;
      this->m_H[3]  = -0.194993;
      this->m_H[4]  =  0.030127;
      this->m_H[5]  =  0.0699433;
      this->m_H[6]  = -0.0108345;
      this->m_H[7]  = -0.0252663;
      this->m_H[8]  =  0.00391424;
      this->m_H[9]  =  0.00912967;
      this->m_H[10] = -0.00141437;
      this->m_H[11] = -0.00329892;
      this->m_H[12] =  0.000511068;
      this->m_H[13] =  0.00119204;
      this->m_H[14] = -0.00018467;
      this->m_H[15] = -0.000430732;
      this->m_H[16] =  0.0000667289;
      this->m_H[17] =  0.000155641;
      this->m_H[18] = -0.0000241119;
      this->m_H[19] = -0.0000562396;
      break;
    default:
      // Throw an exception for the unsupported splines.
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation(ITK_LOCATION);
      err.SetDescription(
        "SplineOrder for Centered L2 pyramid filter must be 1 through 4. Requested spline order has not been implemented.");
      throw err;
      break;
    }
}
} // namespace itk

#endif
