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
#ifndef itkBSplineL2ResampleImageFilterBase_hxx
#define itkBSplineL2ResampleImageFilterBase_hxx
#include "itkBSplineL2ResampleImageFilterBase.h"
#include "itkImageLinearIteratorWithIndex.h"
namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
BSplineL2ResampleImageFilterBase< TInputImage, TOutputImage >
::BSplineL2ResampleImageFilterBase()
{}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage, typename TOutputImage >
void
BSplineL2ResampleImageFilterBase< TInputImage, TOutputImage >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TInputImage, typename TOutputImage >
void BSplineL2ResampleImageFilterBase< TInputImage, TOutputImage >
::InitializePyramidSplineFilter(int SplineOrder)
{
  switch ( SplineOrder )
    {
    case 0L:
      this->m_GSize = 1;
      this->m_HSize = 1;
      break;

    case 1L:
      this->m_GSize = 14;
      this->m_HSize = 2;
      this->m_G.resize(this->m_GSize);
      this->m_H.resize(this->m_HSize);
      this->m_G[0]  =  0.683013;
      this->m_G[1]  =  0.316987;
      this->m_G[2]  = -0.116025;
      this->m_G[3]  = -0.0849365;
      this->m_G[4]  =  0.0310889;
      this->m_G[5]  =  0.0227587;
      this->m_G[6]  = -0.00833025;
      this->m_G[7]  = -0.00609817;
      this->m_G[8]  =  0.00223208;
      this->m_G[9]  =  0.001634;
      this->m_G[10] = -0.000598085;
      this->m_G[11] = -0.000437829;
      this->m_G[12] =  0.000160256;
      this->m_G[13] =  0.000117316;
      this->m_H[0]  =  1.;
      this->m_H[1]  =  0.5;
      break;

    case 3L:
      this->m_GSize = 25;
      this->m_HSize = 12;
      this->m_G.resize(this->m_GSize);
      this->m_H.resize(this->m_HSize);
      this->m_G[0]  =  0.594902;
      this->m_G[1]  =  0.31431;
      this->m_G[2]  = -0.0816632;
      this->m_G[3]  = -0.0942586;
      this->m_G[4]  =  0.0541374;
      this->m_G[5]  =  0.0454105;
      this->m_G[6]  = -0.0307778;
      this->m_G[7]  = -0.0236728;
      this->m_G[8]  =  0.0166858;
      this->m_G[9]  =  0.0125975;
      this->m_G[10] = -0.00895838;
      this->m_G[11] = -0.00673388;
      this->m_G[12] =  0.00479847;
      this->m_G[13] =  0.00360339;
      this->m_G[14] = -0.00256892;
      this->m_G[15] = -0.00192868;
      this->m_G[16] =  0.00137514;
      this->m_G[17] =  0.00103237;
      this->m_G[18] = -0.000736093;
      this->m_G[19] = -0.000552606;
      this->m_G[20] =  0.000394017;
      this->m_G[21] =  0.000295799;
      this->m_G[22] = -0.00021091;
      this->m_G[23] = -0.000158335;
      this->m_G[24] =  0.000112896;
      this->m_H[0]  =  1.;
      this->m_H[1]  =  0.600481;
      this->m_H[2]  =  0.0;
      this->m_H[3]  = -0.127405;
      this->m_H[4]  =  0;
      this->m_H[5]  =  0.034138;
      this->m_H[6]  =  0;
      this->m_H[7]  = -0.00914725;
      this->m_H[8]  =  0;
      this->m_H[9]  =  0.002451;
      this->m_H[10] =  0;
      this->m_H[11] = -0.000656743;
      break;

    case 5L:
      this->m_GSize = 35;
      this->m_HSize = 20;
      this->m_G.resize(this->m_GSize);
      this->m_H.resize(this->m_HSize);
      this->m_G[0]  =  0.564388;
      this->m_G[1]  =  0.316168;
      this->m_G[2]  = -0.0597634;
      this->m_G[3]  = -0.0998708;
      this->m_G[4]  =  0.0484525;
      this->m_G[5]  =  0.0539099;
      this->m_G[6]  = -0.0355614;
      this->m_G[7]  = -0.033052;
      this->m_G[8]  =  0.0246347;
      this->m_G[9]  =  0.0212024;
      this->m_G[10] = -0.0166097;
      this->m_G[11] = -0.0138474;
      this->m_G[12] =  0.0110719;
      this->m_G[13] =  0.00911006;
      this->m_G[14] = -0.00734567;
      this->m_G[15] = -0.0060115;
      this->m_G[16] =  0.00486404;
      this->m_G[17] =  0.00397176;
      this->m_G[18] = -0.00321822;
      this->m_G[19] = -0.00262545;
      this->m_G[20] =  0.00212859;
      this->m_G[21] =  0.00173587;
      this->m_G[22] = -0.0014077;
      this->m_G[23] = -0.0011478;
      this->m_G[24] =  0.000930899;
      this->m_G[25] =  0.000758982;
      this->m_G[26] = -0.000615582;
      this->m_G[27] = -0.000501884;
      this->m_G[28] =  0.000407066;
      this->m_G[29] =  0.000331877;
      this->m_G[30] = -0.00026918;
      this->m_G[31] = -0.000219459;
      this->m_G[32] =  0.000178;
      this->m_G[33] =  0.00014512;
      this->m_G[34] = -0.000117706;
      this->m_H[0]  =  1.;
      this->m_H[1]  =  0.619879;
      this->m_H[2]  =  0.0;
      this->m_H[3]  = -0.167965;
      this->m_H[4]  =  0;
      this->m_H[5]  =  0.0686374;
      this->m_H[6]  =  0;
      this->m_H[7]  = -0.0293948;
      this->m_H[8]  =  0.0;
      this->m_H[9]  =  0.0126498;
      this->m_H[10] =  0;
      this->m_H[11] = -0.00544641;
      this->m_H[12] =  0.0;
      this->m_H[13] =  0.00234508;
      this->m_H[14] =  0;
      this->m_H[15] = -0.00100973;
      this->m_H[16] =  0.0;
      this->m_H[17] =  0.000434766;
      this->m_H[18] =  0;
      this->m_H[19] = -0.000187199;
      break;
    default:
      // I don't feel well I think I'm going to throw up.
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation(ITK_LOCATION);
      err.SetDescription(
        "SplineOrder for L2 pyramid filter must be 0,1,3, or 5. Requested spline order has not been implemented.");
      throw err;
      break;
    }
}
} // namespace itk

#endif
