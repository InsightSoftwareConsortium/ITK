/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineL2ResampleImageFilterBase.txx
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

#ifndef _itkBSplineL2ResampleImageFilterBase_txx
#define _itkBSplineL2ResampleImageFilterBase_txx
#include "itkBSplineL2ResampleImageFilterBase.h"
#include "itkImageLinearIteratorWithIndex.h"
//#include "itkImageRegionConstIteratorWithIndex.h"
namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
BSplineL2ResampleImageFilterBase<TInputImage, TOutputImage>
::BSplineL2ResampleImageFilterBase()
{

}

/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutputImage>
void
BSplineL2ResampleImageFilterBase<TInputImage, TOutputImage>
::PrintSelf(
  std::ostream& os, 
  Indent indent) const
{
  Superclass::PrintSelf( os, indent );
}

template <class TInputImage, class TOutputImage>
void BSplineL2ResampleImageFilterBase<TInputImage, TOutputImage>
::InitializePyramidSplineFilter(int SplineOrder)
{
  switch (SplineOrder) 
    {
  
    case 0L :
      m_gSize = 1; 
      m_hSize = 1;
      break;
    
    case 1L :
      m_gSize = 14;
      m_hSize = 2;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.683013; 
      m_g[1]  =  0.316987; 
      m_g[2]  = -0.116025;
      m_g[3]  = -0.0849365; 
      m_g[4]  =  0.0310889; 
      m_g[5]  =  0.0227587;
      m_g[6]  = -0.00833025; 
      m_g[7]  = -0.00609817; 
      m_g[8]  =  0.00223208;
      m_g[9]  =  0.001634; 
      m_g[10] = -0.000598085; 
      m_g[11] = -0.000437829;
      m_g[12] =  0.000160256; 
      m_g[13] =  0.000117316;
      m_h[0]  =  1.; 
      m_h[1]  =  0.5;
      break;
    
    case 3L :
      m_gSize = 25;
      m_hSize = 12;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.594902; 
      m_g[1]  =  0.31431; 
      m_g[2]  = -0.0816632;
      m_g[3]  = -0.0942586; 
      m_g[4]  =  0.0541374; 
      m_g[5]  =  0.0454105;
      m_g[6]  = -0.0307778; 
      m_g[7]  = -0.0236728; 
      m_g[8]  =  0.0166858;
      m_g[9]  =  0.0125975; 
      m_g[10] = -0.00895838; 
      m_g[11] = -0.00673388;
      m_g[12] =  0.00479847; 
      m_g[13] =  0.00360339; 
      m_g[14] = -0.00256892;
      m_g[15] = -0.00192868; 
      m_g[16] =  0.00137514; 
      m_g[17] =  0.00103237;
      m_g[18] = -0.000736093; 
      m_g[19] = -0.000552606;
      m_g[20] =  0.000394017; 
      m_g[21] =  0.000295799; 
      m_g[22] = -0.00021091;
      m_g[23] = -0.000158335; 
      m_g[24] =  0.000112896;
      m_h[0]  =  1.;
      m_h[1]  =  0.600481; 
      m_h[2]  =  0.0; 
      m_h[3]  = -0.127405; 
      m_h[4]  =  0;
      m_h[5]  =  0.034138; 
      m_h[6]  =  0; 
      m_h[7]  = -0.00914725; 
      m_h[8]  =  0;
      m_h[9]  =  0.002451; 
      m_h[10] =  0; 
      m_h[11] = -0.000656743;
      break;
    
    case 5L :
      m_gSize = 35;
      m_hSize = 20;
      m_g.resize(m_gSize);
      m_h.resize(m_hSize);
      m_g[0]  =  0.564388; 
      m_g[1]  =  0.316168; 
      m_g[2]  = -0.0597634;
      m_g[3]  = -0.0998708; 
      m_g[4]  =  0.0484525; 
      m_g[5]  =  0.0539099;
      m_g[6]  = -0.0355614; 
      m_g[7]  = -0.033052; 
      m_g[8]  =  0.0246347;
      m_g[9]  =  0.0212024; 
      m_g[10] = -0.0166097; 
      m_g[11] = -0.0138474;
      m_g[12] =  0.0110719; 
      m_g[13] =  0.00911006; 
      m_g[14] = -0.00734567;
      m_g[15] = -0.0060115; 
      m_g[16] =  0.00486404; 
      m_g[17] =  0.00397176;
      m_g[18] = -0.00321822; 
      m_g[19] = -0.00262545; 
      m_g[20] =  0.00212859;
      m_g[21] =  0.00173587; 
      m_g[22] = -0.0014077; 
      m_g[23] = -0.0011478;
      m_g[24] =  0.000930899; 
      m_g[25] =  0.000758982; 
      m_g[26] = -0.000615582;
      m_g[27] = -0.000501884; 
      m_g[28] =  0.000407066; 
      m_g[29] =  0.000331877;
      m_g[30] = -0.00026918; 
      m_g[31] = -0.000219459; 
      m_g[32] =  0.000178;
      m_g[33] =  0.00014512; 
      m_g[34] = -0.000117706;
      m_h[0]  =  1.; 
      m_h[1]  =  0.619879; 
      m_h[2]  =  0.0; 
      m_h[3]  = -0.167965; 
      m_h[4]  =  0;
      m_h[5]  =  0.0686374; 
      m_h[6]  =  0; 
      m_h[7]  = -0.0293948; 
      m_h[8]  =  0.0;
      m_h[9]  =  0.0126498; 
      m_h[10] =  0; 
      m_h[11] = -0.00544641; 
      m_h[12] =  0.0;
      m_h[13] =  0.00234508; 
      m_h[14] =  0; 
      m_h[15] = -0.00100973; 
      m_h[16] =  0.0;
      m_h[17] =  0.000434766; 
      m_h[18] =  0; 
      m_h[19] = -0.000187199;
      break;
    default :
      // I don't feel well I think I'm going to throw up.
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( "BSplineL2ResampleImageFilterBase" );
      err.SetDescription( "SplineOrder for L2 pyramid filter must be 0,1,3, or 5. Requested spline order has not been implemented." );
      throw err;
      break;
    }
}



} // namespace itk

#endif
