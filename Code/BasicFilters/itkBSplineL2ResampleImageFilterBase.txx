/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineL2ResampleImageFilterBase.txx
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
      this->m_gSize = 1; 
      this->m_hSize = 1;
      break;
    
    case 1L :
      this->m_gSize = 14;
      this->m_hSize = 2;
      this->m_g.resize(this->m_gSize);
      this->m_h.resize(this->m_hSize);
      this->m_g[0]  =  0.683013; 
      this->m_g[1]  =  0.316987; 
      this->m_g[2]  = -0.116025;
      this->m_g[3]  = -0.0849365; 
      this->m_g[4]  =  0.0310889; 
      this->m_g[5]  =  0.0227587;
      this->m_g[6]  = -0.00833025; 
      this->m_g[7]  = -0.00609817; 
      this->m_g[8]  =  0.00223208;
      this->m_g[9]  =  0.001634; 
      this->m_g[10] = -0.000598085; 
      this->m_g[11] = -0.000437829;
      this->m_g[12] =  0.000160256; 
      this->m_g[13] =  0.000117316;
      this->m_h[0]  =  1.; 
      this->m_h[1]  =  0.5;
      break;
    
    case 3L :
      this->m_gSize = 25;
      this->m_hSize = 12;
      this->m_g.resize(this->m_gSize);
      this->m_h.resize(this->m_hSize);
      this->m_g[0]  =  0.594902; 
      this->m_g[1]  =  0.31431; 
      this->m_g[2]  = -0.0816632;
      this->m_g[3]  = -0.0942586; 
      this->m_g[4]  =  0.0541374; 
      this->m_g[5]  =  0.0454105;
      this->m_g[6]  = -0.0307778; 
      this->m_g[7]  = -0.0236728; 
      this->m_g[8]  =  0.0166858;
      this->m_g[9]  =  0.0125975; 
      this->m_g[10] = -0.00895838; 
      this->m_g[11] = -0.00673388;
      this->m_g[12] =  0.00479847; 
      this->m_g[13] =  0.00360339; 
      this->m_g[14] = -0.00256892;
      this->m_g[15] = -0.00192868; 
      this->m_g[16] =  0.00137514; 
      this->m_g[17] =  0.00103237;
      this->m_g[18] = -0.000736093; 
      this->m_g[19] = -0.000552606;
      this->m_g[20] =  0.000394017; 
      this->m_g[21] =  0.000295799; 
      this->m_g[22] = -0.00021091;
      this->m_g[23] = -0.000158335; 
      this->m_g[24] =  0.000112896;
      this->m_h[0]  =  1.;
      this->m_h[1]  =  0.600481; 
      this->m_h[2]  =  0.0; 
      this->m_h[3]  = -0.127405; 
      this->m_h[4]  =  0;
      this->m_h[5]  =  0.034138; 
      this->m_h[6]  =  0; 
      this->m_h[7]  = -0.00914725; 
      this->m_h[8]  =  0;
      this->m_h[9]  =  0.002451; 
      this->m_h[10] =  0; 
      this->m_h[11] = -0.000656743;
      break;
    
    case 5L :
      this->m_gSize = 35;
      this->m_hSize = 20;
      this->m_g.resize(this->m_gSize);
      this->m_h.resize(this->m_hSize);
      this->m_g[0]  =  0.564388; 
      this->m_g[1]  =  0.316168; 
      this->m_g[2]  = -0.0597634;
      this->m_g[3]  = -0.0998708; 
      this->m_g[4]  =  0.0484525; 
      this->m_g[5]  =  0.0539099;
      this->m_g[6]  = -0.0355614; 
      this->m_g[7]  = -0.033052; 
      this->m_g[8]  =  0.0246347;
      this->m_g[9]  =  0.0212024; 
      this->m_g[10] = -0.0166097; 
      this->m_g[11] = -0.0138474;
      this->m_g[12] =  0.0110719; 
      this->m_g[13] =  0.00911006; 
      this->m_g[14] = -0.00734567;
      this->m_g[15] = -0.0060115; 
      this->m_g[16] =  0.00486404; 
      this->m_g[17] =  0.00397176;
      this->m_g[18] = -0.00321822; 
      this->m_g[19] = -0.00262545; 
      this->m_g[20] =  0.00212859;
      this->m_g[21] =  0.00173587; 
      this->m_g[22] = -0.0014077; 
      this->m_g[23] = -0.0011478;
      this->m_g[24] =  0.000930899; 
      this->m_g[25] =  0.000758982; 
      this->m_g[26] = -0.000615582;
      this->m_g[27] = -0.000501884; 
      this->m_g[28] =  0.000407066; 
      this->m_g[29] =  0.000331877;
      this->m_g[30] = -0.00026918; 
      this->m_g[31] = -0.000219459; 
      this->m_g[32] =  0.000178;
      this->m_g[33] =  0.00014512; 
      this->m_g[34] = -0.000117706;
      this->m_h[0]  =  1.; 
      this->m_h[1]  =  0.619879; 
      this->m_h[2]  =  0.0; 
      this->m_h[3]  = -0.167965; 
      this->m_h[4]  =  0;
      this->m_h[5]  =  0.0686374; 
      this->m_h[6]  =  0; 
      this->m_h[7]  = -0.0293948; 
      this->m_h[8]  =  0.0;
      this->m_h[9]  =  0.0126498; 
      this->m_h[10] =  0; 
      this->m_h[11] = -0.00544641; 
      this->m_h[12] =  0.0;
      this->m_h[13] =  0.00234508; 
      this->m_h[14] =  0; 
      this->m_h[15] = -0.00100973; 
      this->m_h[16] =  0.0;
      this->m_h[17] =  0.000434766; 
      this->m_h[18] =  0; 
      this->m_h[19] = -0.000187199;
      break;
    default :
      // I don't feel well I think I'm going to throw up.
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( ITK_LOCATION );
      err.SetDescription( "SplineOrder for L2 pyramid filter must be 0,1,3, or 5. Requested spline order has not been implemented." );
      throw err;
      break;
    }
}



} // namespace itk

#endif
