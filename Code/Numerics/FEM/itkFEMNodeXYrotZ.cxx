/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeXYrotZ.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMNodeXYrotZ.h"

namespace itk {
namespace fem {




/**
 * Write the NodeXYrotZ to the output stream
 */
void NodeXYrotZ::Write( std::ostream& f, int ofid ) const 
  {

  /**
   * if not set already, se set the ofid
   */
  if (ofid<0) ofid=OFID;

  /**
   * first call the parent's write function
   */
  Superclass::Write(f,ofid);

  /* 
   * we don't need to write anything else, since
   * all the data is already handeled by parent class
   */

}

/** 
 * Windows visualization 
 */
#ifdef FEM_BUILD_VISUALIZATION
  
  /**
   * draws the node on DC
   */
  void NodeXYrotZ::Draw(CDC* pDC) const 
    {

    CPen pen(PS_SOLID, 0, (COLORREF) RGB(255,255,255) );
    CPen* pOldpen=pDC->SelectObject(&pen);
    CBrush brush( RGB(0,0,0) );
    CBrush* pOldbrush=pDC->SelectObject(&brush);

    int x1=X*DC_Scale+uX.value*DC_Scale;
    int y1=Y*DC_Scale+uY.value*DC_Scale;

    CPoint r1=CPoint(0,0);
    CPoint r=CPoint(5,5);
  
    pDC->DPtoLP(&r1);
    pDC->DPtoLP(&r);
    r=r-r1;

    pDC->Ellipse(x1-r.x, y1-r.y, x1+r.x, y1+r.y);

    pDC->SelectObject(pOldbrush);
    pDC->SelectObject(pOldpen);

    }
#endif

FEM_CLASS_REGISTER(NodeXYrotZ)




}} // end namespace itk::fem
