/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeXY.cxx
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

#include "itkFEMNodeXY.h"
#include "itkFEMUtility.h"

namespace itk {
namespace fem {




/**
 * read the NodeXY from input stream
 */
void NodeXY::Read(  std::istream& f, void* info )
{
  Float d;

  /**
   * first call the parent's read function
   */
  Superclass::Read(f,info);

  /**
   * read and set first coordinate
   */
  SkipWhiteSpace(f); f>>d; if(!f) goto out;
  X=d;

  /**
   * read and set second coordinate
   */
  SkipWhiteSpace(f); f>>d; if(!f) goto out;
  Y=d;


out:

  if( !f )
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"NodeXY::Read()","Error reading FEM node!");
  }

}

/**
 * writes the NodeXY to the output stream
 */
void NodeXY::Write( std::ostream& f, int ofid ) const 
{

  /**
   * if not set already, se set the ofid
   */
  if (ofid<0) 
    {
    ofid=OFID;
    }

  /**
   * first call the parent's write function
   */
  Superclass::Write(f,ofid);

  /**
   * write actual data (node, and properties numbers)
   */
  
  f<<"\t"<<X<<"\t% X"<<"\n";
  f<<"\t"<<Y<<"\t% Y"<<"\n";

  /** check for errors */
  if (!f)
  {
    throw FEMExceptionIO(__FILE__,__LINE__,"NodeXY::Write()","Error writing FEM node!");
  }

}


/**
 * Windows visualization
 */
#ifdef FEM_BUILD_VISUALIZATION
  /**
   * draws the node on DC
   */
  void NodeXY::Draw(CDC* pDC, Solution::ConstPointer sol) const 
    {

    CPen pen(PS_SOLID, 0, (COLORREF) RGB(0,0,0) );
    CPen* pOldpen=pDC->SelectObject(&pen);
    CBrush brush( RGB(255,255,255) );
    CBrush* pOldbrush=pDC->SelectObject(&brush);

    int x1=X*DC_Scale;
    int y1=Y*DC_Scale;
    x1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(0))*DC_Scale;
    y1+=sol->GetSolutionValue(this->GetDegreeOfFreedom(1))*DC_Scale;

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

FEM_CLASS_REGISTER(NodeXY)




}} // end namespace itk::fem
