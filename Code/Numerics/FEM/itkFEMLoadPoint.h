/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMLoadPoint_h
#define __itkFEMLoadPoint_h

#include "itkFEMLoadElementBase.h"
#include "vnl/vnl_vector.h"

namespace itk {
namespace fem {




/**
 * \class LoadPoint
 * \brief This load is applied on a point in an element.
 *
 * FIXME: To be implemented. Nothing works yet
 */
class LoadPoint : public LoadElement {
FEM_CLASS(LoadPoint,LoadElement)
public:

  /**
   * Point of which the load acts in global coord. sys.
   */
  vnl_vector<Float> point;

  /**
   * the actual load vector
   */
  vnl_vector<Float> Fp;

  /**
   * Default constructor
   */
  LoadPoint() :
    point(2), Fp(2) {}    /**  we initialize 2D point and force vector */

};

FEM_CLASS_INIT(LoadPoint)




}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadPoint_h
