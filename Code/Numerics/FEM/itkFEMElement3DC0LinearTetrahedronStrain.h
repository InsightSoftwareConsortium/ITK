/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DC0LinearTetrahedronStrain.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement3DC0LinearTetrahedronStrain_h
#define __itkFEMElement3DC0LinearTetrahedronStrain_h

#include "itkFEMElement3DC0LinearTetrahedron.h"
#include "itkFEMElement3DStrain.h"

namespace itk {
namespace fem {




/**
 * \class Element3DC0LinearTetrahedronStrain
 * \brief 4-noded finite element class in 3D space for linear elasticity problem
 */
class Element3DC0LinearTetrahedronStrain : public Element3DStrain<Element3DC0LinearTetrahedron>
{
FEM_CLASS(Element3DC0LinearTetrahedronStrain,Element3DStrain<Element3DC0LinearTetrahedron>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearTetrahedronStrain();

  /**
   * Construct an element by specifying pointers to
   * an array of 4 points and a material.
   */
  Element3DC0LinearTetrahedronStrain(
      NodeIDType ns_[], 
      Material::ConstPointer p_ );

}; // class Element3DC0LinearTetrahedronStrain

FEM_CLASS_INIT(Element3DC0LinearTetrahedronStrain)




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement3DC0LinearTetrahedronStrain_h
