/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DC0LinearHexahedronMembrane.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMElement3DC0LinearHexahedronMembrane_h
#define __itkFEMElement3DC0LinearHexahedronMembrane_h

#include "itkFEMElement3DC0LinearHexahedron.h"
#include "itkFEMElement3DMembrane.h"

namespace itk {
namespace fem {




/**
 * \class Element3DC0LinearHexahedronMembrane
 * \brief 8-noded finite element class in 3D space for linear elasticity problem
 */
class Element3DC0LinearHexahedronMembrane : public Element3DMembrane<Element3DC0LinearHexahedron>
{
FEM_CLASS(Element3DC0LinearHexahedronMembrane,Element3DMembrane<Element3DC0LinearHexahedron>)
public:

  HANDLE_ELEMENT_LOADS();

  /**
   * Default constructor only clears the internal storage
   */
  Element3DC0LinearHexahedronMembrane();

  /**
   * Construct an element by specifying pointers to
   * an array of 8 points and a material.
   */
  Element3DC0LinearHexahedronMembrane(
      NodeIDType ns_[], 
      Material::ConstPointer p_ );

}; // class Element3DC0LinearHexahedronMembrane

FEM_CLASS_INIT(Element3DC0LinearHexahedronMembrane)




}} // end namespace itk::fem

#endif  // #ifndef __itkFEMElement3DC0LinearHexahedronMembrane_h
