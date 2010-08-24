/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadraticTriangleCellTopology.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkQuadraticTriangleCellTopology.h"

namespace itk
{
/**
 * The triangle's topology data: Edges
 */
const int
QuadraticTriangleCellTopology
:: m_Edges[3][3] = { { 0, 4, 1 }, { 1, 5, 2 }, { 2, 3, 0 } };

QuadraticTriangleCellTopology
::QuadraticTriangleCellTopology()
{}

QuadraticTriangleCellTopology
::~QuadraticTriangleCellTopology()
{}
} // end namespace itk
