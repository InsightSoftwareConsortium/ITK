/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadraticTriangleCellTopology.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadraticTriangleCellTopology_h
#define __itkQuadraticTriangleCellTopology_h

#include "itkWin32Header.h"

namespace itk
{

/** \class QuadraticTriangleCellTopology
 * QuadraticTriangleCellTopology holds data defining the topological 
 * connections of the vertices and edges of an Triangle Cell.
 *
 * This class is used to localize static variables out of .txx 
 * files. This prevents multiple definition of static variables.
 *
 * \ingroup MeshObjects
 */

class ITKCommon_EXPORT QuadraticTriangleCellTopology
{
protected:
   
   /** Triangle topology data. */
  static const int m_Edges[3][3];

  
public:
  QuadraticTriangleCellTopology();
  ~QuadraticTriangleCellTopology();
  
};

} // end namespace itk


#endif
