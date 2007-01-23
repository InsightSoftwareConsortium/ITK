/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeometricalQuadEdgeTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkGeometricalQuadEdge.h"
#include <iostream>

int itkGeometricalQuadEdgeTest1( int , char* [] )
{
  typedef unsigned int PointIdentifier;
  typedef unsigned int FaceIdentifier;
  typedef float        PointData;
  typedef std::string  FaceData;


  typedef itk::GeometricalQuadEdge<
     PointIdentifier, FaceIdentifier, 
     PointData, FaceData, true >        PrimalQuadEdgeType;


  typedef itk::GeometricalQuadEdge<
     PointIdentifier, FaceIdentifier, 
     PointData, FaceData, false >       DualQuadEdgeType;

 
  return EXIT_SUCCESS;
}

