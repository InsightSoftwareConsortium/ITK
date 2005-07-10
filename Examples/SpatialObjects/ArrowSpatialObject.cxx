/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ArrowSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// \index{itk::ArrowSpatialObject}
//
// This example shows how to create a \doxygen{ArrowSpatialObject}.
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include <itkArrowSpatialObject.h>
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{
// Software Guide : BeginLatex
// The \doxygen{ArrowSpatialObject}, like many SpatialObjects, is templated
// over the dimensionality of the object.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  typedef itk::ArrowSpatialObject<3>   ArrowType;
  ArrowType::Pointer myArrow = ArrowType::New(); 
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// The length of the arrow in the local coordinate frame is done using the
// SetLength() function. By default the length is set to 1.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  myArrow->SetLength(2);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// The direction of the arrow can be set using the SetDirection() function.
// The SetDirection() function modifies the ObjectToParentTransform (not the IndexToObjectTransform).
// By default the direction is set along the X axis (first direction).
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  ArrowType::VectorType direction;
  direction.Fill(0);
  direction[1] = 1.0;
  myArrow->SetDirection(direction);
// Software Guide : EndCodeSnippet
 
  return EXIT_SUCCESS;

}
