/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GroupSpatialObject.cxx
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
// \index{itk::GroupSpatialObject}
//
// A \doxygen{GroupSpatialObject} does not have
// any data associated with it. It can be used to group objects
// or to add transforms to a current object.
// In this example we show how to use a GroupSpatialObject.
//
// Let's begin by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include <itkGroupSpatialObject.h>
// Software Guide : EndCodeSnippet
#include <itkEllipseSpatialObject.h>

int main( int , char *[] )
{
// Software Guide : BeginLatex
// The \doxygen{GroupSpatialObject} is templated
// over the dimensionality of the object.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  typedef itk::GroupSpatialObject<3>   GroupType;
  GroupType::Pointer myGroup = GroupType::New(); 
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We create an \doxygen{EllipseSpatialObject} and add it to
// the group.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<3>   EllipseType;
  EllipseType::Pointer myEllipse = EllipseType::New(); 
  myEllipse->SetRadius(2);

  myGroup->AddSpatialObject(myEllipse);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// Next, we translate the group. Therefore the ellipse
// is translated in physical space at the same time.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  GroupType::VectorType offset;
  offset.Fill(10);
  myGroup->GetObjectToParentTransform()->SetOffset(offset);
  myGroup->ComputeObjectToWorldTransform();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We can then query if a point is inside the group using the
// \code{IsInside()} function. We need to specify in this case that
// we want to consider all the hierarchy, therefore we set the depth to 2.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  GroupType::PointType point;
  point.Fill(10);
  std::cout << "Is my point " << point << " inside?: " <<  myGroup->IsInside(point,2) << std::endl;

// Software Guide : BeginLatex
// Like any other SpatialObjects we can remove the ellipse from the group
// using the \code{RemoveSpatialObject()} method.
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet  
  myGroup->RemoveSpatialObject(myEllipse);
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}
