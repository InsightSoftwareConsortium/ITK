/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    Scene.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// \index{itk::Scene}
// This example describes how to use the \doxygen{Scene}.
// A scene contains a collection of SpatialObjects.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkScene.h"
// Software Guide : EndCodeSnippet
#include "itkEllipseSpatialObject.h"

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// An \doxygen{itkScene} is templated over the dimension of the space which means that all the objects
// in the scene should have the same dimension.
//
// First we define some type definitions and we create the scene.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Scene<3> SceneType;
  SceneType::Pointer scene = SceneType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we create two \doxygen{EllipseSpatialObjects}.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<3> EllipseType;
  EllipseType::Pointer ellipse1 = EllipseType::New();
  ellipse1->SetRadius(1);
  ellipse1->SetId(1);
  EllipseType::Pointer ellipse2 = EllipseType::New();
  ellipse2->SetId(2);
  ellipse2->SetRadius(2);
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We, then, add the two ellipses into the scene.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  scene->AddSpatialObject(ellipse1);
  scene->AddSpatialObject(ellipse2);
// Software Guide : EndCodeSnippet

  
// Software Guide : BeginLatex
//
// We can query the number of object in the scene with the
// GetNumberOfObjects() function. This function takes two optional arguments:
// the depth at which we should count the number of objects (default is set to infinity) 
// and the name of the object to count (default is set to NULL). 
// This allows the user to count, for example, only ellipses ,
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  std::cout << "Number of objects in the scene = ";
  std::cout << scene->GetNumberOfObjects() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The GetObjectById() returns the first object in the scene that has the specified identification
// number.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  std::cout << "Object in the scene with an ID == 2: " << std::endl;
  scene->GetObjectById(2)->Print(std::cout);
// Software Guide : EndCodeSnippet
  
// Software Guide : BeginLatex
//
// Objects can also be removed from the scene using the RemoveSpatialObject() function.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  scene->RemoveSpatialObject(ellipse1);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The list of current objects in the scene can be retrieve using the GetObjects() function.
// Like the GetNumberOfObjects() function, GetObjects() can take two arguments: 
// a search depth and a matching name.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  SceneType::ObjectListType * myObjectList =  scene->GetObjects();
  std::cout << "Number of objects in the scene = ";
  std::cout << myObjectList->size() << std::endl;
// Software Guide : EndCodeSnippet

  
// Software Guide : BeginLatex
//
// In some cases, it is useful to define a hierarchy by using ParentId() and current identification
// number. This results in having a flat list of SpatialObjects in the scene. Therefore, the \doxygen{Scene} 
// provides the FixHierarchy() function which reorganizes the Parent-Child hierarchy based on identification numbers.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet 
  scene->FixHierarchy();
// Software Guide : EndCodeSnippet

  return 0;
}
