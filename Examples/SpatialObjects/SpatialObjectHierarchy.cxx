/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SpatialObjectHierarchy.cxx
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
// \index{itk::SpatialObjectHierarchy}
// This example describes how \doxygen{SpatialObject} can form a hierarchy.
// This first example also shows how to create and manipulate SpatialObjects.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int argc, char *argv[] )
{

// Software Guide : BeginLatex
//
// First, we create two SpatialObjects and give them the names "First Object" 
// and "Second Object" respectively.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::SpatialObject<3> SpatialObjectType;

  SpatialObjectType::Pointer object1 = SpatialObjectType ::New();
  object1->GetProperty()->SetName("First Object");

  SpatialObjectType::Pointer object2 = SpatialObjectType ::New();
  object2->GetProperty()->SetName("Second Object");
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
//
// We then add the second object to the first one by using the AddSpatialObject() function.
// As a result object2 becames a child of object1.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet 
  object1->AddSpatialObject(object2);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We can query if an object has a parent by using the HasParent() function. If it has one, the
// GetParent() function returns a constant pointer to the parent.
// In our case, if we ask the parent's name of the object2 we should obtain: "First Object".
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  if(object2->HasParent())
    {
    std::cout << "Name of the parent of the object2: ";
    std::cout << object2->GetParent()->GetProperty()->GetName() << std::endl;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// To access the list of children of the object, the GetChildren() function
// returns a pointer to the (STL) list of children.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  SpatialObjectType::ChildrenListType * childrenList = object1->GetChildren();
  std::cout << "object1 has " << childrenList->size() << " child" << std::endl;
  
  SpatialObjectType::ChildrenListType::const_iterator it = childrenList->begin();
  while(it != childrenList->end())
    {
    std::cout << "Name of the child of the object 1: ";
    std::cout << (*it)->GetProperty()->GetName() << std::endl;
    it++;
    }
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
//
// An object can also be removed by using the RemoveSpatialObject() method.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  object1->RemoveSpatialObject(object2);
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
//
// We can query the number of children an object has with the GetNumberOfChildren() function
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  std::cout << "Number of children for object1: ";
  std::cout << object1->GetNumberOfChildren() << std::endl;
// Software Guide : EndCodeSnippet
// Software Guide : BeginLatex
//
// The Clear() method erasess all the information regarding the object as well as the data. This function is,
// most of the time, overloaded by the derived classes.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  object1->Clear();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The output of this first example looks like the following:
//
// Name of the parent of the object2: First Object
//
// object1 has 1 child
//
// Name of the child of the object 1: Second Object
//
// Number of children for object1: 0
//
// Software Guide : EndLatex 


  return 0;
}
