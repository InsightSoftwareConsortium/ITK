/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    TubeSpatialObject.cxx
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
// \index{itk::TubeSpatialObject}
// TubeSpatialObject defines an N-Dimensional Tube.
// A tube is defined as a list of centerline points which have a position,
// a radius, some normals and other properties.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkTubeSpatialObject.h"
#include "itkTubeSpatialObjectPoint.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// TubeSpatialObject is templated over the dimension of the space.
// A TubeSpatialObject contains a list of TubeSpatialObjectPoints. 
//
// First we define some type definitions and we create the tube.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::TubeSpatialObject<3>            TubeType;
  typedef TubeType::Pointer                    TubePointer;
  typedef itk::TubeSpatialObjectPoint<3>       TubePointType;
  typedef TubePointType::CovariantVectorType   VectorType;

   TubePointer tube = TubeType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We create a point list and we set:
//   
// 1) The position of each point in the local coordinate system using the SetPosition() method.
//
// 2) The radius of the tube at this position using SetRadius().
//
// 3) The two normals at the tube is set using SetNormal1() and SetNormal2().
//
// 4) The color of the point is set to red in our case.
//
// Other values can be set to tell  how well the tube was extracted from the image. 
// These values are indeed optional if the tube is not extracted from an image.
// First, the ridgeness coefficient tells how well the extracted tube falls into a ridge.
// Second, the medialness value shows how medial the point is on the ridge. Third and last,
// the branchness value tends to take a high value when a branch is present nearby the centerline point.
// All these values can be set using SetRidgeness(), SetMedialness(), SetBranchness().
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  TubeType::PointListType list;
  for( unsigned int i=0; i<5; i++)
    {
    TubePointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetRadius(1);
    VectorType normal1;
    VectorType normal2;
    for(unsigned int j=0;j<3;j++)
      {
      normal1[j]=j;
      normal2[j]=j*2;
      }
    
    p.SetNormal1(normal1);
    p.SetNormal2(normal2);
    p.SetColor(1,0,0,1);
  
    p.SetRidgeness(1.0);
    p.SetMedialness(1.0);
    p.SetBranchness(1.0);
    list.push_back(p);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, we create the tube and set its name using SetName(). We also set its identification
// number with SetId() and, at the end, we add the list of points previously created.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  tube->GetProperty()->SetName("Tube1");
  tube->SetId(1);
  tube->SetPoints(list);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The GetPoints() method return a reference to the internal list of points of the object.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  TubeType::PointListType pointList = tube->GetPoints();
  std::cout << "Number of points representing the tube: ";
  std::cout << pointList.size() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The ComputeTangentAndNormals() function computes the normals and the tangent for each point
// using finite differences.
//
// Software Guide : EndLatex 
// Software Guide : BeginCodeSnippet
  tube->ComputeTangentAndNormals();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we can access the points using STL iterators.
// GetPosition() and GetColor() functions return respectively the position and the color
// of the point.GetRadius return the radius at that point. GetNormal1() and GetNormal1() functions
// return a \doxygen{CovariantVector} and GetTangent() returns a \doxygen{Vector}.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  TubeType::PointListType::const_iterator it = tube->GetPoints().begin();
  unsigned int i=0;
  while(it != tube->GetPoints().end())
    {
    std::cout << std::endl;
    std::cout << "Tube #" << i << std::endl;
    std::cout << "Position: " << (*it).GetPosition() << std::endl;
    std::cout << "Radius: " << (*it).GetRadius() << std::endl;
    std::cout << "Ridgeness: " << (*it).GetRidgeness() << std::endl;
    std::cout << "Medialness: " << (*it).GetMedialness() << std::endl;
    std::cout << "Branchness: " << (*it).GetBranchness() << std::endl;
    std::cout << "Tangent: " << (*it).GetTangent() << std::endl;
    std::cout << "First Normal: " << (*it).GetNormal1() << std::endl;
    std::cout << "Second Normal: " << (*it).GetNormal2() << std::endl;
    std::cout << "Color = " << (*it).GetColor() << std::endl;
    it++;
    i++;
    }
// Software Guide : EndCodeSnippet

  return 0;
}
