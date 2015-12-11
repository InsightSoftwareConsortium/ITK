/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// Software Guide : BeginLatex
//
// \index{itk::SurfaceSpatialObject}
//
// \doxygen{SurfaceSpatialObject} defines a surface in n-dimensional space.
// A SurfaceSpatialObject is defined by a list of points which lie on the
// surface. Each point has a position and a unique normal. The example begins
// by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSurfaceSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// SurfaceSpatialObject is templated over the dimension of the space.  A
// SurfaceSpatialObject contains a list of SurfaceSpatialObjectPoints.  A
// SurfaceSpatialObjectPoint has a position, a normal and a color.
//
// First we define some type definitions
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::SurfaceSpatialObject<3>        SurfaceType;
  typedef SurfaceType::Pointer                SurfacePointer;
  typedef itk::SurfaceSpatialObjectPoint<3>   SurfacePointType;
  typedef itk::CovariantVector<double,3>      VectorType;

  SurfacePointer Surface = SurfaceType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We create a point list and we set the position of each point in the local
// coordinate system using the \code{SetPosition()} method. We also set the
// color of each point to red.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  SurfaceType::PointListType list;

  for( unsigned int i=0; i<3; i++)
    {
    SurfacePointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetColor(1,0,0,1);
    VectorType normal;
    for(unsigned int j=0;j<3;j++)
      {
      normal[j]=j;
      }
    p.SetNormal(normal);
    list.push_back(p);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, we create the surface and set his name using \code{SetName()}. We
// also set its Identification number with \code{SetId()} and we add the list
// of points previously created.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  Surface->GetProperty()->SetName("Surface1");
  Surface->SetId(1);
  Surface->SetPoints(list);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The \code{GetPoints()} method returns a reference to the internal list of points
// of the object.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
   SurfaceType::PointListType pointList = Surface->GetPoints();
   std::cout << "Number of points representing the surface: ";
   std::cout << pointList.size() << std::endl;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Then we can access the points using standard STL iterators.
// \code{GetPosition()} and \code{GetColor()} functions return respectively
// the position and the color of the point. \code{GetNormal()} returns the
// normal as a \doxygen{CovariantVector}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
   SurfaceType::PointListType::const_iterator it
                                               = Surface->GetPoints().begin();
   while(it != Surface->GetPoints().end())
     {
     std::cout << "Position = " << (*it).GetPosition() << std::endl;
     std::cout << "Normal = " << (*it).GetNormal() << std::endl;
     std::cout << "Color = " << (*it).GetColor() << std::endl;
     std::cout << std::endl;
     it++;
     }
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
