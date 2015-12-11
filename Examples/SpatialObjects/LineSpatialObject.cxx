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
// \index{itk::LineSpatialObject}
//
// \doxygen{LineSpatialObject} defines a line in an n-dimensional space.  A
// line is defined as a list of points which compose the line, i.e a
// polyline. We begin the example by including the appropriate header files.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkLineSpatialObject.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{
// Software Guide : BeginLatex
//
// LineSpatialObject is templated over the dimension of the space.
// A LineSpatialObject contains a list of LineSpatialObjectPoints.
// A LineSpatialObjectPoint has a position, $n-1$ normals and a color.
// Each normal is expressed as a \doxygen{CovariantVector} of size N.
//
// First, we define some type definitions and we create our line.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::LineSpatialObject<3>        LineType;
  typedef LineType::Pointer                LinePointer;
  typedef itk::LineSpatialObjectPoint<3>   LinePointType;
  typedef itk::CovariantVector<double,3>   VectorType;

  LinePointer Line = LineType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We create a point list and we set the position of each point in the local
// coordinate system using the \code{SetPosition()} method. We also set the
// color of each point to red.
//
// The two normals are set using the \code{SetNormal()} function; the first
// argument is the normal itself and the second argument is the index of the
// normal.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  LineType::PointListType list;

  for (unsigned int i=0; i<3; ++i)
    {
    LinePointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetColor(1,0,0,1);

    VectorType normal1;
    VectorType normal2;
    for (unsigned int j=0; j<3; ++j)
      {
      normal1[j]=j;
      normal2[j]=j*2;
      }

    p.SetNormal(normal1,0);
    p.SetNormal(normal2,1);
    list.push_back(p);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, we set the name of the object using \code{SetName()}. We also set its
// identification number with \code{SetId()} and we set the list of points
// previously created.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  Line->GetProperty()->SetName("Line1");
  Line->SetId(1);
  Line->SetPoints(list);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The \code{GetPoints()} method returns a reference to the internal list of points
// of the object.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
   LineType::PointListType pointList = Line->GetPoints();
   std::cout << "Number of points representing the line: ";
   std::cout << pointList.size() << std::endl;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Then we can access the points using standard STL iterators.  The
// \code{GetPosition()} and \code{GetColor()} functions return respectively the position
// and the color of the point. Using the GetNormal(unsigned int) function we
// can access each normal.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
   LineType::PointListType::const_iterator it = Line->GetPoints().begin();
   while (it != Line->GetPoints().end())
     {
     std::cout << "Position = " << (*it).GetPosition() << std::endl;
     std::cout << "Color = " << (*it).GetColor() << std::endl;
     std::cout << "First normal = " << (*it).GetNormal(0) << std::endl;
     std::cout << "Second normal = " << (*it).GetNormal(1) << std::endl;
     std::cout << std::endl;
     ++it;
     }
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
