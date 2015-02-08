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
// \index{itk::BlobSpatialObject}
//
// \doxygen{BlobSpatialObject} defines an N-dimensional blob. Like other
// SpatialObjects this class derives from \doxygen{itkSpatialObject}.  A blob
// is defined as a list of points which compose the object.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBlobSpatialObject.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// BlobSpatialObject is templated over the dimension of the space.
// A BlobSpatialObject contains a list of SpatialObjectPoints. Basically,
// a SpatialObjectPoint has a position and a color.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSpatialObjectPoint.h"
// Software Guide : EndCodeSnippet

int main( int, char *[] )
{
// Software Guide : BeginLatex
//
// First we declare some type definitions.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::BlobSpatialObject<3>    BlobType;
  typedef BlobType::Pointer            BlobPointer;
  typedef itk::SpatialObjectPoint<3>   BlobPointType;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then, we create a list of points and we set the position of each point in
// the local coordinate system using the \code{SetPosition()} method. We also
// set the color of each point to be red.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
 BlobType::PointListType list;

  for( unsigned int i=0; i<4; i++)
    {
    BlobPointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetRed(1);
    p.SetGreen(0);
    p.SetBlue(0);
    p.SetAlpha(1.0);
    list.push_back(p);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, we create the blob and set its name using the \code{SetName()}
// function. We also set its Identification number with \code{SetId()} and we
// add the list of points previously created.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  BlobPointer blob = BlobType::New();
  blob->GetProperty()->SetName("My Blob");
  blob->SetId(1);
  blob->SetPoints(list);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The \code{GetPoints()} method returns a reference to the internal list of
// points of the object.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
   BlobType::PointListType pointList = blob->GetPoints();
   std::cout << "The blob contains " << pointList.size();
   std::cout << " points" << std::endl;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Then we can access the points using standard STL iterators and
// \code{GetPosition()} and \code{GetColor()} functions return respectively
// the position and the color of the point.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  BlobType::PointListType::const_iterator it = blob->GetPoints().begin();
  while(it != blob->GetPoints().end())
    {
    std::cout << "Position = " << (*it).GetPosition() << std::endl;
    std::cout << "Color = " << (*it).GetColor() << std::endl;
    ++it;
    }
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
