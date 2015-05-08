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

//  Software Guide : BeginLatex
//
//  The \doxygen{PointSet} class uses an internal container to manage the storage of
//  \doxygen{Point}s. It is more efficient, in general, to manage points by using the
//  access methods provided directly on the points container. The following
//  example illustrates how to interact with the point container and how to use
//  point iterators.
//
//  Software Guide : EndLatex


#include "itkPointSet.h"

int main(int, char *[])
{
  typedef itk::PointSet< unsigned short, 3 > PointSetType;

  //  Software Guide : BeginLatex
  //
  //  The type is defined by the \emph{traits} of the \code{PointSet}
  //  class. The following line conveniently takes the \code{PointsContainer} type
  //  from the \code{PointSet} traits and declares it in the global namespace.
  //
  //  \index{itk::PointSet!PointsContainer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef PointSetType::PointsContainer      PointsContainer;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The actual type of \code{PointsContainer} depends on what style of
  //  \code{PointSet} is being used. The dynamic \code{PointSet} uses
  //  \doxygen{MapContainer} while the static \code{PointSet} uses
  //  \doxygen{VectorContainer}. The vector and map containers are basically
  //  ITK wrappers around the \href{http://www.sgi.com/tech/stl/}{STL}
  //  classes \href{http://www.sgi.com/tech/stl/Map.html}{\code{std::map}}
  //  and \href{http://www.sgi.com/tech/stl/Vector.html}{\code{std::vector}}.
  //  By default, \code{PointSet} uses a static style, and therefore the default
  //  type of point container is \code{VectorContainer}.  Both map
  //  and vector containers are templated over the type of element they
  //  contain. In this case they are templated over \code{PointType}.
  //  Containers are reference counted objects, created with the
  //  \code{New()} method and assigned to a \doxygen{SmartPointer}.
  //  The following line creates a point container compatible with
  //  the type of the \code{PointSet} from which the trait has been taken.
  //
  //  \index{PointsContainer!New()}
  //  \index{PointsContainer!Pointer}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  PointsContainer::Pointer points = PointsContainer::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  \code{Point}s can now be defined using the \code{PointType} trait from the
  //  \code{PointSet}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef PointSetType::PointType   PointType;
  PointType p0;
  PointType p1;
  p0[0] = -1.0; p0[1] = 0.0; p0[2] = 0.0; // Point 0 = {-1,0,0 }
  p1[0] =  1.0; p1[1] = 0.0; p1[2] = 0.0; // Point 1 = { 1,0,0 }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The created points can be inserted in the \code{PointsContainer} using the
  //  generic method \code{InsertElement()} which requires an identifier to
  //  be provided for each point.
  //
  //  \index{PointsContainer!InsertElement()}
  //  \index{PointsContainer!InsertElement()}
  //  \index{itk::VectorContainer!InsertElement()}
  //  \index{itk::MapContainer!InsertElement()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int pointId = 0;
  points->InsertElement( pointId++ , p0 );
  points->InsertElement( pointId++ , p1 );
  // Software Guide : EndCodeSnippet

  PointSetType::Pointer  pointSet = PointSetType::New();

  //  Software Guide : BeginLatex
  //
  //  Finally, the \code{PointsContainer} can be assigned to the \code{PointSet}. This will
  //  substitute any previously existing \code{PointsContainer} assigned to the \code{PointSet}. The
  //  assignment is done using the \code{SetPoints()} method.
  //
  //  \index{itk::PointSet!SetPoints()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  pointSet->SetPoints( points );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{PointsContainer} object can be obtained from the \code{PointSet} using the
  //  \code{GetPoints()} method.  This method returns a pointer
  //  to the actual container owned by the PointSet which is then assigned to
  //  a \code{SmartPointer}.
  //
  //  \index{itk::PointSet!GetPoints()}
  //  \index{PointsContainer!Pointer}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  PointsContainer::Pointer  points2 = pointSet->GetPoints();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The most efficient way to sequentially visit the points is to use the
  //  iterators provided by PointsContainer. The \code{Iterator} type belongs
  //  to the traits of the PointsContainer classes. It behaves pretty much like
  //  the STL iterators.\footnote{If you dig deep enough into the code, you
  //  will discover that these iterators are actually ITK wrappers around STL
  //  iterators.}  The Points iterator is not a reference counted class, so it
  //  is created directly from the traits without using SmartPointers.
  //
  //  \index{PointsContainer!Iterator}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef PointsContainer::Iterator     PointsIterator;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The subsequent use of the iterator follows what you may expect from a STL
  //  iterator. The iterator to the first point is obtained from the container
  //  with the \code{Begin()} method and assigned to another iterator.
  //
  //  \index{PointsContainer!Begin()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointsIterator  pointIterator = points->Begin();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{++} operator on the iterator can be used to advance from one
  //  point to the next. The actual value of the Point to which the iterator is
  //  pointing can be obtained with the \code{Value()} method. The loop for
  //  walking through all the points can be controlled by comparing the current
  //  iterator with the iterator returned by the \code{End()} method of the
  //  PointsContainer. The following lines illustrate the typical loop for
  //  walking through the points.
  //
  //  \index{PointsContainer!End()}
  //  \index{PointsContainer!Iterator}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointsIterator end = points->End();
  while( pointIterator != end )
    {
    PointType p = pointIterator.Value();   // access the point
    std::cout << p << std::endl;           // print the point
    ++pointIterator;                       // advance to next point
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that as in STL, the iterator returned by the \code{End()} method is
  //  not a valid iterator. This is called a past-end iterator in order to
  //  indicate that it is the value resulting from advancing one step after
  //  visiting the last element in the container.
  //
  //  The number of elements stored in a container can be queried with the
  //  \code{Size()} method. In the case of the PointSet, the following two
  //  lines of code are equivalent, both of them returning the number of points
  //  in the PointSet.
  //
  //  \index{itk::PointSet!GetNumberOfPoints()}
  //  \index{itk::PointSet!GetPoints()}
  //  \index{PointsContainer!Size()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  std::cout << pointSet->GetNumberOfPoints() << std::endl;
  std::cout << pointSet->GetPoints()->Size() << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
