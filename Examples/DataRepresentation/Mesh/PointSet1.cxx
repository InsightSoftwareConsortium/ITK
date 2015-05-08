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
//  The \doxygen{itk::PointSet} is a basic class intended to represent geometry
//  in the form of a set of points in $N$-dimensional space. It is the base
//  class for the \doxygen{itk::Mesh} providing the methods necessary to
//  manipulate sets of points. Points can have values associated with
//  them. The type of such values is defined by a template parameter of the
//  \code{itk::PointSet} class (i.e., \code{TPixelType}). Two basic
//  interaction styles of PointSets are available in ITK. These styles are
//  referred to as \emph{static} and \emph{dynamic}. The first style is used
//  when the number of points in the set is known in advance and is not
//  expected to change as a consequence of the manipulations performed on the
//  set. The dynamic style, on the other hand, is intended to support
//  insertion and removal of points in an efficient manner. Distinguishing
//  between the two styles is meant to facilitate the fine tuning of a
//  \code{PointSet}'s behavior while optimizing performance and memory
//  management.
//
//  \index{itk::PointSet}
//  \index{itk::PointSet!Static}
//  \index{itk::PointSet!Dynamic}
//
//  In order to use the PointSet class, its header file should be included.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkPointSet.h"
// Software Guide : EndCodeSnippet

int main(int, char *[])
{
  //  Software Guide : BeginLatex
  //
  //  Then we must decide what type of value to associate with the
  //  points. This is generally called the \code{PixelType} in order to make the
  //  terminology consistent with the \code{itk::Image}. The PointSet is also
  //  templated over the dimension of the space in which the points are
  //  represented. The following declaration illustrates a typical
  //  instantiation of the PointSet class.
  //
  //  \index{itk::PointSet!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::PointSet< unsigned short, 3 > PointSetType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  A \code{PointSet} object is created by invoking the \code{New()} method
  //  on its type.  The resulting object must be assigned to a
  //  \code{SmartPointer}.  The PointSet is then reference-counted and can be
  //  shared by multiple objects. The memory allocated for the PointSet will
  //  be released when the number of references to the object is reduced to
  //  zero. This simply means that the user does not need to be concerned
  //  with invoking the \code{Delete()} method on this class.  In fact, the
  //  \code{Delete()} method should \textbf{never} be called directly within
  //  any of the reference-counted ITK classes.
  //
  //  \index{itk::PointSet!New()}
  //  \index{itk::PointSet!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointSetType::Pointer  pointsSet = PointSetType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Following the principles of Generic Programming, the \code{PointSet} class has a
  //  set of associated defined types to ensure that interacting objects can be
  //  declared with compatible types. This set of type definitions is commonly known
  //  as a set of \emph{traits}.  Among the traits of the \code{PointSet} class is
  //  \code{PointType}, which is used by the point set to represent points in space.
  //  The following declaration takes the point type as defined in the \code{PointSet}
  //  traits and renames it to be conveniently used in the global namespace.
  //
  //  \index{itk::PointSet!PointType}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef PointSetType::PointType     PointType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{PointType} can now be used to declare point objects to be
  //  inserted in the \code{PointSet}. Points are fairly small objects, so
  //  it is inconvenient to manage them with reference counting and smart
  //  pointers. They are simply instantiated as typical C++ classes. The Point
  //  class inherits the \code{[]} operator from the \code{itk::Array} class.
  //  This makes it possible to access its components using index notation. For
  //  efficiency's sake no bounds checking is performed during index access. It is
  //  the user's responsibility to ensure that the index used is in the range
  //  $\{0,Dimension-1\}$. Each of the components in the point is associated
  //  with space coordinates. The following code illustrates how to instantiate
  //  a point and initialize its components.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointType p0;
  p0[0] = -1.0;     //  x coordinate
  p0[1] = -1.0;     //  y coordinate
  p0[2] =  0.0;     //  z coordinate
  // Software Guide : EndCodeSnippet


  PointType p1;

  p1[0] =  1.0; // Point 1 = { 1,-1,0 }
  p1[1] = -1.0;
  p1[2] =  0.0;


  PointType p2; // Point 2 = { 1,1,0 }
  p2[0] =  1.0;
  p2[1] =  1.0;
  p2[2] =  0.0;

  //  Software Guide : BeginLatex
  //
  //  Points are inserted in the PointSet by using the \code{SetPoint()} method.
  //  This method requires the user to provide a unique identifier for the
  //  point. The identifier is typically an unsigned integer that will enumerate
  //  the points as they are being inserted. The following code shows how three
  //  points are inserted into the PointSet.
  //
  //  \index{itk::PointSet!SetPoint()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  pointsSet->SetPoint( 0, p0 );
  pointsSet->SetPoint( 1, p1 );
  pointsSet->SetPoint( 2, p2 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // It is possible to query the PointSet in order to determine how many points
  // have been inserted into it. This is done with the \code{GetNumberOfPoints()}
  // method as illustrated below.
  //
  //  \index{itk::PointSet!GetNumberOfPoints()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfPoints = pointsSet->GetNumberOfPoints();
  std::cout << numberOfPoints << std::endl;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // Points can be read from the PointSet by using the \code{GetPoint()} method
  // and the integer identifier. The point is stored in a pointer provided by
  // the user. If the identifier provided does not match an
  // existing point, the method will return \code{false} and the contents of the
  // point will be invalid. The following code illustrates point access
  // using defensive programming.
  //
  //  \index{itk::PointSet!GetPoint()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointType pp;
  bool pointExists =  pointsSet->GetPoint( 1, & pp );

  if( pointExists )
    {
    std::cout << "Point is = " << pp << std::endl;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \code{GetPoint()} and \code{SetPoint()} are not the most efficient methods
  // to access points in the PointSet. It is preferable to get direct access
  // to the internal point container defined by the \emph{traits} and use
  // iterators to walk sequentially over the list of points (as shown in
  // the following example).
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
