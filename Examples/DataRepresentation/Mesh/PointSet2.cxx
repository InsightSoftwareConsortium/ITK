/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    PointSet2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  The PointSet class uses an internal container to manage the storage of
//  Points. It is more efficient, in general, to manage points by using the
//  access methods provided directly on the points container. The following
//  example illustrates how to interact with the point container and how to use
//  point iterators.
//
//  Software Guide : EndLatex 


#include "itkPointSet.h"

int main()
{

  typedef itk::PointSet< unsigned short, 3 > PointSetType;

  //  Software Guide : BeginLatex
  //
  //  The \code{PointContainer} type is defined among the \emph{traits} of the
  //  PointSet class. The following line conveniently takes the
  //  \code{PointContainer} type from the PointSet traits and declare it on the
  //  global namespace.
  //
  //  \index{itk::PointSet!PointContainer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef PointSetType::PointsContainer      PointsContainer;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The actual type of the \code{PointsContainer} depends on what style of
  //  \code{PointSet} is being used. Dynamic PointSet use the
  //  \code{itk::MapContainer} while the Static PointSet uses the
  //  \code{itk::VectorContainer}. The vector and map container are basically
  //  ITK wrappers around the STL classes \code{std::map} and
  //  \code{std::vector}. By default, the PointSet uses a Static style, hence
  //  the default type of point container is an \code{itk::VectorContainer}.
  //  Both the map and vector container are templated over the type of the
  //  elements they contain. In this case they are templated over
  //  \code{PointType}. Containers are reference counted object. They are
  //  then created with the \code{New()} method and assigned to a SmartPointer
  //  after creation.  The following line creates a point container compatible
  //  with the type of the PointSet from which the trait has been taken.
  //
  //  \index{itk::PointSet!New()}
  //  \index{itk::PointSet!Pointer}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  PointsContainer::Pointer points = PointsContainer::New();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  Points can now be defined using the \code{PointType} trait from the PointSet.
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
  //  The created points can be inserted in the PointsContainer using the generic
  //  method \code{InsertElement()} which requires an identified to be provided
  //  for each point.
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
  //  Finally the PointsContainer can be assigned to the PointSet. This will
  //  substitute any previously existing PointContainer on the PointSet. The
  //  assignment is done using the \code{SetPoints()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  pointSet->SetPoints( points );
  // Software Guide : EndCodeSnippet






  //  Software Guide : BeginLatex
  //
  //  The PointContainer can be obtained from the PointSet using the
  //  \code{GetPoints()} method.  This method returns a SmartPointer to the
  //  actual container owned by the PointSet.
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
  //  the STL iterators \footnote{If you dig deep enough into the code, you
  //  will discover that these iterators are actually ITK wrappers around STL
  //  iterators.}.  The Points iterator is not a reference counted class, so it
  //  is just created directly from the traits without using SmartPointers.
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
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  PointsIterator  pointIterator = points->Begin();  
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  The \code{++} operator on the iterator can be used to advance from one
  //  point to the next. The actual value of the Point to which the iterator is
  //  pointing can be obtained with the \code{Value()} method. The loop for
  //  walking through all the points can be controled by comparing the current
  //  iterator with the iterator returned by the \code{End()} method of the
  //  PointsContainer. The following lines illustrate the typical loop for
  //  walking through the points.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  PointsIterator end = points->End();
  while( pointIterator != end ) {
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
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  std::cout << pointSet->GetNumberOfPoints() << std::endl;
  std::cout << pointSet->GetPoints()->Size() << std::endl;
  // Software Guide : EndCodeSnippet



  return 0;

}



