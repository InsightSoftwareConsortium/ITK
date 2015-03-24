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
//  The \doxygen{PointSet} class was designed to interact with the \code{Image} class.
//  For this reason it was found convenient to allow the points in the set to
//  hold values that could be computed from images. The value associated with
//  the point is referred as \code{PixelType} in order to make it consistent
//  with image terminology. Users can define the type as they please thanks to
//  the flexibility offered by the Generic Programming approach used in the
//  toolkit.  The \code{PixelType} is the first template parameter of the
//  PointSet.
//
//  \index{itk::PointSet!PixelType}
//
//  Software Guide : EndLatex


#include "itkPointSet.h"

int main(int, char *[])
{
  //  Software Guide : BeginLatex
  //
  //  The following code defines a particular type for a pixel type and
  //  instantiates a PointSet class with it.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned short                PixelType;
  typedef itk::PointSet< PixelType, 3 > PointSetType;
  // Software Guide : EndCodeSnippet


  // A point set is instantiated here
  PointSetType::Pointer  pointSet = PointSetType::New();


  //  Software Guide : BeginLatex
  //
  //  Data can be inserted into the PointSet using the \code{SetPointData()}
  //  method. This method requires the user to provide an identifier. The data
  //  in question will be associated to the point holding the same identifier.
  //  It is the user's responsibility to verify the appropriate matching between
  //  inserted data and inserted points. The following line illustrates the use
  //  of the \code{SetPointData()} method.
  //
  //  \index{itk::PointSet!SetPointData()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int dataId =  0;
  PixelType value     = 79;
  pointSet->SetPointData( dataId++, value );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Data associated with points can be read from the PointSet using the
  //  \code{GetPointData()} method. This method requires the user to provide
  //  the identifier to the point and a valid pointer to a location where the
  //  pixel data can be safely written. In case the identifier does not match
  //  any existing identifier on the PointSet the method will return
  //  \code{false} and the pixel value returned will be invalid. It is the
  //  user's responsibility to check the returned boolean value before
  //  attempting to use it.
  //
  //  \index{itk::PointSet!GetPointData()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet

  const bool found = pointSet->GetPointData( dataId, & value );
  if( found )
    {
    std::cout << "Pixel value = " << value << std::endl;
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The \code{SetPointData()} and \code{GetPointData()} methods are not the
  //  most efficient way to get access to point data. It is far more efficient
  //  to use the Iterators provided by the \code{PointDataContainer}.
  //
  //  Data associated with points is internally stored in
  //  \code{PointDataContainer}s.  In the same way as with points, the actual
  //  container type used depend on whether the style of the PointSet is static
  //  or dynamic. Static point sets will use an \doxygen{VectorContainer} while
  //  dynamic point sets will use an \doxygen{MapContainer}.  The type of the
  //  data container is defined as one of the traits in the PointSet. The
  //  following declaration illustrates how the type can be taken from the
  //  traits and used to conveniently declare a similar type on the global
  //  namespace.
  //
  //  \index{itk::PointSet!PointDataContainer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef PointSetType::PointDataContainer      PointDataContainer;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Using the type it is now possible to create an instance of the data
  //  container. This is a standard reference counted object, henceforth it
  //  uses the \code{New()} method for creation and assigns the newly created
  //  object to a SmartPointer.
  //
  //  \index{PointDataContainer!New()}
  //  \index{PointDataContainer!Pointer}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  PointDataContainer::Pointer pointData = PointDataContainer::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Pixel data can be inserted in the container with the method
  //  \code{InsertElement()}. This method requires an identified to be provided
  //  for each point data.
  //
  //  \index{PointDataContainer!InsertElement()}
  //  \index{itk::VectorContainer!InsertElement()}
  //  \index{itk::MapContainer!InsertElement()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int pointId = 0;

  PixelType value0 = 34;
  PixelType value1 = 67;

  pointData->InsertElement( pointId++ , value0 );
  pointData->InsertElement( pointId++ , value1 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the PointDataContainer can be assigned to the PointSet. This will
  //  substitute any previously existing PointDataContainer on the PointSet. The
  //  assignment is done using the \code{SetPointData()} method.
  //
  //  \index{itk::PointSet!SetPointData()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  pointSet->SetPointData( pointData );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The PointDataContainer can be obtained from the PointSet using the
  //  \code{GetPointData()} method.  This method returns a pointer (assigned to
  //  a SmartPointer) to the actual container owned by the PointSet.
  //
  //  \index{itk::PointSet!GetPointData()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointDataContainer::Pointer  pointData2 = pointSet->GetPointData();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The most efficient way to sequentially visit the data associated with
  //  points is to use the iterators provided by \code{PointDataContainer}. The
  //  \code{Iterator} type belongs to the traits of the PointsContainer
  //  classes. The iterator is not a reference counted class, so it is just
  //  created directly from the traits without using SmartPointers.
  //
  //  \index{PointDataContainer!Iterator}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef PointDataContainer::Iterator     PointDataIterator;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The subsequent use of the iterator follows what you may expect from a STL
  //  iterator. The iterator to the first point is obtained from the container
  //  with the \code{Begin()} method and assigned to another iterator.
  //
  //  \index{PointDataContainer!Begin()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointDataIterator  pointDataIterator = pointData2->Begin();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{++} operator on the iterator can be used to advance from one
  //  data point to the next. The actual value of the PixelType to which the
  //  iterator is pointing can be obtained with the \code{Value()}
  //  method. The loop for walking through all the point data can be
  //  controlled by comparing the current iterator with the iterator returned
  //  by the \code{End()} method of the PointsContainer. The following lines
  //  illustrate the typical loop for walking through the point data.
  //
  //  \index{PointDataContainer!End()}
  //  \index{PointDataContainer!increment ++}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointDataIterator end = pointData2->End();
  while( pointDataIterator != end )
    {
    PixelType p = pointDataIterator.Value();  // access the pixel data
    std::cout << p << std::endl;              // print the pixel data
    ++pointDataIterator;                      // advance to next pixel/point
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that as in STL, the iterator returned by the \code{End()} method is
  //  not a valid iterator. This is called a \emph{past-end} iterator in order
  //  to indicate that it is the value resulting from advancing one step after
  //  visiting the last element in the container.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
