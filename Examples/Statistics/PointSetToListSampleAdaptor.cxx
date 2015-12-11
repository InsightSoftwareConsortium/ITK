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
// We will describe how to use \doxygen{PointSet} as a
// \subdoxygen{Statistics}{Sample} using an adaptor in this example.
//
// \index{itk::Statistics::PointSetToListSampleAdaptor}
//
// The \subdoxygen{Statistics}{PointSetToListSampleAdaptor} class requires a
// PointSet as input. The PointSet class is an associative data
// container. Each point in a PointSet object can have an associated
// optional data value. For the statistics subsystem, the current
// implementation of PointSetToListSampleAdaptor takes only the point part
// into consideration. In other words, the measurement vectors from a
// PointSetToListSampleAdaptor object are points from the PointSet
// object that is plugged into the adaptor object.
//
// To use an PointSetToListSampleAdaptor class, we include the header file for the
// class.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkPointSetToListSampleAdaptor.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Since we are using an adaptor, we also include the header file for
// the PointSet class.
//
// Software Guide :EndLatex

// Software Guide : BeginCodeSnippet
#include "itkPointSet.h"
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // Next we create a PointSet object.
  // The following code
  // snippet will create a PointSet object that stores points (its coordinate
  // value type is float) in 3D space.
  //
  // Software Guide :EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::PointSet< short > PointSetType;
  PointSetType::Pointer pointSet = PointSetType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Note that the \code{short} type used in the declaration of
  // \code{PointSetType} pertains to the pixel type associated with every
  // point, not to the type used to represent point coordinates.  If we want
  // to change the type of the point in terms of the coordinate value and/or
  // dimension, we have to modify the \code{TMeshTraits} (one of the optional
  // template arguments for the \code{PointSet} class). The easiest way of
  // creating a custom mesh traits instance is to specialize the existing
  // \doxygen{DefaultStaticMeshTraits}. By specifying the \code{TCoordRep}
  // template argument, we can change the coordinate value type of a point.
  // By specifying the \code{VPointDimension} template argument, we can
  // change the dimension of the point. As mentioned earlier, a
  // \code{PointSetToListSampleAdaptor} object cares only about the points, and the
  // type of measurement vectors is the type of points.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginLatex
  //
  // To make the example a little bit realistic, we add two points
  // into the \code{pointSet}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointSetType::PointType point;
  point[0] = 1.0;
  point[1] = 2.0;
  point[2] = 3.0;

  pointSet->SetPoint( 0UL, point);

  point[0] = 2.0;
  point[1] = 4.0;
  point[2] = 6.0;

  pointSet->SetPoint( 1UL, point );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Now we have a PointSet object with two points in it. The
  // PointSet is ready to be plugged into the adaptor.
  // First, we create an instance of the PointSetToListSampleAdaptor class
  // with the type of the input PointSet object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::PointSetToListSampleAdaptor<
                                                     PointSetType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Second, all we have to do is
  // plug in the PointSet object to the adaptor.  After that,
  // we can use the common methods and iterator interfaces shown in
  // Section~\ref{sec:SampleInterface}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sample->SetPointSet( pointSet );

  SampleType::Iterator iter = sample->Begin();

  while( iter != sample->End() )
    {
    std::cout << "id = " << iter.GetInstanceIdentifier()
              << "\t measurement vector = "
              << iter.GetMeasurementVector()
              << "\t frequency = "
              << iter.GetFrequency()
              << std::endl;
    ++iter;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
