/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    NeighborhoodSampler.cxx
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
// \index{Statistics!Sampling mesurement vectors using radius|textbf}
// \index{itk::Statistics::NeighborhoodSampler|textbf}
//
// When we want to create a \subdoxygen{Statistics}{Subsample} object
// that includes only the measurement vectors within a radius from a
// center in a sample, we can use the
// \subdoxygen{Statistics}{NeighborhoodSampler}. As the input sample, we
// will use the \subdoxygen{Statistics}{ListSample} in this example.
//
// We include the header files for the \code{ListSample} and the
// \code{NeighborhoodSample} classes.
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkNeighborhoodSampler.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class which is a subclass of the \doxygen{FixedArray}
// in this example. 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // The following code snippet will create a \code{ListSample} object
  // with two-component int measurement vectors and put the measurement
  // vectors: [1,1] - 1 time, [2,2] - 2 times, [3,3] - 3 times, [4,4] -
  // 4 times, [5,5] - 5 times into the \code{listSample}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef int MeasurementType ;
  typedef itk::Vector< MeasurementType , 2 > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;

  SampleType::Pointer sample = SampleType::New() ;

  MeasurementVectorType mv ;
  for ( unsigned int i = 1 ; i < 6 ; i++ )
    {
    for (unsigned int j = 0 ; j < 2 ; j++ )
      {
      mv[j] = ( MeasurementType ) i ;
      }
    for ( unsigned int j = 0 ; j < i ; j++ )
      {
      sample->PushBack(mv) ;
      }
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We plug-in the sample to the \code{sampler} using the
  // \code{SetInputSample(sample*)}. The two required inputs for the
  // \code{sampler} are a center and a radius. We set two inputs using
  // the \code{SetCenter(center vector*)} and the
  // \code{SetRadius(double*)} respectively. And then we call the
  // \code{Update()} method to generate the \code{Subsample}
  // object. This sampling procedure subsamples measurement vectors
  // whithin a hyper-spherical kernel that is centered at the \code{center}
  // and has the radius, \code{radius}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::NeighborhoodSampler< SampleType > SamplerType ;
  
  SamplerType::Pointer sampler = SamplerType::New() ;

  sampler->SetInputSample( sample ) ;
  SamplerType::CenterType center ;
  center[0] = 3 ;
  center[1] = 3 ;
  double radius = 1.5 ;
  sampler->SetCenter( &center ) ;
  sampler->SetRadius( &radius ) ;
  sampler->Update() ;

  SamplerType::OutputType::Pointer output = sampler->GetOutput() ;
  // Software Guide : EndCodeSnippet

  
  // Software Guide : BeginLatex
  // The \code{SamplerType::OutputType} is in fact
  // \subdoxygen{Statistics}{Subsample}. The following code prints out
  // the resampled measurement vectors.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SamplerType::OutputType::Iterator iter = output->Begin() ;
  while ( iter != output->End() )
    {
    std::cout << "instance identifier = " << iter.GetInstanceIdentifier() 
              << "\t measurement vector = " 
              << iter.GetMeasurementVector() 
              << "\t frequency = " 
              << iter.GetFrequency() << std::endl ;
    ++iter ;
    }
  // Software Guide : EndCodeSnippet

  return 0 ;
}
