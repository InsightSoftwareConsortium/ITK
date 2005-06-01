/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageMutualInformation1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// This example illustrates how to compute the Mutual Information between two
// images using classes from the Statistics framework. Note that you could also
// use for this purpose the ImageMetrics designed for the image registration
// framework.
//
// For example, you could use:
// 
// \doxygen{MutualInformationImageToImageMetric}
// \doxygen{MattesMutualInformationImageToImageMetric}
// \doxygen{MutualInformationHistogramImageToImageMetric}
// \doxygen{MutualInformationImageToImageMetric}
// \doxygen{NormalizedMutualInformationHistogramImageToImageMetric}
// \doxygen{KullbackLeiblerCompareHistogramImageToImageMetric}
//
// Mutual Information as computed in this example, and as commonly used in the
// context of image registration provides a measure of how much uncertainty on
// the value of a pixel in one image is reduced by measuring the homologous
// pixel in the other image. Note that Mutual Information as used here does not
// measures the amount of information that one image provides on the other
// image, such measure would have required to take into account the spatial
// structures in the images as well as the semantics of the image context in
// terms of an observer.
//
// This implies that there is still an enormous unexploited potential on the
// use of the Mutual Information concept in the domain of medical images.
// Probably the most interesting of which would be the semantic description of
// image on terms of anatomical structures.
//
// \index{Mutual Information!Statistics}
// \index{Statistics!Mutual Information}
// \index{Joint Entropy!Statistics}
// \index{Statistics!Joint Entropy}
// \index{Joint Histogram!Statistics}
// \index{Statistics!Joint Histogram}
// 
// Software Guide : EndLatex 



// Software Guide : BeginLatex
//
// In this particular example we make use of classes from the Statistics
// framework in order to compute the measure of Mutual Information between two
// images. We assume that both images have the same number of pixels along
// every dimension and that they have the same origin and spacing. Therefore
// their pixels.
//
// We must start by including the header files of the image, histogram
// generator, reader and Join image filter. We will read both images and use
// the Join image filter in order to compose an image of two components using
// the information of each one of the input images in one component. This is
// the natural way o fusing the Statistics framework in ITK given that the
// fundamental statistical classes are expecting to receive multivalued
// measured.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkJoinImageFilter.h"
#include "itkImageToHistogramGenerator.h"
// Software Guide : EndCodeSnippet



int main( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageHistogram1  inputImage1 inputImage2 " << std::endl;
    return -1;
    }


// Software Guide : BeginLatex
//
// We define the pixel type and dimension of the images to be read.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef unsigned char                                 PixelComponentType;
  const unsigned int                                    Dimension = 2;

  typedef itk::Image< PixelComponentType, Dimension >   ImageType;
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// Using the image type we proceed to instantiate the readers for both input
// images. Then, we take their filesnames from the command line arguments.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >             ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );
// Software Guide : EndCodeSnippet






// Software Guide : BeginLatex
//
// Using the \doxygen{JoinImageFilter} we use the two input images and put them
// together in an image of two components.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::JoinImageFilter< ImageType, ImageType >  JoinFilterType;

  JoinFilterType::Pointer joinFilter = JoinFilterType::New();
  
  joinFilter->SetInput1( reader1->GetOutput() );
  joinFilter->SetInput2( reader2->GetOutput() );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// At this point we trigger the execution of the pipeline by invoking the
// \code{Update()} method on the Join filter. We must put the call inside a
// try/catch block because the Update() call may potentially result in
// exceptions being thrown.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  try
    {
    joinFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return -1;
    }
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We prepare now the types to be used for the computation of the Joint
// histogram. For this purpose, we take the type of the image resulting from
// the JoinImageFilter and use it as template argument of the
// \doxygen{ImageToHistogramGenerator}. We then construct one by invoking the
// \code{New()} method. 
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef JoinFilterType::OutputImageType               VectorImageType;

  typedef itk::Statistics::ImageToHistogramGenerator< 
                                       VectorImageType >  HistogramGeneratorType;

  HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We pass the multiple components image as input to the histogram generator,
// and setup the marginal scale value that will define the precision to be used
// for classifying values into the histogram bins.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->SetInput(  joinFilter->GetOutput()  );

  histogramGenerator->SetMarginalScale( 10.0 );
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// We must now define the number of bins to use for each one of the components
// in the joint image. For this purpose we take the \code{SizeType} from the
// traits of the histogram generator type. The array of number of bins is
// passed to the generator and we can then invoke the \code{Compute()} method
// in order to trigger the computation of the joint histogram.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef HistogramGeneratorType::SizeType   SizeType;

  SizeType size;

  size[0] = 255;  // number of bins for the first  channel
  size[1] = 255;  // number of bins for the second channel

  histogramGenerator->SetNumberOfBins( size );
  histogramGenerator->Compute();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The histogram can be recovered from the generator by creating a variable
// with the histogram type taken from the generator traits.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// We now walk over all the bins of the joint histogram and compute their
// contribution to the value of the joint Entropy. For this purpose we use
// histogram iterators, and the \code{Begin()} and \code{End()} methods.  Since
// the values returned from the histogram are frequency we must convert them to
// an estimation of probability by dividing them over the total sum of
// frequencies returned by the \code{GetTotalFrequency()} method.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  HistogramType::ConstIterator itr = histogram->Begin();
  HistogramType::ConstIterator end = histogram->End();
 
  const double Sum = histogram->GetTotalFrequency();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// We initialize to zero the variable to use for accumulating the value of the
// joint entropy, and then use the iterator for visiting all the bins of the
// joint histogram. At every bin we compute their contribution to the reduction
// of uncertainty. Note that in order to avoid logarithmic operations on zero
// values, we skip over those bins that have less than one count. The entropy
// contribution must be computed using logarithms in base two in order to be
// able express entropy in \textbf{bits}.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  double JointEntropy = 0.0;

  while( itr != end )
    {
    const double count = itr.GetFrequency();
    if( count > 0.0 )
      {
      const double probability = count / Sum;
      JointEntropy += - probability * log( probability ) / log( 2.0 );
      }
    ++itr;
    }
// Software Guide : EndCodeSnippet

  std::cout << "Joint Entropy      = " << JointEntropy << " bits " << std::endl;




// Software Guide : BeginLatex
//
// Now that we have the value of the joint entropy we can proceed to estimate
// the values of the entropies for each image independently. This can be done
// by simply changing the number of bins and then recomputing the histogram.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  size[0] = 255;  // number of bins for the first  channel
  size[1] =   1;  // number of bins for the second channel

  histogramGenerator->SetNumberOfBins( size );
  histogramGenerator->Compute();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We initialize to zero another variable in order to start accumulating the
// entropy contributions from every bin.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  itr = histogram->Begin();
  end = histogram->End();

  double Entropy1 = 0.0;

  while( itr != end )
    {
    const double count = itr.GetFrequency();
    if( count > 0.0 )
      {
      const double probability = count / Sum;
      Entropy1 += - probability * log( probability ) / log( 2.0 );
      }
    ++itr;
    }
// Software Guide : EndCodeSnippet

  std::cout << "Image1 Entropy   = " << Entropy1 << " bits " << std::endl;




// Software Guide : BeginLatex
//
// The same process is used for computing the entropy of the other component.
// Simply by swapping the number of bins in the histogram.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  size[0] =   1;  // number of bins for the first channel
  size[1] = 255;  // number of bins for the second channel

  histogramGenerator->SetNumberOfBins( size );
  histogramGenerator->Compute();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// The entropy is computed in a similar maner, just by visiting all the bins on
// the histogram and accumulating their entropy contributions.
// 
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  itr = histogram->Begin();
  end = histogram->End();

  double Entropy2 = 0.0;

  while( itr != end )
    {
    const double count = itr.GetFrequency();
    if( count > 0.0 )
      {
      const double probability = count / Sum;
      Entropy2 += - probability * log( probability ) / log( 2.0 );
      }
    ++itr;
    }
// Software Guide : EndCodeSnippet

  std::cout << "Image2 Entropy   = " << Entropy2 << " bits " << std::endl;


// Software Guide : BeginLatex
//
// At this point we can compute any of the popular measures of Mutual
// Information. For example
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  double MutualInformation = Entropy1 + Entropy2 - JointEntropy;
// Software Guide : EndCodeSnippet
  
  std::cout << "Mutual Information = " << MutualInformation << " bits " << std::endl;


  

// Software Guide : BeginLatex
//
// or Normalized Mutual Information, where the value of Mutual Information gets
// divided by the mean entropy of the input images. 
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  double NormalizedMutualInformation1 = 
                     2.0 * MutualInformation / ( Entropy1 + Entropy2 );
// Software Guide : EndCodeSnippet

  std::cout << "Normalized Mutual Information 1 = " << NormalizedMutualInformation1 <<  std::endl;




// Software Guide : BeginLatex
//
// A second form of Normalized Mutual Information has been defined as the mean
// entropy of the two images divided by their joint entropy.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  double NormalizedMutualInformation2 = ( Entropy1 + Entropy2 ) / JointEntropy;
// Software Guide : EndCodeSnippet


  std::cout << "Normalized Mutual Information 2 = " << NormalizedMutualInformation2 <<  std::endl;



// Software Guide : BeginLatex
//
// You probably will find very interesting how the value of Mutual Information
// is strongly dependent on the number of bins over which the histogram is
// defined.
//
// Software Guide : EndLatex 


  return 0;
  
}

