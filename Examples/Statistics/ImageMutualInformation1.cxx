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
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageToHistogramGenerator.h"
#include "itkImage.h"

#include "itkImageFileReader.h"
#include "itkJoinImageFilter.h"

int main( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageHistogram1  inputImage1 inputImage2 " << std::endl;
    return -1;
    }

  typedef unsigned char                                 PixelComponentType;
  const unsigned int                                    Dimension = 2;

  typedef itk::Image< PixelComponentType, Dimension >   ImageType;

  typedef itk::JoinImageFilter< ImageType, ImageType >  JoinFilterType;

  JoinFilterType::Pointer joinFilter = JoinFilterType::New();
  
  typedef itk::ImageFileReader< ImageType >             ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );

  joinFilter->SetInput1( reader1->GetOutput() );
  joinFilter->SetInput2( reader2->GetOutput() );

  try
    {
    joinFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return -1;
    }


  typedef JoinFilterType::OutputImageType               VectorImageType;

  typedef itk::Statistics::ImageToHistogramGenerator< 
                                              VectorImageType 
                                                          >   HistogramGeneratorType;
  typedef HistogramGeneratorType::SizeType   SizeType;


  SizeType size;

  size[0] = 255;  // number of bins for the first  channel
  size[1] =   1;  // number of bins for the second channel


  HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();

  histogramGenerator->SetInput(  joinFilter->GetOutput()  );



  histogramGenerator->SetNumberOfBins( size );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();

  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size   = " << histogramSize << std::endl;

  // Compute the Entropy of the distribution of intensities
  HistogramType::ConstIterator itr = histogram->Begin();
  HistogramType::ConstIterator end = histogram->End();
 
  double Sum = histogram->GetTotalFrequency();

  double Entropy1 = 0.0;

  while( itr != end )
    {
    const double probability = itr.GetFrequency() / Sum;
    if( probability > 1e-16 )
      {
      Entropy1 += - probability * log( probability ) / log( 2.0 );
      }
    ++itr;
    }

  std::cout << "Image1 Entropy   = " << Entropy1 << " bits " << std::endl;


  // Now compute the histogram for the second component
  size[0] =   1;  // number of bins for the first channel
  size[1] = 255;  // number of bins for the second channel

  histogramGenerator->SetNumberOfBins( size );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  itr = histogram->Begin();
  end = histogram->End();
  Sum = histogram->GetTotalFrequency();

  double Entropy2 = 0.0;

  while( itr != end )
    {
    const double probability = itr.GetFrequency() / Sum;
    if( probability > 1e-16 )
      {
      Entropy2 += - probability * log( probability ) / log( 2.0 );
      }
    ++itr;
    }

  std::cout << "Image2 Entropy   = " << Entropy2 << " bits " << std::endl;


  // Now compute the joint histogram for the two components
  size[0] = 255;  // number of bins for the first  channel
  size[1] = 255;  // number of bins for the second channel

  histogramGenerator->SetNumberOfBins( size );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  itr = histogram->Begin();
  end = histogram->End();
  Sum = histogram->GetTotalFrequency();

  double JointEntropy = 0.0;

  while( itr != end )
    {
    const double probability = itr.GetFrequency() / Sum;

    if( probability > 1e-16 )
      {
      JointEntropy += - probability * log( probability ) / log( 2.0 );
      }
    ++itr;
    }

  std::cout << "Joint Entropy      = " << JointEntropy << " bits " << std::endl;

  double MutualInformation = Entropy1 + Entropy2 - JointEntropy;
  
  std::cout << "Mutual Information = " << MutualInformation << " bits " << std::endl;


  // Different types of Normalization have been proposed for Mutual Information.
 
  double NormalizedMutualInformation1 = 2.0 * MutualInformation / ( Entropy1 + Entropy2 );

  std::cout << "Normalized Mutual Information 1 = " << NormalizedMutualInformation1 <<  std::endl;

  double NormalizedMutualInformation2 = ( Entropy1 + Entropy2 ) / JointEntropy;

  std::cout << "Normalized Mutual Information 2 = " << NormalizedMutualInformation2 <<  std::endl;


  return 0;
  
// Software Guide : EndCodeSnippet
}


