/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DiffusionTensor3DReconstructionImageFilter.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif


/** Example for itkDiffusionTensor3DReconstructionImageFilter class */

#include "itkDiffusionTensor3DReconstructionImageFilter.h"
#include "itkTensorFractionalAnisotropyImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkImage.h"
#include "itkNrrdImageIO.h"
#include "itkImageSeriesReader.h"
#include "itkMetaDataDictionary.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include <iostream>

int main( int argc, char *argv[] )
{
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " NrrdFileName(.nhdr) threshold(on B0)"
     << " FAImageFileName [ExtractGradientAndReferenceImage from the NRRD file and "
     << "write them as images]" << std::endl;
    std::cerr << "\tExample args: xt_dwi.nhdr 80 FA.mhd 1" << std::endl;
    return EXIT_FAILURE;
    }
  
  const unsigned int dNrrd = 4;
  const unsigned int dVolume = 3;
  unsigned int nMeasurement;
  
  typedef itk::Image<unsigned short, dNrrd> ImageType;
  
  itk::ImageFileReader<ImageType>::Pointer reader 
    = itk::ImageFileReader<ImageType>::New();
  
  ImageType::Pointer img;
  
  // Set the properties for NrrdReader
  reader->SetFileName(argv[1]);

  // Read in the nrrd data. The file contains the reference image and the gradient
  // images and contains the gradient directions used for diffusion weighting.
  try
    {
    reader->Update();
    img = reader->GetOutput();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  //   parse diffusion vectors
  itk::MetaDataDictionary imgMetaDictionary = img->GetMetaDataDictionary();    
  std::vector<std::string> imgMetaKeys = imgMetaDictionary.GetKeys();
  std::vector<std::string>::iterator itKey = imgMetaKeys.begin();
  std::string metaString;
  
  int k = 0;
  vnl_vector_fixed<double, 3> vect3d;
  std::vector< vnl_vector_fixed<double, 3> > DiffusionVectors;

  for ( ; itKey != imgMetaKeys.end(); itKey ++)
    {
    int pos;
    double x,y,z;

    itk::ExposeMetaData<std::string> (imgMetaDictionary, *itKey, metaString);
    pos = itKey->find("DW_gradient");
    if (pos == -1)
      {
      continue;
      }
      
    std::cout << *itKey << " ---> " << metaString << std::endl;      
    sscanf(metaString.c_str(), "%lf %lf %lf\n", &x, &y, &z);
    vect3d[0] = x; vect3d[1] = y; vect3d[2] = z;
    
    DiffusionVectors.push_back(vect3d);
    k ++;
    }
  nMeasurement = k;
  
  

  // Extract the Reference and gradient images from the NRRD file
  // as seperate images.
  typedef itk::Image< ImageType::PixelType, dVolume > ReferenceImageType;
  typedef itk::Image< ImageType::PixelType, dVolume > GradientImageType;
  typedef itk::ExtractImageFilter< ImageType, ReferenceImageType >
        ExtractFilterType;
  ImageType::SizeType size;
  size = img->GetBufferedRegion().GetSize();
  size[3] = 0;  
  ImageType::IndexType index;
  index = img->GetBufferedRegion().GetIndex();
  
  std::vector< ExtractFilterType::Pointer > extractFilterPointerContainer;
  
  for( unsigned int i = 0; i<=nMeasurement; i++ )
    { 
    ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
    extractFilter->SetInput( img );
    ExtractFilterType::InputImageRegionType extractRegion;
  
    index[3] = i;  
    extractRegion.SetSize( size  );
    extractRegion.SetIndex( index );
    extractFilter->SetExtractionRegion( extractRegion );
    extractFilterPointerContainer.push_back( extractFilter );
    }
  
  
  // If we need to write out the reference and gradient images to a file..
  // Easier viewing them with a viewer than if they were in a single NRRD
  // file
  if( (argc > 4) && (atoi(argv[4])) )
    {
    typedef itk::ImageFileWriter< GradientImageType > GradientWriterType;
    for( unsigned int i = 0; i<=nMeasurement; i++ )
      {
      GradientWriterType::Pointer gradientWriter = GradientWriterType::New();
      gradientWriter->SetInput( extractFilterPointerContainer[i]->GetOutput() );
      char filename[100];
      if (i==0)
        {
        std::string fn("ReferenceImage.mhd");
        sprintf(filename, fn.c_str() );
        }
      else
        {
        std::string fn("Gradient%d.mhd");
        sprintf(filename, fn.c_str(), i );
        }
      gradientWriter->SetFileName( filename );
      gradientWriter->Update();
      }
    }
    
  
  // Here we instantiate the DiffusionTensor3DReconstructionImageFilter class.
  // The class is templated over the pixel types of the reference, gradient
  // and the to be created tensor pixel's precision. It takes as input the
  // Reference (B0 image aquired in the absence of diffusion sensitizing 
  // gradients), 'n' Gradient images and their directions and produces
  // as output an image of tensors with pixel-type DiffusionTensor3D.
  // 
  typedef itk::DiffusionTensor3DReconstructionImageFilter< 
      ImageType::PixelType, ImageType::PixelType, double > 
        TensorReconstructionImageFilterType;
  TensorReconstructionImageFilterType::Pointer tensorReconstructionFilter = 
    TensorReconstructionImageFilterType::New();
  
  tensorReconstructionFilter->SetReferenceImage( 
      extractFilterPointerContainer[0]->GetOutput() );

  
  for( unsigned int i = 1; i<=nMeasurement; i++ )
    {
    tensorReconstructionFilter->AddGradientImage( 
        DiffusionVectors[i-1], extractFilterPointerContainer[i]->GetOutput() );
    }

  // This is necessary until we fix netlib/dsvdc.c
  tensorReconstructionFilter->SetNumberOfThreads( 1 );
 
  tensorReconstructionFilter->SetThreshold( static_cast< 
      TensorReconstructionImageFilterType::ReferencePixelType >( 
                                                    atof(argv[2])));
  tensorReconstructionFilter->Update();

  // Use the TensorFractionalAnisotropyImageFilter to compute the FA image.
  typedef TensorReconstructionImageFilterType::TensorPixelType TensorPixelType;
  typedef TensorPixelType::RealValueType                       RealValueType;
  typedef itk::Image< RealValueType, dVolume >                 FAImageType;
  typedef itk::TensorFractionalAnisotropyImageFilter< 
      TensorReconstructionImageFilterType::OutputImageType, FAImageType >
                                                                  FAFilterType;

  FAFilterType::Pointer fractionalAnisotropyFilter = FAFilterType::New();
  fractionalAnisotropyFilter->SetInput( tensorReconstructionFilter->GetOutput() );
  
  // Write the FA image
  //
  typedef itk::ImageFileWriter< FAFilterType::OutputImageType >  FAWriterType;
  FAWriterType::Pointer faWriter = FAWriterType::New();
  faWriter->SetInput( fractionalAnisotropyFilter->GetOutput() );
  faWriter->SetFileName(argv[3]);
  faWriter->Update();

  return EXIT_SUCCESS;
}
