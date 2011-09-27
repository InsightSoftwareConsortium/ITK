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
//
// This example shows how to use the DiffusionTensor3DReconstructionImageFilter
// to reconstruct an image of tensors from Diffusion weighted images. See the
// documentation of DiffusionTensor3DReconstructionImageFilter,
// TensorRelativeAnisotropyImageFilter, TensorFractionalAnisotropyImageFilter
// first.
//
// The example takes diffusion weighted images in the Nrrd format, writes out
// the gradient and reference images for the users reference and computes and
// writes out the Fractional Anisotropy and Relative Anisotropy images.
//
// The easiest way to get started is to try out the filter on a sample dataset.
//
// Acquiring sample datasets:
//  1. Get the DWI datasets from
//        ftp://public.kitware.com/pub/namic/DTI/Data/dwi.nhdr
//        ftp://public.kitware.com/pub/namic/DTI/Data/dwi.img.gz (gunzip this)
//     These datasets contain a reference T1 image and 30 diffusion weighted
//     images. See the nrrd header for details such as B value etc..
//
//  2. Run the example with the following args
//       dwi.nhdr 80 Tensors.mhd FractionalAnisotropy.mhd RelativeAnisotropy.mhd 1
//
//  3. You should find 30 gradient images, 1 reference image, the FA and RA images
//     in your working directory, which you can fire up in your favourite volume
//     browser.
//
// This work is part of the National Alliance for Medical Image
// Computing (NAMIC), funded by the National Institutes of Health
// through the NIH Roadmap for Medical Research, Grant U54 EB005149.
//
// Additional documentation: For details on the Nrrd format for DTI, see
// http://wiki.na-mic.org/Wiki/index.php/NAMIC_Wiki:DTI:Nrrd_format
//


#include "itkDiffusionTensor3DReconstructionImageFilter.h"
#include "itkTensorFractionalAnisotropyImageFilter.h"
#include "itkTensorRelativeAnisotropyImageFilter.h"
#include "itkNrrdImageIO.h"
#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include <iostream>

int main( int argc, char *argv[] )
{
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " NrrdFileName(.nhdr) threshold(on B0)"
     << " FAImageFileName RelativeAnisotropyFileName " <<
     "[ExtractGradientAndReferenceImage from the NRRD file and "
     << "write them as images]" << std::endl;
    std::cerr << "\tExample args: xt_dwi.nhdr 80 FA.mhd 1" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 3;
  unsigned int numberOfImages = 0;
  unsigned int numberOfGradientImages = 0;
  bool readb0 = false;
  double b0 = 0;

  typedef unsigned short                      PixelType;
  typedef itk::VectorImage<unsigned short, 3> ImageType;

  itk::ImageFileReader<ImageType>::Pointer reader
    = itk::ImageFileReader<ImageType>::New();

  ImageType::Pointer img;

  // Set the properties for NrrdReader
  reader->SetFileName(argv[1]);

  // Read in the nrrd data. The file contains the reference image and the gradient
  // images.
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

  // Here we instantiate the DiffusionTensor3DReconstructionImageFilter class.
  // The class is templated over the pixel types of the reference, gradient
  // and the to be created tensor pixel's precision. (We use double here). It
  // takes as input the Reference (B0 image aquired in the absence of diffusion
  // sensitizing gradients), 'n' Gradient images and their directions and produces
  // as output an image of tensors with pixel-type DiffusionTensor3D.
  //
  typedef itk::DiffusionTensor3DReconstructionImageFilter<
    PixelType, PixelType, double > TensorReconstructionImageFilterType;


  // -------------------------------------------------------------------------
  // Parse the Nrrd headers to get the B value and the gradient directions used
  // for diffusion weighting.
  //
  // The Nrrd headers should look like :
  // The tags specify the B value and the gradient directions. If gradient
  // directions are (0,0,0), it indicates that it is a reference image.
  //
  // DWMRI_b-value:=800
  // DWMRI_gradient_0000:= 0 0 0
  // DWMRI_gradient_0001:=-1.000000       0.000000        0.000000
  // DWMRI_gradient_0002:=-0.166000       0.986000        0.000000
  // DWMRI_gradient_0003:=0.110000        0.664000        0.740000
  // ...
  //
  itk::MetaDataDictionary imgMetaDictionary = img->GetMetaDataDictionary();
  std::vector<std::string> imgMetaKeys = imgMetaDictionary.GetKeys();
  std::vector<std::string>::const_iterator itKey = imgMetaKeys.begin();
  std::string metaString;

  TensorReconstructionImageFilterType::GradientDirectionType vect3d;
  TensorReconstructionImageFilterType::GradientDirectionContainerType::Pointer
    DiffusionVectors =
    TensorReconstructionImageFilterType::GradientDirectionContainerType::New();


  for (; itKey != imgMetaKeys.end(); ++itKey)
    {
    double x,y,z;

    itk::ExposeMetaData<std::string> (imgMetaDictionary, *itKey, metaString);
    if (itKey->find("DWMRI_gradient") != std::string::npos)
      {
      std::cout << *itKey << " ---> " << metaString << std::endl;
      sscanf(metaString.c_str(), "%lf %lf %lf\n", &x, &y, &z);
      vect3d[0] = x; vect3d[1] = y; vect3d[2] = z;
      DiffusionVectors->InsertElement( numberOfImages, vect3d );
      ++numberOfImages;
      // If the direction is 0.0, this is a reference image
      if (vect3d[0] == 0.0 &&
          vect3d[1] == 0.0 &&
          vect3d[2] == 0.0)
        {
        continue;
        }
      ++numberOfGradientImages;
      }
    else if (itKey->find("DWMRI_b-value") != std::string::npos)
      {
      std::cout << *itKey << " ---> " << metaString << std::endl;
      readb0 = true;
      b0 = atof(metaString.c_str());
      }
    }
  std::cout << "Number of gradient images: "
            << numberOfGradientImages
            << " and Number of reference images: "
            << numberOfImages - numberOfGradientImages
            << std::endl;
  if(!readb0)
    {
    std::cerr << "BValue not specified in header file" << std::endl;
    return EXIT_FAILURE;
    }


  // -------------------------------------------------------------------------
  // Extract the Reference and gradient images from the NRRD file
  // as seperate images.
  //
  // This is not really necessary, the filter is capable of gobbling the entire
  // VectorImage (which contains the reference and the gradient image) and chew
  // on it to generate the TensorImage. This is a more intuitive formalism
  // since this is what is usually obtained from the Nrrd DWI format.
  //
  // Nevertheless, we go through the "unnecessary pain" of extracting the
  // gradient and reference images in separate images and writing them out to
  // files, so they can be fired up in you favourite volume viewer.
  //
  typedef itk::Image< PixelType, Dimension > ReferenceImageType;
  typedef ReferenceImageType                 GradientImageType;

  // A container of smart pointers to images. This container will hold
  // the 'numberOfGradientImages' gradient images and the reference image.
  //
  std::vector< GradientImageType::Pointer > imageContainer;

  // iterator to iterate over the DWI Vector image just read in.
  typedef itk::ImageRegionConstIterator< ImageType >         DWIIteratorType;
  DWIIteratorType dwiit( img, img->GetBufferedRegion() );
  typedef itk::ImageRegionIterator< GradientImageType > IteratorType;

  // In this for loop, we will extract the 'n' gradient images + 1 reference
  // image from the DWI Vector image.
  //
  for( unsigned int i = 0; i<numberOfImages; i++ )
    {
    GradientImageType::Pointer image = GradientImageType::New();
    image->CopyInformation( img );
    image->SetBufferedRegion( img->GetBufferedRegion() );
    image->SetRequestedRegion( img->GetRequestedRegion() );
    image->Allocate();

    IteratorType it( image, image->GetBufferedRegion() );
    dwiit.GoToBegin();
    it.GoToBegin();

    while (!it.IsAtEnd())
      {
      it.Set(dwiit.Get()[i]);
      ++it;
      ++dwiit;
      }
    imageContainer.push_back( image );
    }

  // If we need to write out the reference and gradient images to a file..
  // Easier viewing them with a viewer than if they were in a single NRRD
  // file
  if( (argc > 4) && (atoi(argv[6])) )
    {
    unsigned int referenceImageIndex = 0;
    typedef itk::ImageFileWriter< GradientImageType > GradientWriterType;
    for( unsigned int i = 0; i<numberOfImages; i++ )
      {
      GradientWriterType::Pointer gradientWriter = GradientWriterType::New();
      gradientWriter->SetInput( imageContainer[i] );
      char filename[100];
      if (DiffusionVectors->ElementAt(i).two_norm() <= 0.0) // this is a reference image
        {
        std::string fn("ReferenceImage%d.mhd");
        sprintf(filename, fn.c_str(), referenceImageIndex );
        ++referenceImageIndex;
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


  // -------------------------------------------------------------------------
  TensorReconstructionImageFilterType::Pointer tensorReconstructionFilter =
    TensorReconstructionImageFilterType::New();

  // The reference and the gradient images are conveniently provided as
  // input to the DiffusionTensor3DReconstructionImageFilter as
  //   filter->SetGradientImage( directionsContainer, nrrdreader->GetOutput() );
  //
  // The output of the nrrdreader is a VectorImage (akin to a multicomponent
  // 3D image). The nth component image is treated as a reference image if its
  // corresponding gradient direction is (0,0,0). Any number of reference images
  // may be specified.
  //
  // An alternate way to provide the inputs, when you have the reference and
  // gradient images in seperate itk::Image< type, 3 > is  :
  //
  //   tensorReconstructionFilter->SetReferenceImage( image0 );
  //   tensorReconstructionFilter->AddGradientImage( direction1, image1 );
  //   tensorReconstructionFilter->AddGradientImage( direction2, image2 );
  //
  tensorReconstructionFilter->SetGradientImage( DiffusionVectors, reader->GetOutput() );

  // This is necessary until we fix netlib/dsvdc.c
  tensorReconstructionFilter->SetNumberOfThreads( 1 );

  tensorReconstructionFilter->SetBValue(b0);
  tensorReconstructionFilter->SetThreshold( static_cast<
      TensorReconstructionImageFilterType::ReferencePixelType >(
                                                    atof(argv[2])));
  tensorReconstructionFilter->Update();


  // -------------------------------------------------------------------------
  // Write out the image of tensors. This code snippet goes to show that you
  // can use itk::ImageFileWriter to write an image of tensors.
  //
  typedef itk::ImageFileWriter<
    TensorReconstructionImageFilterType::OutputImageType > TensorWriterType;
  TensorWriterType::Pointer tensorWriter = TensorWriterType::New();
  tensorWriter->SetFileName( argv[3] );
  tensorWriter->SetInput( tensorReconstructionFilter->GetOutput() );
  tensorWriter->Update();


  // -------------------------------------------------------------------------
  // Now that we have the image of tensors, we may use one of the many tensor
  // filters in ITK. Below, we use the TensorFractionalAnisotropyImageFilter to
  // compute the FA.
  //
  typedef TensorReconstructionImageFilterType::TensorPixelType TensorPixelType;
  typedef TensorPixelType::RealValueType                       RealValueType;
  typedef itk::Image< RealValueType, Dimension >               FAImageType;
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
  faWriter->SetFileName(argv[4]);
  faWriter->Update();

  // Compute and write the Relative Anisotropy
  //
  typedef TensorReconstructionImageFilterType::TensorPixelType TensorPixelType;
  typedef TensorPixelType::RealValueType                       RealValueType;
  typedef itk::Image< RealValueType, Dimension >               RAImageType;
  typedef itk::TensorRelativeAnisotropyImageFilter<
      TensorReconstructionImageFilterType::OutputImageType, RAImageType >
                                                               RAFilterType;

  RAFilterType::Pointer relativeAnisotropyFilter = RAFilterType::New();
  relativeAnisotropyFilter->SetInput( tensorReconstructionFilter->GetOutput() );

  typedef itk::ImageFileWriter< RAFilterType::OutputImageType >  RAWriterType;
  RAWriterType::Pointer raWriter = RAWriterType::New();
  raWriter->SetInput( relativeAnisotropyFilter->GetOutput() );
  raWriter->SetFileName(argv[5]);
  raWriter->Update();

  return EXIT_SUCCESS;
}
