/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformableRegistration1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkImageFileReader.h" 
#include "itkImageFileWriter.h" 
#include "itkRawImageIO.h"

#include "itkRescaleIntensityImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"



//  Software Guide : BeginLatex
//
// The finite element (FEM) library within the Insight Toolkit can be
// used to solve image registration problems.  The first step in
// implementing a FEM-based registration is to include the following
// header files:
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkFEM.h"
#include "itkFEMRegistrationFilter.h"
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  Next, we use typedefs to instantiate all necessary classes.  We
//  define the image and element types we plan to use to solve a
//  two-dimensional registration problem.  We define multiple element
//  types so that they can be used in meshes without recompiling this
//  code.
//
//  Software Guide : EndLatex 


//  Software Guide : BeginCodeSnippet
typedef itk::Image< unsigned char, 2 >                     fileImageType;

typedef itk::Image< float, 2 >                             ImageType;

typedef itk::fem::Element2DC0LinearQuadrilateralMembrane   ElementType;

typedef itk::fem::Element2DC0LinearTriangularMembrane      ElementType2;
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Note that in order to solve a three-dimensional registration
//  problem, we would simply define 3D image and element types in lieu
//  of those above.  These declarations could be used for a 3D
//  problem:
//
//  SoftwareGuide : EndLatex


//  SoftwareGuide : BeginCodeSnippet
typedef itk::Image< unsigned char, 3 >                  fileImage3DType;

typedef itk::Image< float, 3 >                          Image3DType;

typedef itk::fem::Element3DC0LinearHexahedronMembrane   Element3DType;

typedef itk::fem::Element3DC0LinearTetrahedronMembrane  Element3DType2;
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//  
//  Here, we instantiate the load types and explicitly template the
//  load implementation type.  We also define visitors that allow the
//  elements and loads to communicate with one another.  
//
//  Software Guide : EndLatex


//  Software Guide : BeginCodeSnippet
typedef itk::fem::ImageMetricLoad<ImageType,ImageType>     ImageLoadType;

template class itk::fem::ImageMetricLoadImplementation<ImageLoadType>;

typedef ElementType::LoadImplementationFunctionPointer     LoadImpFP;
typedef ElementType::LoadType                              ElementLoadType;

typedef ElementType2::LoadImplementationFunctionPointer    LoadImpFP2;
typedef ElementType2::LoadType                             ElementLoadType2;

typedef itk::fem::VisitorDispatcher<ElementType,ElementLoadType, LoadImpFP>   
                                                           DispatcherType;

typedef itk::fem::VisitorDispatcher<ElementType2,ElementLoadType2, LoadImpFP2>   
                                                           DispatcherType2;
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Once all the other components have been instantianted, we can
//  instantiate the FEM registration filter as follows:
//
//  Software Guide : EndLatex


//  Software Guide : BeginCodeSnippet
typedef itk::fem::FEMRegistrationFilter<ImageType,ImageType> RegistrationType;
//  Software Guide : EndCodeSnippet

int main(int argc, char *argv[])
{
  char *paramname;
  if ( argc < 2 )
  {
    std::cout << "Parameter file name missing" << std::endl;
    std::cout << "Usage: " << argv[0] << " param.file" << std::endl;
    return -1;
  } 
  else { paramname=argv[1]; }
  
  // Register the correct load implementation with the element typed visitor dispatcher. 
  {
    ElementType::LoadImplementationFunctionPointer fp = 
      &itk::fem::ImageMetricLoadImplementation<ImageLoadType>::ImplementImageMetricLoad;
    DispatcherType::RegisterVisitor((ImageLoadType*)0,fp);
  }
  {
    ElementType2::LoadImplementationFunctionPointer fp =
      &itk::fem::ImageMetricLoadImplementation<ImageLoadType>::ImplementImageMetricLoad;
    DispatcherType2::RegisterVisitor((ImageLoadType*)0,fp);
  }

  // Declare the FEM registration class
  RegistrationType::Pointer X= RegistrationType::New(); 

  // Attempt to read the parameter file, and exit if an error occurs
  X->SetConfigFileName(paramname);
  if ( !X->ReadConfigFile( (X->GetConfigFileName()).c_str() ) ) { return -1; }
 
  // Read the image files
  typedef itk::ImageFileReader< fileImageType >      FileSourceType;
  typedef fileImageType::PixelType PixType;
  const unsigned int ImageDimension=fileImageType::ImageDimension;
  typedef itk::RawImageIO< PixType,ImageDimension>   RawReaderType;

  FileSourceType::Pointer reffilter = FileSourceType::New();
  reffilter->SetFileName( (X->GetReferenceFile()).c_str() );
  FileSourceType::Pointer tarfilter = FileSourceType::New();
  tarfilter->SetFileName( (X->GetTargetFile()).c_str() );

  RawReaderType::Pointer rawReader  = RawReaderType::New();
  rawReader->SetFileDimensionality( ImageDimension );

  ImageType::SizeType ImageSize=X->GetImageSize();
  for (unsigned int ii=0; ii<ImageDimension; ii++)     
  {
    unsigned int temp = (unsigned int) ImageSize[ii];
    rawReader->SetDimensions( ii, temp );
  }
  reffilter->SetImageIO( rawReader );
  tarfilter->SetImageIO( rawReader );

  try
  {
    reffilter->Update();
  }
  catch( itk::ExceptionObject & e )
  {
    std::cerr << "Exception caught during reference file reading " << std::endl;
    std::cerr << e << std::endl;
    return -1;
  }
  try
  {
    tarfilter->Update();
  }
  catch( itk::ExceptionObject & e )
  {
    std::cerr << "Exception caught during target file reading " << std::endl;
    std::cerr << e << std::endl;
    return -1;
  }
  
  // Rescale the image intensities so that they fall between 0 and 255
  typedef itk::RescaleIntensityImageFilter<fileImageType,ImageType> FilterType;
  FilterType::Pointer refrescalefilter = FilterType::New();
  FilterType::Pointer tarrescalefilter = FilterType::New();

  refrescalefilter->SetInput(reffilter->GetOutput());
  tarrescalefilter->SetInput(tarfilter->GetOutput());

  const double desiredMinimum =  0.0;
  const double desiredMaximum =  255.0;

  refrescalefilter->SetOutputMinimum( desiredMinimum );
  refrescalefilter->SetOutputMaximum( desiredMaximum );
  refrescalefilter->UpdateLargestPossibleRegion();
  tarrescalefilter->SetOutputMinimum( desiredMinimum );
  tarrescalefilter->SetOutputMaximum( desiredMaximum );
  tarrescalefilter->UpdateLargestPossibleRegion();
  

  // Histogram match the images
  typedef itk::HistogramMatchingImageFilter<ImageType,ImageType> HEFilterType;
  HEFilterType::Pointer IntensityEqualizeFilter = HEFilterType::New();

  IntensityEqualizeFilter->SetReferenceImage( refrescalefilter->GetOutput() );
  IntensityEqualizeFilter->SetInput( tarrescalefilter->GetOutput() );
  IntensityEqualizeFilter->SetNumberOfHistogramLevels( 100);
  IntensityEqualizeFilter->SetNumberOfMatchPoints( 15);
  IntensityEqualizeFilter->ThresholdAtMeanIntensityOn();
  IntensityEqualizeFilter->Update();

  X->SetReferenceImage(refrescalefilter->GetOutput());
  X->SetTargetImage(IntensityEqualizeFilter->GetOutput());


  
  // Choose the material properties
  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->GN = 0;                  // Global number of the material ///
  m->E = X->GetElasticity();  // Young's modulus -- used in the membrane ///
  m->A = 1.0;                 // Cross-sectional area ///
  m->h = 1.0;                 // Thickness ///
  m->I = 1.0;                 // Moment of inertia ///
  m->nu = 0.;                 // Poisson's ratio -- DONT CHOOSE 1.0!!///
  m->RhoC = 1.0;              // Density
  
  // Create the element type 
  ElementType::Pointer e1=ElementType::New();
  e1->m_mat=dynamic_cast<itk::fem::MaterialLinearElasticity*>( m );
  X->SetElement(e1);
  X->SetMaterial(m);

  // Register the images
  X->RunRegistration();

  // Output the image resulting from the registration 
  X->WriteWarpedImage((X->GetResultsFileName()).c_str());

  // Output the displacement fields associated with the result
  if (X->GetWriteDisplacements()) {
    X->WriteDisplacementField(0);
    X->WriteDisplacementField(1);

    // If this were a 3D example, you might also want to call this line:
    
    // X->WriteDisplacementField(2);
  }

  // Clean up and exit
  delete m;
  delete e1;
  delete X;

  return 0;
}


