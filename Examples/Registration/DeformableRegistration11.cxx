/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformableRegistration11.cxx
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


#include "itkImageFileReader.h" 
#include "itkImageFileWriter.h" 

#include "itkRescaleIntensityImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"

#include "itkFEM.h"
#include "itkFEMRegistrationFilter.h"
       


/* Example of FEM-base deformable registration in 3D */


const unsigned int Dimension = 3;
typedef itk::Image<unsigned char, Dimension>            FileImageType;
typedef itk::Image<float, Dimension>                    ImageType;


typedef itk::fem::Element3DC0LinearHexahedronMembrane   ElementType;
typedef itk::fem::Element3DC0LinearTetrahedronMembrane  ElementType2;


typedef itk::fem::FiniteDifferenceFunctionLoad<
                                     ImageType,ImageType> ImageLoadType;

template class itk::fem::ImageMetricLoadImplementation<ImageLoadType>;

typedef ElementType::LoadImplementationFunctionPointer     LoadImpFP;
typedef ElementType::LoadType                              ElementLoadType;

typedef ElementType2::LoadImplementationFunctionPointer    LoadImpFP2;
typedef ElementType2::LoadType                             ElementLoadType2;

typedef itk::fem::VisitorDispatcher<ElementType,ElementLoadType, LoadImpFP>   
                                                           DispatcherType;

typedef itk::fem::VisitorDispatcher<ElementType2,ElementLoadType2, LoadImpFP2>   
                                                           DispatcherType2;




typedef itk::fem::FEMRegistrationFilter<ImageType,ImageType> RegistrationType;


int main(int argc, char *argv[])
{
  char *paramname;
  if ( argc < 2 )
    {
    std::cout << "Parameter file name missing" << std::endl;
    std::cout << "Usage: " << argv[0] << " param.file" << std::endl;
    return -1;
    } 
  else 
    { 
    paramname=argv[1]; 
    }


  
  // Register the correct load implementation with the element-typed visitor
  // dispatcher. 
  typedef itk::fem::ImageMetricLoadImplementation<
                                       ImageLoadType> LoadImplementationType;
  {
  ElementType::LoadImplementationFunctionPointer fp = 
    &LoadImplementationType::ImplementImageMetricLoad;
  DispatcherType::RegisterVisitor((ImageLoadType*)0,fp);
  }
  {
  ElementType2::LoadImplementationFunctionPointer fp =
    &LoadImplementationType::ImplementImageMetricLoad;
  DispatcherType2::RegisterVisitor((ImageLoadType*)0,fp);
  }



  RegistrationType::Pointer registrationFilter = RegistrationType::New(); 




  // Attempt to read the parameter file, and exit if an error occurs
  registrationFilter->SetConfigFileName(paramname);
  if ( !registrationFilter->ReadConfigFile( 
           (registrationFilter->GetConfigFileName()).c_str() ) ) 
    { 
    return -1; 
    }
 
  // Read the image files
  typedef itk::ImageFileReader< FileImageType >      FileSourceType;
  typedef FileImageType::PixelType PixType;

  FileSourceType::Pointer movingfilter = FileSourceType::New();
  movingfilter->SetFileName( (registrationFilter->GetMovingFile()).c_str() );
  FileSourceType::Pointer fixedfilter = FileSourceType::New();
  fixedfilter->SetFileName( (registrationFilter->GetFixedFile()).c_str() );
 
  std::cout << " reading moving ";
  std::cout << registrationFilter->GetMovingFile() << std::endl;
  std::cout << " reading fixed ";
  std::cout << registrationFilter->GetFixedFile() << std::endl;
  

  try
    {
    movingfilter->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception caught during reference file reading ";
    std::cerr << std::endl << e << std::endl;
    return -1;
    }
  try
    {
    fixedfilter->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception caught during target file reading ";
    std::cerr << std::endl << e << std::endl;
    return -1;
    }
  

  // Rescale the image intensities so that they fall between 0 and 255
  typedef itk::RescaleIntensityImageFilter<
                        FileImageType, ImageType > FilterType;
  
  FilterType::Pointer movingrescalefilter = FilterType::New();
  FilterType::Pointer fixedrescalefilter = FilterType::New();

  movingrescalefilter->SetInput(movingfilter->GetOutput());
  fixedrescalefilter->SetInput(fixedfilter->GetOutput());

  const double desiredMinimum =  0.0;
  const double desiredMaximum =  255.0;

  movingrescalefilter->SetOutputMinimum( desiredMinimum );
  movingrescalefilter->SetOutputMaximum( desiredMaximum );
  movingrescalefilter->UpdateLargestPossibleRegion();
  fixedrescalefilter->SetOutputMinimum( desiredMinimum );
  fixedrescalefilter->SetOutputMaximum( desiredMaximum );
  fixedrescalefilter->UpdateLargestPossibleRegion();
  

  // Histogram match the images
  typedef itk::HistogramMatchingImageFilter<ImageType,ImageType> HEFilterType;
  HEFilterType::Pointer IntensityEqualizeFilter = HEFilterType::New();

  IntensityEqualizeFilter->SetReferenceImage( fixedrescalefilter->GetOutput() );
  IntensityEqualizeFilter->SetInput( movingrescalefilter->GetOutput() );
  IntensityEqualizeFilter->SetNumberOfHistogramLevels( 100);
  IntensityEqualizeFilter->SetNumberOfMatchPoints( 15);
  IntensityEqualizeFilter->ThresholdAtMeanIntensityOn();
  IntensityEqualizeFilter->Update();

  registrationFilter->SetFixedImage(fixedrescalefilter->GetOutput());
  registrationFilter->SetMovingImage(IntensityEqualizeFilter->GetOutput());


  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();

  writer->SetFileName("fixed.mhd");
  writer->SetInput(registrationFilter->GetFixedImage() ); 
  writer->Write();

  itk::ImageFileWriter<ImageType>::Pointer writer2;
  writer2 =  itk::ImageFileWriter<ImageType>::New();
  writer2->SetFileName("moving.mhd");
  writer2->SetInput(registrationFilter->GetMovingImage() ); 
  writer2->Write();
 

  
  // Create the material properties
  itk::fem::MaterialLinearElasticity::Pointer m;
  m = itk::fem::MaterialLinearElasticity::New();
  m->GN = 0;                  // Global number of the material
  m->E = registrationFilter->GetElasticity();  // Young's modulus 
  m->A = 1.0;                 // Cross-sectional area
  m->h = 1.0;                 // Thickness
  m->I = 1.0;                 // Moment of inertia
  m->nu = 0.;                 // Poisson's ratio -- DONT CHOOSE 1.0!!
  m->RhoC = 1.0;              // Density
  
  // Create the element type 
  ElementType::Pointer e1=ElementType::New();
  e1->m_mat=dynamic_cast<itk::fem::MaterialLinearElasticity*>( m );
  registrationFilter->SetElement(e1);
  registrationFilter->SetMaterial(m);


  registrationFilter->RunRegistration();

  registrationFilter->WarpImage( registrationFilter->GetMovingImage() );

  // Warp the moving image and write it to a file.
  writer->SetFileName("warpedMovingImage.mhd");
  writer->SetInput( registrationFilter->GetWarpedImage() );
  writer->Update();

  registrationFilter->WriteDisplacementFieldMultiComponent();

  //  This is a documented sample parameter file that can be used with
  //  this deformable registration example.
  //
  //  ../Data/FiniteElementRegistrationParameters3.txt
  //

  return 0;
}


