/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    BrainWebClassStatisticsApp.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "OptionList.h"
#include "ValidationSampleGenerator.h"

void print_usage()
{
  std::cout << "kmeans clustering 1.0 (17. Dec. 2001)" << std::endl ;

  std::cout << "usage: BrainWebClassStatisticsApp --images file..."  << std::endl ;
  std::cout << "       --mask file"  << std::endl ;

  std::cout << "" << std::endl ;

  std::cout << "--images file" << std::endl ;
  std::cout << "        image file name with intesnity values [meta image format]"  
            << std::endl ;

  std::cout << "--mask file" << std::endl ;
  std::cout << "        class label image file name that will have the class labels for pixels" 
            << std::endl ;
  std::cout << "        in the target image file [meta image format]"  << std::endl ;

  std::cout << "example: BrainWebClassStatisticsGeneratorApp --image brainweb.T1.1mm.0.0.mhd" 
            << std::endl ;
  std::cout << "         --mask phantom_discrete.mhd" << std::endl ;
}

int main(int argc, char* argv[])
{
  if (argc <= 1)
    {
      print_usage() ;
      exit(0) ;
    }

  OptionList options(argc, argv) ;

  std::vector< std::string > imageFileNames ;
  std::string maskFileName ;
  try
    {
      options.GetMultiStringOption("images", &imageFileNames, true) ;
      options.GetStringOption("mask", &maskFileName, true) ;
    }
  catch(OptionList::RequiredOptionMissing e)
    {
      std::cout << "Error: The '" << e.OptionTag 
                << "' option is required but missing." 
                << std::endl ;
      return 1 ;
    }
 
  std::vector< unsigned int > selectedClasses ;
  selectedClasses.push_back(1) ; // csf
  selectedClasses.push_back(2) ; // gray
  selectedClasses.push_back(3) ; // white

  typedef itk::Image< short, 3 > ImageType ;
  typedef itk::FixedArray< short, 2 > VectorPixelType ;
  typedef itk::Image< VectorPixelType, 3 > VectorImageType ;
  typedef ValidationSampleGenerator< ImageType, 
    ImageType, VectorImageType > SampleGeneratorType ;
  SampleGeneratorType sampleGenerator ;

  sampleGenerator.SetOutputSampleType(SampleGeneratorType::LIST_SAMPLE) ;
  sampleGenerator.SetImageFileNames(imageFileNames) ;
  sampleGenerator.SetClassMaskImageFileName(maskFileName.c_str(), 0) ;
  sampleGenerator.SetSelectedClasses(selectedClasses) ;
  sampleGenerator.SetOutputNormalized(true) ;

  sampleGenerator.GenerateData() ;
  std::cout << "sample created." << std::endl ;

  std::string::size_type caseNoBegin = imageFileNames[0].find_last_of('/') + 1 ;
  if ( caseNoBegin > imageFileNames[0].length() )
    {
      caseNoBegin = 0 ;
    }
  
  std::string::size_type caseNoLength = imageFileNames[0].rfind(".mhd") - caseNoBegin ;
  std::string caseNo = imageFileNames[0].substr(caseNoBegin, caseNoLength) ;
  std::cout << "\"class\" \"size\"" ;
  for ( unsigned int i = 0 ; i < imageFileNames.size() ; i++ )
    {
      std::cout << " \"mean." << i + 1 <<"\"" ;
    }
  
  for ( unsigned int i = 0 ; i < imageFileNames.size() * imageFileNames.size() ; i++ )
    {
      std::cout << " \"sigma." << i + 1 <<"\"" ;
    }
  std::cout << std::endl ;
  for ( int i = 0 ; i < selectedClasses.size() ; i++ )
    { 
      //      std::cout << caseNo ;
      std::cout << " " << selectedClasses[i] ;
      std::cout << " " << sampleGenerator.GetClassSize(selectedClasses[i]) ;
      for ( unsigned int j = 0 ; j < imageFileNames.size() ; j++ )
        {
          std::cout << " " << sampleGenerator.GetClassMean(selectedClasses[i])[j] ;
        }
      
      for ( unsigned int j = 0 ; j < imageFileNames.size() ; j++ )
        {
          for (unsigned int k = 0 ; k < imageFileNames.size() ; k++ )
            {
              std::cout << " " 
                        << sampleGenerator.GetClassCovariance(selectedClasses[i]).GetVnlMatrix()[j][k] ;
            }
        }
      std::cout << std::endl ;
    }

  return 0 ;
}
