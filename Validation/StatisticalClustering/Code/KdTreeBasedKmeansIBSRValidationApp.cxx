/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    KdTreeBasedKmeansIBSRValidationApp.cxx
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
#include "ParameterTable.h"

#include "KdTreeBasedKmeansValidationApp.h"

void print_usage()
{
  std::cout << "kmeans clustering 1.0 (17. Dec. 2001)" << std::endl ;

  std::cout << "usage: KdTreeBasedKmeansIBSRValidationApp --images file"  << std::endl ;
  std::cout << "       --mask file --bucket-size int --iteration int"  << std::endl ;
  std::cout << "       --slice-offset int " << std::endl ;
  std::cout << "       --parameters file " << std::endl ;
  std::cout << "       --result file " << std::endl ;

  std::cout << "" << std::endl ;

  std::cout << "--image file" << std::endl ;
  std::cout << "        image file name with intesnity values [meta image format]"  
            << std::endl ;

  std::cout << "--mask file" << std::endl ;
  std::cout << "        class label image file name that will have the class labels for pixels" 
            << std::endl ;
  std::cout << "        in the target image file [meta image format]"  << std::endl ;

  std::cout << "--slice-offset int" << std::endl ;
  std::cout << "--bucket-size int" << std::endl ;
  std::cout << "--iteration int" << std::endl ;
  std::cout << "--parameters file" << std::endl ;
  std::cout << "        data file has initial parameters for each class" << std::endl ;
  std::cout << "" << std::endl ;

  std::cout << "example: KdTreeBasedKmeansIBSRValidationApp --images 20Normals_T1_brain/110_3.mhd" 
            << std::endl ;
  std::cout << "         --mask 20Normals_T1_seg/110_3.mhd" << std::endl ;
  std::cout << "         --bucket-size 10 --slice-offset 0 --iteration 200" << std::endl ;
}

int main(int argc, char* argv[])
{
  namespace stat = itk::Statistics ;
 
  if (argc <= 1)
    {
      print_usage() ;
      exit(0) ;
    }

  KdTreeBasedKmeansValidationApp< unsigned char, 1 > app ;

  app.SetCommandLineOptions(argc, argv) ;

  std::vector< unsigned int > selectedClasses ;
  selectedClasses.push_back(128) ;
  selectedClasses.push_back(192) ;
  selectedClasses.push_back(254) ;

  app.SetSelectedClasses(selectedClasses) ;
  app.Run() ;

  return 0 ;
}

