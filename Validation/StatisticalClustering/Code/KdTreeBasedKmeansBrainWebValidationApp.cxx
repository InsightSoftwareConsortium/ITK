/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    KdTreeBasedKmeansBrainWebValidationApp.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkWin32Header.h"

#include "OptionList.h"
#include "ValidationSampleGenerator.h"
#include "ParameterTable.h"

#include "KdTreeBasedKmeansValidationApp.h"

void print_usage()
{
  std::cout << "kmeans clustering 1.0 (17. Dec. 2001)" << std::endl ;

  std::cout << "usage: KdTreeBasedKmeansBrainWebValidationApp --images file"  << std::endl ;
  std::cout << "       --mask file --bucket-size int --iteration int"  << std::endl ;
  std::cout << "       --parameters file " << std::endl ;
  std::cout << "       --result file " << std::endl ;

  std::cout << "" << std::endl ;

  std::cout << "--images file" << std::endl ;
  std::cout << "        image file name with intesnity values [meta image format]"  
            << std::endl ;

  std::cout << "--mask file" << std::endl ;
  std::cout << "        class label image file name that will have the class labels for pixels" 
            << std::endl ;
  std::cout << "        in the target image file [meta image format]"  << std::endl ;

  std::cout << "--bucket-size int" << std::endl ;
  std::cout << "--iteration int" << std::endl ;
  std::cout << "--parameters file" << std::endl ;
  std::cout << "        data file has initial parameters for each class" << std::endl ;
  std::cout << "" << std::endl ;

  std::cout << "example: KdTreeBasedKmeansBrainWebValidationApp --images brainweb.T1.1mm.0.0.mhd" << std::endl ;
  std::cout << " brainweb.PD.1mm.0.0.mhd" << std::endl ;
  std::cout << "         --mask Inputs/20Normals_T1_seg/110_3.mhd" << std::endl ;
  std::cout << "         --bucket-size 10 --iteration 200" << std::endl ;
}

int main(int argc, char* argv[])
{
  namespace stat = itk::Statistics ;
 
  if (argc <= 1)
    {
      print_usage() ;
      exit(0) ;
    }

  KdTreeBasedKmeansValidationApp< unsigned char, 2 > app ;

  app.SetCommandLineOptions(argc, argv) ;

  std::vector< unsigned int > selectedClasses ;
  selectedClasses.push_back(1) ;
  selectedClasses.push_back(2) ;
  selectedClasses.push_back(3) ;

  app.SetSelectedClasses(selectedClasses) ;
  app.Run() ;

  return 0 ;
}

