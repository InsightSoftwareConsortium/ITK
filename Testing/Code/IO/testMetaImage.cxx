/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    testMetaImage.cxx
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

#include <stdio.h>
#include <ctype.h>
#include <cstdlib>
#include <metaImage.h>
#include <itksys/SystemTools.hxx>

int testMetaImage(int , char * [])
  {

  MetaImage tIm(8, 8, 1, 2, MET_CHAR);
  MetaImage tImCopy(&tIm);

  int i;
  for(i=0; i<64; i++)
    tIm.ElementData(i, i);

  for(i=0; i<64; i++)
    {
    if(i != tIm.ElementData(i))
      {
      std::cout << "Assigned Element Values Maintained: FAIL" << std::endl;
      return EXIT_FAILURE;
      }
    }

  tIm.Write("test.mha");
  tIm.PrintInfo();

  MetaImage tIm2("test.mha");

  int im2Zero = 0;
  std::cout << "Header size = " << tIm2.HeaderSize() << std::endl;
  tIm2.HeaderSize(tIm2.HeaderSize());
  tIm2.Modality(MET_MOD_CT);
  std::cout << "NDims = " << tIm2.NDims() << std::endl;
  std::cout << "Modality = " << tIm2.Modality() << std::endl;
  std::cout << "DimSize = " << tIm2.DimSize() << std::endl;
  std::cout << "Quantity = " << tIm2.Quantity() << std::endl;
  std::cout << "SubQuantity = " << tIm2.SubQuantity() << std::endl;
  std::cout << "SubQuantity(0) = " << tIm2.SubQuantity(im2Zero) << std::endl;
  std::cout << "SequenceID = " << tIm2.SequenceID() << std::endl;
  std::cout << "SequenceID[0] = " << tIm2.SequenceID(im2Zero) << std::endl;
  float* sequID = new float[2];
  sequID[0]=1;
  sequID[1]=1;
  tIm2.SequenceID(sequID);
  delete [] sequID;
  tIm2.SequenceID(0,1.0f);

  std::cout << "ElementSizeValid = " << tIm2.ElementSizeValid() << std::endl;
  tIm2.ElementSizeValid(tIm2.ElementSizeValid());
  std::cout << "ElementSize = " << tIm2.ElementSize() << std::endl;
  std::cout << "ElementSize(0) = " << tIm2.ElementSize(im2Zero) << std::endl;

  tIm2.ElementSize(0,1.0f);
  float* elmtSize = new float[2];
  elmtSize[0]=1;
  elmtSize[1]=2;
  tIm2.ElementSize(elmtSize);
  delete [] elmtSize;

  std::cout << "ElementType = " << tIm2.ElementType() << std::endl;
  std::cout << "ElementNumberOfChannels = " << tIm2.ElementNumberOfChannels() << std::endl;
  tIm2.ElementNumberOfChannels(tIm2.ElementNumberOfChannels());

  std::cout << "ElementMinMaxValid = " << tIm2.ElementMinMaxValid() << std::endl;
  tIm2.ElementMinMaxValid(tIm2.ElementMinMaxValid());
  
  tIm2.ElementMinMaxRecalc();
 
  std::cout << "ElementMin = " << tIm2.ElementMin() << std::endl;
  std::cout << "ElementMax = " << tIm2.ElementMax() << std::endl;

  tIm2.ElementMin(tIm2.ElementMin());
  tIm2.ElementMax(tIm2.ElementMax());

  std::cout << "AutoFreeElementData = " << tIm2.AutoFreeElementData() << std::endl;
  std::cout << "ElementDataFileName = " << tIm2.ElementDataFileName() << std::endl;

  std::cout << "Element Data: " << tIm2.ElementData() << std::endl;

  std::cout << "Testing ConvertElementDataTo: ";
  if(tIm2.ConvertElementDataTo(MET_CHAR,0,255))
    {
    std::cout << "[PASSED]" << std::endl;
    }
  else
    { 
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  tIm2.PrintInfo();
  for(i=0; i<64; i++)
    {
    if(i != tIm.ElementData(i))
      {
      std::cout << "Read Element Values: FAIL" << std::endl;
      return EXIT_FAILURE;
      }
    }


  tIm2.AutoFreeElementData(tIm2.AutoFreeElementData());

  // Testing copy
  std::cout << "Testing copy:";
  MetaImage imCopy(&tIm2);
  std::cout << " [PASSED]" << std::endl;


  // testing metaImageUtils
  char* modality = new char[255];
  if(!MET_ImageModalityToString(MET_MOD_CT,modality))
    {
      std::cout << "MET_ImageModalityToString: FAIL" << std::endl;
      return EXIT_FAILURE;
    }
  else
    {
    std::cout << "Modality  = " << modality << std::endl;
    }

  delete [] modality;

  //Testing Append function
  std::cout << "Testing Append:";
  
  if(tIm2.Append("test.mha"))
    {
    std::cout << " [PASSED]" << std::endl;
    }
  else
    {
    std::cout << " [FAILED]" << std::endl;
    }

  itksys::SystemTools::RemoveFile("test.mha");

  std::cout << "[DONE]" << std::endl;

  return EXIT_SUCCESS;
  }
