/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    testMetaArray.cxx
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
#include <itkArray.h>
#include <itkVariableLengthVector.h>
#include <itkMetaArrayWriter.h>
#include <itkMetaArrayReader.h>

int testMetaArray(int , char * [])
  {
  std::cout << "Array" << std::endl;
  itk::Array<short> arr;
  arr.SetSize(5);
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 2;
  arr[4] = 1;

  // Write them
  itk::MetaArrayWriter::Pointer arrayWriter = itk::MetaArrayWriter::New();
  arrayWriter->SetFileName("test.mva");
  arrayWriter->SetInput<short>(MET_SHORT, &arr);
  arrayWriter->Update();

  std::cout << "Fixed array" << std::endl;
  itk::FixedArray<short, 5> farr;
  farr[0] = 1;
  farr[1] = 2;
  farr[2] = 3;
  farr[3] = 2;
  farr[4] = 1;
  arrayWriter->SetFileName("test_far.mva");
  arrayWriter->SetInput<short, 5>(MET_SHORT, &farr);
  arrayWriter->Update();

  std::cout << "Vector" << std::endl;
  itk::Vector<float, 5> vec;
  vec[0] = 1;
  vec[1] = 2;
  vec[2] = 3;
  vec[3] = 2;
  vec[4] = 1;
  arrayWriter->SetFileName("test_vec.mva");
  arrayWriter->SetInput<float, 5>(MET_FLOAT, &vec);
  arrayWriter->Update();

  std::cout << "CovariantVector" << std::endl;
  itk::CovariantVector<float, 5> cvec;
  cvec[0] = 1;
  cvec[1] = 2;
  cvec[2] = 3;
  cvec[3] = 2;
  cvec[4] = 1;
  arrayWriter->SetFileName("test_cvec.mvh");
  arrayWriter->SetInput<float, 5>(MET_FLOAT, &cvec);
  arrayWriter->Update();

  std::cout << "VariableLengthVector" << std::endl;
  itk::VariableLengthVector<float> vvec;
  vvec.Reserve(5);
  vvec[0] = 1;
  vvec[1] = 2;
  vvec[2] = 3;
  vvec[3] = 2;
  vvec[4] = 1;
  arrayWriter->SetFileName("test_vvec.mvh");
  arrayWriter->SetInput<float>(MET_FLOAT, &vvec);
  arrayWriter->Update();

  // Read them
  std::cout << "Read VariableLengthVector short" << std::endl;
  itk::VariableLengthVector<short> rvecs;
  itk::MetaArrayReader::Pointer arrayReader = itk::MetaArrayReader::New();
  arrayReader->SetFileName("test.mva");
  arrayReader->Update();
  arrayReader->GetOutput<short>(MET_SHORT, & rvecs);
  std::cout << "  vec short = " << rvecs << std::endl;

  std::cout << "Read VariableLengthVector float" << std::endl;
  itk::VariableLengthVector<float> rvecf;
  arrayReader->GetOutput<float>(MET_FLOAT, & rvecf);
  std::cout << "  rvec float = " << rvecf << std::endl;

  std::cout << "Read fixed array" << std::endl;
  itk::FixedArray<float, 5> farray;
  arrayReader->GetOutput<float, 5>(MET_FLOAT, & farray);
  std::cout << "  fixed array float = " << farray << std::endl;

  std::cout << "Read vector" << std::endl;
  itk::Vector<float, 5> rvecf5;
  arrayReader->GetOutput<float, 5>(MET_FLOAT, & rvecf5);
  std::cout << "  vector float = " << rvecf5 << std::endl;

  std::cout << "Read CovariantVector" << std::endl;
  itk::CovariantVector<float, 5> rcovec;
  arrayReader->GetOutput<float, 5>(MET_FLOAT, & rcovec);
  std::cout << "  covariant vector float = " << rcovec << std::endl;

  return EXIT_SUCCESS;
  }
