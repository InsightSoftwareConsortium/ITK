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

#include <iostream>
#include <fstream>
#include "itkMINCTransformIOFactory.h"
#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itksys/SystemTools.hxx"
#include "itkDisplacementFieldTransform.h"
#include "itkIOTestHelper.h"

void RandomPix(vnl_random &randgen,itk::Vector<double,3> &pix,
                      double _max=itk::NumericTraits<double>::max() )
{
  for(unsigned int i = 0; i < 3; i++)
    {
    pix[i] = randgen.drand64(_max);
    }
}

double abs_diff(const itk::Vector<double> &pix1,const itk::Vector<double> &pix2)
{
  double diff=0.0;

  for(int i=0; i<3; i++)
    {
    diff += fabs(pix1[i]-pix2[i]);
    }
  return diff;
}

static int oneTest(const char *goodname,const char *gridname)
{
  unsigned int i;
  double tolerance=1e-5;

  typedef itk::AffineTransform<double,3>                    AffineTransformType;
  typedef itk::DisplacementFieldTransform<double,3>         DisplacementFieldTransform;
  typedef DisplacementFieldTransform::DisplacementFieldType DisplacementFieldType;

  AffineTransformType::Pointer        affine = AffineTransformType::New();
  AffineTransformType::InputPointType cor;

  itk::ObjectFactoryBase::RegisterFactory(itk::MINCTransformIOFactory::New() );

  // Set it's parameters
  AffineTransformType::ParametersType p = affine->GetParameters();
  for ( i = 0; i < p.GetSize(); i++ )
  {
    p[i] = i;
  }
  affine->SetParameters ( p );
  p = affine->GetFixedParameters ();
  for ( i = 0; i < p.GetSize(); i++ )
  {
    p[i] = i;
  }
  affine->SetFixedParameters ( p );
  itk::TransformFileWriter::Pointer writer;
  itk::TransformFileReader::Pointer reader;

  reader = itk::TransformFileReader::New();
  writer = itk::TransformFileWriter::New();
  writer->AddTransform(affine);

  writer->SetFileName( goodname );
  reader->SetFileName( goodname );

  // Testing writing std::cout << "Testing write : ";
  affine->Print ( std::cout );
  try
  {
    writer->Update();
    std::cout << std::endl;
    std::cout << "Testing read : " << std::endl;
    reader->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    itk::TransformFileReader::TransformListType *list;
    list = reader->GetTransformList();
    itk::TransformFileReader::TransformListType::iterator lit = list->begin();
    while ( lit != list->end() )
    {
      (*lit)->Print ( std::cout );
      lit++;
    }
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  DisplacementFieldTransform::Pointer disp = DisplacementFieldTransform::New();
  DisplacementFieldType::Pointer field=DisplacementFieldType::New();

  //create zero displacement field
  DisplacementFieldType::SizeType    imageSize3D = {{ 10, 10, 10}};
  DisplacementFieldType::IndexType   startIndex3D = { {0, 0, 0}};

  double spacing[]={2.0, 2.0, 2.0};
  double origin[]={-10.0, -10.0, -10.0};
  DisplacementFieldType::RegionType  region;

  region.SetSize  (imageSize3D);
  region.SetIndex (startIndex3D);

  field->SetLargestPossibleRegion (region);
  field->SetBufferedRegion (region);
  field->SetRequestedRegion (region);

  field->SetSpacing( spacing );
  field->SetOrigin( origin );
  field->Allocate ();

  DisplacementFieldType::PixelType zeroDisplacement;
  zeroDisplacement.Fill( 0.0 );
  field->FillBuffer( zeroDisplacement );

  vnl_random                          randgen(12345678);
  itk::ImageRegionIterator<DisplacementFieldType> it(field,field->GetLargestPossibleRegion() );

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    DisplacementFieldType::PixelType pix;
     if(tolerance>0.0)
      RandomPix(randgen,pix,100);
     else
      RandomPix(randgen,pix);
    it.Set(pix);
    }

  disp->SetDisplacementField(field);

  disp->Print ( std::cout );

  itk::TransformFileWriter::Pointer gridwriter;
  itk::TransformFileReader::Pointer gridreader;

  gridreader = itk::TransformFileReader::New();
  gridwriter = itk::TransformFileWriter::New();
  gridwriter->AddTransform(disp);
  gridwriter->SetFileName(gridname);
  gridreader->SetFileName(gridname);

  // Testing writing
  std::cout << "Testing write of non linear transform : " << std::endl;

  try
  {
    gridwriter->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  // Testing writing
  std::cout << "Testing read of non linear transform : " << std::endl;
  try
    {
    gridreader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Comparing of non linear transform : " << std::endl;
  itk::TransformFileReader::TransformListType list=*gridreader->GetTransformList();
  std::cout<<"Read :"<<list.size()<<" transformations"<<std::endl;

  if(list.front()->GetTransformTypeAsString() != "DisplacementFieldTransform_double_3_3")
    {
      std::cerr<<"Read back transform of type:"<<list.front()->GetTransformTypeAsString()<<std::endl;
      return EXIT_FAILURE;
    }
  DisplacementFieldTransform::Pointer disp2 = static_cast<DisplacementFieldTransform*>(list.front().GetPointer());
  DisplacementFieldType::ConstPointer field2=disp2->GetDisplacementField();

  itk::ImageRegionConstIterator<DisplacementFieldType> it2(field2,field2->GetLargestPossibleRegion() );
  if(tolerance == 0.0)
    {
    for(it.GoToBegin(),it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it,++it2)
      {
      if(it.Value() != it2.Value() )
        {
        std::cout << "Original Pixel (" << it.Value()
                  << ") doesn't match read-in Pixel ("
                  << it2.Value() << " ) " << std::endl
                  << " in "<< gridname  << std::endl;
        return EXIT_FAILURE;
        }
      }
    } else { //account for rounding errors
    for( it.GoToBegin(),it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it,++it2 )
      {
      if( abs_diff(it.Value(),it2.Value() ) > tolerance)
        {
        std::cout << "Original Pixel (" << it.Value()
                  << ") doesn't match read-in Pixel ("
                  << it2.Value() << " ) "
                  << " in "<< gridname <<std::endl;
        return EXIT_FAILURE;
        }
      }
    }


  return EXIT_SUCCESS;
}

int secondTest()
{
  std::filebuf fb;
  fb.open("Rotation.xfm",std::ios::out);
  std::ostream os(&fb);
  os << "MNI Transform File"<<std::endl;
  os << "Transform_Type = Linear;"<<std::endl;
  os << "Linear_Transform ="<<std::endl;
  os << "1 0 0 0"<<std::endl;
  os << "0 0.866025447845459 -0.5 0"<<std::endl;
  os << "0 0.5 0.866025447845459 0;"<<std::endl;
  fb.close();

  itk::TransformFileReader::Pointer reader;
  reader = itk::TransformFileReader::New();
  reader->SetFileName("Rotation.xfm");

  reader->Update();

  itk::TransformFileReader::TransformListType *list;
  list = reader->GetTransformList();
  itk::TransformFileReader::TransformListType::iterator lit =  list->begin();
  while ( lit != list->end() )
  {
    (*lit)->Print ( std::cout );
    lit++;
  }
  return EXIT_SUCCESS;
}

int itkIOTransformMINCTest(int argc, char* argv[])
{
  if (argc > 1)
  {
    itksys::SystemTools::ChangeDirectory(argv[1]);
  }
  itk::TransformFactory< itk::DisplacementFieldTransform<double,3> >::RegisterTransform ();

  int result1 =  oneTest("TransformLinear.xfm", "TransformNonLinear.xfm" );
  int result2 =  secondTest();
  return !( result1 == EXIT_SUCCESS && result2 == EXIT_SUCCESS);
}
