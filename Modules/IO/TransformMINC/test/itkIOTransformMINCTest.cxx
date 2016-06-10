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
#include "itkMath.h"


static ITK_CONSTEXPR_VAR int    point_counter=1000;


template<typename T>void RandomPix(vnl_random &randgen,itk::Vector<T,3> &pix,
                      double _max=itk::NumericTraits<T>::max() )
{
  for(unsigned int i = 0; i < 3; i++)
    {
    pix[i] = randgen.drand64(_max);
    }
}

template<typename T>void RandomPoint(vnl_random &randgen,itk::Point<T,3> &pix,
                      double _max=itk::NumericTraits<T>::max() )
{
  for(unsigned int i = 0; i < 3; i++)
    {
    pix[i] = randgen.drand64(_max);
    }
}


static int check_linear(const char *linear_transform)
{
  typedef itk::AffineTransform<double,3>  AffineTransformType;
  const double tolerance = 1e-5;

  AffineTransformType::Pointer        affine = AffineTransformType::New();
  AffineTransformType::InputPointType cor;

  itk::ObjectFactoryBase::RegisterFactory(itk::MINCTransformIOFactory::New() );

  // Set it's parameters
  AffineTransformType::OutputVectorType rot_axis;
  rot_axis[0]=0.0;
  rot_axis[1]=1.0;
  rot_axis[2]=0.0;
  // Set it's parameters
  affine->Rotate3D(rot_axis,itk::Math::pi/6);

  AffineTransformType::OutputVectorType offset;

  offset[0]=0.0;
  offset[1]=0.0;
  offset[2]=10.0;

  affine->Translate(offset);

  affine->Scale(4.0);

  itk::TransformFileWriter::Pointer writer;
  itk::TransformFileReader::Pointer reader;

  reader = itk::TransformFileReader::New();
  writer = itk::TransformFileWriter::New();
  writer->AddTransform(affine);

  writer->SetFileName( linear_transform );
  reader->SetFileName( linear_transform );

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
    itk::TransformFileReader::TransformListType list=*reader->GetTransformList();

    if(list.front()->GetTransformTypeAsString() != "AffineTransform_double_3_3")
      {
        std::cerr<<"Read back transform of type:"<<list.front()->GetTransformTypeAsString()<<std::endl;
        return EXIT_FAILURE;
      }
    AffineTransformType::Pointer affine2 = static_cast<AffineTransformType*>(list.front().GetPointer());

    std::cout<<"Read transform : " << std::endl;
    affine2->Print( std::cout );

    vnl_random randgen(12345678);

    AffineTransformType::InputPointType pnt,pnt2;

    std::cout << "Testing that transformations are the same ..." << std::endl;
    for(int i=0;i<point_counter;i++)
      {
      AffineTransformType::OutputPointType v1;
      AffineTransformType::OutputPointType v2;

      RandomPoint<double>(randgen,pnt,100);
      pnt2=pnt;
      v1= affine->TransformPoint( pnt );
      v2= affine2->TransformPoint( pnt2 );

      if( ( v1-v2 ).GetSquaredNorm() > tolerance)
        {
        std::cerr << "Original Pixel (" << v1
                  << ") doesn't match read-in Pixel ("
                  << v2 << " ) "
                  << " in "<< linear_transform << " at "<< pnt <<std::endl;
        return EXIT_FAILURE;
        }
      }
      std::cout << " Done !" << std::endl;
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

static int check_nonlinear_double(const char *nonlinear_transform)
{
  const double tolerance = 1e-5;

  typedef itk::DisplacementFieldTransform<double,3>         DisplacementFieldTransform;
  typedef DisplacementFieldTransform::DisplacementFieldType DisplacementFieldType;

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

  vnl_random  randgen(12345678);
  itk::ImageRegionIterator<DisplacementFieldType> it(field,field->GetLargestPossibleRegion() );

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    DisplacementFieldType::PixelType pix;
     if( tolerance > 0.0 )
       {
       RandomPix<double>(randgen,pix,100);
       }
     else
       {
       RandomPix<double>(randgen,pix);
       }
    it.Set(pix);
    }

  disp->SetDisplacementField(field);

  disp->Print ( std::cout );

  itk::TransformFileWriter::Pointer nlwriter;
  itk::TransformFileReader::Pointer nlreader;

  nlreader = itk::TransformFileReader::New();
  nlwriter = itk::TransformFileWriter::New();
  nlwriter->AddTransform(disp);
  nlwriter->SetFileName(nonlinear_transform);
  nlreader->SetFileName(nonlinear_transform);

  // Testing writing
  std::cout << "Testing write of non linear transform (double) : " << std::endl;

  try
    {
    nlwriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  // Testing writing
  std::cout << "Testing read of non linear transform (double): " << std::endl;
  try
    {
    nlreader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Comparing of non linear transform (double) : " << std::endl;
  itk::TransformFileReader::TransformListType list=*nlreader->GetTransformList();
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
                  << " in "<< nonlinear_transform  << std::endl;
        return EXIT_FAILURE;
        }
      }
    } else { //account for rounding errors
    for( it.GoToBegin(),it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it,++it2 )
      {
      if( ( it.Value() - it2.Value() ).GetSquaredNorm() > tolerance)
        {
        std::cout << "Original Pixel (" << it.Value()
                  << ") doesn't match read-in Pixel ("
                  << it2.Value() << " ) "
                  << " in "<< nonlinear_transform <<std::endl;
        return EXIT_FAILURE;
        }
      }
    }


  return EXIT_SUCCESS;
}


static int check_nonlinear_float(const char *nonlinear_transform)
{
  double tolerance=1e-5;

  typedef itk::TransformFileWriterTemplate<float> TransformFileWriterFloat;
  typedef itk::TransformFileReaderTemplate<float> TransformFileReaderFloat;

  typedef itk::DisplacementFieldTransform<float,3>         DisplacementFieldTransform;
  typedef DisplacementFieldTransform::DisplacementFieldType DisplacementFieldType;

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
     if(tolerance > 0.0)
       {
       RandomPix<float>(randgen,pix,100);
       }
     else
       {
       RandomPix<float>(randgen,pix);
       }
    it.Set(pix);
    }

  disp->SetDisplacementField(field);

  disp->Print ( std::cout );

  TransformFileWriterFloat::Pointer nlwriter;
  TransformFileReaderFloat::Pointer nlreader;

  nlreader = TransformFileReaderFloat::New();
  nlwriter = TransformFileWriterFloat::New();
  nlwriter->AddTransform(disp);
  nlwriter->SetFileName(nonlinear_transform);
  nlreader->SetFileName(nonlinear_transform);

  // Testing writing
  std::cout << "Testing write of non linear transform (float): " << std::endl;

  try
  {
    nlwriter->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  // Testing writing
  std::cout << "Testing read of non linear transform (float) : " << std::endl;
  try
    {
    nlreader->Update();
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
  TransformFileReaderFloat::TransformListType list=*nlreader->GetTransformList();
  std::cout<<"Read :"<<list.size()<<" transformations"<<std::endl;

  if(list.front()->GetTransformTypeAsString() != "DisplacementFieldTransform_float_3_3")
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
                  << " in "<< nonlinear_transform  << std::endl;
        return EXIT_FAILURE;
        }
      }
    } else { //account for rounding errors
    for( it.GoToBegin(),it2.GoToBegin(); !it.IsAtEnd() && !it2.IsAtEnd(); ++it,++it2 )
      {
      if( (it.Value() - it2.Value()).GetSquaredNorm() > tolerance)
        {
        std::cout << "Original Pixel (" << it.Value()
                  << ") doesn't match read-in Pixel ("
                  << it2.Value() << " ) "
                  << " in "<< nonlinear_transform <<std::endl;
        return EXIT_FAILURE;
        }
      }
    }


  return EXIT_SUCCESS;
}

static int secondTest()
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

  const itk::TransformFileReader::TransformListType * list = reader->GetTransformList();
  itk::TransformFileReader::TransformListType::const_iterator lit =  list->begin();
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
  itk::TransformFactory< itk::DisplacementFieldTransform<float,3> >::RegisterTransform ();

  int result1 =  check_linear("itkIOTransformMINCTestTransformLinear.xfm");
  int result2 =  check_nonlinear_double( "itkIOTransformMINCTestTransformNonLinear.xfm" );
  int result3 =  check_nonlinear_float( "itkIOTransformMINCTestTransformNonLinear_float.xfm" );
  int result4 =  secondTest();

  return !( result1 == EXIT_SUCCESS &&
            result2 == EXIT_SUCCESS &&
            result3 == EXIT_SUCCESS &&
            result4 == EXIT_SUCCESS
          );
}
