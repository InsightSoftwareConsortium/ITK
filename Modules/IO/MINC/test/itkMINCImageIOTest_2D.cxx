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

#include "itkImage.h"
#include <iostream>

#include "itkMINCImageIO.h"
#include "itkMINCImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkImageMomentsCalculator.h"
#include "itkStdStreamStateSave.h"

template<typename ImageType> int test_image_moments(const char *input_image,const char *output_image,double total, double mx,double my,double epsilon)
{
  typedef itk::ImageFileReader< ImageType > ReaderType;

  typedef itk::ImageFileWriter< ImageType > WriterType;

  typedef itk::ImageMomentsCalculator< ImageType > MomentsCalculatorType;

  typename ReaderType::Pointer reader = ReaderType::New();

  typename MomentsCalculatorType::Pointer calculator=MomentsCalculatorType::New();

  reader->SetFileName( input_image );

  reader->Update();
  calculator->SetImage(reader->GetOutput());
  calculator->Compute();

  std::cout<<"Image:"<<input_image<<" sum="<<calculator->GetTotalMass()<<" COM="<<calculator->GetCenterOfGravity()<<std::endl;

  if(total>0.0) //assume that if no total was provided this test should not be performed
    {
    if(fabs(calculator->GetTotalMass()-total)>epsilon)
      {
      std::cerr<<"Total sum mismatch:"<<calculator->GetTotalMass()<<" difference="<<(calculator->GetTotalMass()-total)<<std::endl;
      return EXIT_FAILURE;
      }
    if(fabs(calculator->GetCenterOfGravity()[0]-mx)>epsilon)
      {
      std::cerr<<"Total mx mismatch:"<<calculator->GetCenterOfGravity()[0]<<std::endl;
      return EXIT_FAILURE;
      }
    if(fabs(calculator->GetCenterOfGravity()[1]-my)>epsilon)
      {
      std::cerr<<"Total my mismatch:"<<calculator->GetCenterOfGravity()[1]<<std::endl;
      return EXIT_FAILURE;
      }
    }

  if( output_image )
  {
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( output_image );
    writer->SetInput( reader->GetOutput() );
    writer->Update();
  }
  return EXIT_SUCCESS;
}


int itkMINCImageIOTest_2D( int argc, char * argv [] )
{

// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  if ( argc < 3 )
    {
    std::cerr << "Missing Arguments " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputfile outputfile [sum mx my ]" << std::endl;
    return EXIT_FAILURE;
    }

  const char *input=argv[1];
  const char *output=argv[2];

  double total = 0.0;
  double mx = 0.0;
  double my = 0.0;

  itk::MINCImageIOFactory::RegisterOneFactory();

  if(argc>3)
    {
      if(argc==7)
        {
        total=atof( argv[3] );
        mx=atof( argv[4] );
        my=atof( argv[5] );
        }
      else
        {
        std::cerr << "Incorrecte number of additional arguments " << std::endl;
        std::cerr << "Usage: " << std::endl;
        std::cerr << argv[0] << " inputfile outputfile [sum mx my ]" << std::endl;
        return EXIT_FAILURE;
        }
    }

  double epsilon=1e-3;

  try
    {
      int ret=EXIT_SUCCESS;

      // save and restore cout's precision, to silence a Coverity warning
      std::cout.precision( 10 );
      if( test_image_moments<itk::Image< double, 2 > >(input,ITK_NULLPTR,total,mx,my,epsilon) != EXIT_SUCCESS )
        {
        ret=EXIT_FAILURE;
        }
      // write out only float image
      if( test_image_moments<itk::Image< float, 2 > >(input,output,total,mx,my,epsilon) != EXIT_SUCCESS )
        {
        ret=EXIT_FAILURE;
        }
      return ret;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
