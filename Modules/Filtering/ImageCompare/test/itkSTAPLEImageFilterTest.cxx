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
#include "itkSTAPLEImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

class StaplerBase
{
public:
  StaplerBase()  {}
  virtual ~StaplerBase() {}

  void SetOutputFileName( const char *s )
  { m_OutputFile = s; }

  const std::string &GetOutputFileName() const
  { return m_OutputFile; }

  void AddFileName( const char *s )
  {
    std::string tmp(s);
    m_Files.push_back(tmp);
  }

  const std::string &GetFileName( unsigned int i ) const
  { return m_Files[i]; }

  void ClearFileNames()
  { m_Files.clear(); }

  unsigned int GetNumberOfFiles() const
  {
    return static_cast<unsigned int>( m_Files.size() );
  }

  virtual double GetSensitivity( unsigned int ) = 0;
  virtual double GetSpecificity( unsigned int ) = 0;
  virtual unsigned short GetForeground() const  = 0;
  virtual void SetForeground( unsigned short )  = 0;
  virtual void SetConfidenceWeight( double ) = 0;
  virtual double GetConfidenceWeight() const = 0;


  virtual int Execute() = 0;
  virtual unsigned int GetElapsedIterations() = 0;

protected:
  std::vector< std::string > m_Files;
  std::string                m_OutputFile;
};

template< unsigned int VDimension>
class Stapler : public StaplerBase
{
public:
  typedef itk::Image< double, VDimension >         OutputImageType;
  typedef itk::Image< unsigned short, VDimension > InputImageType;
  typedef itk::STAPLEImageFilter<InputImageType, OutputImageType>
                                                   StapleFilterType;

  Stapler()
  {
    m_Stapler = StapleFilterType::New();
    this->SetForeground(1);
  }
  virtual ~Stapler() ITK_OVERRIDE {}

  virtual double GetConfidenceWeight( ) const ITK_OVERRIDE
  { return m_Stapler->GetConfidenceWeight(); }
  virtual void SetConfidenceWeight( double w ) ITK_OVERRIDE
  { m_Stapler->SetConfidenceWeight( w); }

  virtual double GetSensitivity( unsigned int i ) ITK_OVERRIDE
  { return m_Stapler->GetSensitivity(i); }
  virtual double GetSpecificity( unsigned int i ) ITK_OVERRIDE
  { return m_Stapler->GetSpecificity(i); }

  virtual unsigned short GetForeground() const ITK_OVERRIDE
  { return m_Stapler->GetForegroundValue(); }
  virtual void SetForeground( unsigned short l ) ITK_OVERRIDE
  { m_Stapler->SetForegroundValue( l ); }

  virtual unsigned int GetElapsedIterations() ITK_OVERRIDE
  { return m_Stapler->GetElapsedIterations(); }

  virtual int Execute() ITK_OVERRIDE;

private:
  typename StapleFilterType::Pointer m_Stapler;
};


template< unsigned int VDimension >
int Stapler<VDimension>::Execute()
{
  size_t i;

  typename itk::ImageFileReader<InputImageType>::Pointer  reader;
  typename itk::ImageFileWriter<OutputImageType>::Pointer writer
    = itk::ImageFileWriter<OutputImageType>::New();

  size_t number_of_files = m_Files.size();

  // Set the inputs
  for (i = 0; i < number_of_files; i++)
    {
    try
      {
      reader = itk::ImageFileReader<InputImageType>::New();
      reader->SetFileName( m_Files[i].c_str() );
      reader->Update();
      m_Stapler->SetInput(itk::Math::CastWithRangeCheck<unsigned int>(i), reader->GetOutput());
      }
    catch (itk::ExceptionObject &e)
      {
      std::cerr << e << std::endl;
      return -1;
      }
    }

  try
    {
    writer->SetFileName( m_OutputFile.c_str() );
    writer->SetInput(m_Stapler->GetOutput());
    writer->Update();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << e << std::endl;
    return -2;
    }

  return 0;
}


int itkSTAPLEImageFilterTest( int argc, char * argv[])
{
  int i;
  StaplerBase *stapler;

  if (argc < 5)
    {
    std::cerr << "Use: " << argv[0] <<
      " file_dimensionality output.mhd foreground_value confidence_weight "
      "file1 file2 ... fileN" << std::endl;
    return -1;
    }

  if ( ::atoi(argv[1]) == 2 )
    {
    stapler = new Stapler<2>;
    }
  else if ( ::atoi(argv[1]) == 3 )
    {
    stapler = new Stapler<3>;
    }
  else
    {
    std::cerr << "Only 2D and 3D data is currently supported" << std::endl;
    return -2;
    }

  for (i = 0; i < argc - 5; i++)
    {
    stapler->AddFileName( argv[i+5] );
    }

  stapler->SetConfidenceWeight( static_cast<double>( atof(argv[4]) ));
  stapler->SetOutputFileName( argv[2] );
  stapler->SetForeground( static_cast<unsigned short>( atoi(argv[3])) );

  // Execute the stapler
  int ret = stapler->Execute();
  if ( ret != 0 )
    {
    std::cerr << "Stapler failed!  Returned error code " << ret << "." << std::endl;
    return -3;
    }

  double avg_p = 0.0;
  double avg_q = 0.0;
  // Print out the specificities
  std::cout << "Number of elapsed iterations = "
            << stapler->GetElapsedIterations() << std::endl;

  //  std::cout.precision(5);
  //  std::cout.setf(ios::fixed, ios::floatfield);

  std::cout << "File " << "\t\tSensitivity(p) " <<  "\tSpecificity(q)" << std::endl;
  std::cout << "-----" << "\t\t-------------- " <<  "\t--------------" << std::endl;
  unsigned int j;
  for (j = 0; j < stapler->GetNumberOfFiles(); j++)
    {
    avg_q += stapler->GetSpecificity(j);
    avg_p += stapler->GetSensitivity(j);
    std::cout << j << ": " << stapler->GetFileName(j) << "\t"
              << stapler->GetSensitivity(j) << "\t\t"
              << stapler->GetSpecificity(j) << std::endl;
    }
  avg_p /= static_cast<double>( stapler->GetNumberOfFiles() );
  avg_q /= static_cast<double>( stapler->GetNumberOfFiles() );

  std::cout << "Mean:\t\t" << avg_p << "\t\t" << avg_q << std::endl;

  delete stapler;

  return EXIT_SUCCESS;
}
