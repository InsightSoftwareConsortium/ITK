/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdSegmentationLevelSetImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkThresholdSegmentationLevelSetImageFilter.h"
//#include "itkImageFileWriter.h"
//#include "itkRawImageIO.h"
#include "itkCastImageFilter.h"
#include "itkCommand.h"
#include "itkEventObject.h"

namespace TSIFTN {

 typedef itk::Image<float, 3> ImageType;
typedef itk::Image<char, 3> SeedImageType;

const int V_WIDTH  = 64;
const int V_HEIGHT = 64;
const int V_DEPTH  = 64;

float sphere(float x, float y, float z)
{
    float dis;
    dis = (x - (float)V_WIDTH/2.0)*(x - (float)V_WIDTH/2.0)
      /((0.2f*V_WIDTH)*(0.2f*V_WIDTH)) + 
      (y - (float)V_HEIGHT/2.0)*(y - (float)V_HEIGHT/2.0)
      /((0.2f*V_HEIGHT)*(0.2f*V_HEIGHT)) + 
      (z - (float)V_DEPTH/2.0)*(z - (float)V_DEPTH/2.0)
      /((0.2f*V_DEPTH)*(0.2f*V_DEPTH));
    return(1.0f-dis);
}

void evaluate_function(itk::Image<char, 3> *im,
          float (*f)(float, float, float) )
{
  itk::Image<char, 3>::IndexType idx;

  for(int z = 0; z < V_DEPTH; ++z)
    {
      idx[2] = z;
      for (int y = 0; y < V_HEIGHT; ++y)
        {
          idx[1] = y;
          for (int x = 0; x < V_WIDTH; ++x)
            {
              idx[0] = x;
              if ( f((float)x,(float)y,(float)z) >= 0.0 )
                {  im->SetPixel(idx, 1 ); }
              else
                {  im->SetPixel(idx, 0 ); }
            }
        }
    }
}

} // end namespace


namespace itk {

class RMSCommand : public Command
{
public:
  /** Smart pointer declaration methods */
  typedef RMSCommand Self;
  typedef Command Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  itkTypeMacro( RMSCommand, Command );
  itkNewMacro(Self);

  /** Standard Command virtual methods */
  void Execute(Object *caller, const EventObject &)
  {
    std::cout <<
      (dynamic_cast<SparseFieldLevelSetImageFilter< ::TSIFTN::SeedImageType, ::TSIFTN::ImageType> *>(caller))->GetRMSChange()
              << std::endl;
    std::cout <<
      (dynamic_cast<SegmentationLevelSetImageFilter< ::TSIFTN::SeedImageType, ::TSIFTN::ImageType> *>(caller))->GetSegmentationFunction()->GetPropagationWeight()
              << std::endl;
   
  }
  void Execute(const Object *, const EventObject &)
  {
    std::cout << "ack" << std::endl;

  }

protected:
  RMSCommand()  {}
  virtual ~RMSCommand() {}
};

}


int itkThresholdSegmentationLevelSetImageFilterTest(int, char * [] )
{
  std::cout << "Last modified 11/08/02" << std::endl;
  
  TSIFTN::ImageType::RegionType reg;
  TSIFTN::ImageType::RegionType::SizeType sz;
  TSIFTN::ImageType::RegionType::IndexType idx;
  idx[0] = idx[1] = idx[2] = 0;
  sz[0] = sz[1] = sz[2] = 64;
  reg.SetSize(sz);
  reg.SetIndex(idx);

  TSIFTN::ImageType::Pointer     inputImage = TSIFTN::ImageType::New();
  TSIFTN::SeedImageType::Pointer seedImage = TSIFTN::SeedImageType::New();
  inputImage->SetRegions(reg);
  seedImage->SetRegions(reg);
  inputImage->Allocate();
  seedImage->Allocate();

  // Starting surface is a sphere in the center of the image.
  TSIFTN::evaluate_function(seedImage, TSIFTN::sphere);

  // Target surface is a diamond
  float val;
  unsigned int i;
  //  TSIFTN::ImageType::IndexType idx;
  for (idx[2] = 0; idx[2] < 64; idx[2]++)
    for (idx[1] = 0; idx[1] < 64; idx[1]++)
        for (idx[0] = 0; idx[0] < 64; idx[0]++)
          {
            val = 0;
            for (i = 0; i < 3; ++i)
              {
                if (idx[i] < 32) val+=idx[i];
                else val += 64 - idx[i];
              }
            inputImage->SetPixel(idx, val);
          }

  itk::ThresholdSegmentationLevelSetImageFilter< ::TSIFTN::SeedImageType, ::TSIFTN::ImageType>::Pointer
    filter = itk::ThresholdSegmentationLevelSetImageFilter< ::TSIFTN::SeedImageType, ::TSIFTN::ImageType>::New();
  filter->SetInput(seedImage);
  filter->SetFeatureImage(inputImage);

  filter->SetUpperThreshold(63);
  filter->SetLowerThreshold(50);

  filter->SetMaximumRMSError(0.04);
  filter->SetMaximumIterations(50);
  filter->SetUseNegativeFeaturesOn(); // Change the default behavior of the speed
                                      // function so that negative values result in
                                      // surface growth.
  
  itk::RMSCommand::Pointer c = itk::RMSCommand::New();
  filter->AddObserver(itk::IterationEvent(), c); 
  filter->SetIsoSurfaceValue(0.5);  //<--- IMPORTANT!  Default is zero.
  
  try {
    filter->Update();
    std::cout << "Done first trial" << std::endl;
    // Repeat to make sure that the filter is reinitialized properly
    filter->SetMaximumIterations(20);
    filter->Update();
    std::cout << "Done second trial" << std::endl;
    // Repeat once more just for fun
    filter->SetMaximumIterations(21);
    filter->Update();
    std::cout << "Done third trial" << std::endl;
    
    // Write the output for debugging purposes
    //       itk::ImageFileWriter<TSIFTN::ImageType>::Pointer writer
    //          = itk::ImageFileWriter<TSIFTN::ImageType>::New();
    //        itk::RawImageIO<float, 3>::Pointer io = itk::RawImageIO<float, 3>::New();
    //        io->SetFileTypeToBinary();
    //        io->SetFileDimensionality(3);
    //        io->SetByteOrderToLittleEndian();
    //        writer->SetImageIO(io);
    
    //        itk::CastImageFilter<TSIFTN::SeedImageType, TSIFTN::ImageType>::Pointer
    //         caster = itk::CastImageFilter<TSIFTN::SeedImageType, TSIFTN::ImageType>::New();
    //        caster->SetInput(seedImage);
    //        caster->Update();
    
        // writer->SetInput(caster->GetOutput());
        //     writer->SetInput(filter->GetSpeedImage());
        //        writer->SetInput(filter->GetFeatureImage());
    // writer->SetInput(inputImage);
    //        writer->SetInput(filter->GetOutput());
    //       writer->SetFileName("output.raw");
    //        writer->Write();
        
  }
  catch (itk::ExceptionObject &e)
    {
      std::cerr << e << std::endl;
    }
  
  return 0;
}
