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

#include "itkImageFileReader.h"
#include "itkVOLImageIOFactory.h"
#include "itkVOLImageIO.h"
#include "itkImage.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkVOLImageIOTest(int ac, char* av[])
{

  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << std::endl;
    return EXIT_FAILURE;
    }

  // Register at least one factory capable of producing
  // VOL image file readers
  itk::VOLImageIOFactory::RegisterOneFactory();

  typedef unsigned char            PixelType;
  typedef itk::Image<PixelType, 4> myImage;


  itk::VOLImageIO::Pointer io;
  io = itk::VOLImageIO::New();

  itk::ImageFileReader<myImage>::Pointer reader = itk::ImageFileReader<myImage>::New();
  //reader->DebugOn();
  reader->SetFileName(av[1]);
  reader->SetImageIO(io);
  try
    {
    reader->Update();
    }
  catch (itk::ImageFileReaderException& e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

  myImage::Pointer image = reader->GetOutput();
  image->Print(std::cout);
  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cerr << "region " << region << std::endl;

  // This is where we call all of the Get Functions to increase coverage.
  std::cerr << "File_type " << io->GetFile_type() << std::endl;
  std::cerr << "File_rev " << io->GetFile_rev() << std::endl;
  std::cerr << "Description " << io->GetDescription() << std::endl;
  std::cerr << "Date " << io->GetDate() << std::endl;
  std::cerr << "Time " << io->GetTime() << std::endl;
  std::cerr << "Patient " << io->GetPatient() << std::endl;
  std::cerr << "Clinic " << io->GetClinic() << std::endl;
  std::cerr << "NumEchoFrames " << io->GetNumEchoFrames() << std::endl;
  std::cerr << "NumDopFrames " << io->GetNumDopFrames() << std::endl;
  std::cerr << "Dopmode " << io->GetDopmode() << std::endl;
  std::cerr << "EchoLPF " << io->GetEchoLPF() << std::endl;
  std::cerr << "DopLPF " << io->GetDopLPF() << std::endl;
  std::cerr << "Repetition " << io->GetRepetition() << std::endl;
  std::cerr << "Xducer_name " << io->GetXducer_name() << std::endl;
  std::cerr << "Xducer_ID " << io->GetXducer_ID() << std::endl;
  std::cerr << "Xducer_freq " << io->GetXducer_freq() << std::endl;
  std::cerr << "Depth " << io->GetDepth() << std::endl;
  std::cerr << "Default_depth " << io->GetDefault_depth() << std::endl;
  std::cerr << "App_name " << io->GetApp_name() << std::endl;
  std::cerr << "Application " << io->GetApplication() << std::endl;
  std::cerr << "Scan_fmt " << io->GetScan_fmt() << std::endl;
  std::cerr << "Dataset_name " << io->GetDataset_name() << std::endl;
  std::cerr << "First_tx_line " << io->GetFirst_tx_line() << std::endl;
  std::cerr << "Last_tx_line " << io->GetLast_tx_line() << std::endl;
  std::cerr << "Lines " << io->GetLines() << std::endl;
  std::cerr << "Az_lines " << io->GetAz_lines() << std::endl;
  std::cerr << "Az_angle " << io->GetAz_angle() << std::endl;
  std::cerr << "Az_angular_separation " << io->GetAz_angular_separation() << std::endl;
  std::cerr << "El_lines" << io->GetEl_lines() << std::endl;
  std::cerr << "El_angle " << io->GetEl_angle() << std::endl;
  std::cerr << "El_angular_separation " << io->GetEl_angular_separation() << std::endl;
  std::cerr << "Tx_offset " << io->GetTx_offset() << std::endl;
  std::cerr << "Rx_offset " << io->GetRx_offset() << std::endl;
  std::cerr << "MclkFreq " << io->GetMclkFreq() << std::endl;
  std::cerr << "SampleSize " << io->GetSampleSize() << std::endl;
  std::cerr << "Mclk2Size " << io->GetMclk2Size() << std::endl;
  std::cerr << "SampleRate " << io->GetSampleRate() << std::endl;
  std::cerr << "LineGroupSize " << io->GetLineGroupSize() << std::endl;
  std::cerr << "NumECGSamples " << io->GetNumECGSamples() << std::endl;
  std::cerr << "GrayImageSize " << io->GetGrayImageSize() << std::endl;
  std::cerr << "DopplerImageSize " << io->GetDopplerImageSize() << std::endl;
  std::cerr << "EcgSize " << io->GetEcgSize() << std::endl;
  std::cerr << "MiscDataSize " << io->GetMiscDataSize() << std::endl;
  std::cerr << "GrayImageOffset " << io->GetGrayImageOffset() << std::endl;
  std::cerr << "DopplerImageOffset " << io->GetDopplerImageOffset() << std::endl;
  std::cerr << "EcgOffset " << io->GetEcgOffset() << std::endl;
  std::cerr << "MiscDataOffset " << io->GetMiscDataOffset() << std::endl;
  std::cerr << "File_control_timing_type " << io->GetFile_control_timing_type() << std::endl;
  std::cerr << "DopplerVolInfo " << io->GetDopplerVolInfo() << std::endl;
  std::cerr << "ScanDepthCount "  << io->GetScanDepthCount() << std::endl;
  std::cerr << "ScanDepth " << io->GetScanDepth() << std::endl;
  std::cerr << "Az_sector_tilt " << io->GetAz_sector_tilt() << std::endl;
  std::cerr << "Elev_sector_tilt " << io->GetElev_sector_tilt() << std::endl;
  std::cerr << "DopplerSegData " << io->GetDopplerSegData() << std::endl;
  std::cerr << "FrameRate " << io->GetFrameRate() << std::endl;
  std::cerr << "Sweepspeed " << io->GetSweepspeed() << std::endl;
  std::cerr << "Update_interval " << io->GetUpdate_interval() << std::endl;
  std::cerr << "Contrast_on " << io->GetContrast_on() << std::endl;
  std::cerr << "Comp_curve_p0_x " << io->GetComp_curve_p0_x() << std::endl;
  std::cerr << "Comp_curve_p0_y " << io->GetComp_curve_p0_y() << std::endl;
  std::cerr << "Comp_curve_p1_x " << io->GetComp_curve_p1_x() << std::endl;
  std::cerr << "Comp_curve_p1_y " << io->GetComp_curve_p1_y() << std::endl;
  std::cerr << "Comp_curve_p2_x " << io->GetComp_curve_p2_x() << std::endl;
  std::cerr << "Comp_curve_p2_y " << io->GetComp_curve_p2_y() << std::endl;
  std::cerr << "Comp_curve_p3_x " << io->GetComp_curve_p3_x() << std::endl;
  std::cerr << "Comp_curve_p3_y " << io->GetComp_curve_p3_y() << std::endl;
  std::cerr << "Comp_curve_scaling_index " << io->GetComp_curve_scaling_index() << std::endl;
  std::cerr << "Echo_reject " << io->GetEcho_reject() << std::endl;
  std::cerr << "Mt_tp " << io->GetMt_tp() << std::endl;
  std::cerr << "True_axis_defined " << io->GetTrue_axis_defined() << std::endl;
  std::cerr << "True_axis_on " << io->GetTrue_axis_on() << std::endl;
  std::cerr << "Parallel_x_tilt " << io->GetParallel_x_tilt() << std::endl;
  std::cerr << "Parallel_y_tilt " << io->GetParallel_y_tilt() << std::endl;
  std::cerr << "Parallel_depth " << io->GetParallel_depth() << std::endl;
  std::cerr << "Parallel_spacing " << io->GetParallel_spacing() << std::endl;
  std::cerr << "Parallel_thickness " << io->GetParallel_thickness() << std::endl;
  std::cerr << "Viewport_transform_flags " << io->GetViewport_transform_flags() << std::endl;
  std::cerr << "Stress_mode " << io->GetStress_mode() << std::endl;
  std::cerr << "Stress_label " << io->GetStress_label() << std::endl;
  std::cerr << "Heart_rate " << io->GetHeart_rate() << std::endl;
  std::cerr << "Stage_timer_value " << io->GetStage_timer_value() << std::endl;
  std::cerr << "Ecg_display_on " << io->GetEcg_display_on() << std::endl;
  std::cerr << "Blanking " << io->GetBlanking() << std::endl;
  std::cerr << "Samples " << io->GetSamples() << std::endl;
  std::cerr << "ColorImageSize " << io->GetColorImageSize() << std::endl;
  std::cerr << "ColorImageOffset " << io->GetColorImageOffset() << std::endl;
  std::cerr << "Oag_params " << io->GetOag_params() << std::endl;
  std::cerr << "Cscanfmt " << io->GetCscanfmt() << std::endl;
  std::cerr << "Oaglinear " << io->GetOaglinear() << std::endl;
  std::cerr << "Maxradius " << io->GetMaxradius() << std::endl;
  std::cerr << "Anglescale " << io->GetAnglescale() << std::endl;
  std::cerr << "Skinoffset " << io->GetSkinoffset() << std::endl;

  return EXIT_SUCCESS;
}
