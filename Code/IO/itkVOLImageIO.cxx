/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVOLImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVOLImageIO.h"

namespace itk
{


// simple class to call fopen on construct and
// fclose on destruct
struct VOLFileWrapper
{
  VOLFileWrapper(const char* fname)
  {
    m_FilePointer = fopen(fname, "rb");
  }
  FILE* m_FilePointer;
  ~VOLFileWrapper()
  {
    if(m_FilePointer)
      {
      fclose(m_FilePointer);
      }
  }
};

VOLImageIO::VOLImageIO()
{
  m_PixelType = UCHAR;
  this->SetNumberOfComponents(1);
  this->SetNumberOfDimensions(4);
  strcpy (m_File_rev, "");
  m_Skinoffset = -1;
  m_Blanking = -1;
}

VOLImageIO::~VOLImageIO()
{
}


bool VOLImageIO::CanReadFile(const char* file) 
{ 

  VOLFileWrapper volfp(file); 
  FILE* fp = volfp.m_FilePointer;
  if(!fp)
    {
    return false;
    }

  ReadData(fp, &m_File_type, sizeof(m_File_type), 0);
  if (m_File_type != VOL_MAGIC_NUMBER)
    {
    return false;
    }

  return true;
}
  
const std::type_info& VOLImageIO::GetPixelType() const
{
  return typeid(unsigned char);
}

  
unsigned int VOLImageIO::GetComponentSize() const
{
  return sizeof(unsigned char);
}

void VOLImageIO::Read(void* buffer)
{

  // use this class so return will call close
  VOLFileWrapper volfp(this->GetFileName()); 
  FILE* fp = volfp.m_FilePointer;
  if(!fp)
    {
    itkExceptionMacro("Error VOLImageIO could not open file: " 
                      << this->GetFileName());
    return;
    }

  // Read the image
  unsigned char *tempImage = static_cast<unsigned char*>(buffer);
  unsigned char * imgset = new unsigned char [256*512*4*4];
  for (unsigned int timeCounter = 0; timeCounter < m_Dimensions[3]; timeCounter++)
    {
    ReadData(fp, (char *)imgset, 
             sizeof(unsigned char) * 256*512*4*4, 
             m_GrayImageOffset + timeCounter*256*512*4*4);
    for(unsigned int i=0; i < m_Dimensions[2]; i++)
      {
      for(int j=0; j < 16; j++)
        {
        for(int k=0; k < 16; k++)
          {
          for(int m=0; m < 4; m++)
            {
            for(int n=0; n < 4; n++)
              {
              tempImage[timeCounter * m_Strides[4] + i * m_Strides[3] 
                        + (j*4+m) * m_Strides[2] + (k*4+n)] 
                = imgset[(16*k+j)*512*4*4 + i*4*4 + n*4 + m];
              }
            }
          }
        }
      }
    }
  delete imgset;
}


double const * const * VOLImageIO::GetMt_tp() const
{
  return reinterpret_cast<double const * const *>(m_Mt_tp);
}

double VOLImageIO::GetSkinoffset()
{
  if (m_Skinoffset == -1)
    m_Skinoffset = m_Blanking * m_SampleSize;
  return m_Skinoffset;
}

double VOLImageIO::GetBlanking()
{
  if (m_Blanking != -1 && m_Skinoffset == -1)
    m_Blanking = m_SampleSize/m_Skinoffset;
  return m_Blanking;
}


void VOLImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if (m_File_rev[0] != '\0')
    {
    os << indent << "File_type " << m_File_type<< "\n";
    os << indent << "File_rev " << m_File_rev << "\n";      
    os << indent << "Description " << m_Description << "\n";     
    os << indent << "Date " << m_Date << "\n";
    os << indent << "Time " << m_Time << "\n";   
    os << indent << "Patient " << m_Patient << "\n";
    os << indent << "Clinic " << m_Clinic << "\n";        
    os << indent << "NumEchoFrames " << m_NumEchoFrames << "\n"; 
    os << indent << "NumDopFrames " << m_NumDopFrames << "\n"; 
    os << indent << "Dopmode " << m_Dopmode << "\n"; 
    os << indent << "EchoLPF " << m_EchoLPF << "\n"; 
    os << indent << "DopLPF " << m_DopLPF << "\n"; 
    os << indent << "Repetition " << m_Repetition << "\n"; 
    os << indent << "Xducer_name " << m_Xducer_name << "\n";
    os << indent << "Xducer_ID " << m_Xducer_ID << "\n";
    os << indent << "Xducer_freq " << m_Xducer_freq << "\n";
    os << indent << "Depth " << m_Depth << "\n";
    os << indent << "Default_depth " << m_Default_depth << "\n";
    os << indent << "App_name " << m_App_name << "\n";
    os << indent << "Application " << m_Application << "\n"; 
    os << indent << "Scan_fmt " << m_Scan_fmt << "\n"; 
    os << indent << "Dataset_name " << m_Dataset_name << "\n";
    os << indent << "First_tx_line " << m_First_tx_line << "\n";
    os << indent << "Last_tx_line " << m_Last_tx_line << "\n";
    os << indent << "Lines " << m_Lines << "\n";
    os << indent << "Az_lines " << m_Az_lines << "\n";
    os << indent << "Az_angle " << m_Az_angle << "\n";
    os << indent << "Az_angular_separation " << m_Az_angular_separation << "\n";
    os << indent << "El_lines" << m_El_lines << "\n";
    os << indent << "El_angle " << m_El_angle << "\n";
    os << indent << "El_angular_separation " << m_El_angular_separation << "\n";
    os << indent << "Tx_offset " << m_Tx_offset << "\n";
    os << indent << "Rx_offset " << m_Rx_offset << "\n";
    os << indent << "MclkFreq " << m_MclkFreq << "\n";
    os << indent << "SampleSize " << m_SampleSize << "\n";
    os << indent << "Mclk2Size " << m_Mclk2Size << "\n";
    os << indent << "SampleRate " << m_SampleRate << "\n";
    os << indent << "LineGroupSize " << m_LineGroupSize << "\n";
    os << indent << "NumECGSamples " << m_NumECGSamples << "\n";
    os << indent << "GrayImageSize " << m_GrayImageSize << "\n";
    os << indent << "DopplerImageSize " << m_DopplerImageSize << "\n";
    os << indent << "EcgSize " << m_EcgSize << "\n";
    os << indent << "MiscDataSize " << m_MiscDataSize << "\n";
    os << indent << "GrayImageOffset " << m_GrayImageOffset << "\n";
    os << indent << "DopplerImageOffset " << m_DopplerImageOffset << "\n";
    os << indent << "EcgOffset " << m_EcgOffset << "\n";
    os << indent << "MiscDataOffset " << m_MiscDataOffset << "\n";
    os << indent << "File_control_timing_type " << m_File_control_timing_type << "\n";
    os << indent << "DopplerVolInfo " << m_DopplerVolInfo << "\n";
    os << indent << "ScanDepthCount "  << m_ScanDepthCount << "\n"; 
    os << indent << "ScanDepth " << m_ScanDepth << "\n"; 
    os << indent << "Az_sector_tilt " << m_Az_sector_tilt << "\n"; 
    os << indent << "Elev_sector_tilt " << m_Elev_sector_tilt << "\n"; 
    os << indent << "DopplerSegData " << m_DopplerSegData << "\n"; 
    os << indent << "FrameRate " << m_FrameRate << "\n"; 
    os << indent << "Sweepspeed " << m_Sweepspeed << "\n"; 
    os << indent << "Update_interval " << m_Update_interval << "\n"; 
    os << indent << "Contrast_on " << m_Contrast_on << "\n";
    os << indent << "Comp_curve_p0_x " << m_Comp_curve_p0_x << "\n";
    os << indent << "Comp_curve_p0_y " << m_Comp_curve_p0_y << "\n";
    os << indent << "Comp_curve_p1_x " << m_Comp_curve_p1_x << "\n";
    os << indent << "Comp_curve_p1_y " << m_Comp_curve_p1_y << "\n";
    os << indent << "Comp_curve_p2_x " << m_Comp_curve_p2_x << "\n";
    os << indent << "Comp_curve_p2_y " << m_Comp_curve_p2_y << "\n";
    os << indent << "Comp_curve_p3_x " << m_Comp_curve_p3_x << "\n";
    os << indent << "Comp_curve_p3_y " << m_Comp_curve_p3_y << "\n";
    os << indent << "Comp_curve_scaling_index " << m_Comp_curve_scaling_index << "\n";
    os << indent << "Echo_reject " << m_Echo_reject << "\n";
    os << indent << "Mt_tp " << m_Mt_tp << "\n";
    os << indent << "True_axis_defined " << m_True_axis_defined << "\n";
    os << indent << "True_axis_on " << m_True_axis_on << "\n";
    os << indent << "Parallel_x_tilt " << m_Parallel_x_tilt << "\n";
    os << indent << "Parallel_y_tilt " << m_Parallel_y_tilt << "\n";
    os << indent << "Parallel_depth " << m_Parallel_depth << "\n";
    os << indent << "Parallel_spacing " << m_Parallel_spacing << "\n";
    os << indent << "Parallel_thickness " << m_Parallel_thickness << "\n";
    os << indent << "Viewport_transform_flags " << m_Viewport_transform_flags << "\n";
    os << indent << "Stress_mode " << m_Stress_mode << "\n";
    os << indent << "Stress_label " << m_Stress_label << "\n";
    os << indent << "Heart_rate " << m_Heart_rate << "\n";
    os << indent << "Stage_timer_value " << m_Stage_timer_value << "\n";
    os << indent << "Ecg_display_on " << m_Ecg_display_on << "\n"; 
    //stuff inside file_control_timing_type
    os << indent << "Blanking " << m_Blanking << "\n";
    os << indent << "Samples " << m_Samples << "\n";

    //version 1.0 specific
    os << indent << "ColorImageSize " << m_ColorImageSize << "\n";
    os << indent << "ColorImageOffset " << m_ColorImageOffset << "\n";
    os << indent << "Oag_params " << m_Oag_params << "\n";
    os << indent << "Cscanfmt " << m_Cscanfmt << "\n";
    os << indent << "Oaglinear " << m_Oaglinear << "\n";
    os << indent << "Maxradius " << m_Maxradius << "\n";
    os << indent << "Anglescale " << m_Anglescale << "\n";
    os << indent << "Skinoffset " << m_Skinoffset << "\n";
    }

}

void VOLImageIO::ReadImageInformation()
{ 
  VOLFileWrapper volfp(m_FileName.c_str()); 
  FILE* fp = volfp.m_FilePointer;
  if(!fp)
    {
    itkExceptionMacro("Error VOLImageIO could not open file: " 
                      << this->GetFileName());
    return;
    }

  ReadData(fp, m_File_rev, 12, 4);
  if      (strcmp(m_File_rev, "V1.0       ")==0) ReadVersion1_0(fp);
  else if (strcmp(m_File_rev, "V1.1       ")==0)  ReadVersion1_1(fp);
  else if ((strcmp(m_File_rev, "V2.1       ")==0) || (strcmp(m_File_rev, "V2.2       ")==0))  ReadVersion2_1(fp);
  else if (strcmp(m_File_rev, "V2.3       ")==0)  ReadVersion2_3(fp);
  else
    {
    itkExceptionMacro("Cannot read version " << m_File_rev << " in file " 
                      << this->GetFileName() << " -- will try to read as V2.3");
    ReadVersion2_3(fp);
    }

  this->m_Dimensions[0] = m_Az_lines * 4;
  this->m_Dimensions[1] = m_El_lines * 4;
  this->m_Dimensions[2] = m_Samples;
  this->m_Dimensions[3] = m_NumEchoFrames;

  this->m_Spacing[0] = m_Az_angular_separation/4.0;
  this->m_Spacing[1] = m_El_angular_separation/4.0;
  this->m_Spacing[2] = m_SampleSize;
  this->m_Spacing[3] = 1.0;

  m_Origin[0] = 0.0;
  m_Origin[1] = 0.0;
  m_Origin[2] = 0.0;
  m_Origin[3] = 0.0;

  m_Strides.resize(5);
  this->ComputeStrides();
  return;
}

void VOLImageIO::ReadVersion1_0(FILE * fp){
  ReadData(fp, m_Description, 128, 16);
  ReadData(fp, m_Date, 12,144);
  ReadData(fp, m_Time, 12, 156);
  ReadData(fp, m_Patient, 64, 168);
  ReadData(fp, m_Clinic, 64, 232);
  ReadData(fp, &m_NumEchoFrames, sizeof(m_NumEchoFrames), 296); 
  m_NumDopFrames = m_NumEchoFrames;
  ReadData(fp, m_Xducer_name, 16, 300);
  ReadData(fp, &m_Xducer_ID, sizeof(m_Xducer_ID), 316);
  ReadData(fp, &m_Xducer_freq, sizeof(m_Xducer_freq), 320);
  ReadData(fp, &m_Depth, sizeof(m_Depth), 328);
  ReadData(fp, &m_Default_depth, sizeof(m_Default_depth), 336);
  ReadData(fp, m_App_name, 24, 344);
  ReadData(fp, &m_Application, sizeof(m_Application), 368);
  ReadData(fp, &m_Scan_fmt, sizeof(m_Scan_fmt), 372);
  ReadData(fp, m_Dataset_name, 64, 376);
  ReadData(fp, &m_First_tx_line, sizeof(m_First_tx_line), 440);
  ReadData(fp, &m_Last_tx_line, sizeof(m_Last_tx_line), 442);
  ReadData(fp, &m_Lines, sizeof(m_Lines), 444);
  ReadData(fp, &m_Az_lines, sizeof(m_Az_lines), 446);
  ReadData(fp, &m_Az_angle, sizeof(m_Az_angle), 448);
  ReadData(fp, &m_Az_angular_separation, sizeof(m_Az_angular_separation), 456);
  ReadData(fp, &m_El_lines, sizeof(m_El_lines), 464);
  ReadData(fp, &m_El_angle, sizeof(m_El_angle), 466);
  ReadData(fp, &m_El_angular_separation, sizeof(m_El_angular_separation), 474);
  ReadData(fp, &m_Tx_offset, sizeof(m_Tx_offset), 482);
  ReadData(fp, &m_Rx_offset, sizeof(m_Rx_offset), 486);
  ReadData(fp, &m_MclkFreq, sizeof(m_MclkFreq), 490);
  ReadData(fp, &m_SampleSize, sizeof(m_SampleSize), 498);
  ReadData(fp, &m_Mclk2Size, sizeof(m_Mclk2Size), 506);
  ReadData(fp, &m_SampleRate, sizeof(m_SampleRate), 514);
  ReadData(fp, &m_LineGroupSize, sizeof(m_LineGroupSize), 518);
  ReadData(fp, &m_NumECGSamples, sizeof(m_NumECGSamples), 522);
  ReadData(fp, &m_GrayImageSize, sizeof(m_GrayImageSize), 526);
  ReadData(fp, &m_ColorImageSize, sizeof(m_ColorImageSize), 530);
  ReadData(fp, &m_DopplerImageSize, sizeof(m_DopplerImageSize), 534);
  ReadData(fp, &m_EcgSize, sizeof(m_EcgSize), 538);
  ReadData(fp, &m_MiscDataSize, sizeof(m_MiscDataSize), 542);
  ReadData(fp, &m_GrayImageOffset, sizeof(m_GrayImageOffset), 546);
  ReadData(fp, &m_ColorImageOffset, sizeof(m_ColorImageOffset), 550);
  ReadData(fp, &m_DopplerImageOffset, sizeof(m_DopplerImageOffset), 554);
  ReadData(fp, &m_EcgOffset, sizeof(m_EcgOffset), 558);
  ReadData(fp, &m_MiscDataOffset, sizeof(m_MiscDataOffset), 562);
  ReadData(fp, &m_File_control_timing_type, sizeof(m_File_control_timing_type), 566);
  ReadData(fp, &m_Oag_params, sizeof(m_Oag_params), 634);
  ReadData(fp, &m_Cscanfmt, sizeof(m_Cscanfmt), 634);
  ReadData(fp, &m_Oaglinear, sizeof(m_Oaglinear), 638);
  ReadData(fp, &m_Maxradius, sizeof(m_Maxradius), 642);
  ReadData(fp, &m_Anglescale , sizeof(m_Anglescale), 646);
  ReadData(fp, &m_Skinoffset, sizeof(m_Skinoffset), 654);
  m_Samples = (short)m_Maxradius;
}//ReadVersion1_0


void VOLImageIO::ReadVersion1_1(FILE * fp){
  ReadData(fp, m_Description, 128, 16);
  ReadData(fp, m_Date, 12, 144);
  ReadData(fp, m_Time, 12, 156);
  ReadData(fp, m_Patient, 64, 168);
  ReadData(fp, m_Clinic, 64, 232);
  ReadData(fp, &m_NumEchoFrames, sizeof(m_NumEchoFrames), 296); 
  m_NumDopFrames = m_NumEchoFrames;
  ReadData(fp, m_Xducer_name, 16, 300);
  ReadData(fp, &m_Xducer_ID, sizeof(m_Xducer_ID), 316);
  ReadData(fp, &m_Xducer_freq, sizeof(m_Xducer_freq), 320);
  ReadData(fp, &m_Depth, sizeof(m_Depth), 328);
  ReadData(fp, &m_Default_depth, sizeof(m_Default_depth), 336);
  ReadData(fp, m_App_name, 24, 344);
  ReadData(fp, &m_Application, sizeof(m_Application), 368);
  ReadData(fp, &m_Scan_fmt, sizeof(m_Scan_fmt), 372);
  ReadData(fp, m_Dataset_name, 64, 376);
  ReadData(fp, &m_First_tx_line, sizeof(m_First_tx_line), 440);
  ReadData(fp, &m_Last_tx_line, sizeof(m_Last_tx_line), 442);
  ReadData(fp, &m_Lines, sizeof(m_Lines), 444);
  ReadData(fp, &m_Az_lines, sizeof(m_Az_lines), 446);
  ReadData(fp, &m_Az_angle, sizeof(m_Az_angle), 448);
  ReadData(fp, &m_Az_angular_separation, sizeof(m_Az_angular_separation), 456);
  ReadData(fp, &m_El_lines, sizeof(m_El_lines), 464);
  ReadData(fp, &m_El_angle, sizeof(m_El_angle), 466);
  ReadData(fp, &m_El_angular_separation, sizeof(m_El_angular_separation), 474);
  ReadData(fp, &m_Tx_offset, sizeof(m_Tx_offset), 482);
  ReadData(fp, &m_Rx_offset, sizeof(m_Rx_offset), 486);
  ReadData(fp, &m_MclkFreq, sizeof(m_MclkFreq), 490);
  ReadData(fp, &m_SampleSize, sizeof(m_SampleSize), 498);
  ReadData(fp, &m_Mclk2Size, sizeof(m_Mclk2Size), 506);
  ReadData(fp, &m_SampleRate, sizeof(m_SampleRate), 514);
  ReadData(fp, &m_LineGroupSize, sizeof(m_LineGroupSize), 518);
  ReadData(fp, &m_NumECGSamples, sizeof(m_NumECGSamples), 522);
  ReadData(fp, &m_GrayImageSize, sizeof(m_GrayImageSize), 526);
  ReadData(fp, &m_ColorImageSize, sizeof(m_ColorImageSize), 530);
  ReadData(fp, &m_DopplerImageSize, sizeof(m_DopplerImageSize), 534);
  ReadData(fp, &m_EcgSize, sizeof(m_EcgSize), 538);
  ReadData(fp, &m_MiscDataSize, sizeof(m_MiscDataSize), 542);
  ReadData(fp, &m_GrayImageOffset, sizeof(m_GrayImageOffset), 546);
  ReadData(fp, &m_ColorImageOffset, sizeof(m_ColorImageOffset), 550);
  ReadData(fp, &m_DopplerImageOffset, sizeof(m_DopplerImageOffset), 554);
  ReadData(fp, &m_EcgOffset, sizeof(m_EcgOffset), 558);
  ReadData(fp, &m_MiscDataOffset, sizeof(m_MiscDataOffset), 562);
  ReadData(fp, &m_File_control_timing_type, sizeof(m_File_control_timing_type), 566);
  ReadData(fp, &m_Oag_params, sizeof(m_Oag_params), 838);
  ReadData(fp, &m_Cscanfmt, sizeof(m_Cscanfmt), 838);
  ReadData(fp, &m_Oaglinear, sizeof(m_Oaglinear), 842);
  ReadData(fp, &m_Maxradius, sizeof(m_Maxradius), 846);
  ReadData(fp, &m_Anglescale , sizeof(m_Anglescale), 850);
  ReadData(fp, &m_Skinoffset, sizeof(m_Skinoffset), 858);
  ReadData(fp, &m_ScanDepthCount, sizeof(m_ScanDepthCount), 866); 
  ReadData(fp, &m_ScanDepth, sizeof(m_ScanDepth), 868); 
  m_Samples = (short)m_Maxradius;
}//ReadVersion1_1


void VOLImageIO::ReadVersion2_1(FILE * fp){
  ReadData(fp, m_Description, 128, 16);
  ReadData(fp, m_Date, 12,144);
  ReadData(fp, m_Time, 12, 156);
  ReadData(fp, m_Patient, 64, 168);
  ReadData(fp, m_Clinic, 64, 232);
  ReadData(fp, &m_NumEchoFrames, sizeof(m_NumEchoFrames), 296); 
  ReadData(fp, &m_NumDopFrames, sizeof(m_NumDopFrames), 300); 
  ReadData(fp, &m_Dopmode, sizeof(m_Dopmode), 304); 
  ReadData(fp, &m_EchoLPF, sizeof(m_EchoLPF), 305); 
  ReadData(fp, &m_DopLPF, sizeof(m_DopLPF), 309); 
  ReadData(fp, &m_Repetition, sizeof(m_Repetition), 313); 
  ReadData(fp, m_Xducer_name, 16, 317);
  ReadData(fp, &m_Xducer_ID, sizeof(m_Xducer_ID), 333);
  ReadData(fp, &m_Xducer_freq, sizeof(m_Xducer_freq), 337);
  ReadData(fp, &m_Depth, sizeof(m_Depth), 345);
  ReadData(fp, &m_Default_depth, sizeof(m_Default_depth), 353);
  ReadData(fp, m_App_name, 24, 361);
  ReadData(fp, &m_Application, sizeof(m_Application), 385);
  ReadData(fp, &m_Scan_fmt, sizeof(m_Scan_fmt), 386);
  ReadData(fp, m_Dataset_name, 64, 390);
  ReadData(fp, &m_First_tx_line, sizeof(m_First_tx_line), 454);
  ReadData(fp, &m_Last_tx_line, sizeof(m_Last_tx_line), 456);
  ReadData(fp, &m_Lines, sizeof(m_Lines), 458);
  ReadData(fp, &m_Az_lines, sizeof(m_Az_lines), 460);
  ReadData(fp, &m_Az_angle, sizeof(m_Az_angle), 462);
  ReadData(fp, &m_Az_angular_separation, sizeof(m_Az_angular_separation), 470);
  ReadData(fp, &m_El_lines, sizeof(m_El_lines), 478);
  ReadData(fp, &m_El_angle, sizeof(m_El_angle), 480);
  ReadData(fp, &m_El_angular_separation, sizeof(m_El_angular_separation), 488);
  ReadData(fp, &m_Tx_offset, sizeof(m_Tx_offset), 496);
  ReadData(fp, &m_Rx_offset, sizeof(m_Rx_offset), 500);
  ReadData(fp, &m_MclkFreq, sizeof(m_MclkFreq), 504);
  ReadData(fp, &m_SampleSize, sizeof(m_SampleSize), 512);
  ReadData(fp, &m_Mclk2Size, sizeof(m_Mclk2Size), 520);
  ReadData(fp, &m_SampleRate, sizeof(m_SampleRate), 528);
  ReadData(fp, &m_LineGroupSize, sizeof(m_LineGroupSize), 532);
  ReadData(fp, &m_NumECGSamples, sizeof(m_NumECGSamples), 536);
  ReadData(fp, &m_GrayImageSize, sizeof(m_GrayImageSize), 540);
  ReadData(fp, &m_DopplerImageSize, sizeof(m_DopplerImageSize), 544);
  ReadData(fp, &m_EcgSize, sizeof(m_EcgSize), 548);
  ReadData(fp, &m_MiscDataSize, sizeof(m_MiscDataSize), 552);
  ReadData(fp, &m_GrayImageOffset, sizeof(m_GrayImageOffset), 556);
  ReadData(fp, &m_DopplerImageOffset, sizeof(m_DopplerImageOffset), 560);
  ReadData(fp, &m_EcgOffset, sizeof(m_EcgOffset), 564);
  ReadData(fp, &m_MiscDataOffset, sizeof(m_MiscDataOffset), 568);
  ReadData(fp, &m_File_control_timing_type, sizeof(m_File_control_timing_type), 572);
  ReadData(fp, &m_DopplerVolInfo, sizeof(m_DopplerVolInfo), 844); 
  ReadData(fp, &m_ScanDepthCount, sizeof(m_ScanDepthCount), 1833); 
  ReadData(fp, &m_ScanDepth, sizeof(m_ScanDepth), 1835); 
  ReadData(fp, &m_Az_sector_tilt, sizeof(m_Az_sector_tilt), 1851); 
  ReadData(fp, &m_Elev_sector_tilt, sizeof(m_Elev_sector_tilt), 1859); 
  ReadData(fp, &m_DopplerSegData, sizeof(m_DopplerSegData), 1867); 
  ReadData(fp, &m_FrameRate, sizeof(m_FrameRate), 1935); 
  ReadData(fp, &m_Sweepspeed, sizeof(m_Sweepspeed), 1943); 
  ReadData(fp, &m_Update_interval, sizeof(m_Update_interval), 1947); 

  //file_control_timing_type stuff
  ReadData(fp, &m_Blanking, sizeof(m_Blanking), 596); 
//  ReadData(fp, &m_Samples, sizeof(m_Samples), 604); 
  ReadData(fp, &m_Samples, sizeof(m_Samples), 706); 

  m_Maxradius = (unsigned long)m_Samples;
}//ReadVersion2_1




void VOLImageIO::ReadVersion2_3(FILE * fp){
  ReadData(fp, m_Description, 128, 16);
  ReadData(fp, m_Date, 12,144);
  ReadData(fp, m_Time, 12, 156);
  ReadData(fp, m_Patient, 64, 168);
  ReadData(fp, m_Clinic, 64, 232);
  ReadData(fp, &m_NumEchoFrames, sizeof(m_NumEchoFrames), 296); 
  ReadData(fp, &m_NumDopFrames, sizeof(m_NumDopFrames), 300); 
  ReadData(fp, &m_Dopmode, sizeof(m_Dopmode), 304); 
  ReadData(fp, &m_EchoLPF, sizeof(m_EchoLPF), 305); 
  ReadData(fp, &m_DopLPF, sizeof(m_DopLPF), 309); 
  ReadData(fp, &m_Repetition, sizeof(m_Repetition), 313); 
  ReadData(fp, m_Xducer_name, 16, 317);
  ReadData(fp, &m_Xducer_ID, sizeof(m_Xducer_ID), 333);
  ReadData(fp, &m_Xducer_freq, sizeof(m_Xducer_freq), 337);
  ReadData(fp, &m_Depth, sizeof(m_Depth), 345);
  ReadData(fp, &m_Default_depth, sizeof(m_Default_depth), 353);
  ReadData(fp, m_App_name, 24, 361);
  ReadData(fp, &m_Application, sizeof(m_Application), 385);
  ReadData(fp, &m_Scan_fmt, sizeof(m_Scan_fmt), 386);
  ReadData(fp, m_Dataset_name, 64, 390);
  ReadData(fp, &m_First_tx_line, sizeof(m_First_tx_line), 454);
  ReadData(fp, &m_Last_tx_line, sizeof(m_Last_tx_line), 456);
  ReadData(fp, &m_Lines, sizeof(m_Lines), 458);
  ReadData(fp, &m_Az_lines, sizeof(m_Az_lines), 460);
  ReadData(fp, &m_Az_angle, sizeof(m_Az_angle), 462);
  ReadData(fp, &m_Az_angular_separation, sizeof(m_Az_angular_separation), 470);
  ReadData(fp, &m_El_lines, sizeof(m_El_lines), 478);
  ReadData(fp, &m_El_angle, sizeof(m_El_angle), 480);
  ReadData(fp, &m_El_angular_separation, sizeof(m_El_angular_separation), 488);
  ReadData(fp, &m_Tx_offset, sizeof(m_Tx_offset), 496);
  ReadData(fp, &m_Rx_offset, sizeof(m_Rx_offset), 500);
  ReadData(fp, &m_MclkFreq, sizeof(m_MclkFreq), 504);
  ReadData(fp, &m_SampleSize, sizeof(m_SampleSize), 512);
  ReadData(fp, &m_Mclk2Size, sizeof(m_Mclk2Size), 520);
  ReadData(fp, &m_SampleRate, sizeof(m_SampleRate), 528);
  ReadData(fp, &m_LineGroupSize, sizeof(m_LineGroupSize), 532);
  ReadData(fp, &m_NumECGSamples, sizeof(m_NumECGSamples), 536);
  ReadData(fp, &m_GrayImageSize, sizeof(m_GrayImageSize), 540);
  ReadData(fp, &m_DopplerImageSize, sizeof(m_DopplerImageSize), 544);
  ReadData(fp, &m_EcgSize, sizeof(m_EcgSize), 548);
  ReadData(fp, &m_MiscDataSize, sizeof(m_MiscDataSize), 552);
  ReadData(fp, &m_GrayImageOffset, sizeof(m_GrayImageOffset), 556);
  ReadData(fp, &m_DopplerImageOffset, sizeof(m_DopplerImageOffset), 560);
  ReadData(fp, &m_EcgOffset, sizeof(m_EcgOffset), 564);
  ReadData(fp, &m_MiscDataOffset, sizeof(m_MiscDataOffset), 568);
  ReadData(fp, &m_File_control_timing_type, sizeof(m_File_control_timing_type), 572);
  ReadData(fp, &m_DopplerVolInfo, sizeof(m_DopplerVolInfo), 844); 
  ReadData(fp, &m_ScanDepthCount, sizeof(m_ScanDepthCount), 1833); 
  ReadData(fp, &m_ScanDepth, sizeof(m_ScanDepth), 1835); 
  ReadData(fp, &m_Az_sector_tilt, sizeof(m_Az_sector_tilt), 1851); 
  ReadData(fp, &m_Elev_sector_tilt, sizeof(m_Elev_sector_tilt), 1859); 
  ReadData(fp, &m_DopplerSegData, sizeof(m_DopplerSegData), 1867); 
  ReadData(fp, &m_FrameRate, sizeof(m_FrameRate), 1935); 
  ReadData(fp, &m_Sweepspeed, sizeof(m_Sweepspeed), 1943); 
  ReadData(fp, &m_Update_interval, sizeof(m_Update_interval), 1947); 
  ReadData(fp, &m_Contrast_on, sizeof(m_Contrast_on), 1951);
  ReadData(fp, &m_Comp_curve_p0_x, sizeof(m_Comp_curve_p0_x), 1955); 
  ReadData(fp, &m_Comp_curve_p0_y, sizeof(m_Comp_curve_p0_y), 1959); 
  ReadData(fp, &m_Comp_curve_p1_x, sizeof(m_Comp_curve_p1_x), 1963); 
  ReadData(fp, &m_Comp_curve_p1_y, sizeof(m_Comp_curve_p1_y), 1967); 
  ReadData(fp, &m_Comp_curve_p2_x, sizeof(m_Comp_curve_p2_x), 1971); 
  ReadData(fp, &m_Comp_curve_p2_x, sizeof(m_Comp_curve_p2_y), 1975); 
  ReadData(fp, &m_Comp_curve_p3_x, sizeof(m_Comp_curve_p3_x), 1979); 
  ReadData(fp, &m_Comp_curve_p3_x, sizeof(m_Comp_curve_p3_y), 1983); 
  ReadData(fp, &m_Comp_curve_scaling_index, sizeof(m_Comp_curve_scaling_index), 1987);
  ReadData(fp, &m_Echo_reject, sizeof(m_Echo_reject), 1991);
  ReadData(fp, &m_Mt_tp, sizeof(m_Mt_tp), 1995);
  ReadData(fp, &m_True_axis_defined, sizeof(m_True_axis_defined), 2123);
  ReadData(fp, &m_True_axis_on, sizeof(m_True_axis_on), 2127);
  ReadData(fp, &m_Parallel_x_tilt, sizeof(m_Parallel_x_tilt), 2131);
  ReadData(fp, &m_Parallel_y_tilt, sizeof(m_Parallel_y_tilt), 2139);
  ReadData(fp, &m_Parallel_depth, sizeof(m_Parallel_depth), 2147);
  ReadData(fp, &m_Parallel_spacing, sizeof(m_Parallel_spacing), 2155);
  ReadData(fp, &m_Parallel_thickness, sizeof(m_Parallel_thickness), 2163);
  ReadData(fp, &m_Viewport_transform_flags, sizeof(m_Viewport_transform_flags), 2167);
  ReadData(fp, &m_Stress_mode, sizeof(m_Stress_mode), 2171);
  ReadData(fp, &m_Stress_label, sizeof(m_Stress_label), 2175);
  ReadData(fp, &m_Heart_rate, sizeof(m_Heart_rate), 2239);
  ReadData(fp, &m_Stage_timer_value, sizeof(m_Stage_timer_value), 2247);
  ReadData(fp, &m_Ecg_display_on, sizeof(m_Ecg_display_on), 2251);

  //file_control_timing_type stuff
  ReadData(fp, &m_Blanking, sizeof(m_Blanking), 596); 
//  ReadData(fp, &m_Samples, sizeof(m_Samples), 604); 
  ReadData(fp, &m_Samples, sizeof(m_Samples), 706); 

  m_Maxradius = (unsigned long)m_Samples;
}//ReadVersion2_3

void VOLImageIO::ReadData(FILE * fp, char * buffer, size_t size, long pos)
{
  // for straight characters
  // assume size is correct and just read
  // if not default, then actually seek

  if (pos != -1)
    fseek(fp, pos, SEEK_SET);

  fread((unsigned char *)buffer, 1, size, fp);
}

void VOLImageIO::ReadData(FILE * fp, void * buffer, size_t size, long pos)
{
  // for any kind of data using void pointer
  // assume size is correct and just read
  // if not default, then actually seek

  if (pos != -1)
    fseek(fp, pos, SEEK_SET);

  fread((unsigned char *) buffer, 1, size, fp);


#ifndef _WIN32
  // reverse byte order for big-endian machines, which i guess would be not win32?
  unsigned char * puch = (unsigned char *)buffer;
  unsigned char c;
  for (unsigned u = 0; u < size/2; u++)
    {
    c = puch[u];
    puch[u] = puch[size-1-u];
    puch[size-1-u] = c;
    }
#endif

} 


} // namespace itk
