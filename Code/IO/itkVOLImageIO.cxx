/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVOLImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

void VOLImageIO::Load(void* buffer)
{

  // use this class so return will call close
  VOLFileWrapper volfp(this->GetFileName()); 
  FILE* fp = volfp.m_FilePointer;
  if(!fp)
		{
    itkErrorMacro("Error VOLImageIO could not open file: " 
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

const double* VOLImageIO::GetOrigin() const
{
  return m_Origin;
}


const double* VOLImageIO::GetSpacing() const
{
  return m_Spacing;
}


const double** VOLImageIO::GetMt_tp() const
{
	return (const double**)m_Mt_tp;
}


VOLImageIO::VOLImageIO()
{
  m_VOLPixelType = UCHAR;
	this->SetNumberOfComponents(1);
  this->SetNumberOfDimensions(4);
}

VOLImageIO::~VOLImageIO()
{
}



void VOLImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "VOLPixelType " << m_VOLPixelType << "\n";
}

void VOLImageIO::ReadImageInformation()
{ 
  VOLFileWrapper volfp(m_FileName.c_str()); 
  FILE* fp = volfp.m_FilePointer;
  if(!fp)
		{
    itkErrorMacro("Error VOLImageIO could not open file: " 
                  << this->GetFileName());
    return;
		}

	ReadData(fp, m_File_rev, 12);
	ReadData(fp, m_Description, 128);
	ReadData(fp, m_Date, 12);
	ReadData(fp, m_Time, 12);
	ReadData(fp, m_Patient, 64);
	ReadData(fp, m_Clinic, 64);
	ReadData(fp, &m_NumEchoFrames, sizeof(m_NumEchoFrames), 296); 
	ReadData(fp, &m_NumDopFrames, sizeof(m_NumDopFrames), 300); 
	ReadData(fp, &m_Dopmode, sizeof(m_Dopmode), 304); 
	ReadData(fp, &m_EchoLPF, sizeof(m_EchoLPF), 305); 
	ReadData(fp, &m_DopLPF, sizeof(m_DopLPF), 309); 
	ReadData(fp, &m_Repetition, sizeof(m_Repetition), 313); 
	ReadData(fp, m_Xducer_name, 16);
	ReadData(fp, &m_Xducer_ID, sizeof(m_Xducer_ID));
	m_Xducer_freq = 0;
	ReadData(fp, &m_Xducer_freq, sizeof(m_Xducer_freq), 337);
	ReadData(fp, &m_Depth, sizeof(m_Depth), 345);
	ReadData(fp, &m_Default_depth, sizeof(m_Default_depth), 353);
	ReadData(fp, m_App_name, 24);
	ReadData(fp, &m_Application, sizeof(m_Application), 385);
	ReadData(fp, &m_Scan_fmt, sizeof(m_Scan_fmt), 386);
	ReadData(fp, m_Dataset_name, 64);
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

	this->m_Dimensions[0] = m_Az_lines * 4;
	this->m_Dimensions[1] = m_El_lines * 4;
	this->m_Dimensions[2] = m_EchoLPF;
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
