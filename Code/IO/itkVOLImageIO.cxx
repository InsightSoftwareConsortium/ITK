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
   return this->ReadHeader(file);
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

	ReadData(fp, &file_type, sizeof(file_type), 0);
  if (file_type != VOL_MAGIC_NUMBER)
		{
    itkErrorMacro("Error File is not .VOL type" << this->GetFileName());
    return;
		}


  // Read the image
	unsigned char *tempImage = static_cast<unsigned char*>(buffer);
	unsigned char * imgset = new unsigned char [256*512*4*4];
	for (int timeCounter = 0; timeCounter < m_Dimensions[3]; timeCounter++)
		{
		ReadData(fp, (char *)imgset, 
						 sizeof(unsigned char) * 256*512*4*4, 
						 grayImageOffset + timeCounter*256*512*4*4);
		for(int i=0; i < m_Dimensions[2]; i++)
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


bool VOLImageIO::ReadHeader(const char* fname)
{ 
	VOLFileWrapper volfp(fname); 
  FILE* fp = volfp.m_FilePointer;
  if(!fp)
		{
    itkErrorMacro("Error VOLImageIO could not open file: " 
                  << this->GetFileName());
    return false;
		}

	ReadData(fp, &file_type, sizeof(file_type), 0);
  if (file_type != VOL_MAGIC_NUMBER)
		{
    itkErrorMacro("Error File is not .VOL type" << this->GetFileName());
    return false;
		}

	ReadData(fp, &file_type, sizeof(file_type), 0);
	ReadData(fp, file_rev, 12);
	ReadData(fp, description, 128);
	ReadData(fp, date, 12);
	ReadData(fp, time, 12);
	ReadData(fp, patient, 64);
	ReadData(fp, clinic, 64);
	ReadData(fp, &numEchoFrames, sizeof(numEchoFrames), 296); 
	ReadData(fp, &numDopFrames, sizeof(numDopFrames), 300); 
	ReadData(fp, &dopmode, sizeof(dopmode), 304); 
	ReadData(fp, &EchoLPF, sizeof(EchoLPF), 305); 
	ReadData(fp, &DopLPF, sizeof(DopLPF), 309); 
	ReadData(fp, &Repetition, sizeof(Repetition), 313); 
	ReadData(fp, xducer_name, 16);
	ReadData(fp, &xducer_ID, sizeof(xducer_ID));
	xducer_freq = 0;
	ReadData(fp, &xducer_freq, sizeof(xducer_freq), 337);
	ReadData(fp, &depth, sizeof(depth), 345);
	ReadData(fp, &default_depth, sizeof(default_depth), 353);
	ReadData(fp, app_name, 24);
	ReadData(fp, &application, sizeof(application), 385);
	ReadData(fp, &scan_fmt, sizeof(scan_fmt), 386);
	ReadData(fp, dataset_name, 64);
	ReadData(fp, &first_tx_line, sizeof(first_tx_line), 454);
	ReadData(fp, &last_tx_line, sizeof(last_tx_line), 456);
	ReadData(fp, &lines, sizeof(lines), 458);
	ReadData(fp, &az_lines, sizeof(az_lines), 460);
	ReadData(fp, &az_angle, sizeof(az_angle), 462);
	ReadData(fp, &az_angular_separation, sizeof(az_angular_separation), 470);
	ReadData(fp, &el_lines, sizeof(el_lines), 478);
	ReadData(fp, &el_angle, sizeof(el_angle), 480);
	ReadData(fp, &el_angular_separation, sizeof(el_angular_separation), 488);
	ReadData(fp, &tx_offset, sizeof(tx_offset), 496);
	ReadData(fp, &rx_offset, sizeof(rx_offset), 500);
	ReadData(fp, &mclkFreq, sizeof(mclkFreq), 504);
	ReadData(fp, &sampleSize, sizeof(sampleSize), 512);
	ReadData(fp, &mclk2Size, sizeof(mclk2Size), 520);
	ReadData(fp, &sampleRate, sizeof(sampleRate), 528);
	ReadData(fp, &lineGroupSize, sizeof(lineGroupSize), 532);
	ReadData(fp, &numECGSamples, sizeof(numECGSamples), 536);
	ReadData(fp, &grayImageSize, sizeof(grayImageSize), 540);
	ReadData(fp, &dopplerImageSize, sizeof(dopplerImageSize), 544);
	ReadData(fp, &ecgSize, sizeof(ecgSize), 548);
	ReadData(fp, &miscDataSize, sizeof(miscDataSize), 552);
	ReadData(fp, &grayImageOffset, sizeof(grayImageOffset), 556);
	ReadData(fp, &dopplerImageOffset, sizeof(dopplerImageOffset), 560);
	ReadData(fp, &ecgOffset, sizeof(ecgOffset), 564);
	ReadData(fp, &miscDataOffset, sizeof(miscDataOffset), 568);
	ReadData(fp, &file_control_timing_type, sizeof(file_control_timing_type), 572);
	ReadData(fp, &DopplerVolInfo, sizeof(DopplerVolInfo), 844); 
	ReadData(fp, &scanDepthCount, sizeof(scanDepthCount), 1833); 
	ReadData(fp, &scanDepth, sizeof(scanDepth), 1835); 
	ReadData(fp, &az_sector_tilt, sizeof(az_sector_tilt), 1851); 
	ReadData(fp, &elev_sector_tilt, sizeof(elev_sector_tilt), 1859); 
	ReadData(fp, &DopplerSegData, sizeof(DopplerSegData), 1867); 
	ReadData(fp, &FrameRate, sizeof(FrameRate), 1935); 
	ReadData(fp, &sweepspeed, sizeof(sweepspeed), 1943); 
	ReadData(fp, &update_interval, sizeof(update_interval), 1947); 
	ReadData(fp, &contrast_on, sizeof(contrast_on), 1951);
	ReadData(fp, &comp_curve_p0_x, sizeof(comp_curve_p0_x), 1955); 
	ReadData(fp, &comp_curve_p0_y, sizeof(comp_curve_p0_y), 1959); 
	ReadData(fp, &comp_curve_p1_x, sizeof(comp_curve_p1_x), 1963); 
	ReadData(fp, &comp_curve_p1_y, sizeof(comp_curve_p1_y), 1967); 
	ReadData(fp, &comp_curve_p2_x, sizeof(comp_curve_p2_x), 1971); 
	ReadData(fp, &comp_curve_p2_x, sizeof(comp_curve_p2_y), 1975); 
	ReadData(fp, &comp_curve_p3_x, sizeof(comp_curve_p3_x), 1979); 
	ReadData(fp, &comp_curve_p3_x, sizeof(comp_curve_p3_y), 1983); 
	ReadData(fp, &comp_curve_scaling_index, sizeof(comp_curve_scaling_index), 1987);
	ReadData(fp, &echo_reject, sizeof(echo_reject), 1991);
	ReadData(fp, &Mt_tp, sizeof(Mt_tp), 1995);
	ReadData(fp, &true_axis_defined, sizeof(true_axis_defined), 2123);
	ReadData(fp, &true_axis_on, sizeof(true_axis_on), 2127);
	ReadData(fp, &parallel_x_tilt, sizeof(parallel_x_tilt), 2131);
	ReadData(fp, &parallel_y_tilt, sizeof(parallel_y_tilt), 2139);
	ReadData(fp, &parallel_depth, sizeof(parallel_depth), 2147);
	ReadData(fp, &parallel_spacing, sizeof(parallel_spacing), 2155);
	ReadData(fp, &parallel_thickness, sizeof(parallel_thickness), 2163);
	ReadData(fp, &viewport_transform_flags, sizeof(viewport_transform_flags), 2167);
	ReadData(fp, &stress_mode, sizeof(stress_mode), 2171);
	ReadData(fp, &stress_label, sizeof(stress_label), 2175);
	ReadData(fp, &heart_rate, sizeof(heart_rate), 2239);
	ReadData(fp, &stage_timer_value, sizeof(stage_timer_value), 2247);
	ReadData(fp, &ecg_display_on, sizeof(ecg_display_on), 2251);

	this->m_Dimensions[0] = az_lines * 4;
	this->m_Dimensions[1] = el_lines * 4;
	this->m_Dimensions[2] = EchoLPF;
	this->m_Dimensions[3] = numEchoFrames;

	m_Strides.resize(5);

  this->ComputeStrides();

	return true;
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
