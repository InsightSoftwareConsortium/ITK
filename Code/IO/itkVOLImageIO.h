/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVOLImageIO.h
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
#ifndef __itkVOLImageIO_h
#define __itkVOLImageIO_h

#include "itkImageIOBase.h"
#include "stdio.h"
#ifndef VOL_MAGIC_NUMBER
#define VOL_MAGIC_NUMBER 0x21654387
#endif


namespace itk
{

/**
 * \class VOLImageIO
 * \brief Read and write .VOL 3D ultrasound images.
 *
 * \sa ImageFileReader
 * 
 * \ingroup IOFilters
 * */

class ITK_EXPORT VOLImageIO : public ImageIOBase
{
public:

  /**
   * Smart pointer typedef support.
   */
  typedef VOLImageIO Self;
  typedef SmartPointer<Self>  Pointer;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageIOBase  Superclass;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(VOLImageIO, Superclass);

  /**
   * Determine the file type. Returns true if this ImageIOBase can read the
   * file specified.  Reads in the header as well.
   */
  virtual bool CanReadFile(const char*);

  ///! Set the spacing and diemention information for the set filename.
  virtual void ReadImageInformation();
  
  /**
   * Get the type of the pixel. 
   */
  virtual const std::type_info& GetPixelType() const ;
  
  /**
   * Loads the data from disk into the memory buffer provided.
   */
  virtual void Load(void* buffer);

  /**
   * Get the image spacing.
   */
  virtual const double* GetSpacing() const;
	const double* GetOrigin() const;


	unsigned int GetComponentSize() const;

	itkGetMacro(Blanking, double);

protected:
  VOLImageIO();
  ~VOLImageIO();
  VOLImageIO(const Self&) {}
  void operator=(const Self&) {}

  void PrintSelf(std::ostream& os, Indent indent) const;
	ComponentType		m_VOLPixelType;
  double m_Spacing[4] ;
  double m_Origin[4] ;

	void ReadData(FILE * fp, void * buffer, size_t size, long pos = -1);
	void ReadData(FILE * fp, char * buffer, size_t size, long pos = -1);


private:

	/**
	 *  All of the information read in from the header file
	 */

	unsigned long		m_File_type;
	char						m_File_rev[12];      
	char						m_Description[128];     
	char						m_Date[12];
	char						m_Time[12];   
	char						m_Patient[64];
	char						m_Clinic[64];        
	unsigned long		m_NumEchoFrames; 
	unsigned long		m_NumDopFrames; 
	char						m_Dopmode; 
	long						m_EchoLPF; 
	long						m_DopLPF; 
	long						m_Repetition; 
	char						m_Xducer_name[16];
	long						m_Xducer_ID;
	double					m_Xducer_freq;
	double					m_Depth;
	double					m_Default_depth;
	char						m_App_name[24];
	char						m_Application; 
	long						m_Scan_fmt; 
	char						m_Dataset_name[64];
	short						m_First_tx_line;
	short						m_Last_tx_line;
	short						m_Lines;
	short						m_Az_lines;
	double					m_Az_angle;
	double					m_Az_angular_separation;
	short						m_El_lines;
	double					m_El_angle;
	double					m_El_angular_separation;
	long						m_Tx_offset;
	long						m_Rx_offset;
	double					m_MclkFreq;
	double					m_SampleSize;
	double					m_Mclk2Size;
	long						m_SampleRate;
	unsigned long		m_LineGroupSize;
	unsigned long		m_NumECGSamples;
	unsigned long		m_GrayImageSize;
	unsigned long		m_DopplerImageSize;
	unsigned long		m_EcgSize;
	unsigned long		m_MiscDataSize;
	unsigned long		m_GrayImageOffset;
	unsigned long		m_DopplerImageOffset;
	unsigned long		m_EcgOffset;
	unsigned long		m_MiscDataOffset;
	char						m_File_control_timing_type[272];
	char						m_DopplerVolInfo[989];
	short						m_ScanDepthCount; 
	short						m_ScanDepth; 
	double					m_Az_sector_tilt; 
	double					m_Elev_sector_tilt; 
	char						m_DopplerSegData[68]; 
	double					m_FrameRate; 
	long						m_Sweepspeed; 
	long						m_Update_interval; 
	unsigned long		m_Contrast_on;
	long						m_Comp_curve_p0_x;
	long						m_Comp_curve_p0_y;
	long						m_Comp_curve_p1_x;
	long						m_Comp_curve_p1_y;
	long						m_Comp_curve_p2_x;
	long						m_Comp_curve_p2_y;
	long						m_Comp_curve_p3_x;
	long						m_Comp_curve_p3_y;
	long						m_Comp_curve_scaling_index;
	long						m_Echo_reject;
	double					m_Mt_tp[4][4];
	unsigned long		m_True_axis_defined;
	unsigned long		m_True_axis_on;
	double					m_Parallel_x_tilt;
	double					m_Parallel_y_tilt;
	double					m_Parallel_depth;
	double					m_Parallel_spacing;
	long						m_Parallel_thickness;
	unsigned long		m_Viewport_transform_flags;
	unsigned long		m_Stress_mode;
	char						m_Stress_label[64];
	double					m_Heart_rate;
	long						m_Stage_timer_value;
	unsigned long		m_Ecg_display_on;	
	//stuff inside file_control_timing_type
	double					m_Blanking;
};


} // namespace itk


#endif 

