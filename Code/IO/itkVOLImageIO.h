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

	unsigned long		file_type;
	char						file_rev[12];      
	char						description[128];     
	char						date[12];
	char						time[12];   
	char						patient[64];
	char						clinic[64];        
	unsigned long		numEchoFrames; 
	unsigned long		numDopFrames; 
	char						dopmode; 
	long						EchoLPF; 
	long						DopLPF; 
	long						Repetition; 
	char						xducer_name[16];
	long						xducer_ID;
	double					xducer_freq;
	double					depth;
	double					default_depth;
	char						app_name[24];
	char						application; 
	long						scan_fmt; 
	char						dataset_name[64];
	short						first_tx_line;
	short						last_tx_line;
	short						lines;
	short						az_lines;
	double					az_angle;
	double					az_angular_separation;
	short						el_lines;
	double					el_angle;
	double					el_angular_separation;
	long						tx_offset;
	long						rx_offset;
	double					mclkFreq;
	double					sampleSize;
	double					mclk2Size;
	long						sampleRate;
	unsigned long		lineGroupSize;
	unsigned long		numECGSamples;
	unsigned long		grayImageSize;
	unsigned long		dopplerImageSize;
	unsigned long		ecgSize;
	unsigned long		miscDataSize;
	unsigned long		grayImageOffset;
	unsigned long		dopplerImageOffset;
	unsigned long		ecgOffset;
	unsigned long		miscDataOffset;
	char						file_control_timing_type[272];
	char						DopplerVolInfo[989];
	short						scanDepthCount; 
	short						scanDepth; 
	double					az_sector_tilt; 
	double					elev_sector_tilt; 
	char						DopplerSegData[68]; 
	double					FrameRate; 
	long						sweepspeed; 
	long						update_interval; 
	unsigned long		contrast_on;
	long						comp_curve_p0_x;
	long						comp_curve_p0_y;
	long						comp_curve_p1_x;
	long						comp_curve_p1_y;
	long						comp_curve_p2_x;
	long						comp_curve_p2_y;
	long						comp_curve_p3_x;
	long						comp_curve_p3_y;
	long						comp_curve_scaling_index;
	long						echo_reject;
	double					Mt_tp[4][4];
	unsigned long		true_axis_defined;
	unsigned long		true_axis_on;
	double					parallel_x_tilt;
	double					parallel_y_tilt;
	double					parallel_depth;
	double					parallel_spacing;
	long						parallel_thickness;
	unsigned long		viewport_transform_flags;
	unsigned long		stress_mode;
	char						stress_label[64];
	double					heart_rate;
	long						stage_timer_value;
	unsigned long		ecg_display_on;	
};


} // namespace itk


#endif 

