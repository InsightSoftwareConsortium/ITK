/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVOLImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVOLImageIO_h
#define __itkVOLImageIO_h

#include "itkImageIOBase.h"
#include "stdio.h"
#include <string>

#ifndef VOL_MAGIC_NUMBER
#define VOL_MAGIC_NUMBER 0x21654387
#endif


namespace itk
{

/** \class VOLImageIO
 * \brief Read and write .VOL 3D ultrasound images.
 *
 * \sa ImageFileReader
 * 
 * \ingroup IOFilters
 */
class ITK_EXPORT VOLImageIO : public ImageIOBase
{
public:
  /** Smart pointer typedef support. */
  typedef VOLImageIO Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VOLImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine the file type. Returns true if this ImageIOBase can read the
   * file specified.  Reads in the header as well. */
  virtual bool CanReadFile(const char*);

  /** Set the spacing and diemention information for the set filename. */
  virtual void ReadImageInformation();
  void ReadVersion1_0(FILE * fp);
  void ReadVersion1_1(FILE * fp);
  void ReadVersion2_1(FILE * fp);
  void ReadVersion2_3(FILE * fp);
  
  /** Get the type of the pixel.  */
  virtual const std::type_info& GetPixelType() const ;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char*)
    { return false; }

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation() {};
  
  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  virtual void Write(const void*)
    { return; }

  /** Methods that provide information about the data file. This
   * information is only valid after the file is read successfully. */
  unsigned int GetComponentSize() const;
  itkGetMacro(File_type, unsigned long);
  itkGetMacro(File_rev, char*);
  itkGetMacro(Description, char*);                 
  itkGetMacro(Date, char*);           
  itkGetMacro(Time, char*);              
  itkGetMacro(Patient, char*);            
  itkGetMacro(Clinic, char*);                   
  itkGetMacro(NumEchoFrames, unsigned long);     
  itkGetMacro(NumDopFrames, unsigned long);    
  itkGetMacro(Dopmode, char);            
  itkGetMacro(EchoLPF, long);            
  itkGetMacro(DopLPF, long);             
  itkGetMacro(Repetition, long);             
  itkGetMacro(Xducer_name, char*);            
  itkGetMacro(Xducer_ID, long);           
  itkGetMacro(Xducer_freq, double);         
  itkGetMacro(Depth, double);         
  itkGetMacro(Default_depth, double);         
  itkGetMacro(App_name, char*);           
  itkGetMacro(Application, char);            
  itkGetMacro(Scan_fmt, long);             
  itkGetMacro(Dataset_name, char*);           
  itkGetMacro(First_tx_line, short);            
  itkGetMacro(Last_tx_line, short);           
  itkGetMacro(Lines, short);            
  itkGetMacro(Az_lines, short);           
  itkGetMacro(Az_angle, double);          
  itkGetMacro(Az_angular_separation, double);         
  itkGetMacro(El_lines, short);           
  itkGetMacro(El_angle, double);          
  itkGetMacro(El_angular_separation, double);         
  itkGetMacro(Tx_offset, long);           
  itkGetMacro(Rx_offset, long);           
  itkGetMacro(MclkFreq, double);          
  itkGetMacro(SampleSize, double);          
  itkGetMacro(Mclk2Size, double);         
  itkGetMacro(SampleRate, long);            
  itkGetMacro(LineGroupSize, unsigned long);    
  itkGetMacro(NumECGSamples, unsigned long);    
  itkGetMacro(GrayImageSize, unsigned long);    
  itkGetMacro(DopplerImageSize, unsigned long);   
  itkGetMacro(EcgSize, unsigned long);    
  itkGetMacro(MiscDataSize, unsigned long);   
  itkGetMacro(GrayImageOffset, unsigned long);    
  itkGetMacro(DopplerImageOffset, unsigned long);   
  itkGetMacro(EcgOffset, unsigned long);    
  itkGetMacro(MiscDataOffset, unsigned long);   
  itkGetMacro(File_control_timing_type, char*);           
  itkGetMacro(DopplerVolInfo, char*);           
  itkGetMacro(ScanDepthCount,  short);            
  itkGetMacro(ScanDepth,  short);           
  itkGetMacro(Az_sector_tilt, double);           
  itkGetMacro(Elev_sector_tilt, double);           
  itkGetMacro(DopplerSegData, char*);            
  itkGetMacro(FrameRate, double);          
  itkGetMacro(Sweepspeed, long);             
  itkGetMacro(Update_interval, long);            
  itkGetMacro(Contrast_on, unsigned long);    
  itkGetMacro(Comp_curve_p0_x, long);           
  itkGetMacro(Comp_curve_p0_y, long);           
  itkGetMacro(Comp_curve_p1_x, long);           
  itkGetMacro(Comp_curve_p1_y, long);           
  itkGetMacro(Comp_curve_p2_x, long);           
  itkGetMacro(Comp_curve_p2_y, long);           
  itkGetMacro(Comp_curve_p3_x, long);           
  itkGetMacro(Comp_curve_p3_y, long);           
  itkGetMacro(Comp_curve_scaling_index, long);            
  itkGetMacro(Echo_reject, long);           
  const double** GetMt_tp() const;
  itkGetMacro(True_axis_defined, unsigned long);    
  itkGetMacro(True_axis_on, unsigned long);   
  itkGetMacro(Parallel_x_tilt, double);         
  itkGetMacro(Parallel_y_tilt, double);         
  itkGetMacro(Parallel_depth, double);          
  itkGetMacro(Parallel_spacing, double);          
  itkGetMacro(Parallel_thickness, long);            
  itkGetMacro(Viewport_transform_flags, unsigned long);   
  itkGetMacro(Stress_mode, unsigned long);    
  itkGetMacro(Stress_label, char*);           
  itkGetMacro(Heart_rate, double);          
  itkGetMacro(Stage_timer_value, long);           
  itkGetMacro(Ecg_display_on, unsigned long);   
  double GetBlanking();
  itkGetMacro(Samples, short);
  
  /** VOL file format version 1.0 specific. */
  itkGetMacro(ColorImageSize, unsigned long);
  itkGetMacro(ColorImageOffset, unsigned long);
  itkGetMacro(Oag_params, char*);
  itkGetMacro(Cscanfmt, unsigned long);
  itkGetMacro(Oaglinear, unsigned long);
  itkGetMacro(Maxradius, unsigned long);
  itkGetMacro(Anglescale, double);
  double GetSkinoffset();
  
protected:
  VOLImageIO();
  ~VOLImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Read the data into a void* or char* pointer. */
  void ReadData(FILE * fp, void * buffer, size_t size, long pos = -1);
  void ReadData(FILE * fp, char * buffer, size_t size, long pos = -1);
  
private:
  VOLImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /**  All of the information read in from the header file */
  unsigned long   m_File_type;
  char            m_File_rev[12];      
  char            m_Description[128];     
  char            m_Date[12];
  char            m_Time[12];   
  char            m_Patient[64];
  char            m_Clinic[64];        
  unsigned long   m_NumEchoFrames; 
  unsigned long   m_NumDopFrames; 
  char            m_Dopmode; 
  long            m_EchoLPF; 
  long            m_DopLPF; 
  long            m_Repetition; 
  char            m_Xducer_name[16];
  long            m_Xducer_ID;
  double          m_Xducer_freq;
  double          m_Depth;
  double          m_Default_depth;
  char            m_App_name[24];
  char            m_Application; 
  long            m_Scan_fmt; 
  char            m_Dataset_name[64];
  short           m_First_tx_line;
  short           m_Last_tx_line;
  short           m_Lines;
  short           m_Az_lines;
  double          m_Az_angle;
  double          m_Az_angular_separation;
  short           m_El_lines;
  double          m_El_angle;
  double          m_El_angular_separation;
  long            m_Tx_offset;
  long            m_Rx_offset;
  double          m_MclkFreq;
  double          m_SampleSize;
  double          m_Mclk2Size;
  long            m_SampleRate;
  unsigned long   m_LineGroupSize;
  unsigned long   m_NumECGSamples;
  unsigned long   m_GrayImageSize;
  unsigned long   m_DopplerImageSize;
  unsigned long   m_EcgSize;
  unsigned long   m_MiscDataSize;
  unsigned long   m_GrayImageOffset;
  unsigned long   m_DopplerImageOffset;
  unsigned long   m_EcgOffset;
  unsigned long   m_MiscDataOffset;
  char            m_File_control_timing_type[272];
  char            m_DopplerVolInfo[989];
  short           m_ScanDepthCount; 
  short           m_ScanDepth; 
  double          m_Az_sector_tilt; 
  double          m_Elev_sector_tilt; 
  char            m_DopplerSegData[68]; 
  double          m_FrameRate; 
  long            m_Sweepspeed; 
  long            m_Update_interval; 
  unsigned long   m_Contrast_on;
  long            m_Comp_curve_p0_x;
  long            m_Comp_curve_p0_y;
  long            m_Comp_curve_p1_x;
  long            m_Comp_curve_p1_y;
  long            m_Comp_curve_p2_x;
  long            m_Comp_curve_p2_y;
  long            m_Comp_curve_p3_x;
  long            m_Comp_curve_p3_y;
  long            m_Comp_curve_scaling_index;
  long            m_Echo_reject;
  double          m_Mt_tp[4][4];
  unsigned long   m_True_axis_defined;
  unsigned long   m_True_axis_on;
  double          m_Parallel_x_tilt;
  double          m_Parallel_y_tilt;
  double          m_Parallel_depth;
  double          m_Parallel_spacing;
  long            m_Parallel_thickness;
  unsigned long   m_Viewport_transform_flags;
  unsigned long   m_Stress_mode;
  char            m_Stress_label[64];
  double          m_Heart_rate;
  long            m_Stage_timer_value;
  unsigned long   m_Ecg_display_on; 
  //stuff inside file_control_timing_type
  double          m_Blanking;
  short           m_Samples;
  
  /** Version 1.0 specific. */
  unsigned long   m_ColorImageSize;
  unsigned long   m_ColorImageOffset;
  char            m_Oag_params[28];
  unsigned long   m_Cscanfmt;
  unsigned long   m_Oaglinear;
  unsigned long   m_Maxradius;
  double          m_Anglescale;
  double          m_Skinoffset;
  };


} // namespace itk


#endif 

