#include "itkVideoReaderBase.h"
#include "itkExceptionObject.h"
#include "itkImportImageFilter.h"
#include "cv.h"
#include "highgui.h"

#ifndef __itkOpenCVReader_h
#define __itkOpenCVReader_h

namespace itk
{

/** \class OpenCVReader
 * \brief 
 *  
 *
 * \sa VideoReaderBase
 *
 * \ingroup OpenCVFilters
 */

template <class TImage>
class ITK_EXPORT OpenCVReader : public VideoReaderBase < TImage >
{
public:
  /** Standard class typedefs. **/
  typedef OpenCVReader                               Self;
  typedef VideoReaderBase<TImage>                    Superclass;
  typedef SmartPointer< Self >                       Pointer;
  typedef TImage                                     ImageType;
  typedef typename ImageType::PixelType              PixelType;

  /** Runtime type information (and related methods). **/
  itkTypeMacro(OpenCVReader, Superclass);

  /** Method for creation through the object factory. **/
  itkNewMacro(Self);
  
  /** Convinient typedef **/ 
  typedef itk::ImportImageFilter<typename TImage::PixelType,2>   ImportFilterType;

  /** Try to open a video **/
  /** Return true if in case of a success, false for a faillure **/
  bool OpenReader (const char* filename);
 
  /** Return the state of the video (opened or not) **/
  bool IsReaderOpen () {return this->m_ReaderOpen;};

  /** Return the image read form a video file **/
  typename ImageType::Pointer Read();

  /** Set the next frame to read and return ture if operation succesfull **/
  bool SetNextFrameToRead( unsigned long frameNumber );

  /** A bunch of accessor **/
  int GetWidth() {return this->m_Width;};
  int GetHeight() {return this->m_Height;};
  double GetXOrigin() {return this->m_Origin[0];};
  double GetYOrigin() {return this->m_Origin[1];};
  double GetXSpacing() {return this->m_Spacing[0];};
  double GetYSpacing() {return this->m_Spacing[1];};
  double GetPositionInMSec () {return this->m_PositionInMSec;};
  double GetRatio () {return this->m_Ratio;};
  unsigned long GetFrameTotal () {return this->m_FrameTotal;};
  double GetFpS() {return this->m_FpS;};
  unsigned long GetCurrentFrame() {return this->m_CurrentFrame;}

protected:  
  OpenCVReader();
  ~OpenCVReader(){};

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  OpenCVReader(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void UpdateProperties ();

  IplImage                              *m_CVImage;
  IplImage                              *m_Temp;
  CvCapture                             *m_Capture;
  bool                                  m_ReaderOpen;

  int                                   m_FourCC;
  double                                m_FpS;
  unsigned long                         m_FrameTotal;
  unsigned long                         m_CurrentFrame;
  int                                   m_Width;
  int                                   m_Height;
  double                                m_Ratio;
  double                                m_PositionInMSec;
  double                                m_Origin[2];
  double                                m_Spacing[2];
  typename ImportFilterType::SizeType   m_Size;
  typename ImportFilterType::IndexType  m_Start;
  typename ImportFilterType::RegionType m_Region;

  typename ImportFilterType::Pointer    m_ImportFilter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOpenCVReader.txx"
#endif

#endif // __itkOpenCVReader_h
