#include "itkVideoReaderBase.h"
#include "itkExceptionObject.h"
#include "itkImportImageFilter.h"
#include "vidl/vidl_ffmpeg_istream.h"

#ifndef __itkVXLReader_h
#define __itkVXLReader_h

namespace itk
{

/** \class VXLReader
 * \brief 
 *  
 *
 * \sa VideoReaderBase
 *
 * \ingroup OpenCVFilters
 */

template <class TImage>
class ITK_EXPORT VXLReader : public VideoReaderBase < TImage >
{
public:
  /** Standard class typedefs. **/
  typedef VXLReader                               Self;
  typedef VideoReaderBase<TImage>                 Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef TImage                                  ImageType;
  typedef typename ImageType::PixelType           PixelType;

  /** Runtime type information (and related methods). **/
  itkTypeMacro(VXLReader, Superclass);
  
  /** Method for creation through the object factory. **/
  itkNewMacro(Self);
  
  /** Convinient typedef **/ 
  typedef itk::ImportImageFilter<PixelType,2>   ImportFilterType;

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
  double GetPositionInMSec () {return -1;}; //Nothing so far
  double GetRatio () {return -1;};//Nothing so far
  unsigned long GetFrameTotal () {return this->m_FrameTotal;};
  unsigned long GetCurrentFrame() {return this->m_CurrentFrame;};
  double GetFpS() {return this->m_FpS;};

protected:
  
  VXLReader();
  ~VXLReader(){};

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  VXLReader(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void UpdateProperties ();

  vidl_pixel_format                         m_PixelFormat;
  vidl_frame_sptr                           m_VIDLImage;
  vidl_frame_sptr                           m_VIDLFrame;
  vidl_ffmpeg_istream                       *m_Reader;
  bool                                      m_ReaderOpen;
  bool                                      m_VideoSeekable;

  double                                    m_FpS;
  unsigned long                             m_FrameTotal;
  unsigned long                             m_CurrentFrame;
  int                                       m_Width;
  int                                       m_Height;
  double                                    m_Origin[2];
  double                                    m_Spacing[2];
  typename ImportFilterType::SizeType       m_Size;
  typename ImportFilterType::IndexType      m_Start;
  typename ImportFilterType::RegionType     m_Region;

  typename ImportFilterType::Pointer        m_ImportFilter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVXLReader.txx"
#endif

#endif // __itkVXLReader_h
