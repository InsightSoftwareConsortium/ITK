#include "itkVideoWriterBase.h"
#include "itkExceptionObject.h"
#include "cv.h"
#include "highgui.h"

#ifndef __itkOpenCVWriter_h
#define __itkOpenCVWriter_h

namespace itk
{

/** \class OpenCVWriter
 * \brief 
 *  
 *
 * \sa VideoWriterBase
 *
 * \ingroup OpenCVFilters
 */

template <class TImage>
class ITK_EXPORT OpenCVWriter : public VideoWriterBase < TImage >
{
public:
  /** Standard class typedefs. **/
  typedef OpenCVWriter                               Self;
  typedef VideoWriterBase<TImage>                    Superclass;
  typedef SmartPointer< Self >                       Pointer;
  typedef TImage                                     ImageType;
  typedef typename ImageType::PixelType              PixelType;

  /** Runtime type information (and related methods). **/
  itkTypeMacro(OpenCVWriter, Superclass);

  /** Method for creation through the object factory. **/
  itkNewMacro(Self);

  /** Try to Write a video **/
  /** Return true if in case of a success, false for a faillure **/
  bool OpenWriter (const char* filename, typename itk::Image<typename TImage::PixelType,2>::Pointer ITKImage);

  /** Try to close a video **/
  /** Return true if in case of a success, false for a faillure **/
  bool Close (const char* filename);

  /** Return the state of the video (opened or not) **/
  bool IsWriterOpen () {return this->m_WriterOpen;};

  /** Write a frame and return true if succeed (false otherwise) **/
  bool Write (typename ImageType::Pointer ITKImage);

  /** A bunch of accessor **/
  int GetWidth() {return this->m_Width;};
  int GetHeight() {return this->m_Height;};
  double GetXSpacing() {return this->m_Spacing[0];};
  double GetYSpacing() {return this->m_Spacing[1];};
  double GetPositionInMSec () {return this->m_PositionInMSec;};
  double GetRatio () {return this->m_Ratio;};
  unsigned long GetFrameTotal () {return this->m_FrameTotal;};
  double GetFpS() {return this->m_FpS;};

protected:  
  OpenCVWriter();
  ~OpenCVWriter(){};

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  OpenCVWriter(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void UpdateProperties ();

  IplImage                              *m_CVImage;
  IplImage                              *m_Temp;
  CvVideoWriter                         *m_Writer;
  bool                                  m_WriterOpen;

  int                                   m_FourCC;
  double                                m_FpS;
  int                                   m_Width;
  int                                   m_Height;
  itk::Size<2>        m_Size;
  itk::Index<2>       m_Start;
  itk::ImageRegion<2> m_Region;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOpenCVWriter.txx"
#endif

#endif // __itkOpenCVWriter_h
