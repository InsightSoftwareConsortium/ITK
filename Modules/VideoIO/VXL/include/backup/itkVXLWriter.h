#include "itkVideoWriterBase.h"
#include "itkExceptionObject.h"
#include "vidl/vidl_ffmpeg_ostream.h"

#ifndef __itkVXLWriter_h
#define __itkVXLWriter_h

namespace itk
{

/** \class VXLWriter
 * \brief 
 *  
 *
 * \sa VideoWriterBase
 *
 * \ingroup OpenCVFilters
 */

template <class TImage>
class ITK_EXPORT VXLWriter : public VideoWriterBase < TImage >
{
public:
  /** Standard class typedefs. **/
  typedef VXLWriter                              Self;
  typedef VideoWriterBase<TImage>                Superclass;
  typedef SmartPointer< Self >                   Pointer;

  /** Runtime type information (and related methods). **/
  itkTypeMacro(VXLWriter, Superclass);

  /** Convenient typedef **/
  typedef TImage ImageType;
  typedef typename ImageType::PixelType          PixelType;
  
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
  double GetXOrigin() {return this->m_Origin[0];};
  double GetYOrigin() {return this->m_Origin[1];};
  double GetXSpacing() {return this->m_Spacing[0];};
  double GetYSpacing() {return this->m_Spacing[1];};
  double GetFpS() {return this->m_FpS;};

 

protected:
  
  VXLWriter();
  ~VXLWriter(){};

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  VXLWriter(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void UpdateProperties ();

  vidl_pixel_format                         m_PixelFormat;
  vidl_frame_sptr                           m_VIDLFrame;
  vidl_ffmpeg_ostream                       *m_Writer;
  vidl_ffmpeg_ostream_params::encoder_type  m_Encoder;
  bool                                      m_WriterOpen;
  bool                                      m_VideoSeekable;

  double                                    m_FpS;
  int                                       m_Width;
  int                                       m_Height;
  //Need to figure out a way to declare them in a better way
  itk::Size<2>                              m_Size;
  itk::Index<2>                             m_Start;
  itk::ImageRegion<2>                       m_Region;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVXLWriter.txx"
#endif

#endif // __itkVXLWriter_h
