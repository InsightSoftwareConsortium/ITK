#include "itkLightVideoFileReader.h"

#ifndef __itkVideoFileReader_h
#define __itkVideoFileReader_h

namespace itk
{
/** \class VideoFileReader
 * \brief Open an AVI File and stream it using OpenCV libs
 *  Just like the light Video file reader but more heavier
 *  Enable to play a video and to access a frame.
 *  
 *  When to use : -When you want to choose wich frame you want to access
 *                -Preferentialy when you want to write a video  
 *
 *  Should be used exactly as LightVideoFileReader.
 *
 * \sa LightVideoFileReader
 *
 * \ingroup OpenCVFilters
 */

template <class TOutputImage>
class ITK_EXPORT VideoFileReader : public LightVideoFileReader<TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VideoFileReader                                 Self;
  typedef LightVideoFileReader< TOutputImage >            Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. **/
  itkNewMacro(Self);

  /** Run-time type information (and related methods). **/
  itkTypeMacro(VideoFileReader, ImageSource);

  /** Set/Get the frame number you want to read **/
  void SetFrameRequested( unsigned long frame);
  itkGetMacro(FrameRequested,unsigned long);
  itkGetConstMacro(FrameRequested,unsigned long);

  /** Get the properties **/
  itkGetMacro(PositionInMSec, double);
  itkGetMacro(Ratio, double);
  itkGetMacro(FrameWidth, int);
  itkGetMacro(FrameHeight,int);
  itkGetMacro(FpS,double);
  itkSetMacro(FpS,double);
  itkGetMacro(FourCC,int);


  /** Get/Set if you want to choose the frame requested or **/
  /** if you want the video to simply play (false by default) **/  
  itkSetMacro(NextFrameIsFrameRequested,bool);
  itkGetMacro(NextFrameIsFrameRequested,bool);

  /** Set the filename **/
  void SetFileName (const char *filename);

  /** Play a video **/
  void PlayInput (unsigned long frame);
  void PlayInput ();
  void PlayOutput (unsigned long frame);
  void PlayOutput ();  
  
  /** Try to load a video **/
  void LoadVideo(); 

  /** Parse the image **/
  //typename itk::Image<typename TOutputImage::PixelType,2>::Pointer StreamVideo();

protected:  
 
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

  VideoFileReader();
  ~VideoFileReader(){};

  unsigned long   m_FrameRequested;
  double          m_PositionInMSec;
  double          m_Ratio;
  int             m_FrameWidth;
  int             m_FrameHeight;
  double          m_FpS;
  int             m_FourCC;

  bool            m_NextFrameIsFrameRequested;

private:
  VideoFileReader(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented

  void UpdateProperties();
  void InitializeProperties();

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVideoFileReader.txx"
#endif

#endif