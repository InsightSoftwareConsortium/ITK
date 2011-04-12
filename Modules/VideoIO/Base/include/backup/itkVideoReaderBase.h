
#include "itkLightProcessObject.h"
#include "itkExceptionObject.h"
#include "itkImage.h"

#ifndef __itkVideoReaderBase_h
#define __itkVideoReaderBase_h

namespace itk
{

template <class TImage>
class ITK_EXPORT VideoReaderBase : public LightProcessObject
{
public:
  /** Standard class typedefs. **/
  typedef VideoReaderBase                               Self;
  typedef LightProcessObject                        Superclass;
  typedef SmartPointer< Self >                      Pointer;

  /** Convinient typedef **/
  typedef TImage                              ImageType;
  typedef typename ImageType::PixelType       PixelType;

  /** Run-time type information (and related methods). **/
  itkTypeMacro(VideoReaderBase, Superclass);
  
  /** Try to open a video **/
  /** Return true if in case of a success, false for a faillure **/
  /** Intended to be overloaded by subclasses **/
  virtual bool OpenReader (const char* filename) = 0;

 /** Return the state of the video (opened or not) **/
  virtual bool IsReaderOpen () = 0;

 /** Return the image read form a video file **/
  virtual typename ImageType::Pointer Read() = 0;

  /** return true if you operation succesful **/
  virtual bool SetNextFrameToRead(unsigned long frameNumber) = 0;

  /** A bunch of accessors**/
  virtual int GetWidth() = 0;
  virtual int GetHeight() = 0;
  virtual double GetXOrigin() = 0;
  virtual double GetYOrigin() = 0;
  virtual double GetXSpacing() = 0;
  virtual double GetYSpacing() = 0;
  virtual double GetPositionInMSec () = 0 ;
  virtual double GetRatio () = 0;
  virtual unsigned long GetFrameTotal () = 0;
  virtual double GetFpS() = 0;
  virtual unsigned long GetCurrentFrame() = 0;

protected:  
  
  VideoReaderBase();
  ~VideoReaderBase(){};

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  VideoReaderBase(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVideoReaderBase.txx"
#endif

#endif // __itkVideoReaderBase_h
