
#ifndef __itkConnectedComponentImageFilter_h
#define __itkConnectedComponentImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkConceptChecking.h"
#include <vector>
#include <map>
#include "itkProgressReporter.h"

namespace itk
{

/**
 * \class ConnectedComponentImageFilter
 * \brief Label the objects in a binary image
 *
 * ConnectedComponentImageFilter labels the objects in a binary image.
 * Each distinct object is assigned a unique label. The filter experiments
 * with some improvements to the existing implementation, and is based on
 * run length encoding along raster lines.
 * The final object labels start with 1 and are consecutive. Objects
 * that are reached earlier by a raster order scan have a lower
 * label. This is different to the behaviour of the original connected
 * component image filter which did not produce consecutive labels or
 * impose any particular ordering.
 *
 *
 * \sa ImageToImageFilter
 */

template <class TInputImage, class TOutputImage, class TMaskImage=TInputImage>
class ITK_EXPORT ConnectedComponentImageFilter : 
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef ConnectedComponentImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Types from the Superclass
   */
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;
  typedef typename TMaskImage::PixelType MaskPixelType;
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  
  /**
   * Image typedef support
   */
  typedef TInputImage  InputImageType;
  typedef TMaskImage   MaskImageType;
  typedef TOutputImage OutputImageType;
  typedef   typename TInputImage::IndexType       IndexType;
  typedef   typename TInputImage::SizeType        SizeType;
  typedef   typename TOutputImage::RegionType     RegionType;
  typedef   std::list<IndexType>                  ListType;
  typedef typename MaskImageType::Pointer MaskImagePointer;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(ConnectedComponentImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);
  
  // only set after completion
  itkGetConstReferenceMacro(ObjectCount, unsigned long);

  // Concept checking -- input and output dimensions must be the same
  itkConceptMacro(SameDimension,
      (Concept::SameDimension<itkGetStaticConstMacro(InputImageDimension),itkGetStaticConstMacro(OutputImageDimension)>));


  void SetMaskImage(TMaskImage* mask) {
    this->SetNthInput(1, const_cast<TMaskImage *>( mask ));
  }

  const TMaskImage* GetMaskImage() const {
    return (static_cast<const TMaskImage*>(this->ProcessObject::GetInput(1)));
  }
  
protected:
  ConnectedComponentImageFilter() 
    {
    m_FullyConnected = false;
    m_ObjectCount = -1;
    }
  virtual ~ConnectedComponentImageFilter() {}
  ConnectedComponentImageFilter(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Standard pipeline method. 
   */
  void GenerateData();

  /** ConnectedComponentImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** ConnectedComponentImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

  bool m_FullyConnected;
  
private:
  unsigned long m_ObjectCount;
  // some additional types
  typedef typename TOutputImage::RegionType::SizeType OutSizeType;

  // types to support the run length encoding of lines
  typedef class runLength
    {
    public:
      long int length;  // run length information - may be a more type safe way of doing this
      typename InputImageType::IndexType where;  // Index of the start of the run
      unsigned long int label; // the initial label of the run
    };

  typedef std::vector<runLength> lineEncoding;

  // the map storing lines
  typedef std::map<long, lineEncoding> LineMapType;
  
  typedef std::vector<long> OffsetVec;

  // the types to support union-find operations
  typedef std::vector<unsigned long int> UnionFindType;
  UnionFindType m_UnionFind;
  UnionFindType m_Consecutive;
  // functions to support union-find operations
  void InitUnion(const unsigned long int size) 
  {
    m_UnionFind = UnionFindType(size + 1);
  }
  void InsertSet(const unsigned long int label);
  unsigned long int LookupSet(const unsigned long int label);
  void LinkLabels(const unsigned long int lab1, const unsigned long int lab2);
  unsigned long int CreateConsecutive();
  //////////////////
  void CompareLines(lineEncoding &current, const lineEncoding &Neighbour);

  void FillOutput(const LineMapType &LineMap,
      ProgressReporter &progress);

  void SetupLineOffsets(OffsetVec &LineOffsets);
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConnectedComponentImageFilter.txx"
#endif

#endif
