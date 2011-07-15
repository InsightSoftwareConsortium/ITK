/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/**
 *         The specification for this file format is taken from the
 *         web site http://www.bic.mni.mcgill.ca/ServicesSoftware/MINC
 * \author Leila Baghdadi
 *         Mouse Imaging Centre, Toronto, Canada 2005.
 */

#ifndef __itkMINC2ImageIO_h
#define __itkMINC2ImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImageIOBase.h"

#include "itkMatrix.h"

extern "C" {
#include <minc2.h>
}

namespace itk
{
/** \class MINC2ImageIO
 *
 * \author Leila Baghdadi
 * \brief Class that defines how to read MINC2 file format. Note,like
 * ITK, MINC2 is N dimensional and dimensions can be submitted in any
 * arbitrary order. Here we make sure the dimensions are ordered as
 * xspace, yspace, zspace, time and vector_dimension and so on or
 * xfrequencey, yfrequency, zfrequency, tfrequency and
 * vector_dimension and so on
 * NOTE** This class only reads the regularly sampled dimensions as I
 * am not sure how to deal with "iregularly sampled" dimensions yet!
 *
 * This code was contributed in the Insight Journal paper:
 * "MINC2.0 IO Support for ITK"
 * by Baghdadi L.
 * http://hdl.handle.net/1926/191
 * http://www.insight-journal.org/browse/publication/88
 *
 * \ingroup IOFilters
 *
 * \ingroup ITKReview
 */
class ITK_EXPORT MINC2ImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef MINC2ImageIO          Self;
  typedef ImageIOBase           Superclass;
  typedef SmartPointer< Self >  Pointer;
  typedef Matrix< float, 3, 3 > MatrixType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MINC2ImageIO, ImageIOBase);

  /*-------- This part of the interface deals with reading data. ------ */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer);

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Writes the spacing and dimentions of the image.
   * Assumes SetFileName has been called with a valid file name. */
  virtual void WriteImageInformation();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegion has been set properly. */
  virtual void Write(const void *buffer);

  char * GetDimensionOrder() { return m_DimensionOrder; }
  void SetDimensionOrder(char *dimorder) { m_DimensionOrder = dimorder; }

  void XYZFromDirectionCosines(midimhandle_t *hdims, int *dim_indices, unsigned int *number_of_components);

protected:
  MINC2ImageIO();
  ~MINC2ImageIO();
  void PrintSelf(std::ostream & os, Indent indent) const;

  void WriteSlice(std::string & fileName, const void *buffer);

  // Num. dimensions in base class (c.f. GetNumberOfDimensions); why keep a
  // second copy here?
  unsigned int m_NDims;

  char **m_DimensionName;
  virtual void SetDimensionName(unsigned int i, char *name);

  virtual char * GetDimensionName(unsigned int i){ return m_DimensionName[i]; }

  char *m_DimensionOrder;

  // shift and scale parameter (god help me with slice scaling!!)
  double m_Shift;
  double m_Scale;

  // dimension size and start and step
  unsigned int *m_DimensionSize;
  double       *m_DimensionStart;
  double       *m_DimensionStep;

  MatrixType m_DirectionCosines;

  int *m_DimensionIndices;

  // Description:
  // Check the DimensionOrder and adjust according to what
  // dimensions the user has actually specified via
  // SetDimensionOrder()
  int CheckDimensionOrder(char *userdimorder);

  double m_OriginalStart[3];
  // complex type images, composed of complex numbers
  int m_Complex;
private:
  MINC2ImageIO(const Self &);   //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  // Description
  // Get slice scaling from local slice scaling
  void SetSliceScalingFromLocalScaling(mihandle_t volume);

  // Description
  // Get slice scaling from global slice scaling
  void SetSliceScalingFromGlobalScaling(mihandle_t volume);
};
} // end namespace itk

#endif // __itkMINC2ImageIO_h
