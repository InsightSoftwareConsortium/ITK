#ifndef __itkBloxBoundaryProfilePixel_h
#define __itkBloxBoundaryProfilePixel_h

#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_vector_fixed.h"

#include "itkObject.h"
#include "itkBloxBoundaryProfileItem.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "itkBloxPixel.h"

namespace itk
{

template <unsigned int NDimensions>
class BloxBoundaryProfilePixel : public BloxPixel< BloxBoundaryProfileItem<NDimensions> >
{
public:
  /** The type of boundary profile item we process. */
  typedef BloxBoundaryProfilePixel<NDimensions> TBoundaryProfileItemType;

  /** The type of boundary point item we process. */
  typedef BloxBoundaryPointItem<NDimensions> TBPItemType;

  /** The type used to store the position of the boundary point item. */
  typedef Point<double, NDimensions> TPositionType;
  
  BloxBoundaryProfilePixel();
  ~BloxBoundaryProfilePixel();

private:

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryProfilePixel.txx"
#endif

#endif
