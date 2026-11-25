/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmSplitGridFilter.h"
#include "gdcmAttribute.h"
#include "gdcmImageHelper.h"
#include "gdcmDirectionCosines.h"
#include "gdcmIPPSorter.h"

namespace gdcm {
SplitGridFilter::SplitGridFilter()
  : F(new File),
    I(new Image) {
}

SplitGridFilter::~SplitGridFilter() = default;

namespace details {
/*
 */
static bool reorganize_grid(const unsigned short* input,
                            const unsigned int* inputdims,
                            unsigned int square, const unsigned int* outputdims,
                            unsigned short* output) {
  for (unsigned int z = 0; z < outputdims[2]; ++z) {
    for (unsigned int y = 0; y < outputdims[1]; ++y) {
      for (unsigned int x = 0; x < outputdims[0]; ++x) {
        const size_t outputidx =
            x + y * outputdims[0] + z * outputdims[0] * outputdims[1];
        const size_t inputidx = (x + (z % square) * outputdims[0]) +
                                (y + (z / square) * outputdims[1]) * inputdims[
                                  0];
        output[outputidx] = input[inputidx];
      }
    }
  }
  return true;
}

static bool reorganize_grid_invert(const unsigned short* input,
                                   const unsigned int* inputdims,
                                   unsigned int square,
                                   const unsigned int* outputdims,
                                   unsigned short* output) {
  for (unsigned int z = 0; z < outputdims[2]; ++z) {
    for (unsigned int y = 0; y < outputdims[1]; ++y) {
      for (unsigned int x = 0; x < outputdims[0]; ++x) {
        const size_t outputidx =
            x + y * outputdims[0] + (outputdims[2] - 1 - z) * outputdims[0] *
            outputdims[1];
        const size_t inputidx = (x + (z % square) * outputdims[0]) +
                                (y + (z / square) * outputdims[1]) * inputdims[
                                  0];
        output[outputidx] = input[inputidx];
      }
    }
  }
  return true;
}
}

void SplitGridFilter::SetImage(const Image& image) {
  I = image;
}

bool SplitGridFilter::GetAcquisitionSize(unsigned int size[2],
                                         DataSet const& ds) {
  /*
  Dimensions of the acquired frequency /phase data before reconstruction.
  Multi-valued: frequency rows\frequency columns\phase rows\phase columns.
   */
  Attribute<0x0018, 0x1310> acquisitionMatrix;
  acquisitionMatrix.SetFromDataSet(ds);
  const unsigned short* pMat = acquisitionMatrix.GetValues();
  /*
  The axis of phase encoding with respect to the image.

  Enumerated Values:

  ROW
  phase encoded in rows.

  COL
  phase encoded in columns.
   */
  Attribute<0x0018, 0x1312> inPlanePhaseEncodingDirection;
  inPlanePhaseEncodingDirection.SetFromDataSet(ds);
  CSComp val = inPlanePhaseEncodingDirection.GetValue();
  std::string dir = val.Trim();
  // http://dicom.nema.org/medical/dicom/current/output/chtml/part03/sect_C.8.3.html
  if (dir == "COL") {
    /* pay attention that size is: { columns  , rows } */
    // [256\0\0\134]
    size[0] = pMat[3];
    size[1] = pMat[0];
  } else if (dir == "ROW") {
    // [0\512\213\0]
    size[0] = pMat[1];
    size[1] = pMat[2];
  } else {
    size[0] = size[1] = 0;
  }
  const bool found = size[0] && size[1];
  return found;
}

unsigned int SplitGridFilter::GetNumberOfImagesInGrid(File const& file) {
  unsigned int numberOfImagesInGrid = 0;
  // (0065,1050) DS [80]                                               # 2,1 MR Number Of Slice In Volume
  static const PrivateTag t1(0x0065, 0x50, "Image Private Header");
  const DataSet& ds = file.GetDataSet();
  if (ds.FindDataElement(t1)) {
    DataElement const& de1 = ds.GetDataElement(t1);
    Element<VR::DS, VM::VM1> el1 = {{0}};
    el1.SetFromDataElement(de1);
    // FIXME: why DS ?
    numberOfImagesInGrid = static_cast<unsigned int>(el1.GetValue());
  }
  return numberOfImagesInGrid;
}

bool SplitGridFilter::ComputeGRIDDimensions(unsigned int dims[3]) {
  const unsigned int numberOfImagesInGrid = GetNumberOfImagesInGrid(GetFile());

  if (!numberOfImagesInGrid) {
    gdcmErrorMacro("Could not find/compute NumberOfImagesInGrid");
    return false;
  }

  std::vector<unsigned int> colrow =
      ImageHelper::GetDimensionsValue(GetFile());

  dims[0] = colrow[0];
  dims[1] = colrow[1];

  const unsigned int div = static_cast<unsigned int>(std::ceil(
    std::sqrt(static_cast<double>(numberOfImagesInGrid))));
  dims[0] /= div;
  const unsigned int div2 = static_cast<unsigned int>(std::ceil(
    static_cast<double>(numberOfImagesInGrid) / div));
  assert( div * div2 >= numberOfImagesInGrid);
  dims[1] /= div2;
  dims[2] = numberOfImagesInGrid;
  return true;
}

bool SplitGridFilter::ComputeGRIDSliceNormal(double slicenormalvector[3],
                                             bool& inverted) {
  DataSet& ds = GetFile().GetDataSet();

  double normal[3];
  bool snvfound = false;
  {
    {
      static const PrivateTag t1(0x0065, 0x14, "Image Private Header");
      if (ds.FindDataElement(t1)) {
        DataElement const& de1 = ds.GetDataElement(t1);
        Element<VR::DS, VM::VM3> el1 = {{0}};
        el1.SetFromDataElement(de1);

        normal[0] = el1[0];
        normal[1] = el1[1];
        normal[2] = el1[2];
        {
          snvfound = true;
        }
      }
    }
  }
  if (snvfound) {
    Attribute<0x20, 0x37> iop;
    iop.SetFromDataSet(ds);
    DirectionCosines dc(iop.GetValues());
    double z[3];
    dc.Cross(z);
    const double snv_dot = dc.Dot(normal, z);
    if (fabs(1. - snv_dot) < 1e-6) {
      gdcmDebugMacro("Same direction");
      inverted = false;
    } else if (fabs(-1. - snv_dot) < 1e-6) {
      gdcmDebugMacro("SliceNormalVector is opposite direction");
      // FIXME SNV is always opposite dir but actually not used in code
      inverted = true;
    } else {
      gdcmErrorMacro(
        "Unexpected normal for SliceNormalVector, dot is: " << snv_dot);
      return false;
    }
  }

  for (int i = 0; i < 3; ++i) slicenormalvector[i] = normal[i];

  return snvfound;
}

bool SplitGridFilter::ComputeGRIDImagePositionPatient(double ret[3],
  const double ipp[6],
  const double dircos[6],
  const double pixelspacing[3],
  const unsigned int image_dims[3],
  const unsigned int grid_dims[3], bool inverted) {
  DataSet& ds = GetFile().GetDataSet();
  DirectionCosines dc(dircos);
  dc.Normalize();
  double z[3] = {};
  dc.Cross(z);
  DirectionCosines::Normalize(z);

  const double* dircos_normalized = dc;
  const double* x = dircos_normalized;
  const double* y = dircos_normalized + 3;

  bool hasIppCsa = false;
  double ipp_dcm[3] = {};
  double pos1[3];
  double pos2[3];
  {
    // (0065,1051) SQ (Sequence with undefined length)                   # u/l,1 MR VFrame Sequence
    static const PrivateTag t1(0x0065, 0x51, "Image Private Header");
    if (ds.FindDataElement(t1)) {
      DataElement const& de1 = ds.GetDataElement(t1);
      SmartPointer<SequenceOfItems> sq = de1.GetValueAsSQ();
      size_t size = sq->GetNumberOfItems();
      const unsigned int numberOfImagesInGrid = grid_dims[2];
      if (size == numberOfImagesInGrid) {
        // all grid have their own slice position, always pick the first one for computation:
        //    (0020,0032) DS [-90.7223282\-120.779121\-60.4080467 ]         # 36,3 Image Position (Patient)
        const Tag timagepositionpatient(0x0020, 0x0032);
        {
          const Item& item1 = sq->GetItem(1);
          DataSet const& subds1 = item1.GetNestedDataSet();
          assert(subds1.FindDataElement( timagepositionpatient ));
          const DataElement& de = subds1.GetDataElement(timagepositionpatient);
          Attribute<0x0020, 0x0032> at = {{0, 0, 0}}; // default value if empty
          at.SetFromDataElement(de);
          for (unsigned int i = 0; i < at.GetNumberOfValues(); ++i) {
            pos1[i] = at.GetValue(i);
          }
        }
        double dist1 = 0;
        for (int i = 0; i < 3; ++i) dist1 += z[i] * pos1[i];
        {
          const Item& item2 = sq->GetItem(size);
          DataSet const& subds2 = item2.GetNestedDataSet();
          assert(subds2.FindDataElement( timagepositionpatient ));
          const DataElement& de = subds2.GetDataElement(timagepositionpatient);
          Attribute<0x0020, 0x0032> at = {{0, 0, 0}}; // default value if empty
          at.SetFromDataElement(de);
          for (unsigned int i = 0; i < at.GetNumberOfValues(); ++i) {
            pos2[i] = at.GetValue(i);
          }
        }
        double dist2 = 0;
        for (int i = 0; i < 3; ++i) dist2 += z[i] * pos2[i];
        // (0018,0088) DS [2 ]                                               # 2,1 Spacing Between Slices
        const double delta = (dist1 - dist2) / (grid_dims[2] - 1);
        if (std::fabs(std::fabs(delta) - pixelspacing[2]) > 1e-6) {
          gdcmWarningMacro("Pixel Spacing is incorrect");
        }
        if (inverted) {
          if (delta > 0) {
            gdcmWarningMacro("SNV is inverted inconsistent with slice ordering");
          }
        } else {
          if (delta < 0) {
            gdcmWarningMacro(
              "SNV is not inverted inconsistent with slice ordering");
          }
        }
        hasIppCsa = true;
      } else {
        gdcmWarningMacro(
          "Inconsistent SliceArray: " << size << " vs expected: " << grid_dims[2
          ]);
      }
    }
  }

  // no error set origin:
  if (inverted && false) {
    for (int i = 0; i < 3; ++i) ret[i] = pos2[i];
  } else {
    for (int i = 0; i < 3; ++i) ret[i] = pos1[i];
  }

  return true;
}

bool SplitGridFilter::GetGRIDSlicePosition( unsigned int index, double pos[3])
{
  pos[0] = pos[1] = pos[2] = 0.0;
  DataSet& ds = GetFile().GetDataSet();
  // (0065,1051) SQ (Sequence with undefined length)                   # u/l,1 MR VFrame Sequence
  static const PrivateTag t1(0x0065, 0x51, "Image Private Header");
  if (ds.FindDataElement(t1)) {
    DataElement const& de1 = ds.GetDataElement(t1);
    SmartPointer<SequenceOfItems> sq = de1.GetValueAsSQ();
    size_t size = sq->GetNumberOfItems();
    if ( /*index >= 0 &&*/ index < size ) {
      const Item& item = sq->GetItem(index+1);
      DataSet const& subds2 = item.GetNestedDataSet();
      const Tag timagepositionpatient(0x0020, 0x0032);
      assert(subds2.FindDataElement( timagepositionpatient ));
      const DataElement& de = subds2.GetDataElement(timagepositionpatient);
      Attribute<0x0020, 0x0032> at = {{0, 0, 0}}; // default value if empty
      at.SetFromDataElement(de);
      for (unsigned int i = 0; i < at.GetNumberOfValues(); ++i) {
        pos[i] = at.GetValue(i);
      } 
      return true;
    }
  }
  return false;
}

bool SplitGridFilter::Split() {
  bool success = true;
  DataSet& ds = GetFile().GetDataSet();
  // sentinel
  Attribute<0x0008, 0x0008> imtype;
  imtype.SetFromDataSet( ds );
  const unsigned int nvalues = imtype.GetNumberOfValues();
  if( nvalues < 2 || imtype[nvalues-1].Trim() != "VFRAME" ) {
    gdcmErrorMacro("Unhandled VFRAME");
    return false;
  }

  unsigned int dims[3] = {0, 0, 0};
  if (!ComputeGRIDDimensions(dims)) {
    return false;
  }
  const unsigned int div = static_cast<unsigned int>(std::ceil(
    std::sqrt(static_cast<double>(dims[2]))));
  bool inverted = false;
  double normal[3];
  bool hasOriginCSA = true;
  bool hasNormalCSA = true;
  if (!ComputeGRIDSliceNormal(normal, inverted)) {
    gdcmDebugMacro("Normal will not be accurate");
    hasNormalCSA = false;
  }
  (void)hasNormalCSA;
  double origin[3] = {};
  const Image& inputimage = GetImage();
  if (!ComputeGRIDImagePositionPatient(origin, inputimage.GetOrigin(),
                                       inputimage.GetDirectionCosines(),
                                       inputimage.GetSpacing(),
                                       inputimage.GetDimensions(), dims,
                                       inverted)) {
    gdcmWarningMacro("Origin will not be accurate");
    hasOriginCSA = false;
  }

  if (inputimage.GetPixelFormat() != PixelFormat::UINT16) {
    gdcmErrorMacro("Expecting UINT16 PixelFormat");
    return false;
  }
  unsigned long l = inputimage.GetBufferLength();
  std::vector<char> buf;
  buf.resize(l);
  inputimage.GetBuffer(buf.data());
  DataElement pixeldata(Tag(0x7fe0, 0x0010));

  std::vector<char> outbuf;
  outbuf.resize(l);

  bool b;
  if (inverted && false) {
    b = details::reorganize_grid_invert(
      (unsigned short*)(void*)buf.data(), inputimage.GetDimensions(), div, dims,
      (unsigned short*)(void*)outbuf.data());
  } else {
    b = details::reorganize_grid(
      (unsigned short*)(void*)buf.data(), inputimage.GetDimensions(), div, dims,
      (unsigned short*)(void*)outbuf.data());
  }
  if (!b) return false;

  VL::Type outbufSize = (VL::Type)outbuf.size();
  pixeldata.SetByteValue(outbuf.data(), outbufSize);

  Image& image = GetImage();
  const TransferSyntax& ts = image.GetTransferSyntax();
  if (ts.IsExplicit()) {
    image.SetTransferSyntax(TransferSyntax::ExplicitVRLittleEndian);
  } else {
    image.SetTransferSyntax(TransferSyntax::ImplicitVRLittleEndian);
  }
  image.SetNumberOfDimensions(3);
  image.SetDimension(0, dims[0]);
  image.SetDimension(1, dims[1]);
  image.SetDimension(2, dims[2]);

  // Fix origin (direction is ok since we reorganize the tiles):
  if (hasOriginCSA) image.SetOrigin(origin);

  image.SetDataElement(pixeldata);

  // Second part need to fix the Media Storage, now that this is not a single slice anymore
  MediaStorage ms = MediaStorage::SecondaryCaptureImageStorage;
  ms.SetFromFile(GetFile());

  if (ms == MediaStorage::MRImageStorage) {
    // Ok make it a MediaStorage::EnhancedMRImageStorage
    //    ms = MediaStorage::EnhancedMRImageStorage;
    //
    //    // Remove old MRImageStorage attribute then:
    //    ds.Remove( Tag(0x0020,0x0032) ); // Image Position (Patient)
    //    ds.Remove( Tag(0x0020,0x0037) ); // Image Orientation (Patient)
    //    ds.Remove( Tag(0x0028,0x1052) ); // Rescale Intercept
    //    ds.Remove( Tag(0x0028,0x1053) ); // Rescale Slope
    //    ds.Remove( Tag(0x0028,0x1054) ); // Rescale Type
  } else {
    gdcmDebugMacro("Expecting MRImageStorage");
    return false;
  }
  DataElement de(Tag(0x0008, 0x0016));
  const char* msstr = MediaStorage::GetMSString(ms);
  VL::Type strlenMsstr = (VL::Type)strlen(msstr);
  de.SetByteValue(msstr, strlenMsstr);
  de.SetVR(Attribute<0x0008, 0x0016>::GetVR());
  ds.Replace(de);

  return success;
}
} // end namespace gdcm
