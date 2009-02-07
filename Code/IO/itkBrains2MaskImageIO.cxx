/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBrains2MaskImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/

#include "itkBrains2MaskImageIO.h"
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkIOCommon.h"
#include "itkMetaDataObject.h"
#if !defined(_MSC_VER) || (_MSC_VER > 1300) 
#include "itkSpatialOrientationAdapter.h"
#endif
#include <stdio.h>
#include "itk_zlib.h"
#include <time.h>
#include <itksys/SystemTools.hxx>
#include <sstream>
static const unsigned char DEF_WHITE_MASK=255;
namespace itk
{
#define Brains2_MASKFILE_WHITE  0
#define Brains2_MASKFILE_BLACK  1
#define Brains2_MASKFILE_GRAY  2


template <class TPixel>
class Brains2MaskMappingFunction {
public:
  unsigned int Evaluate(const TPixel *pixel);
};
template <class TPixel>
unsigned int
Brains2MaskMappingFunction<TPixel>::
Evaluate(const TPixel *pixel)
{
  return *pixel == 0 ? 0 : 1;
}


// Default constructor
Brains2MaskImageIO::Brains2MaskImageIO()
{
  //by default, only have 3 dimensions
  this->SetNumberOfDimensions(3);
  m_PixelType         = SCALAR;
  m_ComponentType     = UCHAR;
  //The file byte order
  m_MachineByteOrder  = ( ByteSwapper<int>::SystemIsBigEndian() == true ) ?
    LittleEndian : BigEndian;
}

Brains2MaskImageIO::~Brains2MaskImageIO()
{
  //Purposefully left blank
}

void Brains2MaskImageIO::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

bool Brains2MaskImageIO::CanWriteFile(const char * FileNameToWrite)
{
  m_FileName=FileNameToWrite;
  // Check filename to ensure that it meets the requirement of ending with .mask
  const std::string FileExtension=itksys::SystemTools::GetFilenameLastExtension(m_FileName);
  if( FileExtension == std::string(".mask"))// .mask is at the end of the filename
    {
    return true;
    }
  return false;
}

//The function that is used to read the octree stream to an octree.
OctreeNodeBranch * Brains2MaskImageIO::
readOctree (std::ifstream & octreestream,
            const ImageIOBase::ByteOrder machineByteOrder,
            const ImageIOBase::ByteOrder fileByteOrder)
{
  //Read in the color to set
  unsigned short int colorCode;
  octreestream.read((char *)&colorCode, sizeof (unsigned short int));
  if (machineByteOrder != fileByteOrder)
    {
    if ( machineByteOrder == LittleEndian )
      {
      ByteSwapper<unsigned short int>::
        SwapFromSystemToBigEndian( &colorCode );
      }
    else if ( machineByteOrder == BigEndian )
      {
      ByteSwapper<unsigned short int>::
        SwapFromSystemToLittleEndian( &colorCode );
      }
    }
  //Create child array of nodes.
  OctreeNodeBranch *CurrentNodeBranch = new OctreeNodeBranch(m_Octree);
  //7766554433221100  ChildID
  //1111110000000000  Bit
  //5432109876543210  Numbers
  for (unsigned int i = ZERO; i <= SEVEN; i++)
    {
    OctreeNode *curnode =
      CurrentNodeBranch->GetLeaf(static_cast<enum LeafIdentifier>(i));

    switch ((colorCode >> (i << 1)) & 3)  //(colorCode/vcl_pow(2,i*2) ) & 00000011b
      {
      case Brains2_MASKFILE_WHITE: // 0
        curnode->SetColor(Brains2_MASKFILE_WHITE);
        break;
      case Brains2_MASKFILE_BLACK: // 1
        curnode->SetColor(Brains2_MASKFILE_BLACK);
        break;
      case Brains2_MASKFILE_GRAY:  // 2
        //NOTE recursive call on all children to set them.
        curnode->SetBranch(
          readOctree(octreestream,machineByteOrder,
                     fileByteOrder));
        break;
      }
    }
  return CurrentNodeBranch;
}

void Brains2MaskImageIO
::Read(void* buffer)
{
  std::ifstream   local_InputStream;
  local_InputStream.open( this->m_FileName.c_str(), std::ios::in | std::ios::binary );
  if( local_InputStream.fail() )
    {
    itkExceptionMacro(<< "File "<< this->m_FileName << "cannot be opened for reading");
    }
  //Just fast forward throuth the file header NOTE: This re-reads the header information.
  this->m_B2MaskHeader.ReadBrains2Header(local_InputStream);
  //Actually start reading the octree
  unsigned int octreeHdr[6];
  //Need to gobble up the end of line character here and move one more byte.
  //Except for Borland where the operator>> has already gobbled the endline char
#if !defined(__BORLANDC__)
  local_InputStream.ignore();
#endif
  local_InputStream.read((char *)octreeHdr,6*sizeof(unsigned int));
  if(this->m_ByteOrder != this->m_MachineByteOrder)
    {
    if ( m_MachineByteOrder == LittleEndian )
      {
      ByteSwapper<unsigned int>::SwapRangeFromSystemToBigEndian( octreeHdr,6 );
      }
    else
      {
      ByteSwapper<unsigned int>::SwapRangeFromSystemToLittleEndian( octreeHdr,6 );
      }
    }
  Octree<unsigned char,2,Brains2MaskMappingFunction<unsigned char> >::Pointer octree =
    Octree<unsigned char,2,Brains2MaskMappingFunction<unsigned char> >::New();
  octree->SetDepth(octreeHdr[0]);
  octree->SetWidth(octreeHdr[1]);
  octree->SetTrueDims(octreeHdr[2],octreeHdr[3],octreeHdr[4]);
  this->m_Octree = octree;
  switch (octreeHdr[5])
    {
    case Brains2_MASKFILE_WHITE:
      //NOTE: THIS ALMOST NEVER HAPPENS!! All white image
      octree->SetColor(DEF_WHITE_MASK);
      break;
    case Brains2_MASKFILE_BLACK:
      //NOTE: THIS ALMOST NEVER HAPPENS!! All black image
      octree->SetColor(0);
      break;
    case Brains2_MASKFILE_GRAY:
      octree->SetTree(readOctree(local_InputStream,this->m_MachineByteOrder,this->m_ByteOrder ));
    }
  local_InputStream.close();
  //DEBUG: Now just convert the octree into an image for returning!!!
  //DEBUG:  This is written for 3D octreees only right now
  unsigned char * const p = static_cast<unsigned char *>(buffer);
  for(unsigned int k=0; k< this->m_Dimensions[2]; k++)
    {
    const unsigned int slice_offset=k*this->m_Dimensions[1]*this->m_Dimensions[0];
    for(unsigned int j=0; j< this->m_Dimensions[1]; j++)
      {
      const unsigned int sliceandrowoffset=slice_offset+j*this->m_Dimensions[0];
      for(unsigned int i=0; i< this->m_Dimensions[0]; i++)
        {
        //unsigned int val =  octree->GetValue(i,this->m_Dimensions[1]-1-j,k);
        unsigned int val =  octree->GetValue(i,j,k);
        if(val != 0)
          {
          p[sliceandrowoffset+i] = DEF_WHITE_MASK;
          }
        else
          {
          p[sliceandrowoffset+i] = 0;
          }
        }
      }
    }
  return;
}
// This method will only test if the header looks like an
// Brains2Mask Header.  Some code is redundant with ReadImageInformation
// a StateMachine could provide a better implementation
bool Brains2MaskImageIO::CanReadFile( const char* FileNameToRead )
{
    { // Check filename to ensure that it meets the requirement of ending with .mask
    const std::string FileName(FileNameToRead);
    const std::string FileExtension=itksys::SystemTools::GetFilenameLastExtension(FileName);
    if( FileExtension != std::string(".mask"))// .mask is at the end of the filename
      {
      return false;
      }
    }
  std::ifstream   local_InputStream;
  local_InputStream.open( FileNameToRead, std::ios::in | std::ios::binary );
  if( local_InputStream.fail() )
    {
    return false;
    }
  try
    {
    this->m_IPLHeaderInfo.ClearHeader();
    this->m_IPLHeaderInfo.ReadBrains2Header(local_InputStream);
    }
  catch (ExceptionObject & itkNotUsed(e))
    {
    return false;
    }
  //
  // to try and maintain some backwards compatibility
  unsigned dims = this->GetNumberOfDimensions();
  std::vector<double> dirx(dims,0), diry(dims,0), dirz(dims,0);
  if(this->m_IPLHeaderInfo.DoesKeyExist("DIRX0:"))
    {
    dirx[0] = this->m_IPLHeaderInfo.getFloat("DIRX0:");
    dirx[1] = this->m_IPLHeaderInfo.getFloat("DIRX1:");
    dirx[2] = this->m_IPLHeaderInfo.getFloat("DIRX2:");
    diry[0] = this->m_IPLHeaderInfo.getFloat("DIRY0:");
    diry[1] = this->m_IPLHeaderInfo.getFloat("DIRY1:");
    diry[2] = this->m_IPLHeaderInfo.getFloat("DIRY2:");
    dirz[0] = this->m_IPLHeaderInfo.getFloat("DIRZ0:");
    dirz[1] = this->m_IPLHeaderInfo.getFloat("DIRZ1:");
    dirz[2] = this->m_IPLHeaderInfo.getFloat("DIRZ2:");
    }
  else if(this->m_IPLHeaderInfo.DoesKeyExist("MASK_ACQ_PLANE:"))
    {
    // backwards compatibility -- no newly created mask file will
    // include this tag.
    MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
    SpatialOrientation::ValidCoordinateOrientationFlags
      coord_orient(SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
    std::string acqVal = this->m_IPLHeaderInfo.getString("MASK_ACQ_PLANE:");
    if(acqVal == "SAGITTAL")
      {
      coord_orient = SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR;
      }
    else if(acqVal == "AXIAL")
      {
      coord_orient = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI;
      }
    else if(acqVal == "CORONAL")
      {
      coord_orient = SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP;
      }
    else
      {
      itkExceptionMacro(<< "If MASK_ACQ_PLANE is specified, then it must be one of CORONAL, AXIAL, or SAGITAL flags.");
      }
    EncapsulateMetaData<SpatialOrientation::ValidCoordinateOrientationFlags>
      (thisDic,ITK_CoordinateOrientation, coord_orient);
#if !defined(_MSC_VER) || (_MSC_VER > 1300) 
    //An error was encountered in code that depends upon the valid coord_orientation.
    typedef SpatialOrientationAdapter OrientAdapterType;
    SpatialOrientationAdapter::DirectionType dir;
    dir =  OrientAdapterType().ToDirectionCosines(coord_orient);
    dirx[0] = dir[0][0];
    dirx[1] = dir[1][0];
    dirx[2] = dir[2][0];
    diry[0] = dir[0][1];
    diry[1] = dir[1][1];
    diry[2] = dir[2][1];
    dirz[0] = dir[0][2];
    dirz[1] = dir[1][2];
    dirz[2] = dir[2][2];
#endif
    for(unsigned i = 3; i < dims; i++)
      {
      dirx[i] = diry[i] = dirz[i] = 0;
      }
    }
  else
    {
    itkExceptionMacro(<< "No orientation specified.");
    }
  this->SetDirection(0,dirx);
  this->SetDirection(1,diry);
  this->SetDirection(2,dirz);
  if(this->m_IPLHeaderInfo.DoesKeyExist("ORIGIN0:"))
    {
    double origin[3];
    origin[0] = this->m_IPLHeaderInfo.getFloat("ORIGIN0:");
    origin[1] = this->m_IPLHeaderInfo.getFloat("ORIGIN1:");
    origin[2] = this->m_IPLHeaderInfo.getFloat("ORIGIN2:");
    this->SetOrigin(0,origin[0]);
    this->SetOrigin(1,origin[1]);
    this->SetOrigin(2,origin[2]);
    }
  
  local_InputStream.close();
  if(this->m_IPLHeaderInfo.DoesKeyExist("MASK_HEADER_BEGIN") == false)
    {
    return false;
    }
  this->m_ByteOrder=(this->m_IPLHeaderInfo.getString("BYTE_ORDER:")
                     =="LITTLE_ENDIAN") ? LittleEndian : BigEndian;
  this->m_MachineByteOrder=(ByteSwapper<int>::SystemIsBigEndian() == true ) ?
    BigEndian : LittleEndian;

  //this->m_IPLHeaderInfo.PrintSelf(std::cout);
  const int TempNumDims=this->m_IPLHeaderInfo.getInt("MASK_NUM_DIMS:");
  this->SetNumberOfDimensions(TempNumDims);
  //NOTE: Brains2MaskImage dim[0] are the number of dims, and dim[1..7] are the
  // actual dims.
  m_Dimensions[ 0 ] = this->m_IPLHeaderInfo.getInt("MASK_X_SIZE:");
  m_Dimensions[ 1 ] = this->m_IPLHeaderInfo.getInt("MASK_Y_SIZE:");
  m_Dimensions[ 2 ] = this->m_IPLHeaderInfo.getInt("MASK_Z_SIZE:");
  m_Spacing[ 0 ]  = this->m_IPLHeaderInfo.getFloat("MASK_X_RESOLUTION:");
  m_Spacing[ 1 ]  = this->m_IPLHeaderInfo.getFloat("MASK_Y_RESOLUTION:");
  m_Spacing[ 2 ]  = this->m_IPLHeaderInfo.getFloat("MASK_Z_RESOLUTION:");

  m_ComponentType = UCHAR;
  return true;
}

void Brains2MaskImageIO::ReadImageInformation()
{
  this->CanReadFile( this->m_FileName.c_str() );
}

// cut the gordian knot, just do the header in one
// long printf
static const char mask_header_format[] =
  "IPL_HEADER_BEGIN\n"
  "PATIENT_ID: %s\n"            // 0
  "SCAN_ID: %s\n"               // 1
  "FILENAME: %s\n"              // 2
  "DATE: %s\n"                  // 3
  "CREATOR: %s\n"               // 4
  "PROGRAM: %s\n"               // 5
  "MODULE: %s\n"                // 6
  "VERSION: %s\n"               // 7
  "NAME: %s\n"                  // 8
  "BYTE_ORDER: BIG_ENDIAN\n"
  "MASK_HEADER_BEGIN\n"
  "MASK_NUM_DIMS: %d\n"         // 9
  "MASK_X_SIZE: %d\n"           // 10
  "MASK_X_RESOLUTION: %f\n"     // 11
  "MASK_Y_SIZE: %d\n"           // 12
  "MASK_Y_RESOLUTION: %f\n"     // 13
  "MASK_Z_SIZE: %d\n"           // 14
  "MASK_Z_RESOLUTION: %f\n"     // 15
  "MASK_THRESHOLD: %f\n"        // 16
  "MASK_NAME: %d\n"             // 17
#if defined(OBSOLETE)
  "MASK_ACQ_PLANE: %s\n"        // 18
#endif
  "DIRX0: %f\n"                 // 18
  "DIRX1: %f\n"                 // 19
  "DIRX2: %f\n"                 // 20
  "DIRY0: %f\n"                 // 21
  "DIRY1: %f\n"                 // 22
  "DIRY2: %f\n"                 // 23
  "DIRZ0: %f\n"                 // 24
  "DIRZ1: %f\n"                 // 25
  "DIRZ2: %f\n"                 // 26
  "ORIGIN0: %f\n"               // 27
  "ORIGIN1: %f\n"               // 28
  "ORIGIN2: %f\n"               // 29
  "MASK_HEADER_END\n"
  "IPL_HEADER_END\n";

void
Brains2MaskImageIO
::WriteImageInformation(void)
{
  return;
}
static bool
writeOctree (OctreeNode *branch,std::ofstream &output)
{
  unsigned i;
  unsigned short  colorCode = 0;

  for (i = 0; i < 8; i++)
    {
    OctreeNode &subnode =
      branch->GetChild(static_cast<enum LeafIdentifier>(i));
    if (subnode.IsNodeColored())
      {
      if(subnode.GetColor() == Brains2_MASKFILE_BLACK)
        {
        colorCode |= Brains2_MASKFILE_BLACK << (i << 1);
        }
      else
        {
        colorCode |= Brains2_MASKFILE_WHITE << (i << 1);
        }
      }
    else
      {
      colorCode |= Brains2_MASKFILE_GRAY << (i << 1);
      }
    }
  ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&colorCode);
  output.write((const char *)&colorCode,sizeof(colorCode));
  for (i = 0; i < 8; i++)
    {
    OctreeNode &subnode =
      branch->GetChild(static_cast<enum LeafIdentifier>(i));
    if (!subnode.IsNodeColored())
      {
      writeOctree (&subnode, output);
      }
    }
  return true;
}


static  void replace_blanks(std::string &s)
{
  for(unsigned i = 0; i < s.size(); i++)
    {
    if(s[i] == ' ')
      {
      s[i] = '_';
      }
    }
}

void
Brains2MaskImageIO
::Write( const void* buffer)
{
  if(this->m_FileName == "")
    {
    itkExceptionMacro(<< "Error in missing Filename");
    }
  std::ofstream output(this->m_FileName.c_str(), std::ios::out | std::ios::binary );
  if(output.fail())
    {
    itkExceptionMacro(<< "Error in opening file "<< this->m_FileName << " for writing");
    }
  const unsigned xsize = this->GetDimensions(0);
  const unsigned ysize = this->GetDimensions(1);
  const unsigned zsize = this->GetDimensions(2);

  const double xres = this->GetSpacing(0);
  const double yres = this->GetSpacing(1);
  const double zres = this->GetSpacing(2);

  MetaDataDictionary &thisDic=this->GetMetaDataDictionary();
  std::string temp;
  std::string patient_id("00000");
  if(ExposeMetaData<std::string>(thisDic,ITK_PatientID,temp))
    {
    patient_id = temp;
    }
  // to do -- add more header crap
  //Write the image Information before writing data
  char buf[16384];
  time_t rawtime;
  struct tm *timeinfo;
  time(&rawtime);
  timeinfo = localtime(&rawtime);
  std::string timestr = asctime(timeinfo);
  replace_blanks(timestr);
  std::string::size_type newline = timestr.rfind('\n');
  if(newline != std::string::npos)
    {
    timestr.erase(newline);
    }
  if(patient_id == "" || patient_id == " " )
    {
    patient_id = "00000";
    }
  std::string fname = this->m_FileName;
  replace_blanks(fname);
  std::string orientation = "UNKNOWN";
  std::vector<double> dirx = this->GetDirection(0);
  std::vector<double> diry = this->GetDirection(1);
  std::vector<double> dirz = this->GetDirection(2);
  double origin[3];
  origin[0] = this->GetOrigin(0);
  origin[1] = this->GetOrigin(1);
  origin[2] = this->GetOrigin(2);
  //
  // the old way of doing things...
#if defined(OBSOLETE)
  SpatialOrientationAdapter::DirectionType dir;
  SpatialOrientation::ValidCoordinateOrientationFlags coord_orient;
  for(unsigned int i = 0; i < 3; i++)
    {
    dir[i][1] = dirx[i];
    dir[i][1] = diry[i];
    dir[i][2] = dirz[i];
    }
  coord_orient = SpatialOrientationAdapter().FromDirectionCosines(dir);

  switch (coord_orient)
    {
    case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      // AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_RPI_TRANSVERSE;
      orientation = "AXIAL";
      break;
    case SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      // AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_PIR_SAGITTAL;
      orientation = "SAGITTAL";
      break;
    case SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      // AnalyzeImageIO::ITK_ANALYZE_ORIENTATION_RIP_CORONAL;
      orientation = "CORONAL";
      break;
    default:
      SpatialOrientationAdapter::DirectionType AXIdir=SpatialOrientationAdapter().ToDirectionCosines(SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI);
      SpatialOrientationAdapter::DirectionType SAGdir=SpatialOrientationAdapter().ToDirectionCosines(SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR);
      SpatialOrientationAdapter::DirectionType CORdir=SpatialOrientationAdapter().ToDirectionCosines(SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP);
      itkExceptionMacro(<< "Error: Invalid orientation specified for writing mask. \n"
                        << "\nGIVEN    " << coord_orient << "\n" << dir
                        << "\n Only Axial, Sagital, and Coronal orietations are supported in this file format."
                        << "\nAXIAL    " << SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI << "\n" << AXIdir
                        << "\nSAGITTAL " << SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR << "\n" << SAGdir
                        << "\nCORONAL  " << SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP << "\n" << CORdir
        );
      break;
    }
#endif
  sprintf(buf,mask_header_format,
          patient_id.c_str(),      // 0
          "00000",                 // scan_id 1
          fname.c_str(),           // file_name 2
          timestr.c_str(),         // date 3
          "Anonymous",             // creator 4
          "itkBrains2MaskImageIO", // program 5
          "None",                  // module 6
          "1",                     // version 7
          itksys::SystemTools::GetFilenameName(m_FileName).c_str(),   // name 8
          3,                       // num_dims 9
          xsize,                   // xsize 10
          xres,                     // x_res 11
          ysize,                   // ysize 12
          yres,                     // y_res 13
          zsize,                   // zsize 14
          zres,                     // z_res 15
          0.0,                     // threshold 16 
          -1,                      // mask_name 17
#if defined(OBSOLETE)
          orientation.c_str()      // acq plane
#endif
          dirx[0],dirx[1],dirx[2], // 18, 19, 20
          diry[0],diry[1],diry[2], // 21, 22, 23
          dirz[0],dirz[1],dirz[2], // 24, 25, 26
          origin[0],origin[1],origin[2] // 27, 28, 29
    );
  output.write(buf,strlen(buf));
  unsigned octreeHdr[6];
  OctreeNode *tree;
  OctreeBase::Pointer octBasePtr;
  if(m_ComponentType == CHAR)
    {
    octBasePtr =  Octree<char,2,Brains2MaskMappingFunction<char> >::New();
    }
  else if(m_ComponentType == UCHAR)
    {
    octBasePtr =  Octree<unsigned char,2,Brains2MaskMappingFunction<unsigned char> >::New();
    }
  else if(m_ComponentType == SHORT)
    {
    octBasePtr =  Octree<short,2,Brains2MaskMappingFunction<short> >::New();
    }
  else if(m_ComponentType == USHORT)
    {
    octBasePtr =  Octree<unsigned short,2,Brains2MaskMappingFunction<unsigned short> >::New();
    }
  else if(m_ComponentType == INT)
    {
    octBasePtr =  Octree<int,2,Brains2MaskMappingFunction<int> >::New();
    }
  else if(m_ComponentType == UINT)
    {
    octBasePtr =  Octree<unsigned int,2,Brains2MaskMappingFunction<unsigned int> >::New();
    }
  else if(m_ComponentType == LONG)
    {
    octBasePtr =  Octree<long,2,Brains2MaskMappingFunction<long> >::New();
    }
  else if(m_ComponentType == ULONG)
    {
    octBasePtr =  Octree<unsigned long,2,Brains2MaskMappingFunction<unsigned long> >::New();
    }
  else if(m_ComponentType == FLOAT)
    {
    octBasePtr =  Octree<float,2,Brains2MaskMappingFunction<float> >::New();
    }
  else if(m_ComponentType == DOUBLE)
    {
    octBasePtr =  Octree<double,2,Brains2MaskMappingFunction<double> >::New();
    }
  else
    {
    itkExceptionMacro(<< "Pixel type unsupported in this file type.");
    }
  octBasePtr->BuildFromBuffer(buffer,xsize,ysize,zsize);
  tree = octBasePtr->GetTree();

  octreeHdr[0] = octBasePtr->GetDepth();
  octreeHdr[1] = octBasePtr->GetWidth();
  octreeHdr[2] = xsize;
  octreeHdr[3] = ysize;
  octreeHdr[4] = zsize;
  if(tree->IsNodeColored())
    {
    octreeHdr[5] = tree->GetColor();
    }
  else
    {
    octreeHdr[5] = Brains2_MASKFILE_GRAY;
    }
  ByteSwapper<unsigned>::SwapRangeFromSystemToBigEndian(octreeHdr,
                                                             6);
  output.write((const char *)octreeHdr,sizeof(unsigned)*6);

  if(!tree->IsNodeColored())
    {
    writeOctree(tree,output);
    }
  output.close();
}
} // end namespace itk
