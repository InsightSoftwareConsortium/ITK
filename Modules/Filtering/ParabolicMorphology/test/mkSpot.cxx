#include "ioutils.h"

int
main(int argc, char * argv[])
{
  const int dim = 2;

  typedef unsigned char          PType;
  typedef itk::Image<PType, dim> IType;

  // create the input image - we will blur a dot, threshold then blur again
  IType::Pointer     input = IType::New();
  IType::SizeType    size;
  IType::IndexType   index;
  IType::RegionType  region;
  IType::SpacingType spacing;

  size.Fill(100);
  spacing.Fill(1);
  region.SetSize(size);

  index.Fill(50);

  input->SetRegions(region);
  input->SetSpacing(spacing);
  input->Allocate();

  input->FillBuffer(255);
  input->SetPixel(index, 0);
  writeIm<IType>(input, argv[1]);
  return EXIT_SUCCESS;
}
