#include "nifti1_io.h"

int main(int argc, char **argv)
{
  int error = 0;
  int argdims[] = { 2, 2, 2 };
  nifti_1_header *hdr = nifti_make_new_header(argdims,NIFTI_TYPE_UINT8);
  if(sizeof(*hdr) != 348)
    {
    printf("NIfTI Header size not 348: %lu\n", sizeof(*hdr));
    error++;
    }
  if(hdr->sizeof_hdr != 348)
    {
    printf("NIfTI sizeof_hdr != 348: %d\n", hdr->sizeof_hdr);
    error++;
    }
  exit(error);
}
