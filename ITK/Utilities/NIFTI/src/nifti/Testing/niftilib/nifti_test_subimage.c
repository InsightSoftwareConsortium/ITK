#include <stdio.h>
#include <strings.h>
#include <unistd.h>
#include <nifti1_io.h>
enum NIFTITEST_BOOL {
  NIFTITEST_TRUE=1,
  NIFTITEST_FALSE=0
};

void _PrintTest(const int line,const char * message,const int FailureOccured, const enum NIFTITEST_BOOL isFatal,int *ErrorAccum)
{
  if(FailureOccured==NIFTITEST_TRUE)  /* This line can be commented out for a more verbose output */
    {
    char const * const PREFIX= (FailureOccured)?"==========ERROR":"..........SUCCESS";
    char const * const ISFATALPREFIX= (isFatal && FailureOccured)?" FATAL":"";
    printf("%s%s (LINE %d): %s\n",PREFIX,ISFATALPREFIX,line,message);
    fflush(stdout);
    *ErrorAccum+=FailureOccured;
    if(isFatal==NIFTITEST_TRUE && FailureOccured==NIFTITEST_TRUE)
      {
      printf("\n\nTOTAL ERRORS=%d\n",*ErrorAccum);
      exit( *ErrorAccum);
      }
    }
  return;
}
#define PrintTest(message,failure,isfailure,errorcount) \
  _PrintTest(__LINE__,message,failure,isfailure,errorcount)


nifti_image *
generate_reference_image(const char * write_image_filename,
                         int * const Errors)
{
  nifti_1_header reference_header;
  memset(&reference_header,0,sizeof(reference_header));
  reference_header.sizeof_hdr=sizeof(reference_header);
  reference_header.regular='r';
  reference_header.extents=16384;

  reference_header.dim[0]=3;
  reference_header.dim[1]=4;
  reference_header.dim[2]=4;
  reference_header.dim[3]=4;
  reference_header.dim[4]=1;
  reference_header.dim[5]=1;
  reference_header.dim[6]=1; //This MUST be 1 anything else is invalid due to code that usees huristics to fix other possible problems;
  reference_header.dim[7]=1; //This MUST be 1 anything else is invalid due to code that usees huristics to fix other possible problems;
  reference_header.intent_p1=10101010.101F;
  reference_header.intent_p2=987654321.0F;
  reference_header.intent_p3=-1234.0F;
  reference_header.intent_code=NIFTI_INTENT_ESTIMATE;
  reference_header.datatype=DT_INT32;
  reference_header.pixdim[0]=-1.0F; /* this is really qfac */
  reference_header.pixdim[1]=0.25F;
  reference_header.pixdim[2]=0.5F;
  reference_header.pixdim[3]=1.0F;
  reference_header.pixdim[4]=2.0F;
  reference_header.pixdim[5]=4.0F;
  reference_header.pixdim[6]=-2.0e10F;
  reference_header.pixdim[7]=-2.0e10F;
  reference_header.vox_offset=0;
  reference_header.scl_slope=1;
  reference_header.scl_inter=0;
  reference_header.qform_code=NIFTI_XFORM_SCANNER_ANAT;
  reference_header.quatern_b=-0.5F;
  reference_header.quatern_c= 0.5F;
  reference_header.quatern_d=-0.5F;
  reference_header.qoffset_x=reference_header.dim[1]/2.0F;
  reference_header.qoffset_y=reference_header.dim[2]/2.0F;
  reference_header.qoffset_z=reference_header.dim[3]/2.0F;
  reference_header.sform_code=NIFTI_XFORM_SCANNER_ANAT;
  reference_header.srow_x[0]=0.5;
  reference_header.srow_x[1]=0.0;
  reference_header.srow_x[2]=0.0;
  reference_header.srow_x[3]=0.0;
  reference_header.srow_y[0]=0.0;
  reference_header.srow_y[1]=1.0;
  reference_header.srow_y[2]=0.0;
  reference_header.srow_y[3]=0.0;
  reference_header.srow_z[0]=0.0;
  reference_header.srow_z[1]=0.0;
  reference_header.srow_z[2]=2.0;
  reference_header.srow_z[3]=0.0;
  reference_header.magic[0]='n';
  reference_header.magic[1]='+';
  reference_header.magic[2]='1';
  reference_header.magic[3]='\0';
  /* String is purposfully too long */
  strncpy(reference_header.intent_name,"PHANTOM_DATA to be used for regression testing the nifti reader/writer",16);
  strncpy(reference_header.descrip,"This is a very long dialog here to use up more than 80 characters of space to test to see if the code is robust enough to deal appropriatly with very long and obnoxious lines.",80);

  {
  int nbyper;
  int swapsize;
  nifti_datatype_sizes(reference_header.datatype ,&nbyper,&swapsize);
  reference_header.bitpix=nbyper*8;
  }

  nifti_image * reference_image=nifti_convert_nhdr2nim(reference_header,write_image_filename);
  {
  const unsigned int NumVoxels=reference_image->nx*reference_image->ny*reference_image->nz*reference_image->nt*reference_image->nu;
  reference_image->data=(signed int *)calloc(NumVoxels,sizeof(signed int)) ; /*!< pointer to data: nbyper*nvox bytes     */
  PrintTest("Checking memory allocation",reference_image->data ==0 ,NIFTITEST_TRUE,Errors);
  {
  signed int i=0;
  for(; i < (signed int)NumVoxels ; i++)
    {
    ((signed int *)(reference_image->data))[i]=i;
    }
  }
  }
  PrintTest("Setting filenames",nifti_set_filenames( reference_image,write_image_filename, 0, 0 ) != 0, NIFTITEST_TRUE,Errors);
  PrintTest("Setting type from names",nifti_set_type_from_names( reference_image ) != 0, NIFTITEST_TRUE,Errors);
  /*   PrintTest("Checking type and names",nifti_type_and_names_match( reference_image , 1 ) != 1, NIFTITEST_TRUE,Errors); */
  PrintTest("Check reference_image data is non null",(reference_image->data==0),NIFTITEST_TRUE,Errors);
  return reference_image;
}

int
compare_subregion(const int *reference_data,
                  const int *imagedata,
                  int *origin,
                  int *regionsize,
                  int *imagesize)
{
  /*
  ** The origin/regionsize describes what subregion of the image to compare.
  ** in imagedata it is a contiguous range of memory representing a volume, and
  ** in reference_data, the orgin and regionsize describe offsets into the
  ** array.
  */
  int i, j, k;
  const int *ip, *rp;
  for(i = 0; i < regionsize[2]; i++)
    {
    for(j = 0; j < regionsize[1]; j++)
      {
      for(k = 0; k < regionsize[0]; k++)
        {
        int imageIndex =
          (i * regionsize[1] * regionsize[0]) +
          (j * regionsize[0]) + k;
        int referenceIndex =
          ((origin[2] + i) * imagesize[1] * imagesize[0]) +
          ((origin[1] + j) * imagesize[0]) +
          origin[0] + k;
        if(reference_data[referenceIndex] !=
           imagedata[imageIndex])
          return -1;
        }
      }
    }
  return 0;
}

const char *testfilename="testimage.nii.gz";
int main(int argc, char **argv)
{
  int Errors = 0;
  nifti_image *read_image;
  nifti_image *reference_image =
    generate_reference_image(testfilename,&Errors);
  if(Errors != 0)
    {
    printf("Can't create testimage.nii.gz");
    exit(1);
    }
  nifti_image_write(reference_image);
  read_image = nifti_image_read(testfilename,0);
  PrintTest("initial_image_read",read_image == 0,NIFTITEST_TRUE,&Errors);
  {
  void *imagedata = 0;
  int origin[7] = { 2, 1, 1, 0, 0, 0, 0 };
  int regionsize[7] = { 1, 2, 2, 1, 1, 1, 1 };
  PrintTest("read subregion ",
            nifti_read_subregion_image(read_image,
                                       origin,
                                       regionsize,
                                       &imagedata) == -1,
            NIFTITEST_TRUE,&Errors);
  const int *reference_data = (const int *)reference_image->data;
  const int *read_data = (const int *)imagedata;
  PrintTest("compare subregion",
            compare_subregion(reference_data,
                              read_data,
                              origin,
                              regionsize,
                              reference_image->dim+1) != 0,
            NIFTITEST_TRUE,&Errors);
  free(imagedata);
  }
  {
  /* test using a case where nifti_collapsed_region would be called */
  void *imagedata = 0;
  int origin[7] = { 0, 0, 1, 0, 0, 0, 0 };
  int regionsize[7] = { 4, 4, 1, 1, 1, 1, 1 };
  PrintTest("read subregion",
            nifti_read_subregion_image(read_image,
                                       origin,
                                       regionsize,
                                       &imagedata) == -1,
            NIFTITEST_TRUE,&Errors);
  const int *reference_data = (const int *)reference_image->data;
  const int *read_data = (const int *)imagedata;
  PrintTest("compare subregion (collapsed)",
            compare_subregion(reference_data,
                              read_data,
                              origin,
                              regionsize,
                              reference_image->dim+1) != 0,
            NIFTITEST_TRUE,&Errors);
  free(imagedata);
  }
  nifti_image_free(read_image);
  nifti_image_free(reference_image);
  unlink(testfilename);
  exit(0);
}
