itk_module_test()

if(NOT ITK_USE_SYSTEM_HDF5)
  include_directories(
    "${ITKHDF5_SOURCE_DIR}/src/itkhdf5"
    "${ITKHDF5_SOURCE_DIR}/src/itkhdf5/src"
    "${ITKHDF5_BINARY_DIR}/src/itkhdf5"
    )
endif()

set(ITKIOMINCTests
   itkMINCImageIOTest.cxx
   itkMINCImageIOTest2.cxx
   itkMINCImageIOTest4.cxx
   itkMINCImageIOTest_2D.cxx
   itkMINCImageIOTest_4D.cxx
   itkMINCImageIOTest_Labels.cxx
  )

CreateTestDriver(ITKIOMINC "${ITKIOMINC-Test_LIBRARIES}" "${ITKIOMINCTests}")

itk_add_test(NAME itkMINCImageIOTest1
  COMMAND ITKIOMINCTestDriver itkMINCImageIOTest ${ITK_TEST_OUTPUT_DIR} )

itk_add_test(NAME itkMINCImageIOTest2
  COMMAND ITKIOMINCTestDriver itkMINCImageIOTest2
      DATA{Input/t1_z+_byte_cor.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_byte_cor_3.mnc)

itk_add_test(NAME itkMINCImageIOTest3
  COMMAND ITKIOMINCTestDriver
      --compare DATA{Input/t1_z+_byte_cor.mnc}
                ${ITK_TEST_OUTPUT_DIR}/t1_z+_byte_cor_2.mnc
  itkMINCImageIOTest4
      DATA{Input/t1_z+_byte_cor.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_byte_cor_2.mnc)

itk_add_test(NAME itkMINCImageIOTest4
  COMMAND ITKIOMINCTestDriver
      --compare DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw}
               ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mnc
  itkMINCImageIOTest4
      DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mnc)

# Test to make sure that inter-slice normalization was properly dealt with
itk_add_test(NAME itkMINCImageIOTest5
  COMMAND ITKIOMINCTestDriver
      --compare DATA{Input/t1_z+_float_yxz_norm.mnc}
                ${ITK_TEST_OUTPUT_DIR}/t1_z+_ubyte_yxz_nonorm_noParams.mnc
  itkMINCImageIOTest4
      DATA{Input/t1_z+_ubyte_yxz_nonorm.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_ubyte_yxz_nonorm_noParams.mnc)

itk_add_test(NAME itkMINCImageIOTest6
  COMMAND ITKIOMINCTestDriver
      --compare DATA{Input/t1_z+_float_yxz_norm.mnc}
                ${ITK_TEST_OUTPUT_DIR}/t1_z+_float_yxz_nonorm_noParams.mnc
  itkMINCImageIOTest4
      DATA{Input/t1_z+_float_yxz_nonorm.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_float_yxz_nonorm_noParams.mnc)

itk_add_test(NAME itkMINCImageIOTest_2D
  COMMAND ITKIOMINCTestDriver
      --compare DATA{${ITK_DATA_ROOT}/Input/cthead1.tif}
                ${ITK_TEST_OUTPUT_DIR}/cthead1.mnc
  itkMINCImageIOTest_2D
      DATA{${ITK_DATA_ROOT}/Input/cthead1.tif} ${ITK_TEST_OUTPUT_DIR}/cthead1.mnc)


itk_add_test(NAME itkMINCImageIOTest_4D
  COMMAND ITKIOMINCTestDriver
      --compare DATA{Input/dti_sample.mnc}
                ${ITK_TEST_OUTPUT_DIR}/dti_sample.mnc
  itkMINCImageIOTest_4D
      DATA{Input/dti_sample.mnc} ${ITK_TEST_OUTPUT_DIR}/dti_sample.mnc)

itk_add_test(NAME itkMINCImageIOTest_Labels
  COMMAND ITKIOMINCTestDriver
      --compare DATA{Input/labels_sample.mnc}
                ${ITK_TEST_OUTPUT_DIR}/labels_sample.mnc
  itkMINCImageIOTest_Labels
      DATA{Input/labels_sample.mnc} ${ITK_TEST_OUTPUT_DIR}/labels_sample.mnc)

# test different cases

itk_add_test(NAME itkMINCImageIOTest-COM-t1_z+_float_yxz_nonorm
 COMMAND ITKIOMINCTestDriver
 --compare DATA{Input/t1_z+_float_yxz_nonorm.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_float_yxz_nonorm.mnc
 itkMINCImageIOTest4
 DATA{Input/t1_z+_float_yxz_nonorm.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_float_yxz_nonorm.mnc 427621.7839 -8.195741583 72.45998819 -3.148534512 )


itk_add_test(NAME itkMINCImageIOTest-COM-t1_z+_float_yxz_norm
 COMMAND ITKIOMINCTestDriver
 --compare DATA{Input/t1_z+_float_yxz_norm.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_float_yxz_norm.mnc
 itkMINCImageIOTest4
 DATA{Input/t1_z+_float_yxz_norm.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_float_yxz_norm.mnc 427621.7839 -8.195741583 72.45998819 -3.148534512 )


itk_add_test(NAME itkMINCImageIOTest-COM-t1_z+_ubyte_yxz_nonorm
 COMMAND ITKIOMINCTestDriver
 --compare DATA{Input/t1_z+_ubyte_yxz_nonorm.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_ubyte_yxz_nonorm.mnc
 itkMINCImageIOTest4
 DATA{Input/t1_z+_ubyte_yxz_nonorm.mnc} ${ITK_TEST_OUTPUT_DIR}/t1_z+_ubyte_yxz_nonorm.mnc 427621.7838 -8.195741583 72.45998819 -3.148534512 )



# multiple loops because of different numerical parameters

foreach(type byte;short;ubyte)
  foreach(axis cor;sag;trans)
    foreach(plusMinus -;+)
      set(imageName t1_z${plusMinus}_${type}_${axis})
      set(outImage ${ITK_TEST_OUTPUT_DIR}/${imageName}.mnc)

      itk_add_test(NAME itkMINCImageIOTest-COM-${imageName}
        COMMAND ITKIOMINCTestDriver --compare DATA{Input/${imageName}.mnc} ${outImage}
          itkMINCImageIOTest4 DATA{Input/${imageName}.mnc} ${outImage}
          427620.3115 -8.195582241 72.46002233 -3.148594157) # this line is different
    endforeach()
  endforeach()
endforeach()

foreach(type double;float;long;ulong)
  foreach(axis cor;sag;trans)
    foreach(plusMinus -;+)
      set(imageName t1_z${plusMinus}_${type}_${axis})
      set(outImage ${ITK_TEST_OUTPUT_DIR}/${imageName}.mnc)

      itk_add_test(NAME itkMINCImageIOTest-COM-${imageName}
        COMMAND ITKIOMINCTestDriver --compare DATA{Input/${imageName}.mnc} ${outImage}
          itkMINCImageIOTest4 DATA{Input/${imageName}.mnc} ${outImage}
          427590.7631 -8.195995507 72.45943584 -3.148635493) # this line is different
    endforeach()
  endforeach()
endforeach()

foreach(type ushort)
  foreach(axis cor;sag;trans)
    foreach(plusMinus -;+)
      set(imageName t1_z${plusMinus}_${type}_${axis})
      set(outImage ${ITK_TEST_OUTPUT_DIR}/${imageName}.mnc)

      itk_add_test(NAME itkMINCImageIOTest-COM-${imageName}
        COMMAND ITKIOMINCTestDriver --compare DATA{Input/${imageName}.mnc} ${outImage}
          itkMINCImageIOTest4 DATA{Input/${imageName}.mnc} ${outImage}
          427590.7957 -8.195997123 72.45943721 -3.148635961) # this line is different
    endforeach()
  endforeach()
endforeach()
