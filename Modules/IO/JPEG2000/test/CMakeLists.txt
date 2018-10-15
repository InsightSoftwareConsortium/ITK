itk_module_test()
set(ITKIOJPEG2000Tests
itkJPEG2000ImageIOFactoryTest01.cxx
itkJPEG2000ImageIORegionOfInterest.cxx
itkJPEG2000ImageIOTest00.cxx
itkJPEG2000ImageIOTest01.cxx
itkJPEG2000ImageIOTest02.cxx
itkJPEG2000ImageIOTest03.cxx
itkJPEG2000ImageIOTest04.cxx
itkJPEG2000ImageIOTest05.cxx
itkJPEG2000ImageIOTest06.cxx
)

CreateTestDriver(ITKIOJPEG2000  "${ITKIOJPEG2000-Test_LIBRARIES}" "${ITKIOJPEG2000Tests}")
itk_add_test(NAME itkJPEG2000Test00
  COMMAND ITKIOJPEG2000TestDriver itkJPEG2000ImageIORegionOfInterest
  DATA{Input/Bretagne1.j2k} ${ITK_TEST_OUTPUT_DIR}/Bretagne1_RegionTest01.tif 160 120 320 240)
itk_add_test(NAME itkJPEG2000Test01
  COMMAND ITKIOJPEG2000TestDriver itkJPEG2000ImageIOTest03
  DATA{Input/Bretagne1.j2k} ${ITK_TEST_OUTPUT_DIR}/Bretagne1_01.png)
itk_add_test(NAME itkJPEG2000Test02
  COMMAND ITKIOJPEG2000TestDriver itkJPEG2000ImageIOTest03
  DATA{Input/Cevennes2.jp2} ${ITK_TEST_OUTPUT_DIR}/Cevennes2_01.png)
itk_add_test(NAME itkJPEG2000Test03
  COMMAND ITKIOJPEG2000TestDriver itkJPEG2000ImageIOTest03
  ${ITK_TEST_OUTPUT_DIR}/Bretagne1_RegionTest01.tif ${ITK_TEST_OUTPUT_DIR}/Bretagne1_02.j2k)
set_tests_properties( itkJPEG2000Test03 PROPERTIES
  DEPENDS itkJPEG2000Test00
  REQUIRED_FILES ${ITK_TEST_OUTPUT_DIR}/Bretagne1_RegionTest01.tif )

itk_add_test(NAME itkJPEG2000Test05
  COMMAND ITKIOJPEG2000TestDriver itkJPEG2000ImageIOTest05
  DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR} jp2)
itk_add_test(NAME itkJPEG2000Test06
  COMMAND ITKIOJPEG2000TestDriver
  --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1-unitspacing.tif}
  ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
  itkJPEG2000ImageIOTest06 DATA{Input/cthead1.j2k} ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)
