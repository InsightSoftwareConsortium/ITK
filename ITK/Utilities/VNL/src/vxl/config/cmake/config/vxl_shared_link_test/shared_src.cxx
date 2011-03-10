
void vxl_static_test_function(int i);

void vxl_shared_test_function(int i)
{
  vxl_static_test_function(i);
}
