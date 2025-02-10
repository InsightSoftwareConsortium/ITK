set(old_module_name "FastBilateral")
set(new_module_name "ITKImageFeature")
message(FATAL_ERROR "Remote module ${old_module_name} has been integrated into ITK proper. It is now part of module ${new_module_name}. Take a look at the migration guide for more information. Migration guide:\nhttps://github.com/InsightSoftwareConsortium/ITK/blob/master/Documentation/docs/migration_guides/itk_6_migration_guide.md#remote-module-integration")
