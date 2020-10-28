from typing import List, Any
import inspect
import importlib
import sys

try:
    # First attempt using convention of build directory
    from pathlib import Path

    wrap_itk_pth: Path = Path(__file__).parent / "WrapITK.pth"

    if not wrap_itk_pth.is_file():
        print(
            "ERROR: itk_generate_pyi.py must be run in the same directory as the WrapITK.pth file"
        )

    with open(wrap_itk_pth, "r") as fid:
        itk_module_paths = [
            itk_module_path.strip() for itk_module_path in fid.readlines()
        ]
        for pp in itk_module_paths:
            if not pp.startswith("#"):
                sys.path.append(pp)
    import itkConfig
except:
    # Second attempt on the standard path
    import itkConfig

itkConfig.LazyLoading = False
itkConfig.DumpInterfaces = True

requested_module_name = "itk"
requested_module = importlib.import_module(requested_module_name)

# Can not dump complete .pyi interface file if LazyLoading is ued
class ITKSignaturesList:
    """
    A pure static class to manage dumping a .pyi file for
    the itk_module.
    """

    _itk_namespace_list: List[str] = []
    _broken_introspection_signatures: List[str] = ["echo", "image", "string", "str"]

    @staticmethod
    def parse_object(obj_name: str, obj: Any):
        # builtin classes do not have introspection signatures.
        if inspect.isbuiltin(obj):
            return
        elif obj_name in ITKSignaturesList._broken_introspection_signatures:
            return
        elif obj_name.startswith("_"):
            return
        elif inspect.isclass(obj):
            ITKSignaturesList._itk_namespace_list.append(f"class {obj_name}:")
            methods_exists: bool = False
            for elem_name, elem_obj in obj.__dict__.items():
                if inspect.ismethod(elem_obj) or inspect.isfunction(elem_obj):
                    methods_exists = True
                    ITKSignaturesList._itk_namespace_list.append(
                        f"    def {elem_name}{inspect.signature(elem_obj)}: ..."
                    )
            if not methods_exists:
                ITKSignaturesList._itk_namespace_list[-1] += " ..."
        elif inspect.isfunction(obj):
            ITKSignaturesList._itk_namespace_list.append(
                f"def {obj_name}{inspect.signature(obj)}: ..."
            )
        # else:
        #     print(f"{obj_name}: {type(obj)}")

    @staticmethod
    def dumps(dump_file: str) -> None:
        with open(dump_file, "w") as fid:
            for ln in ITKSignaturesList._itk_namespace_list:
                fid.write(f"{ln}\n")


# Now iterate through all module items and print of signatures
# of the objects collected.  The generation of a .pyi file
# allows IDE's and other tools to do better introspection
all_items = list(requested_module.__dict__.items())
for k, v in all_items:
    ITKSignaturesList.parse_object(k, v)
ITKSignaturesList.dumps(requested_module.__file__ + "i")
