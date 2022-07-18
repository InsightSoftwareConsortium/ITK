# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/
import types
from itk.support import base
from itkConfig import DefaultFactoryLoading as _DefaultFactoryLoading

# Needed to avoid problem with aliasing of itk.set (itkTemplate)
# inside the itk namespace.  We need to explicitly specify the
# use of the builtin set
from builtins import set as _builtin_set

not_loaded: str = "not loaded"


def _lazy_itk_module_reconstructor(module_name, state):
    # Similar to copyreg._reconstructor
    lazy_module = types.ModuleType.__new__(LazyITKModule, state)
    types.ModuleType.__init__(lazy_module, module_name)

    return lazy_module


def _ThreadingRLock(*args, **kwargs):
    """threading RLock"""
    try:
        from threading import RLock

        return RLock(*args, **kwargs)
    except (ImportError, OSError):
        pass


class ITKLazyLoadLock:
    """Need to use a recursive lock for thread ownership
    within the given thread you can acquire a RLock as often as you like.
    Other threads need to wait until this thread releases the resource again.

    A single lock is needed for all lazy loading.  This lock blocks
    across all threads until this thread has completed all its imports
    and dependencies.  The complex inter-relationship, and the recursive
    nature of imports, makes a more fine-grained locking very difficult
    to implement robustly."""

    # global thread lock so no setup required for multithreading.
    # NB: Do not create multiprocessing lock as it sets the multiprocessing
    # context, disallowing `spawn()`/`forkserver()`
    th_lock = _ThreadingRLock()

    def __init__(self):
        cls = type(self)
        root_lock = cls.th_lock
        if root_lock is not None:
            root_lock.acquire()
        cls.create_mp_lock()
        self.locks = [lk for lk in [cls.mp_lock, cls.th_lock] if lk is not None]
        if root_lock is not None:
            root_lock.release()

    def acquire(self, *a, **k):
        for lock in self.locks:
            lock.acquire(*a, **k)

    def release(self):
        for lock in self.locks[::-1]:  # Release in inverse order of acquisition
            lock.release()

    def __enter__(self):
        self.acquire()

    def __exit__(self, *exc):
        self.release()

    @classmethod
    def create_mp_lock(cls):
        if not hasattr(cls, "mp_lock"):
            try:
                from multiprocessing import RLock

                cls.mp_lock = RLock()
            except (ImportError, OSError):
                cls.mp_lock = None


class LazyITKModule(types.ModuleType):

    """Subclass of ModuleType that implements a custom __getattribute__ method
    to allow lazy-loading of attributes from ITK sub-modules."""

    def __init__(self, name, lazy_attributes):
        types.ModuleType.__init__(self, name)
        for k, v in lazy_attributes.items():
            base.itk_base_global_lazy_attributes.setdefault(k, _builtin_set()).update(v)
        self.__belong_lazy_attributes = {
            k: v[0] for k, v in lazy_attributes.items() if len(v) > 0
        }
        for k in lazy_attributes:
            setattr(self, k, not_loaded)  # use default known value
        # For PEP 366
        setattr(self, "__package__", "itk")
        setattr(self, "itk_base_global_lazy_attributes", lazy_attributes)
        setattr(self, "loaded_lazy_modules", _builtin_set())

    @classmethod
    def set_lock(cls, lock):
        """Set the global lock."""
        cls._lock = lock

    @classmethod
    def get_lock(cls):
        """Get the global lock. Construct it if it does not exist."""
        if not hasattr(cls, "_lock"):
            cls._lock = ITKLazyLoadLock()
        return cls._lock

    def __getattribute__(self, attr):
        value = types.ModuleType.__getattribute__(self, attr)
        if value is not_loaded:
            with type(self).get_lock():  # All but one thread will block here.
                if value is not_loaded:
                    # Only the first thread needs to run this code, all other blocked threads skip
                    module = self.__belong_lazy_attributes[attr]
                    namespace = {}
                    base.itk_load_swig_module(module, namespace)
                    self.loaded_lazy_modules.add(module)
                    for k, v in namespace.items():
                        setattr(self, k, v)
                    value = namespace[attr]
                    if _DefaultFactoryLoading:
                        base.load_module_needed_factories(module)
                else:  # one of the other threads that had been blocking
                    # waiting for first thread to complete. Now the
                    # attribute is REQUIRED to be available
                    # can just fall through now.
                    value = types.ModuleType.__getattribute__(self, attr)
                    assert value is not not_loaded
        return value

    # For pickle support
    def __reduce_ex__(self, proto):
        state = self.__getstate__()
        return _lazy_itk_module_reconstructor, (self.__name__, state), state

    # For pickle support
    def __getstate__(self):
        state = self.__dict__.copy()
        lazy_modules = list()
        for key in self.itk_base_global_lazy_attributes:
            if isinstance(state[key], LazyITKModule):
                lazy_modules.append((key, state[key].itk_base_global_lazy_attributes))
            state[key] = not_loaded
        state["lazy_modules"] = lazy_modules

        return state

    # For pickle support
    def __setstate__(self, state):
        self.__dict__.update(state)
        for module_name, lazy_attributes in state["lazy_modules"]:
            self.__dict__.update(
                {module_name: LazyITKModule(module_name, lazy_attributes)}
            )
        for module in state["loaded_lazy_modules"]:
            namespace = {}
            base.itk_load_swig_module(module, namespace)
            for k, v in namespace.items():
                setattr(self, k, v)
            base.load_module_needed_factories(module)
