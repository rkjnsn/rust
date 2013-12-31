extern "C" void rust_cxx_throw() {
  throw 0;
}

typedef void *(rust_try_fn)(void*, void*);

extern "C" void
rust_cxx_try(rust_try_fn f, void *fptr, void *env) {
  try {
	f(fptr, env);
  } catch (int t) {
  }
}
