// We need to forward routine registration from C to Rust
// to avoid the linker removing the static library.

void R_init_rbm25_extendr(void *dll);

void R_init_rbm25(void *dll) {
    R_init_rbm25_extendr(dll);
}
