// MP+na_rel+con_na
// Message Passing, of data held in non-atomic x,
//   with release/consume synchronisation on y.
// Question: is the read of *p required to see the new data value 1
//   rather than the initial state value 0?
int main() {
  int x=0; atomic_int y; int * p=0;
  {{{ { x=1;
        y.store(&x,memory_order_release); }
  ||| { p=y.load(memory_order_consume).readsvalue(&x);
        r2=*p; }  }}}
  return 0;
}
