// MP+na_rel+acq_na
// Message Passing, of data held in non-atomic x,
//   with release/acquire synchronisation on y.
// Question: is the read of x required to see the new data value 1
//   rather than the initial state value 0?
int main() {
  int x=0; atomic_int y=0;
  {{{ { x=1;
        y.store(1,memory_order_release); }
  ||| { r1=y.load(memory_order_acquire).readsvalue(1);
        r2=x; }  }}}
  return 0;
}
