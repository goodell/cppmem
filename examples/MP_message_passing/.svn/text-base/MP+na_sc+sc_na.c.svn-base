// MP+na_sc+sc_na
// Message Passing, of data held in non-atomic x,
//   with sc atomic stores and loads on y  giving release/acquire synchronisation
// Question: is the read of x required to see the new data value 1
//   rather than the initial state value 0?
int main() {
  int x=0; atomic_int y=0;
  {{{ { x=1;
        y.store(1,memory_order_seq_cst); }
  ||| { r1=y.load(memory_order_seq_cst).readsvalue(1);
        r2=x; }  }}}
  return 0;
}
