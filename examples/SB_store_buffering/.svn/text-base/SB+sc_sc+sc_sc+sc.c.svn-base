// SB+sc_sc+sc_sc
// Store Buffering (or Dekker's), with all four accesses SC atomics
// Question: can the two reads both see 0 in the same execution?
int main() {
  atomic_int x=0; atomic_int y=0;
  {{{ { y.store(1,memory_order_seq_cst);
        r1=x.load(memory_order_seq_cst); }
  ||| { x.store(1,memory_order_seq_cst);
        r2=y.load(memory_order_seq_cst); }  }}}
  return 0;
}
